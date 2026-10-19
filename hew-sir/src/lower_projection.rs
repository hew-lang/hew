//! Produce exact field places and preserve their partitions across SSA edges.
//! Availability and cleanup remain decisions of the ordinary lifetime verifier.

use super::{BindingPlace, Builder};
use crate::ownership::TypeFactTable;
use crate::{
    aggregate_field_recipes, AggregateShapeRef, Operand, OwnKind, OwnerRoot, PlaceBase, PlaceDecl,
    PlaceId, PlaceOrigin, Provenance, SemAggregateShape, SemFunction, SemOpKind, ValueId,
};
use hew_hir::HirExpr;
use hew_types::ResolvedTy;
use std::collections::{BTreeMap, BTreeSet};

/// An owner [`Builder::open_owner`] took apart around one field: its seat and
/// the sibling fields of every level, outermost first.
#[derive(Clone)]
pub(super) struct OpenOwner {
    root: PlaceId,
    levels: Vec<(ResolvedTy, AggregateShapeRef, usize, Vec<crate::ValueDef>)>,
}

impl OpenOwner {
    /// The sibling fields that stay live while the owner is open.
    pub(super) fn siblings(&self) -> impl Iterator<Item = ValueId> + '_ {
        self.levels.iter().flat_map(|(_, _, index, fields)| {
            fields
                .iter()
                .enumerate()
                .filter(move |(at, _)| at != index)
                .map(|(_, field)| field.id)
        })
    }
}

impl Builder<'_, '_> {
    pub(super) fn expression_projection(
        &mut self,
        expression: &HirExpr,
    ) -> Result<Option<PlaceId>, String> {
        let Some(place) = self.resolve_binding_place(expression)? else {
            return Ok(None);
        };
        self.owned_projection(&place)
    }

    pub(super) fn owned_projection(
        &mut self,
        place: &BindingPlace,
    ) -> Result<Option<PlaceId>, String> {
        // A resource's release owns its members as a whole. Reads through it
        // use aggregate loans, rather than inventing independently owned field
        // places beneath the resource. An enclosing ordinary record may still
        // partition the resource itself alongside its other fields.
        if place
            .projections
            .iter()
            .any(|(_, shape, _)| self.is_marked_record(*shape))
        {
            return Ok(None);
        }
        let target = self.binding_target(place.binding)?;
        if self.target_ty(target)? != place.root_ty {
            return Err("aggregate projection changed its exact binding type".into());
        }
        let base = match target {
            super::BindingTarget::Place(root) => {
                // A capture belongs to its environment and a state field to
                // its actor, not to this body's local aggregate partition.
                // Projected reads must borrow through that owner instead of
                // declaring independently initialized aggregate places
                // beneath its field.
                if !place.projections.is_empty()
                    && matches!(
                        self.places[root.0 as usize].origin,
                        PlaceOrigin::Capture { .. }
                            | PlaceOrigin::Runtime
                            | PlaceOrigin::ActorState { .. }
                    )
                {
                    return Ok(None);
                }
                PlaceBase::Place(root)
            }
            super::BindingTarget::Value(root) => {
                if place.projections.is_empty() || self.value_own_kind(root) != Some(OwnKind::Owned)
                {
                    return Ok(None);
                }
                if !self.owned_live.contains_key(&root) {
                    return Err("aggregate projection has no live SSA owner".into());
                }
                PlaceBase::Value(root)
            }
        };
        let path = place
            .projections
            .iter()
            .map(|(_, shape, field)| {
                u32::try_from(*field)
                    .map(|field| (*shape, field))
                    .map_err(|_| "aggregate field exceeds u32".to_string())
            })
            .collect::<Result<Vec<_>, _>>()?;
        declare_path(
            &mut self.places,
            base,
            &place.root_ty,
            &path,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )
        .map(Some)
    }

    fn is_marked_record(&self, shape: AggregateShapeRef) -> bool {
        matches!(shape, AggregateShapeRef::Record(id)
            if self.service.aggregate_shapes[id.0 as usize].marker
                != hew_types::DeclarationMarker::None)
    }

    /// The seat a mutation beneath `place` takes whole: `place` lies beneath
    /// a root this body does not partition into field places - an actor state
    /// seat, a capture, or a record that keeps one whole owner. The mutation
    /// opens the owner with [`Self::open_owner`] and closes it again around
    /// the changed field, so the seat is whole wherever a fault can reach.
    pub(super) fn whole_owner_root(
        &mut self,
        place: &BindingPlace,
    ) -> Result<Option<PlaceId>, String> {
        if place.projections.is_empty() {
            return Ok(None);
        }
        let super::BindingTarget::Place(root) = self.binding_target(place.binding)? else {
            return Ok(None);
        };
        if self.owned_projection(place)?.is_some() {
            return Ok(None);
        }
        Ok(Some(root))
    }

    /// Take the owner `place` is rooted at whole and take it apart down to the
    /// field `place` names, which is returned. The caller closes the owner
    /// again with [`Self::close_owner`] before anything but the one mutation
    /// it opened the owner for can fault, and on every edge that mutation
    /// leaves by.
    pub(super) fn open_owner(
        &mut self,
        root: PlaceId,
        place: &BindingPlace,
        provenance: &Provenance,
    ) -> Result<(ValueId, OpenOwner), String> {
        let mut value = self.emit_typed(
            provenance.clone(),
            &place.root_ty,
            SemOpKind::LoadTake { place: root },
        )?;
        if matches!(
            self.places[root.0 as usize].origin,
            PlaceOrigin::ActorState { .. }
        ) || self.in_var_self_receiver(root)
        {
            self.state_taken.insert(root);
        }
        let mut levels = Vec::with_capacity(place.projections.len());
        for (ty, shape, index) in &place.projections {
            let fields = self.emit_destructure_value(value, ty, *shape, provenance.clone())?;
            value = fields[*index].id;
            levels.push((ty.clone(), *shape, *index, fields));
        }
        Ok((value, OpenOwner { root, levels }))
    }

    /// Rebuild an opened owner around `field` and re-initialize its seat.
    pub(super) fn close_owner(
        &mut self,
        owner: OpenOwner,
        field: ValueId,
        provenance: Provenance,
    ) -> Result<(), String> {
        let mut rebuilt = field;
        for (ty, shape, index, fields) in owner.levels.into_iter().rev() {
            let fields = fields
                .into_iter()
                .enumerate()
                .map(|(at, field)| Operand {
                    value: if at == index { rebuilt } else { field.id },
                })
                .collect::<Vec<_>>();
            for field in &fields {
                self.owned_live.remove(&field.value);
            }
            rebuilt = self.emit_typed(
                provenance.clone(),
                &ty,
                SemOpKind::AggregateMake { shape, fields },
            )?;
        }
        self.restore_taken_place(owner.root, rebuilt, provenance)
    }

    /// An owned copy of the field `place` names beneath a whole owner, or of
    /// the owner itself, read through aggregate loans that end before this
    /// returns. A mutation runs on the copy, so a fault leaves the owner as
    /// it was. `why` names the mutation that cannot open the owner in place.
    pub(super) fn copy_through_whole_owner(
        &mut self,
        root: PlaceId,
        place: &BindingPlace,
        provenance: &Provenance,
        why: &str,
    ) -> Result<ValueId, String> {
        if self
            .service
            .checked_facts
            .rows()
            .get(&hew_types::TypeInstanceKey(place.leaf_ty.clone()))
            .is_none_or(|facts| facts.clone == hew_types::CloneKind::None)
        {
            let leaf = place.leaf_ty.user_facing();
            return Err(format!(
                "mutating a `{leaf}` held by an actor state field, a capture or a resource is \
                 not implemented for {why}: the mutation works on a copy, and `{leaf}` has none"
            ));
        }
        let mut loans = vec![self.emit_typed(
            provenance.clone(),
            &place.root_ty,
            SemOpKind::LoadBorrow { place: root },
        )?];
        let field_tys = place
            .projections
            .iter()
            .skip(1)
            .map(|(ty, _, _)| ty)
            .chain(std::iter::once(&place.leaf_ty))
            .cloned()
            .collect::<Vec<_>>();
        for ((_, shape, field), ty) in place.projections.iter().zip(field_tys) {
            let aggregate = Operand {
                value: *loans.last().expect("a loan roots the projection"),
            };
            let field = u32::try_from(*field).map_err(|_| "aggregate field exceeds u32")?;
            loans.push(self.emit_typed(
                provenance.clone(),
                &ty,
                SemOpKind::AggregateProjectBorrow {
                    shape: *shape,
                    aggregate,
                    field,
                },
            )?);
        }
        let leaf = *loans.last().expect("a loan names the leaf");
        let copy = self.emit_typed(
            provenance.clone(),
            &place.leaf_ty,
            SemOpKind::CopyValue {
                source: Operand { value: leaf },
            },
        )?;
        self.end_call_loans(&loans)?;
        Ok(copy)
    }

    /// Assign a field beneath a whole owner. The owner is opened around the
    /// field and closed again around the replacement, and the replaced value
    /// is released only once the owner is whole again, so a faulting release
    /// leaves a complete owner for cleanup.
    pub(super) fn assign_through_whole_owner(
        &mut self,
        root: PlaceId,
        place: &BindingPlace,
        replacement: ValueId,
        provenance: Provenance,
    ) -> Result<(), String> {
        let (replaced, owner) = self.open_owner(root, place, &provenance)?;
        self.close_owner(owner, replacement, provenance)?;
        if self.owned_live.contains_key(&replaced) {
            self.emit_destroy(replaced)?;
        }
        Ok(())
    }

    pub(super) fn value_projection_place(
        &mut self,
        owner: ValueId,
        ty: &ResolvedTy,
        projections: &[super::AggregateSelection],
    ) -> Result<PlaceId, String> {
        let path = projections
            .iter()
            .map(|(_, shape, field)| {
                u32::try_from(*field)
                    .map(|field| (*shape, field))
                    .map_err(|_| "aggregate field exceeds u32".to_string())
            })
            .collect::<Result<Vec<_>, _>>()?;
        declare_path(
            &mut self.places,
            PlaceBase::Value(owner),
            ty,
            &path,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )
    }

    /// Replace one projected field inside an owning SSA value.
    ///
    /// The owner is not a place, so its selections are declared against the
    /// value itself; the store releases the field it replaces.
    pub(super) fn assign_through_owned_value(
        &mut self,
        owner: ValueId,
        owner_ty: &ResolvedTy,
        projections: &[super::AggregateSelection],
        replacement: ValueId,
        provenance: Provenance,
    ) -> Result<(), String> {
        let leaf = self.value_projection_place(owner, owner_ty, projections)?;
        self.store_projected(leaf, replacement, provenance)
    }

    /// Re-publish a seat this body took: the take emptied it, so there is no
    /// previous value to release and the store is an initialization.
    pub(super) fn restore_taken_place(
        &mut self,
        place: PlaceId,
        value: ValueId,
        provenance: Provenance,
    ) -> Result<(), String> {
        self.emit_place_operation(
            SemOpKind::StoreInit {
                place,
                value: Operand { value },
            },
            provenance,
        )?;
        self.owned_live.remove(&value);
        self.state_taken.remove(&place);
        Ok(())
    }

    pub(super) fn store_projected(
        &mut self,
        place: PlaceId,
        value: ValueId,
        provenance: Provenance,
    ) -> Result<(), String> {
        self.note_release_may_fault(&self.places[place.0 as usize].ty.clone());
        self.emit_place_operation(
            SemOpKind::StoreAssign {
                place,
                value: Operand { value },
            },
            provenance,
        )?;
        self.owned_live.remove(&value);
        self.state_taken.remove(&place);
        if self.cleanup_may_fail && !self.cleanup_draining {
            self.dispatch_value_cleanup()?;
        }
        Ok(())
    }
}

/// Expanding an aggregate always declares every sibling. An intermediate
/// field's availability is derived from its children, never another owner.
fn declare_path(
    places: &mut Vec<PlaceDecl>,
    mut base: PlaceBase,
    root_ty: &ResolvedTy,
    path: &[(AggregateShapeRef, u32)],
    shapes: &[SemAggregateShape],
    facts: &TypeFactTable,
) -> Result<PlaceId, String> {
    let mut ty = root_ty.clone();
    for &(shape, selected) in path {
        let recipes = aggregate_field_recipes(shape, &ty, shapes, facts)?;
        if selected as usize >= recipes.len() {
            return Err("aggregate projection field exceeds its exact shape".into());
        }
        let mut selected_place = None;
        for (field, recipe) in recipes.into_iter().enumerate() {
            let field = u32::try_from(field).map_err(|_| "aggregate field exceeds u32")?;
            let origin = PlaceOrigin::Aggregate { base, shape, field };
            let id = if let Some(existing) = places.iter().find(|place| place.origin == origin) {
                if existing.ty != recipe.ty {
                    return Err("aggregate projection changed its exact field type".into());
                }
                existing.id
            } else {
                let id =
                    PlaceId(u32::try_from(places.len()).map_err(|_| "place count exceeds u32")?);
                places.push(PlaceDecl {
                    id,
                    ty: recipe.ty.clone(),
                    origin,
                });
                id
            };
            if field == selected {
                selected_place = Some(id);
                ty = recipe.ty;
            }
        }
        base = PlaceBase::Place(
            selected_place.ok_or_else(|| "aggregate field disappeared".to_string())?,
        );
    }
    match base {
        PlaceBase::Place(place) => Ok(place),
        PlaceBase::Value(_) => Err("SSA aggregate projection has an empty field path".into()),
    }
}

/// Publish the same structural field partition for every connected SSA root
/// version. This moves no ownership and computes no initialization state.
pub(super) fn complete_edge_partitions(
    function: &mut SemFunction,
    shapes: &[SemAggregateShape],
    facts: &TypeFactTable,
) -> Result<(), String> {
    if !function
        .places
        .iter()
        .any(|place| matches!(place.origin, PlaceOrigin::Aggregate { .. }))
    {
        return Ok(());
    }
    let mut types = BTreeMap::new();
    for param in &function.params {
        types.insert(param.value, (param.own, param.ty.clone()));
    }
    for block in &function.blocks {
        for arg in &block.args {
            types.insert(arg.value, (arg.own, arg.ty.clone()));
        }
        for result in block.ops.iter().flat_map(|op| &op.results) {
            types.insert(result.id, (result.own, result.ty.clone()));
        }
        block.terminator.visit_results(|result| {
            types.insert(result.id, (result.own, result.ty.clone()));
        });
    }
    let mut edges = BTreeSet::new();
    let targets = function
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect::<std::collections::BTreeMap<_, _>>();
    for block in &function.blocks {
        block.terminator.visit_successors(|edge| {
            if let Some(target) = targets.get(&edge.target) {
                for (source, destination) in edge.args.iter().zip(&target.args) {
                    if types
                        .get(&source.value)
                        .is_some_and(|(own, _)| *own == OwnKind::Owned)
                        && destination.own == OwnKind::Owned
                    {
                        edges.insert((source.value, destination.value));
                        edges.insert((destination.value, source.value));
                    }
                }
            }
        });
    }
    loop {
        let before = function.places.len();
        for &(source, destination) in &edges {
            let paths = function
                .places
                .iter()
                .filter_map(|place| {
                    if !matches!(place.origin, PlaceOrigin::Aggregate { .. }) {
                        return None;
                    }
                    match crate::projection::place_path(&function.places, place.id) {
                        Ok((OwnerRoot::Value(root), path)) if root == source => Some(Ok(path
                            .into_iter()
                            .map(|step| (step.shape, step.field))
                            .collect::<Vec<_>>())),
                        Ok(_) => None,
                        Err(error) => Some(Err(error)),
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            let (_, ty) = &types[&destination];
            for path in paths {
                declare_path(
                    &mut function.places,
                    PlaceBase::Value(destination),
                    ty,
                    &path,
                    shapes,
                    facts,
                )?;
            }
        }
        if function.places.len() == before {
            return Ok(());
        }
    }
}
