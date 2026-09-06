//! Produce exact field places and preserve their partitions across SSA edges.
//! Availability and cleanup remain decisions of the ordinary lifetime verifier.

use super::{BindingPlace, Builder};
use crate::ownership::TypeFactTable;
use crate::{
    aggregate_field_recipes, AggregateShapeRef, Operand, OwnKind, OwnerRoot, PlaceBase, PlaceDecl,
    PlaceId, PlaceOrigin, Provenance, SemAggregateShape, SemFunction, SemOp, SemOpKind, ValueId,
};
use hew_hir::HirExpr;
use hew_types::ResolvedTy;
use std::collections::{BTreeMap, BTreeSet};

impl Builder<'_, '_> {
    pub(super) fn expression_projection(
        &mut self,
        expression: &HirExpr,
    ) -> Result<Option<(PlaceId, ValueId)>, String> {
        let Some(place) = self.resolve_binding_place(expression)? else {
            return Ok(None);
        };
        self.owned_projection(&place)
    }

    pub(super) fn owned_projection(
        &mut self,
        place: &BindingPlace,
    ) -> Result<Option<(PlaceId, ValueId)>, String> {
        if place.projections.is_empty() {
            return Ok(None);
        }
        let Some(&root) = self.bindings.get(&place.binding) else {
            return Ok(None);
        };
        if self.value_own_kind(root) != Some(OwnKind::Owned) {
            return Ok(None);
        }
        if self.value_ty(root).as_ref() != Some(&place.root_ty)
            || !self.owned_live.contains_key(&root)
        {
            return Err("aggregate projection has no live, exactly typed binding root".into());
        }
        let path = place
            .projections
            .iter()
            .map(|(_, shape, field)| {
                u32::try_from(*field)
                    .map(|field| (*shape, field))
                    .map_err(|_| "aggregate field index exceeds u32".to_string())
            })
            .collect::<Result<Vec<_>, _>>()?;
        let selected = declare_path(
            &mut self.places,
            root,
            &place.root_ty,
            &path,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        Ok(Some((selected, root)))
    }

    pub(super) fn store_projected(
        &mut self,
        place: PlaceId,
        value: ValueId,
        provenance: Provenance,
    ) -> Result<(), String> {
        let operation = SemOp {
            id: crate::OpId(self.ops),
            results: Vec::new(),
            kind: SemOpKind::StoreAssign {
                place,
                value: Operand { value },
            },
            provenance,
        };
        self.current_block_mut().append_op(operation)?;
        self.ops += 1;
        self.owned_live.remove(&value);
        Ok(())
    }
}

/// Expanding an aggregate always declares every sibling. An intermediate
/// field's availability is derived from its children, never another owner.
fn declare_path(
    places: &mut Vec<PlaceDecl>,
    root: ValueId,
    root_ty: &ResolvedTy,
    path: &[(AggregateShapeRef, u32)],
    shapes: &[SemAggregateShape],
    facts: &TypeFactTable,
) -> Result<PlaceId, String> {
    let mut parent = None;
    let mut ty = root_ty.clone();
    for &(shape, selected) in path {
        let recipes = aggregate_field_recipes(shape, &ty, shapes, facts)?;
        if selected as usize >= recipes.len() {
            return Err("aggregate projection field exceeds its exact shape".into());
        }
        let mut selected_place = None;
        for (field, recipe) in recipes.into_iter().enumerate() {
            let field = u32::try_from(field).map_err(|_| "aggregate field exceeds u32")?;
            let origin = PlaceOrigin::Aggregate {
                base: parent.map_or(PlaceBase::Value(root), PlaceBase::Place),
                shape,
                field,
            };
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
        parent = selected_place;
    }
    parent.ok_or_else(|| "aggregate projection has an empty field path".into())
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
    for block in &function.blocks {
        block.terminator.visit_successors(|edge| {
            if let Some(target) = function.blocks.iter().find(|block| block.id == edge.target) {
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
                declare_path(&mut function.places, destination, ty, &path, shapes, facts)?;
            }
        }
        if function.places.len() == before {
            return Ok(());
        }
    }
}
