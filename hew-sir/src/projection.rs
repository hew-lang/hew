//! Exact local storage and aggregate projections with complete content partitions.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use hew_types::{ResolvedTy, TypeInstanceKey};

use crate::ownership::TypeFactTable;
use crate::{
    aggregate_field_recipes, AggregateFieldRecipe, AggregateShapeRef, OwnKind, OwnerRoot,
    PlaceBase, PlaceDecl, PlaceId, PlaceOrigin, SemAggregateShape, SemFunction, SemOp, SemOpKind,
    ValueId,
};

/// One declaration-order selection in a typed aggregate path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct AggregateProjectionStep {
    pub shape: AggregateShapeRef,
    pub field: u32,
}

/// One exact selection, including a local root with an empty path.
/// Content availability is the availability of all `leaves`.
/// Intermediate projections have no independent initialized bit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AggregateProjection {
    pub place: PlaceId,
    pub root: OwnerRoot,
    pub path: Vec<AggregateProjectionStep>,
    pub recipe: AggregateFieldRecipe,
    /// Complete, non-overlapping partition in declaration order. Cleanup
    /// visits it in reverse order and destroys only initialized contents.
    pub leaves: Vec<PlaceId>,
}

/// A checked snapshot of one function's semantic storage relationships.
///
/// SSA roots retain their owned obligation; local roots have storage activity.
/// Content availability lives in
/// SIR's lifetime flow, not in this structural query. Physical lowering uses
/// these same paths, recipes and edge mappings; it does not rediscover them
/// through source names or storage offsets. Rebuild after changing places or
/// CFG root versions.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PlacePlan {
    projections: BTreeMap<PlaceId, AggregateProjection>,
    bases: BTreeMap<PlaceId, PlaceBase>,
    roots: BTreeMap<OwnerRoot, Vec<PlaceId>>,
}

impl PlacePlan {
    /// Immediate declared dependency of a projection or capture place.
    #[must_use]
    pub fn base(&self, place: PlaceId) -> Option<PlaceBase> {
        self.bases.get(&place).copied()
    }

    #[must_use]
    pub fn projection(&self, place: PlaceId) -> Option<&AggregateProjection> {
        self.projections.get(&place)
    }

    /// Every local or projected SSA root and its complete content partition.
    pub fn roots(&self) -> impl Iterator<Item = (OwnerRoot, &[PlaceId])> {
        self.roots
            .iter()
            .map(|(&root, leaves)| (root, leaves.as_slice()))
    }

    #[must_use]
    pub fn leaves(&self, root: OwnerRoot) -> Option<&[PlaceId]> {
        self.roots.get(&root).map(Vec::as_slice)
    }

    /// Exact source/destination leaf identities for an internal CFG transfer.
    ///
    /// # Errors
    /// Refuses a transfer that loses or invents a projection partition.
    pub fn transfer(
        &self,
        source: ValueId,
        destination: ValueId,
    ) -> Result<Vec<(PlaceId, PlaceId)>, String> {
        match (
            self.leaves(OwnerRoot::Value(source)),
            self.leaves(OwnerRoot::Value(destination)),
        ) {
            (None, None) => Ok(Vec::new()),
            (Some(source), Some(destination)) if source.len() == destination.len() => source
                .iter()
                .zip(destination)
                .map(|(&source, &destination)| {
                    let before = &self.projections[&source];
                    let after = &self.projections[&destination];
                    if before.path != after.path || before.recipe != after.recipe {
                        return Err(
                            "aggregate edge changes its complete projection partition".into()
                        );
                    }
                    Ok((source, destination))
                })
                .collect(),
            _ => Err("aggregate edge loses or invents projected field state".into()),
        }
    }
}

/// Resolve the structural owner without inventing an SSA lifetime for a place.
/// Type/descriptor admission remains in `place_plan`.
pub(crate) fn place_path(
    places: &[PlaceDecl],
    id: PlaceId,
) -> Result<(OwnerRoot, Vec<AggregateProjectionStep>), String> {
    let mut next = PlaceBase::Place(id);
    let mut fields = Vec::new();
    let mut seen = BTreeSet::new();
    let root =
        loop {
            match next {
                PlaceBase::Value(value) => break OwnerRoot::Value(value),
                PlaceBase::Place(id) => {
                    if !seen.insert(id) {
                        return Err("aggregate place has a cyclic parent path".into());
                    }
                    let place = places
                        .iter()
                        .find(|place| place.id == id)
                        .ok_or_else(|| "aggregate place has an unknown parent".to_string())?;
                    match place.origin {
                        PlaceOrigin::Local => break OwnerRoot::Local(id),
                        PlaceOrigin::Aggregate { base, shape, field } => {
                            fields.push(AggregateProjectionStep { shape, field });
                            next = base;
                        }
                        _ => return Err(
                            "aggregate place parent is not an owned local or aggregate projection"
                                .into(),
                        ),
                    }
                }
            }
        };
    fields.reverse();
    Ok((root, fields))
}

fn value_definitions(function: &SemFunction) -> BTreeMap<ValueId, (OwnKind, ResolvedTy)> {
    let mut values = BTreeMap::new();
    for param in &function.params {
        values.insert(param.value, (param.own, param.ty.clone()));
    }
    for block in &function.blocks {
        for arg in &block.args {
            values.insert(arg.value, (arg.own, arg.ty.clone()));
        }
        for result in block.ops.iter().flat_map(|op| &op.results) {
            values.insert(result.id, (result.own, result.ty.clone()));
        }
        block.terminator.visit_results(|result| {
            values.insert(result.id, (result.own, result.ty.clone()));
        });
    }
    values
}

/// Resolve every aggregate projection and verify complete field coverage.
///
/// Each expanded aggregate declares every immediate field exactly once;
/// nested expansion replaces a parent's availability bit with its complete
/// child partition. SSA root versions on CFG edges carry identical partitions.
///
/// # Errors
/// Refuses borrowed roots, invalid paths/descriptors/types, incomplete field
/// coverage and CFG transfers that change the availability partition.
pub fn place_plan(
    function: &SemFunction,
    shapes: &[SemAggregateShape],
    facts: &TypeFactTable,
) -> Result<PlacePlan, String> {
    let values = value_definitions(function);
    let mut plan = PlacePlan::default();
    let mut ids = BTreeSet::new();
    let mut paths = BTreeSet::new();
    for place in &function.places {
        if !ids.insert(place.id) {
            return Err("projected places have duplicate identities".into());
        }
        match place.origin {
            PlaceOrigin::Capture { environment, .. }
            | PlaceOrigin::ActorState {
                state: environment, ..
            } => {
                plan.bases.insert(place.id, PlaceBase::Value(environment));
                continue;
            }
            PlaceOrigin::Runtime => {
                return Err("runtime places have no admitted lifetime contract".into());
            }
            PlaceOrigin::Local => {}
            PlaceOrigin::Aggregate { base, .. } => {
                plan.bases.insert(place.id, base);
            }
        }
        let projection = resolve_projection(function, place, &values, shapes, facts)?;
        if !paths.insert((projection.root, projection.path.clone())) {
            return Err("aggregate projection has duplicate places for one field".into());
        }
        plan.projections.insert(place.id, projection);
    }
    let (expanded, whole) = verify_partition_coverage(function, &plan, &values, shapes, facts)?;
    install_partitions(&mut plan, &expanded, &whole);
    for block in &function.blocks {
        let mut failure = None;
        block.terminator.visit_successors(|edge| {
            let Some(target) = function.blocks.iter().find(|block| block.id == edge.target) else {
                return;
            };
            for (source, destination) in edge.args.iter().zip(&target.args) {
                if let Err(reason) = plan.transfer(source.value, destination.value) {
                    failure = Some(reason);
                }
            }
        });
        if let Some(failure) = failure {
            return Err(failure);
        }
    }
    Ok(plan)
}

fn plain_field_recipes(
    shape: AggregateShapeRef,
    ty: &ResolvedTy,
    shapes: &[SemAggregateShape],
    facts: &TypeFactTable,
) -> Result<Vec<AggregateFieldRecipe>, String> {
    let recipes = aggregate_field_recipes(shape, ty, shapes, facts)?;
    if let AggregateShapeRef::Record(id) = shape {
        let descriptor = shapes
            .get(id.0 as usize)
            .ok_or_else(|| "aggregate ancestor has no exact descriptor".to_string())?;
        if matches!(
            ty,
            ResolvedTy::Named {
                is_opaque: true,
                ..
            }
        ) {
            return Err("aggregate projection cannot traverse an opaque ancestor".into());
        }
        if ty.nominal_instance().as_ref() != Some(&descriptor.instance) {
            return Err("aggregate ancestor descriptor has a different nominal identity".into());
        }
        // A `#[resource]` record's fields are ordinary members: its own
        // `close` reads them, and so does any other borrow. `#[linear]` has
        // no admitted release, so nothing may see through it.
        if descriptor.marker == hew_types::DeclarationMarker::Linear {
            return Err("aggregate projection cannot traverse a linear ancestor".into());
        }
    }
    Ok(recipes)
}

fn resolve_projection(
    function: &SemFunction,
    place: &PlaceDecl,
    values: &BTreeMap<ValueId, (OwnKind, ResolvedTy)>,
    shapes: &[SemAggregateShape],
    facts: &TypeFactTable,
) -> Result<AggregateProjection, String> {
    let (root, path) = place_path(&function.places, place.id)?;
    let root_ty = match root {
        OwnerRoot::Value(value) => {
            let Some((OwnKind::Owned, ty)) = values.get(&value) else {
                return Err("aggregate projection requires an owned root; a loan cannot supply field ownership".into());
            };
            if OwnKind::of_ty(ty, facts)? != OwnKind::Owned {
                return Err("aggregate projection root has no owned type contract".into());
            }
            ty
        }
        OwnerRoot::Local(id) => {
            &function
                .places
                .iter()
                .find(|place| place.id == id)
                .ok_or_else(|| "local root has no declaration".to_string())?
                .ty
        }
    };
    let row = facts
        .get(&TypeInstanceKey(root_ty.clone()))
        .ok_or_else(|| "local root has no exact type facts".to_string())?;
    let mut ty = root_ty.clone();
    let mut selected = AggregateFieldRecipe {
        ty: ty.clone(),
        own: OwnKind::of_class(row.class),
        clone: row.clone,
    };
    for step in &path {
        let recipes = plain_field_recipes(step.shape, &ty, shapes, facts)?;
        let recipe = recipes
            .get(step.field as usize)
            .cloned()
            .ok_or_else(|| "aggregate projection field is out of bounds".to_string())?;
        ty = recipe.ty.clone();
        selected = recipe;
    }
    if ty != place.ty {
        return Err("aggregate projection differs from its exact field type".into());
    }
    Ok(AggregateProjection {
        place: place.id,
        root,
        path,
        recipe: selected,
        leaves: Vec::new(),
    })
}

/// Aggregate roots replaced by their field partition, and roots released
/// whole because their declaration carries an ownership marker.
type PartitionCoverage = (BTreeSet<(OwnerRoot, PlaceId)>, BTreeSet<OwnerRoot>);

fn verify_partition_coverage(
    function: &SemFunction,
    plan: &PlacePlan,
    values: &BTreeMap<ValueId, (OwnKind, ResolvedTy)>,
    shapes: &[SemAggregateShape],
    facts: &TypeFactTable,
) -> Result<PartitionCoverage, String> {
    let mut groups = BTreeMap::<_, (AggregateShapeRef, usize, BTreeSet<u32>)>::new();
    // Roots released whole. A `#[resource]` record is closed by its own
    // `close`, so its fields are reads through the owner, never an ownership
    // partition that could release the members while the owner goes unclosed.
    let mut whole = BTreeSet::new();
    for place in &function.places {
        let PlaceOrigin::Aggregate { base, shape, field } = place.origin else {
            continue;
        };
        let root = plan.projections[&place.id].root;
        let ty = match base {
            PlaceBase::Place(parent) => {
                &plan
                    .projections
                    .get(&parent)
                    .ok_or_else(|| "aggregate projection has no parent recipe".to_string())?
                    .recipe
                    .ty
            }
            PlaceBase::Value(value) => &values[&value].1,
        };
        if is_marked_record(ty, shapes) {
            whole.insert(root);
            continue;
        }
        let count = plain_field_recipes(shape, ty, shapes, facts)?.len();
        let (previous_shape, expected, fields) = groups
            .entry((root, base))
            .or_insert_with(|| (shape, count, BTreeSet::new()));
        if *previous_shape != shape || *expected != count || !fields.insert(field) {
            return Err("aggregate sibling projections disagree on their parent descriptor".into());
        }
    }
    let mut expanded = BTreeSet::new();
    for ((root, base), (_, expected, fields)) in groups {
        if fields.len() != expected
            || fields
                .into_iter()
                .ne(0..u32::try_from(expected).map_err(|_| "aggregate field count exceeds u32")?)
        {
            return Err("aggregate projection partition omits a sibling field".into());
        }
        if let PlaceBase::Place(parent) = base {
            expanded.insert((root, parent));
        }
    }
    Ok((expanded, whole))
}

/// A nominal record whose declaration carries an ownership marker.
fn is_marked_record(ty: &ResolvedTy, shapes: &[SemAggregateShape]) -> bool {
    shapes.iter().any(|shape| {
        shape.aggregate_ty == *ty && shape.marker != hew_types::DeclarationMarker::None
    })
}

fn install_partitions(
    plan: &mut PlacePlan,
    expanded: &BTreeSet<(OwnerRoot, PlaceId)>,
    whole: &BTreeSet<OwnerRoot>,
) {
    let mut leaves: Vec<_> = plan
        .projections
        .values()
        .filter(|projection| !expanded.contains(&(projection.root, projection.place)))
        .filter(|projection| !whole.contains(&projection.root) || projection.path.is_empty())
        .map(|projection| (projection.root, projection.path.clone(), projection.place))
        .collect();
    leaves.sort_by(|a, b| (&a.0, &a.1).cmp(&(&b.0, &b.1)));
    for projection in plan.projections.values_mut() {
        // A root released whole owns nothing through its field places: each
        // projection is its own content, and only the root's own entry
        // carries the release.
        if whole.contains(&projection.root) && !projection.path.is_empty() {
            projection.leaves = Vec::new();
            continue;
        }
        projection.leaves = leaves
            .iter()
            .filter(|(root, path, _)| {
                *root == projection.root && path.starts_with(&projection.path)
            })
            .map(|(_, _, id)| *id)
            .collect();
    }
    for (root, path, place) in leaves {
        if whole.contains(&root) && !path.is_empty() {
            continue;
        }
        plan.roots.entry(root).or_default().push(place);
    }
}

pub(crate) fn verify_operation(
    function: &SemFunction,
    operation: &SemOp,
    types: &HashMap<ValueId, ResolvedTy>,
    facts: &TypeFactTable,
) -> Option<Result<(), String>> {
    let (id, stored, borrowed) = match &operation.kind {
        SemOpKind::LoadCopy { place }
        | SemOpKind::LoadTake { place }
        | SemOpKind::AllocPlace { place }
        | SemOpKind::EndLifetime { place } => (*place, None, false),
        SemOpKind::LoadBorrow { place } => (*place, None, true),
        SemOpKind::StoreInit { place, value } | SemOpKind::StoreAssign { place, value } => {
            (*place, Some(value.value), false)
        }
        _ => return None,
    };
    let place = function.places.iter().find(|place| place.id == id)?;
    let (PlaceOrigin::Aggregate { .. } | PlaceOrigin::Local) = place.origin else {
        return None;
    };
    Some((|| {
        if matches!(
            operation.kind,
            SemOpKind::AllocPlace { .. } | SemOpKind::EndLifetime { .. }
        ) {
            if place.origin != PlaceOrigin::Local || !operation.results.is_empty() {
                return Err(
                    "storage lifetime operations require a local root and no result".into(),
                );
            }
            return Ok(());
        }
        if let Some(value) = stored {
            if !operation.results.is_empty() || types.get(&value) != Some(&place.ty) {
                return Err(
                    "aggregate field assignment must transfer one exact value with no result"
                        .into(),
                );
            }
            return Ok(());
        }
        let [result] = operation.results.as_slice() else {
            return Err("aggregate place load requires exactly one result".into());
        };
        if !crate::call_boundary_types_match(&result.ty, &place.ty) {
            return Err("aggregate place load changes its field type".into());
        }
        if borrowed {
            if result.own != OwnKind::Guaranteed
                || OwnKind::of_ty(&place.ty, facts) != Ok(OwnKind::Owned)
            {
                return Err(
                    "aggregate field loan requires its exact root and an owning field".into(),
                );
            }
        } else if matches!(operation.kind, SemOpKind::LoadCopy { .. })
            && facts
                .get(&TypeInstanceKey(place.ty.clone()))
                .is_none_or(|facts| facts.clone == hew_types::CloneKind::None)
        {
            return Err("aggregate field has no copy operation".into());
        }
        Ok(())
    })())
}
