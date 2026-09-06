//! Target storage for SIR's canonical aggregate leaf partitions.

use super::{
    aggregate_glue, initialized_slot, storage, verify_destroy_action, BlockId, DestroyAction,
    FlowState, FunctionLowerer, InitState, OwnKind, PhysicalAggregateId, PhysicalEdge,
    PhysicalError, PhysicalFunction, PhysicalModule, StorageId, StorageOrigin,
};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PhysicalAggregateStep {
    pub glue: PhysicalAggregateId,
    pub field: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalAggregateLeaf {
    pub storage: StorageId,
    pub destroy: Option<DestroyAction>,
}

/// A root has an empty path; every other entry aliases that root allocation.
/// Only canonical leaves have initialization bits, including no-drop leaves.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalAggregateStorage {
    pub root: StorageId,
    pub path: Vec<PhysicalAggregateStep>,
    pub leaves: Vec<PhysicalAggregateLeaf>,
}

impl FunctionLowerer<'_> {
    pub(super) fn lower_aggregate_storage(
        &self,
    ) -> Result<BTreeMap<StorageId, PhysicalAggregateStorage>, PhysicalError> {
        let leaves = |places: &[hew_sir::PlaceId]| {
            places
                .iter()
                .map(|place| {
                    let projection = self.projections.projection(*place).ok_or_else(|| {
                        PhysicalError::new("aggregate leaf lacks its SIR projection recipe")
                    })?;
                    Ok(PhysicalAggregateLeaf {
                        storage: self.place(*place)?,
                        destroy: if projection.recipe.own == OwnKind::Owned {
                            Some(self.destroy_action(&projection.recipe.ty)?)
                        } else {
                            None
                        },
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()
        };
        let mut result = BTreeMap::new();
        for (root, partition) in self.projections.roots() {
            let root = self.value(root)?;
            result.insert(
                root,
                PhysicalAggregateStorage {
                    root,
                    path: Vec::new(),
                    leaves: leaves(partition)?,
                },
            );
        }
        for place in &self.function.places {
            let Some(projection) = self.projections.projection(place.id) else {
                continue;
            };
            let root = self.value(projection.root)?;
            let mut ty = self.storage[root.0 as usize].ty.clone();
            let mut path = Vec::with_capacity(projection.path.len());
            for step in &projection.path {
                path.push(PhysicalAggregateStep {
                    glue: self.aggregate_id(&ty)?,
                    field: step.field,
                });
                ty = hew_sir::aggregate_field_types(step.shape, &ty, &self.module.aggregate_shapes)
                    .map_err(PhysicalError::new)?
                    .get(step.field as usize)
                    .cloned()
                    .ok_or_else(|| PhysicalError::new("aggregate path field is out of bounds"))?;
            }
            result.insert(
                self.place(place.id)?,
                PhysicalAggregateStorage {
                    root,
                    path,
                    leaves: leaves(&projection.leaves)?,
                },
            );
        }
        Ok(result)
    }
}

pub(super) fn verify_storage(
    module: &PhysicalModule,
    function: &PhysicalFunction,
) -> Result<(), PhysicalError> {
    let entries = &function.aggregate_storage;
    let mut paths = BTreeMap::new();
    for (&id, entry) in entries {
        let slot = storage(function, id)?;
        let root = storage(function, entry.root)?;
        if root.own != OwnKind::Owned
            || matches!(
                root.origin,
                StorageOrigin::Aggregate(_)
                    | StorageOrigin::Capture { .. }
                    | StorageOrigin::Place(_)
            )
            || entries
                .get(&entry.root)
                .is_none_or(|root| root.root != entry.root || !root.path.is_empty())
            || (id == entry.root) != entry.path.is_empty()
            || (id != entry.root) != matches!(slot.origin, StorageOrigin::Aggregate(_))
        {
            return Err(PhysicalError::new(
                "aggregate storage has an invalid owning root or alias",
            ));
        }
        if paths.insert((entry.root, entry.path.clone()), id).is_some() {
            return Err(PhysicalError::new(
                "aggregate storage repeats a projection path",
            ));
        }
        let mut ty = &root.ty;
        let mut own = root.own;
        for step in &entry.path {
            let glue = aggregate_glue(module, step.glue)?;
            if &glue.ty != ty {
                return Err(PhysicalError::new(
                    "aggregate storage path changes its target type",
                ));
            }
            let field = glue.fields.get(step.field as usize).ok_or_else(|| {
                PhysicalError::new("aggregate storage path field is out of bounds")
            })?;
            ty = &field.ty;
            own = field.own;
        }
        if ty != &slot.ty || own != slot.own || entry.leaves.is_empty() {
            return Err(PhysicalError::new(
                "aggregate storage path or leaf recipe differs from its slot",
            ));
        }
    }
    for slot in &function.storage {
        if matches!(slot.origin, StorageOrigin::Aggregate(_)) && !entries.contains_key(&slot.id) {
            return Err(PhysicalError::new(
                "aggregate alias has no verified storage path",
            ));
        }
    }
    for entry in entries.values() {
        if let Some((last, parent)) = entry.path.split_last() {
            if !paths.contains_key(&(entry.root, parent.to_vec())) {
                return Err(PhysicalError::new(
                    "aggregate storage path omits an intermediate parent",
                ));
            }
            let glue = aggregate_glue(module, last.glue)?;
            for field in 0..glue.fields.len() {
                let mut sibling = parent.to_vec();
                sibling.push(PhysicalAggregateStep {
                    glue: last.glue,
                    field: u32::try_from(field)
                        .map_err(|_| PhysicalError::new("aggregate field count exceeds u32"))?,
                });
                if !paths.contains_key(&(entry.root, sibling)) {
                    return Err(PhysicalError::new(
                        "aggregate storage partition omits a sibling",
                    ));
                }
            }
        }
    }
    verify_partitions(module, function)
}

fn verify_partitions(
    module: &PhysicalModule,
    function: &PhysicalFunction,
) -> Result<(), PhysicalError> {
    let entries = &function.aggregate_storage;
    // Revalidate the supplied physical partition against the checked paths.
    // This verifies storage coverage; it does not choose field lifetimes.
    for entry in entries.values() {
        let mut expected: Vec<_> = entries
            .iter()
            .filter(|(_, candidate)| {
                candidate.root == entry.root
                    && candidate.path.starts_with(&entry.path)
                    && !entries.values().any(|child| {
                        child.root == candidate.root
                            && child.path.len() > candidate.path.len()
                            && child.path.starts_with(&candidate.path)
                    })
            })
            .collect();
        expected.sort_by(|(_, left), (_, right)| left.path.cmp(&right.path));
        if entry
            .leaves
            .iter()
            .map(|leaf| leaf.storage)
            .ne(expected.iter().map(|(id, _)| **id))
        {
            return Err(PhysicalError::new(
                "aggregate storage has an inexact leaf partition",
            ));
        }
        for leaf in &entry.leaves {
            let slot = storage(function, leaf.storage)?;
            if !matches!(slot.origin, StorageOrigin::Aggregate(_)) {
                return Err(PhysicalError::new(
                    "aggregate partition leaf must be a root projection",
                ));
            }
            match (slot.own, leaf.destroy) {
                (OwnKind::None, None) => {}
                (OwnKind::Owned, Some(action)) => {
                    verify_destroy_action(module, &slot.ty, slot.own, action)?;
                }
                _ => {
                    return Err(PhysicalError::new(
                        "aggregate leaf destruction differs from its ownership recipe",
                    ))
                }
            }
            let canonical = &entries[&leaf.storage].leaves;
            if canonical.as_slice() != [leaf.clone()] {
                return Err(PhysicalError::new(
                    "aggregate leaf has inconsistent destruction recipes",
                ));
            }
        }
    }
    Ok(())
}

pub(super) fn verify_edge(
    function: &PhysicalFunction,
    edge: &PhysicalEdge,
) -> Result<(), PhysicalError> {
    let mut expected = Vec::new();
    let mut roots = BTreeSet::new();
    for (source, dest) in &edge.transfers {
        match (
            function.aggregate_storage.get(source),
            function.aggregate_storage.get(dest),
        ) {
            (None, None) => {}
            (Some(before), Some(after)) if before.root == *source && after.root == *dest => {
                if !roots.insert(*source) {
                    return Err(PhysicalError::new(
                        "aggregate edge transfers an owning root more than once",
                    ));
                }
                if before.leaves.len() != after.leaves.len() {
                    return Err(PhysicalError::new(
                        "aggregate edge changes its leaf partition",
                    ));
                }
                for (source, dest) in before.leaves.iter().zip(&after.leaves) {
                    if function.aggregate_storage[&source.storage].path
                        != function.aggregate_storage[&dest.storage].path
                        || source.destroy != dest.destroy
                    {
                        return Err(PhysicalError::new(
                            "aggregate edge changes a leaf path or recipe",
                        ));
                    }
                    expected.push((source.storage, dest.storage));
                }
            }
            _ => {
                return Err(PhysicalError::new(
                    "aggregate edge loses or invents a root partition",
                ))
            }
        }
    }
    if edge.leaf_transfers != expected {
        return Err(PhysicalError::new(
            "aggregate edge has an inexact initialization transfer map",
        ));
    }
    Ok(())
}

pub(super) fn set_leaves(
    function: &PhysicalFunction,
    state: &mut FlowState,
    id: StorageId,
    value: InitState,
) {
    if let Some(entry) = function.aggregate_storage.get(&id) {
        for leaf in &entry.leaves {
            state.slots[leaf.storage.0 as usize] = value;
        }
    }
}

pub(super) fn require_root(
    function: &PhysicalFunction,
    state: &FlowState,
    id: StorageId,
    block: BlockId,
    context: &str,
) -> Result<(), PhysicalError> {
    let root = function
        .aggregate_storage
        .get(&id)
        .map_or(id, |entry| entry.root);
    initialized_slot(state, root, block, context)
}
