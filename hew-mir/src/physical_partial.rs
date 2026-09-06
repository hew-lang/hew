//! Target storage for SIR's canonical aggregate leaf partitions.

use super::{
    aggregate_glue, initialized_slot, storage, verify_destroy_action, BlockId, DestroyAction,
    FlowState, FunctionLowerer, InitState, OwnKind, PhysicalAggregateId, PhysicalEdge,
    PhysicalError, PhysicalFunction, PhysicalModule, StorageId, StorageOrigin,
};
use std::collections::{BTreeMap, BTreeSet};

/// A reachable cleanup disposition certified by SIR for this exact physical site.
/// Its private fields prevent a physical producer from inventing a trap waiver.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalCleanup {
    operation: hew_sir::OpId,
    function: hew_sir::CallableId,
    site: (BlockId, usize),
    source: StorageId,
    mode: hew_sir::CleanupMode,
}

impl PhysicalCleanup {
    #[must_use]
    pub fn mode(&self) -> hew_sir::CleanupMode {
        self.mode
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PhysicalAggregateStep {
    pub glue: PhysicalAggregateId,
    pub field: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalPlaceLeaf {
    pub storage: StorageId,
    pub destroy: Option<DestroyAction>,
}

/// A root has an empty path; every other entry aliases that root allocation.
/// Only canonical leaves have initialization bits, including no-drop leaves.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalPlaceStorage {
    pub root: StorageId,
    pub path: Vec<PhysicalAggregateStep>,
    pub leaves: Vec<PhysicalPlaceLeaf>,
}

impl FunctionLowerer<'_> {
    pub(super) fn cleanup_recipe(
        &self,
        operation: hew_sir::OpId,
        site: (BlockId, usize),
        source: StorageId,
    ) -> Result<PhysicalCleanup, PhysicalError> {
        let mode = self.lifetimes.cleanup(operation).ok_or_else(|| {
            PhysicalError::new(format!(
                "cleanup {} has no reachable SIR disposition",
                operation.0
            ))
        })?;
        Ok(PhysicalCleanup {
            operation,
            function: self.function.callable,
            site,
            source,
            mode,
        })
    }

    pub(super) fn optional_destroy(
        &self,
        id: StorageId,
    ) -> Result<Option<DestroyAction>, PhysicalError> {
        let slot = &self.storage[id.0 as usize];
        if slot.own == OwnKind::None {
            Ok(None)
        } else {
            self.destroy_action(&slot.ty).map(Some)
        }
    }

    fn owner_storage(&self, owner: hew_sir::OwnerRoot) -> Result<StorageId, PhysicalError> {
        match owner {
            hew_sir::OwnerRoot::Value(value) => self.value(value),
            hew_sir::OwnerRoot::Local(place) => self.place(place),
        }
    }

    pub(super) fn lower_place_storage(
        &self,
    ) -> Result<BTreeMap<StorageId, PhysicalPlaceStorage>, PhysicalError> {
        let leaves = |places: &[hew_sir::PlaceId]| {
            places
                .iter()
                .map(|place| {
                    let projection = self.projections.projection(*place).ok_or_else(|| {
                        PhysicalError::new("aggregate leaf lacks its SIR projection recipe")
                    })?;
                    Ok(PhysicalPlaceLeaf {
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
            let root = self.owner_storage(root)?;
            result.insert(
                root,
                PhysicalPlaceStorage {
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
            if projection.path.is_empty() {
                continue;
            }
            let root = self.owner_storage(projection.root)?;
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
                PhysicalPlaceStorage {
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
    let entries = &function.place_storage;
    let mut paths = BTreeMap::new();
    for (&id, entry) in entries {
        let slot = storage(function, id)?;
        let root = storage(function, entry.root)?;
        if (!matches!(root.origin, StorageOrigin::Local(_)) && root.own != OwnKind::Owned)
            || matches!(
                root.origin,
                StorageOrigin::Aggregate(_) | StorageOrigin::Capture { .. }
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
        if matches!(
            slot.origin,
            StorageOrigin::Aggregate(_) | StorageOrigin::Local(_)
        ) && !entries.contains_key(&slot.id)
        {
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
    let entries = &function.place_storage;
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
            if !matches!(
                slot.origin,
                StorageOrigin::Aggregate(_) | StorageOrigin::Local(_)
            ) {
                return Err(PhysicalError::new(
                    "place partition leaf must be a local or projection",
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
        if [source, dest].into_iter().any(|id| {
            matches!(
                function.storage[id.0 as usize].origin,
                StorageOrigin::Local(_) | StorageOrigin::Aggregate(_)
            )
        }) {
            return Err(PhysicalError::new(
                "physical CFG transfers require SSA values, not local places",
            ));
        }
        match (
            function.place_storage.get(source),
            function.place_storage.get(dest),
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
                    if function.place_storage[&source.storage].path
                        != function.place_storage[&dest.storage].path
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
    if let Some(entry) = function.place_storage.get(&id) {
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
        .place_storage
        .get(&id)
        .map_or(id, |entry| entry.root);
    if matches!(storage(function, root)?.origin, StorageOrigin::Local(_)) {
        if state.active[root.0 as usize] != InitState::Initialized {
            return Err(PhysicalError::new(format!(
                "physical bb{} {context} accesses inactive local storage {}",
                block.0, root.0
            )));
        }
        Ok(())
    } else {
        initialized_slot(state, root, block, context)
    }
}

pub(super) fn verify_cleanup_site(
    function: &PhysicalFunction,
    operation: &super::PhysicalOp,
    site: (BlockId, usize),
) -> Result<(), PhysicalError> {
    let (source, cleanup) = match operation {
        super::PhysicalOp::Destroy {
            source, cleanup, ..
        } => (*source, cleanup),
        super::PhysicalOp::StorageDead {
            storage, cleanup, ..
        } => (*storage, cleanup),
        _ => return Ok(()),
    };
    if cleanup.function != function.callable || cleanup.source != source || cleanup.site != site {
        return Err(PhysicalError::new(format!(
            "physical cleanup {} differs from its certified source or site",
            cleanup.operation.0
        )));
    }
    Ok(())
}

/// Check that mutable physical control flow still realizes SIR's certificate.
/// This never assigns a cleanup mode: certified Trap operations and explicit
/// panic cleanup edges seed the walk. Every continuation must remain cleanup-only and finish in a
/// trap or fault propagation; a cycle cannot establish that obligation.
pub(super) fn verify_trap_cleanup_refinement(
    function: &PhysicalFunction,
) -> Result<(), PhysicalError> {
    use super::{PhysicalOp, PhysicalTerminator};

    let blocks: BTreeMap<_, _> = function
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect();
    let certified = |operation: &PhysicalOp| {
        matches!(operation,
        PhysicalOp::Destroy { cleanup, .. } | PhysicalOp::StorageDead { cleanup, .. }
        if cleanup.mode() == hew_sir::CleanupMode::Trap)
    };
    let seeds = function
        .blocks
        .iter()
        .filter_map(|block| {
            block
                .ops
                .iter()
                .position(certified)
                .map(|index| (block.id, index))
        })
        .chain(function.blocks.iter().filter_map(|block| {
            if let PhysicalTerminator::Panic { cleanup, .. } = &block.terminator {
                Some((cleanup.target, 0))
            } else {
                None
            }
        }));
    let invalid =
        || PhysicalError::new("physical CFG no longer realizes its certified trap cleanup region");
    let mut complete = BTreeSet::new();
    for seed in seeds {
        let mut visiting = BTreeSet::new();
        let mut pending = vec![(seed, false)];
        while let Some((site, leaving)) = pending.pop() {
            if leaving {
                visiting.remove(&site);
                complete.insert(site);
                continue;
            }
            if complete.contains(&site) {
                continue;
            }
            if !visiting.insert(site) {
                return Err(invalid());
            }
            let block = blocks.get(&site.0).ok_or_else(invalid)?;
            if block.ops[site.1..].iter().any(|operation| {
                !certified(operation) && !matches!(operation, PhysicalOp::EndBorrow { .. })
            }) {
                return Err(invalid());
            }
            pending.push((site, true));
            match &block.terminator {
                PhysicalTerminator::Trap(_) | PhysicalTerminator::PropagateFault => {}
                PhysicalTerminator::Goto(edge) => pending.push(((edge.target, 0), false)),
                PhysicalTerminator::Branch {
                    then_target,
                    else_target,
                    ..
                } => {
                    pending.push(((then_target.target, 0), false));
                    pending.push(((else_target.target, 0), false));
                }
                _ => return Err(invalid()),
            }
        }
    }
    Ok(())
}

pub(super) fn require_local(
    function: &PhysicalFunction,
    id: StorageId,
) -> Result<(), PhysicalError> {
    if !matches!(storage(function, id)?.origin, StorageOrigin::Local(_)) {
        return Err(PhysicalError::new(
            "physical storage lifetime requires a function-owned local",
        ));
    }
    Ok(())
}

pub(super) fn verify_optional_destroy(
    module: &PhysicalModule,
    slot: &super::PhysicalStorage,
    destroy: Option<DestroyAction>,
) -> Result<(), PhysicalError> {
    match (slot.own, destroy) {
        (OwnKind::None, None) => Ok(()),
        (OwnKind::Owned, Some(action)) => verify_destroy_action(module, &slot.ty, slot.own, action),
        _ => Err(PhysicalError::new(
            "physical conditional destruction differs from its content ownership",
        )),
    }
}

pub(super) fn activate(
    function: &PhysicalFunction,
    state: &mut FlowState,
    id: StorageId,
    block: BlockId,
) -> Result<(), PhysicalError> {
    require_local(function, id)?;
    if state.active[id.0 as usize] != InitState::Uninitialized {
        return Err(PhysicalError::new(format!(
            "physical bb{} starts an already active local {}",
            block.0, id.0
        )));
    }
    state.active[id.0 as usize] = InitState::Initialized;
    set_leaves(function, state, id, InitState::Uninitialized);
    Ok(())
}

pub(super) fn require_droppable(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    state: &FlowState,
    id: StorageId,
    mode: hew_sir::CleanupMode,
) -> Result<(), PhysicalError> {
    if mode == hew_sir::CleanupMode::Trap {
        return Ok(());
    }
    let live_linear = |id: StorageId| -> Result<bool, PhysicalError> {
        Ok(state.slots[id.0 as usize] != InitState::Uninitialized
            && super::semantic_type_facts(module, &storage(function, id)?.ty)?.class
                == hew_types::ValueClass::Linear)
    };
    let live = if let Some(entry) = function.place_storage.get(&id) {
        let mut live = false;
        for leaf in &entry.leaves {
            live |= live_linear(leaf.storage)?;
        }
        live
    } else {
        live_linear(id)?
    };
    if live {
        return Err(PhysicalError::new(
            "physical ordinary cleanup cannot discard live linear contents",
        ));
    }
    Ok(())
}
