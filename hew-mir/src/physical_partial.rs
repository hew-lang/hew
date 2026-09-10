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
    contents: BTreeMap<StorageId, hew_sir::LeafContents>,
}

impl PhysicalCleanup {
    #[must_use]
    pub fn mode(&self) -> hew_sir::CleanupMode {
        self.mode
    }

    /// What SIR proved this leaf holds at this site. A backend releases the
    /// present contents, skips the absent ones, and tests the initialization
    /// state only where the incoming paths disagree.
    #[must_use]
    pub fn leaf(&self, leaf: StorageId) -> Option<hew_sir::LeafContents> {
        self.contents.get(&leaf).copied()
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
        let mut contents = BTreeMap::new();
        for (place, state) in self
            .lifetimes
            .cleanup_contents(operation)
            .unwrap_or_default()
        {
            contents.insert(self.place(place)?, state);
        }
        Ok(PhysicalCleanup {
            operation,
            function: self.function.callable,
            site,
            source,
            mode,
            contents,
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

    /// Whether an owner is a record resource, released whole by its `close`.
    pub(super) fn released_whole(&self, owner: hew_sir::OwnerRoot) -> Result<bool, PhysicalError> {
        let storage = self.owner_storage(owner)?;
        Ok(matches!(
            self.module
                .resources
                .get(&self.storage[storage.0 as usize].ty),
            Some(hew_sir::ResourceRelease::RecordClose { .. })
        ))
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
                        // A field of a record resource is a read through
                        // its owner: the owner's `close` is the only release.
                        destroy: if projection.recipe.own == OwnKind::Owned
                            && (projection.path.is_empty()
                                || !self.released_whole(projection.root)?)
                        {
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
    // A record resource is released whole by its own `close`, so a read of one
    // of its fields owns nothing: that storage carries no content partition.
    let released_whole = |entry: &PhysicalPlaceStorage| -> Result<bool, PhysicalError> {
        let root_ty = &storage(function, entry.root)?.ty;
        Ok(module.resources.iter().any(|resource| {
            &resource.ty == root_ty
                && matches!(
                    resource.release,
                    hew_sir::ResourceRelease::RecordClose { .. }
                )
        }))
    };
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
        let field_of_record = released_whole(entry)? && !entry.path.is_empty();
        if ty != &slot.ty || own != slot.own || (entry.leaves.is_empty() && !field_of_record) {
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
        // A record resource is released whole, so a read of one of its fields
        // does not oblige the other fields to have their own storage.
        if released_whole(entry)? {
            continue;
        }
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
        // A record resource is one content: its own release covers it, and a
        // read of one of its fields adds nothing to that partition.
        let released_whole = {
            let root_ty = &storage(function, entry.root)?.ty;
            module.resources.iter().any(|resource| {
                &resource.ty == root_ty
                    && matches!(
                        resource.release,
                        hew_sir::ResourceRelease::RecordClose { .. }
                    )
            })
        };
        if released_whole {
            let covered = if entry.path.is_empty() {
                entry.leaves.len() == 1 && entry.leaves[0].storage == entry.root
            } else {
                entry.leaves.is_empty()
            };
            if !covered {
                return Err(PhysicalError::new(
                    "record resource storage carries a leaf partition",
                ));
            }
            continue;
        }
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
        super::PhysicalOp::Assign { dest, cleanup, .. } => (*dest, cleanup),
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
#[allow(
    clippy::too_many_lines,
    reason = "one graph walk verifies the complete certified cleanup region"
)]
pub(super) fn verify_trap_cleanup_refinement(
    function: &PhysicalFunction,
) -> Result<BTreeSet<BlockId>, PhysicalError> {
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
    let seeds = function.blocks.iter().flat_map(|block| {
        let operation = block
            .ops
            .iter()
            .position(certified)
            .map(|index| (block.id, index));
        let fault = match &block.terminator {
            PhysicalTerminator::Panic { cleanup, .. }
            | PhysicalTerminator::CheckedRaiseFault { cleanup, .. } => Some((cleanup.target, 0)),
            _ => None,
        };
        operation.into_iter().chain(fault)
    });
    let invalid =
        || PhysicalError::new("physical CFG no longer realizes its certified trap cleanup region");
    let mut complete = BTreeMap::new();
    for seed in seeds {
        let mut visiting = BTreeSet::new();
        let mut pending = vec![(seed, false)];
        while let Some((site, leaving)) = pending.pop() {
            if leaving {
                visiting.remove(&site);
                let needs_incoming_fault = match &blocks[&site.0].terminator {
                    PhysicalTerminator::Trap(_) | PhysicalTerminator::CheckedRaiseFault { .. } => {
                        false
                    }
                    PhysicalTerminator::Goto(edge) => complete[&(edge.target, 0)],
                    PhysicalTerminator::Branch {
                        then_target,
                        else_target,
                        ..
                    } => complete[&(then_target.target, 0)] || complete[&(else_target.target, 0)],
                    _ => true,
                };
                complete.insert(site, needs_incoming_fault);
                continue;
            }
            if complete.contains_key(&site) {
                continue;
            }
            if !visiting.insert(site) {
                return Err(invalid());
            }
            let block = blocks.get(&site.0).ok_or_else(invalid)?;
            if let Some(operation) = block.ops[site.1..].iter().find(|operation| {
                !certified(operation)
                    && !matches!(
                        operation,
                        PhysicalOp::EndBorrow { .. } | PhysicalOp::TaskScopeClose { .. }
                    )
            }) {
                return Err(PhysicalError::new(format!(
                    "physical CFG no longer realizes its certified trap cleanup region: callable {:?} block {:?} contains {operation:?}", function.callable, site.0
                )));
            }
            pending.push((site, true));
            match &block.terminator {
                PhysicalTerminator::Trap(_)
                | PhysicalTerminator::PropagateFault
                | PhysicalTerminator::EnterDefer { .. }
                | PhysicalTerminator::FinishDefer { .. }
                | PhysicalTerminator::CheckedRaiseFault { .. }
                | PhysicalTerminator::RecoverFault { .. }
                // Fault dispatch can prove one structurally present cleanup
                // edge impossible; physical lowering keeps the block and
                // marks it unreachable. Such a continuation discharges the
                // obligation structurally: if flow ever does reach it, the
                // initialization walk already refuses it for abandoning the
                // active fault, pending parks, live loans or active storage.
                | PhysicalTerminator::Unreachable => {}
                PhysicalTerminator::CleanupDispatch { fault, .. } => {
                    pending.push(((fault.target, 0), false));
                }
                // A fault drain can park while children finish cancellation.
                // Both outcomes must continue cleanup. The separate physical
                // scope verifier proves the matching drain/close lifetime,
                // and flow verification requires the primary fault to survive.
                PhysicalTerminator::TaskScopeJoin {
                    mode: hew_sir::TaskScopeJoinMode::PropagateFault | hew_sir::TaskScopeJoinMode::CancelLosersAfterFault,
                    normal,
                    unwind,
                    ..
                } => {
                    pending.push(((normal.target, 0), false));
                    pending.push(((unwind.target, 0), false));
                }
                PhysicalTerminator::ValueClose { next: edge, .. }
                | PhysicalTerminator::Goto(edge) => pending.push(((edge.target, 0), false)),
                PhysicalTerminator::Branch {
                    then_target,
                    else_target,
                    ..
                } => {
                    pending.push(((then_target.target, 0), false));
                    pending.push(((else_target.target, 0), false));
                }
                terminator => return Err(PhysicalError::new(format!(
                    "physical CFG no longer realizes its certified trap cleanup region: callable {:?} block {:?} ends with {terminator:?}", function.callable, site.0
                ))),
            }
        }
    }
    // A terminal trap can justify an ordinary predecessor's certified cleanup
    // through a finite diamond. Deferred boundaries instead need an existing
    // fault exit cause; parking that fault must preserve this fact.
    Ok(complete
        .into_iter()
        .filter_map(|((block, _), needs_fault)| needs_fault.then_some(block))
        .collect())
}

pub(super) fn require_local(
    function: &PhysicalFunction,
    id: StorageId,
) -> Result<(), PhysicalError> {
    if !matches!(
        storage(function, id)?.origin,
        StorageOrigin::Local(_)
            | StorageOrigin::ActorState {
                initialized: false,
                ..
            }
    ) {
        return Err(PhysicalError::new(
            "physical storage lifetime requires a function-owned local or a deferred actor seat",
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
