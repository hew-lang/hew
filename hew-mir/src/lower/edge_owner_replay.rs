//! Replayed edge-owner lifecycle and ownership-SSA join materialization.
//!
//! This module derives every rewrite from explicit raw-MIR ownership events
//! and the exact/maybe/must owner lattices. It never consults mutable Builder
//! generation cursors as ownership authority.

use std::collections::{BTreeMap, HashMap, HashSet};

use hew_hir::{BindingId, ScopeId};
use hew_types::ResolvedTy;

use crate::model::{BasicBlock, Instr, Place, Terminator};

use super::cfg_util::{block_dominators, shift_instr_spans_on_insert, shift_instr_spans_on_remove};
use super::{
    canonicalize_release_owner_ids, canonicalize_terminal_transfer_owner_ids,
    deduplicate_ownership_spines, drop_plan, Builder, ScopeInfoEntry, ENTRY_BLOCK_ID,
};

fn ownership_event_owner_ids(event: &crate::model::OwnershipEvent) -> Vec<crate::model::OwnerId> {
    use crate::model::OwnershipEvent;

    match event {
        OwnershipEvent::Mint { owner, .. }
        | OwnershipEvent::DropRecipe { owner, .. }
        | OwnershipEvent::Relocate { owner, .. }
        | OwnershipEvent::Release { owner, .. }
        | OwnershipEvent::GuardedRelease { owner, .. }
        | OwnershipEvent::DemoteToAlias { owner, .. }
        | OwnershipEvent::Guard { owner, .. }
        | OwnershipEvent::EdgeCarry { owner, .. } => vec![*owner],
        OwnershipEvent::Transfer {
            owner, to_owner, ..
        } => std::iter::once(*owner)
            .chain(to_owner.iter().copied())
            .collect(),
        OwnershipEvent::Reset {
            previous,
            replacement,
            ..
        }
        | OwnershipEvent::Rearm {
            previous,
            replacement,
            ..
        } => vec![*previous, *replacement],
        OwnershipEvent::InteriorAlias { receiver_owner, .. } => {
            receiver_owner.iter().copied().collect()
        }
        OwnershipEvent::Join {
            incoming,
            replacement,
            ..
        } => incoming
            .iter()
            .copied()
            .chain(std::iter::once(*replacement))
            .collect(),
        OwnershipEvent::ScopeExit {
            owners, carried, ..
        } => owners.iter().chain(carried).copied().collect(),
        OwnershipEvent::AliasRelocate { .. } | OwnershipEvent::AliasEnd { .. } => Vec::new(),
    }
}

fn rewrite_ownership_event_owner(
    event: &mut crate::model::OwnershipEvent,
    stale: crate::model::OwnerId,
    replacement: crate::model::OwnerId,
) {
    use crate::model::OwnershipEvent;

    for owner in ownership_event_owner_ids(event) {
        if owner != stale {
            continue;
        }
        match event {
            OwnershipEvent::Mint { owner, .. }
            | OwnershipEvent::DropRecipe { owner, .. }
            | OwnershipEvent::Relocate { owner, .. }
            | OwnershipEvent::Release { owner, .. }
            | OwnershipEvent::GuardedRelease { owner, .. }
            | OwnershipEvent::DemoteToAlias { owner, .. }
            | OwnershipEvent::Guard { owner, .. }
            | OwnershipEvent::EdgeCarry { owner, .. }
                if *owner == stale =>
            {
                *owner = replacement;
            }
            OwnershipEvent::Transfer {
                owner, to_owner, ..
            } => {
                if *owner == stale {
                    *owner = replacement;
                }
                if *to_owner == Some(stale) {
                    *to_owner = Some(replacement);
                }
            }
            OwnershipEvent::Reset {
                previous,
                replacement: successor,
                ..
            }
            | OwnershipEvent::Rearm {
                previous,
                replacement: successor,
                ..
            } => {
                if *previous == stale {
                    *previous = replacement;
                }
                if *successor == stale {
                    *successor = replacement;
                }
            }
            OwnershipEvent::InteriorAlias { receiver_owner, .. } => {
                if *receiver_owner == Some(stale) {
                    *receiver_owner = Some(replacement);
                }
            }
            OwnershipEvent::Join {
                incoming,
                replacement: successor,
                ..
            } => {
                for owner in incoming {
                    if *owner == stale {
                        *owner = replacement;
                    }
                }
                if *successor == stale {
                    *successor = replacement;
                }
            }
            OwnershipEvent::ScopeExit {
                owners, carried, ..
            } => {
                for owner in owners.iter_mut().chain(carried) {
                    if *owner == stale {
                        *owner = replacement;
                    }
                }
            }
            OwnershipEvent::AliasRelocate { .. }
            | OwnershipEvent::AliasEnd { .. }
            | OwnershipEvent::Mint { .. }
            | OwnershipEvent::DropRecipe { .. }
            | OwnershipEvent::Relocate { .. }
            | OwnershipEvent::Release { .. }
            | OwnershipEvent::GuardedRelease { .. }
            | OwnershipEvent::DemoteToAlias { .. }
            | OwnershipEvent::Guard { .. }
            | OwnershipEvent::EdgeCarry { .. } => {}
        }
    }
}

/// Scope ids are opaque identities. A parent scope marker closes owners in
/// its descendant scopes only when the explicit parent graph proves that
/// ancestry; missing/cyclic links fail closed.
pub(super) fn lexical_scope_is_closed(
    binding_scope: ScopeId,
    exited_scopes: &HashSet<ScopeId>,
    scope_info: &HashMap<ScopeId, ScopeInfoEntry>,
) -> bool {
    let mut current = binding_scope;
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(current) {
            return false;
        }
        if exited_scopes.contains(&current) {
            return true;
        }
        let Some(entry) = scope_info.get(&current) else {
            return false;
        };
        let Some(parent) = entry.parent else {
            return false;
        };
        current = parent;
    }
}

fn apply_lexical_scope_exit_to_exact_state(
    instruction: &Instr,
    binding_scopes: &HashMap<BindingId, ScopeId>,
    scope_info: &HashMap<ScopeId, ScopeInfoEntry>,
    live: &mut drop_plan::ExactOwnerState,
) {
    drop_plan::apply_exact_owner_ops(std::slice::from_ref(instruction), live);
    let Instr::OwnershipEvent(crate::model::OwnershipEvent::ScopeExit {
        scopes,
        carry_places,
        ..
    }) = instruction
    else {
        return;
    };
    let exited_scopes = scopes.iter().copied().collect::<HashSet<_>>();
    live.retain(|owner, place| {
        carry_places.contains(place)
            || binding_scopes
                .get(&owner.binding)
                .is_none_or(|scope| !lexical_scope_is_closed(*scope, &exited_scopes, scope_info))
    });
}

fn apply_lexical_scope_exit_to_maybe_state(
    instruction: &Instr,
    binding_scopes: &HashMap<BindingId, ScopeId>,
    scope_info: &HashMap<ScopeId, ScopeInfoEntry>,
    live: &mut drop_plan::MaybeOwnerState,
) {
    drop_plan::apply_maybe_owner_ops(std::slice::from_ref(instruction), live);
    let Instr::OwnershipEvent(crate::model::OwnershipEvent::ScopeExit {
        scopes,
        carry_places,
        ..
    }) = instruction
    else {
        return;
    };
    let exited_scopes = scopes.iter().copied().collect::<HashSet<_>>();
    live.retain(|(owner, place)| {
        carry_places.contains(place)
            || binding_scopes
                .get(&owner.binding)
                .is_none_or(|scope| !lexical_scope_is_closed(*scope, &exited_scopes, scope_info))
    });
}

fn apply_lexical_scope_exit_to_must_state(
    instructions: &[Instr],
    binding_scopes: &HashMap<BindingId, ScopeId>,
    scope_info: &HashMap<ScopeId, ScopeInfoEntry>,
    live: &mut drop_plan::MustBindingOwnerState,
) {
    for instruction in instructions {
        match instruction {
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint { owner, place, .. }) => {
                live.insert(owner.binding, *place);
            }
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                owner,
                to,
                to_owner,
                ..
            }) => {
                live.remove(&owner.binding);
                if let Some((next, destination)) = to_owner.zip(*to) {
                    live.insert(next.binding, destination);
                }
            }
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Relocate { owner, to, .. }) => {
                if let Some(place) = live.get_mut(&owner.binding) {
                    *place = *to;
                }
            }
            Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Release { owner, .. }
                | crate::model::OwnershipEvent::GuardedRelease { owner, .. }
                | crate::model::OwnershipEvent::DemoteToAlias { owner, .. },
            ) => {
                live.remove(&owner.binding);
            }
            Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Reset {
                    previous,
                    replacement,
                    place,
                    ..
                }
                | crate::model::OwnershipEvent::Rearm {
                    previous,
                    replacement,
                    place,
                    ..
                },
            ) => {
                live.remove(&previous.binding);
                live.insert(replacement.binding, *place);
            }
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Join {
                incoming,
                replacement,
                place,
                ..
            }) => {
                for owner in incoming {
                    live.remove(&owner.binding);
                }
                live.insert(replacement.binding, *place);
            }
            Instr::OwnershipEvent(crate::model::OwnershipEvent::ScopeExit {
                scopes,
                carry_places,
                ..
            }) => {
                let exited_scopes = scopes.iter().copied().collect::<HashSet<_>>();
                live.retain(|binding, place| {
                    carry_places.contains(place)
                        || binding_scopes.get(binding).is_none_or(|scope| {
                            !lexical_scope_is_closed(*scope, &exited_scopes, scope_info)
                        })
                });
            }
            _ => {}
        }
    }
}

/// The one owner identity that each predecessor edge presents to an ownership
/// SSA join.  The map is deliberately keyed by predecessor block instead of
/// flattening a whole-function maybe-state: a Join consumes one value from
/// each edge, never a bag of generations that happened to be reachable
/// somewhere upstream of that edge.
pub(super) type EdgeOwnerInputs = BTreeMap<u32, crate::model::OwnerId>;

/// Replay the owner fact carried by every predecessor edge of one prospective
/// Join.
///
/// A fact exists when both lattices agree on one owner of `binding` at
/// `place` for that edge.  A cyclic predecessor can temporarily erase that
/// exact identity before its inner join has been materialized. In that one
/// shape we select the uniquely deepest definition which dominates the edge;
/// it is still an immutable per-edge fact, never a Builder generation cursor
/// or a reverse instruction scan. A wrong-place, absent, or tied definition
/// remains unselected for the Checked-MIR verifier to reject.
pub(super) fn replayed_edge_owner_inputs(
    predecessors: &[u32],
    binding: BindingId,
    place: Place,
    exact_exits: &HashMap<u32, drop_plan::ExactOwnerState>,
    maybe_exits: &HashMap<u32, drop_plan::MaybeOwnerState>,
    definition_blocks: &HashMap<crate::model::OwnerId, Vec<u32>>,
    dominators: &HashMap<u32, HashSet<u32>>,
) -> Option<EdgeOwnerInputs> {
    let mut inputs = EdgeOwnerInputs::new();
    for predecessor in predecessors {
        let exact = exact_exits.get(predecessor)?;
        let maybe = maybe_exits.get(predecessor)?;
        let mut exact_owners = exact
            .iter()
            .filter_map(|(owner, owner_place)| {
                (owner.binding == binding).then_some((*owner, *owner_place))
            })
            .collect::<Vec<_>>();
        let mut maybe_owners = maybe
            .iter()
            .filter_map(|(owner, owner_place)| {
                (owner.binding == binding).then_some((*owner, *owner_place))
            })
            .collect::<Vec<_>>();
        exact_owners.sort_by_key(|(owner, _)| *owner);
        maybe_owners.sort_by_key(|(owner, _)| *owner);
        if let ([(exact_owner, exact_place)], [(maybe_owner, maybe_place)]) =
            (exact_owners.as_slice(), maybe_owners.as_slice())
        {
            if exact_owner != maybe_owner || exact_place != &place || maybe_place != &place {
                return None;
            }
            inputs.insert(*predecessor, *exact_owner);
            continue;
        }
        // A cyclic edge can initially have no exact identity when an inner
        // assignment has published a later generation but the pass-through
        // arm still carries the older one. Pick only the one definition that
        // lies closest to, and dominates, this predecessor edge. This is the
        // edge's concrete lifecycle endpoint; a tied or non-dominating owner
        // is deliberately not guessed.
        if maybe_owners
            .iter()
            .all(|(_, owner_place)| *owner_place == place)
        {
            let edge_dominators = dominators.get(predecessor)?;
            let mut candidates = maybe_owners
                .iter()
                .filter_map(|(owner, _)| {
                    let [definition] = definition_blocks.get(owner)?.as_slice() else {
                        return None;
                    };
                    edge_dominators
                        .contains(definition)
                        .then_some((*owner, *definition))
                })
                .collect::<Vec<_>>();
            candidates.sort_by_key(|(owner, definition)| {
                (dominators.get(definition).map_or(0, HashSet::len), *owner)
            });
            let (owner, definition) = candidates.last().copied()?;
            let depth = dominators.get(&definition).map_or(0, HashSet::len);
            if candidates
                .iter()
                .filter(|(_, candidate_definition)| {
                    dominators.get(candidate_definition).map_or(0, HashSet::len) == depth
                })
                .count()
                == 1
                && (exact_owners.is_empty() || exact_owners.as_slice() == [(owner, place)])
            {
                inputs.insert(*predecessor, owner);
                continue;
            }
        }
        return None;
    }
    Some(inputs)
}

/// Index immutable definition sites for the dominance-backed cyclic-edge
/// fallback in [`replayed_edge_owner_inputs`]. Multiple sites mean there is no
/// one fact to select, so the fallback stays fail-closed.
fn replayed_owner_definition_blocks(
    blocks: &[BasicBlock],
) -> HashMap<crate::model::OwnerId, Vec<u32>> {
    let mut definitions = HashMap::<crate::model::OwnerId, Vec<u32>>::new();
    for block in blocks {
        for instruction in &block.instructions {
            let owner = match instruction {
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint { owner, .. }) => {
                    Some(*owner)
                }
                Instr::OwnershipEvent(
                    crate::model::OwnershipEvent::Reset { replacement, .. }
                    | crate::model::OwnershipEvent::Rearm { replacement, .. }
                    | crate::model::OwnershipEvent::Join { replacement, .. },
                ) => Some(*replacement),
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                    to_owner: Some(owner),
                    to: Some(_),
                    to_ty: Some(_),
                    ..
                }) => Some(*owner),
                _ => None,
            };
            if let Some(owner) = owner {
                definitions.entry(owner).or_default().push(block.id);
            }
        }
    }
    for blocks in definitions.values_mut() {
        blocks.sort_unstable();
        blocks.dedup();
    }
    definitions
}

/// Refresh an already-published Join from the exact per-predecessor map.
///
/// An inner branch Join is intentionally sealed before an enclosing loop
/// header. Once the header receives its own replacement, the pass-through arm
/// of that inner Join carries the new header generation instead of the
/// construction-time identity. Refreshing here keeps the declared inputs
/// equal to the immutable replay and, crucially, never admits the Join's own
/// successor as an input.
fn refresh_replayed_join_inputs(
    blocks: &mut [BasicBlock],
    predecessors: &HashMap<u32, Vec<u32>>,
    exact_exits: &HashMap<u32, drop_plan::ExactOwnerState>,
    maybe_exits: &HashMap<u32, drop_plan::MaybeOwnerState>,
    definition_blocks: &HashMap<crate::model::OwnerId, Vec<u32>>,
    dominators: &HashMap<u32, HashSet<u32>>,
) -> bool {
    let mut changed = false;
    for block in blocks {
        let Some(incoming_blocks) = predecessors.get(&block.id) else {
            continue;
        };
        for instruction in &mut block.instructions {
            let Instr::OwnershipEvent(crate::model::OwnershipEvent::Join {
                incoming,
                replacement,
                place,
                ..
            }) = instruction
            else {
                continue;
            };
            let Some(edge_inputs) = replayed_edge_owner_inputs(
                incoming_blocks,
                replacement.binding,
                *place,
                exact_exits,
                maybe_exits,
                definition_blocks,
                dominators,
            ) else {
                continue;
            };
            let mut refreshed = edge_inputs.values().copied().collect::<Vec<_>>();
            refreshed.sort_unstable();
            refreshed.dedup();
            if refreshed.len() < 2 || refreshed.contains(replacement) {
                continue;
            }
            if *incoming != refreshed {
                if std::env::var_os("HEW_DEBUG_OWNER_JOIN").is_some() {
                    eprintln!(
                        "HEW_DEBUG_OWNER_JOIN join-refresh target=bb{} edges={edge_inputs:?} replacement={replacement:?}",
                        block.id
                    );
                }
                *incoming = refreshed;
                changed = true;
            }
        }
    }
    changed
}

/// Redirect a direct CFG successor without touching any other edge of a
/// branch. Only plain `Goto`/`Branch` edges are admitted here; a call or
/// suspension edge has cleanup semantics that must remain validator-visible
/// until it has its own producer.
fn redirect_plain_successor(terminator: &mut Terminator, target: u32, replacement: u32) -> bool {
    match terminator {
        Terminator::Goto { target: current } if *current == target => {
            *current = replacement;
            true
        }
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => {
            let mut redirected = false;
            if *then_target == target {
                *then_target = replacement;
                redirected = true;
            }
            if *else_target == target {
                *else_target = replacement;
                redirected = true;
            }
            redirected
        }
        _ => false,
    }
}

fn can_redirect_plain_successor(terminator: &Terminator, target: u32) -> bool {
    match terminator {
        Terminator::Goto { target: current } => *current == target,
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => *then_target == target || *else_target == target,
        _ => false,
    }
}

/// Materialize one latch for a loop header with several direct backedges.
///
/// A `continue` can otherwise make the header's one CFG predecessor carry two
/// generations: the untouched header owner and an assignment replacement.
/// The latch restores the owner-SSA shape before a Join is emitted: every
/// backedge becomes an explicit predecessor of one inner Join, and the header
/// sees that Join's one successor on its sole cyclic edge. This is a CFG fact
/// derived from dominance, not a Builder owner history.
fn materialize_replayed_cyclic_join_latches(blocks: &mut Vec<BasicBlock>) -> bool {
    let dominators = block_dominators(blocks);
    let mut predecessors = HashMap::<u32, Vec<u32>>::new();
    for block in blocks.iter() {
        for successor in block.successors() {
            predecessors.entry(successor).or_default().push(block.id);
        }
    }
    for incoming in predecessors.values_mut() {
        incoming.sort_unstable();
        incoming.dedup();
    }
    let mut targets = predecessors.keys().copied().collect::<Vec<_>>();
    targets.sort_unstable();
    for target in targets {
        let Some(incoming) = predecessors.get(&target) else {
            continue;
        };
        let backedges = incoming
            .iter()
            .copied()
            .filter(|predecessor| {
                dominators
                    .get(predecessor)
                    .is_some_and(|set| set.contains(&target))
            })
            .collect::<Vec<_>>();
        if backedges.len() < 2 || backedges.len() == incoming.len() {
            continue;
        }
        let mut positions = blocks
            .iter()
            .enumerate()
            .map(|(index, block)| (block.id, index))
            .collect::<HashMap<_, _>>();
        if !backedges.iter().all(|predecessor| {
            positions
                .get(predecessor)
                .and_then(|index| blocks.get(*index))
                .is_some_and(|block| can_redirect_plain_successor(&block.terminator, target))
        }) {
            continue;
        }
        let Some(latch) = blocks
            .iter()
            .map(|block| block.id)
            .max()
            .and_then(|id| id.checked_add(1))
        else {
            continue;
        };
        for predecessor in &backedges {
            let Some(index) = positions.remove(predecessor) else {
                continue;
            };
            let redirected = redirect_plain_successor(&mut blocks[index].terminator, target, latch);
            debug_assert!(redirected, "preflight accepted a plain backedge");
        }
        if std::env::var_os("HEW_DEBUG_OWNER_JOIN").is_some() {
            eprintln!(
                "HEW_DEBUG_OWNER_JOIN latch target=bb{target} backedges={backedges:?} latch=bb{latch}"
            );
        }
        blocks.push(BasicBlock {
            id: latch,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Goto { target },
        });
        return true;
    }
    false
}

/// Return one immutable physical guard for each owner, omitting conflicting
/// publications.  A replayed rebind may preserve a guarded lineage only when
/// this fact proves the predecessor and successor carry the same guard.
fn replayed_owner_guards(
    blocks: &[BasicBlock],
) -> HashMap<crate::model::OwnerId, Option<(Place, crate::model::OwnershipGuardKind)>> {
    let mut guards = HashMap::new();
    for instruction in blocks.iter().flat_map(|block| &block.instructions) {
        let Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard { owner, flag, kind }) =
            instruction
        else {
            continue;
        };
        match guards.entry(*owner) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(Some((*flag, *kind)));
            }
            std::collections::hash_map::Entry::Occupied(mut entry)
                if entry
                    .get()
                    .is_some_and(|existing| existing != (*flag, *kind)) =>
            {
                entry.insert(None);
            }
            std::collections::hash_map::Entry::Occupied(_) => {}
        }
    }
    guards
}

/// Derive one assignment/rebind transition from the owner facts immediately
/// before it.  Lowering reserves a replacement identity while it builds raw
/// instructions, but that cursor cannot say whether a predecessor survives a
/// consuming edge.  The replay can: it emits a fresh Mint with no predecessor,
/// a Reset/Rearm only for one exact edge-local predecessor, and a successor
/// Transfer only from the owner actually live at its physical source.
///
/// Ambiguous, absent-at-the-source, wrong-place, and non-successor-generation
/// shapes deliberately return `None`.  They remain in the stream for the
/// unchanged Checked-MIR verifier rather than receiving a guessed repair.
#[allow(
    clippy::too_many_lines,
    reason = "the closed Mint/Reset/Rearm/Transfer transition table must inspect each replay fact together"
)]
fn replayed_edge_lifecycle_transition(
    event: &crate::model::OwnershipEvent,
    exact: &drop_plan::ExactOwnerState,
    maybe: &drop_plan::MaybeOwnerState,
    guards: &HashMap<crate::model::OwnerId, Option<(Place, crate::model::OwnershipGuardKind)>>,
) -> Option<crate::model::OwnershipEvent> {
    use crate::model::OwnershipEvent;

    match event {
        OwnershipEvent::Reset {
            replacement,
            place,
            ty,
            ..
        }
        | OwnershipEvent::Rearm {
            replacement,
            place,
            ty,
            ..
        } => {
            let mut maybe_predecessors = maybe
                .iter()
                .filter_map(|(owner, owner_place)| {
                    (owner.binding == replacement.binding).then_some((*owner, *owner_place))
                })
                .collect::<Vec<_>>();
            maybe_predecessors.sort_by_key(|(owner, _)| *owner);
            if maybe_predecessors.is_empty() {
                return Some(OwnershipEvent::Mint {
                    owner: *replacement,
                    place: *place,
                    ty: ty.clone(),
                });
            }
            let [(predecessor, predecessor_place)] = maybe_predecessors.as_slice() else {
                return None;
            };
            if predecessor_place != place
                || predecessor.generation.checked_add(1) != Some(replacement.generation)
            {
                return None;
            }
            let mut exact_predecessors = exact
                .iter()
                .filter_map(|(owner, owner_place)| {
                    (owner.binding == replacement.binding).then_some((*owner, *owner_place))
                })
                .collect::<Vec<_>>();
            exact_predecessors.sort_by_key(|(owner, _)| *owner);
            if !exact_predecessors.is_empty()
                && exact_predecessors.as_slice() != [(*predecessor, *place)]
            {
                return None;
            }
            let rearm = guards
                .get(predecessor)
                .copied()
                .flatten()
                .is_some_and(|guard| guards.get(replacement).copied().flatten() == Some(guard));
            Some(if rearm {
                OwnershipEvent::Rearm {
                    previous: *predecessor,
                    replacement: *replacement,
                    place: *place,
                    ty: ty.clone(),
                }
            } else {
                OwnershipEvent::Reset {
                    previous: *predecessor,
                    replacement: *replacement,
                    place: *place,
                    ty: ty.clone(),
                }
            })
        }
        OwnershipEvent::Transfer {
            owner,
            from,
            to,
            to_owner: Some(successor),
            to_ty: Some(ty),
        } => {
            let mut exact_sources = exact
                .iter()
                .filter_map(|(candidate, place)| (*place == *from).then_some(*candidate))
                .collect::<Vec<_>>();
            let mut maybe_sources = maybe
                .iter()
                .filter_map(|(candidate, place)| (*place == *from).then_some(*candidate))
                .collect::<Vec<_>>();
            exact_sources.sort_unstable();
            maybe_sources.sort_unstable();
            let [source] = exact_sources.as_slice() else {
                return None;
            };
            if maybe_sources.as_slice() != [*source] || source == owner {
                return None;
            }
            Some(OwnershipEvent::Transfer {
                owner: *source,
                from: *from,
                to: *to,
                to_owner: Some(*successor),
                to_ty: Some(ty.clone()),
            })
        }
        OwnershipEvent::Mint { .. }
        | OwnershipEvent::Transfer { .. }
        | OwnershipEvent::Relocate { .. }
        | OwnershipEvent::Release { .. }
        | OwnershipEvent::GuardedRelease { .. }
        | OwnershipEvent::DemoteToAlias { .. }
        | OwnershipEvent::Guard { .. }
        | OwnershipEvent::DropRecipe { .. }
        | OwnershipEvent::InteriorAlias { .. }
        | OwnershipEvent::AliasRelocate { .. }
        | OwnershipEvent::AliasEnd { .. }
        | OwnershipEvent::Join { .. }
        | OwnershipEvent::EdgeCarry { .. }
        | OwnershipEvent::ScopeExit { .. } => None,
    }
}

/// Materialize every reserved assignment/rebind transition from replayed
/// per-edge facts.  This is the producer authority for the first C2 slice;
/// the Builder's construction cursor no longer decides whether its historical
/// generation is Reset, Rearm, Mint, or the source of a successor transfer.
#[allow(
    clippy::too_many_lines,
    reason = "the exact physical handoff proof and its span-preserving removal form one transaction"
)]
fn prune_replayed_terminal_handoff_duplicates(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
) -> bool {
    let exact_states = drop_plan::exact_owner_states(blocks);
    let entries = &exact_states.0;
    let mut changed = false;
    for block in blocks {
        let mut live = entries.get(&block.id).cloned().unwrap_or_default();
        let mut terminal_sources = HashMap::<Place, crate::model::OwnerId>::new();
        let mut instruction_index = 0;
        while instruction_index < block.instructions.len() {
            let instruction = block.instructions[instruction_index].clone();
            if let Instr::Move { src, dest } | Instr::WitnessMove { src, dest, .. } = instruction {
                if let Some(terminal_owner) = terminal_sources.get(&src).copied() {
                    let spine_len = block.instructions[instruction_index + 1..]
                        .iter()
                        .take_while(|candidate| {
                            matches!(
                                candidate,
                                Instr::NeutralizePayloadSlot { .. } | Instr::OwnershipEvent(_)
                            )
                        })
                        .count();
                    let stale_indices = block.instructions
                        [instruction_index + 1..instruction_index + 1 + spine_len]
                        .iter()
                        .enumerate()
                        .filter_map(|(offset, candidate)| match candidate {
                            Instr::OwnershipEvent(crate::model::OwnershipEvent::Relocate {
                                owner,
                                from,
                                to,
                            }) if owner.binding == terminal_owner.binding
                                && *from == src
                                && *to == dest =>
                            {
                                Some(instruction_index + 1 + offset)
                            }
                            Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                                owner,
                                from,
                                to_owner: None,
                                ..
                            }) if owner.binding == terminal_owner.binding
                                && (*from == src || *from == dest) =>
                            {
                                Some(instruction_index + 1 + offset)
                            }
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    if !stale_indices.is_empty() {
                        if std::env::var_os("HEW_DEBUG_OWNER_JOIN").is_some() {
                            eprintln!(
                                "HEW_DEBUG_OWNER_JOIN terminal-handoff bb{} i{} owner={terminal_owner:?} source={src:?} destination={dest:?} removes={stale_indices:?}",
                                block.id, instruction_index
                            );
                        }
                        for stale_index in stale_indices.into_iter().rev() {
                            block.instructions.remove(stale_index);
                            shift_instr_spans_on_remove(
                                &mut builder.instr_spans,
                                block.id,
                                u32::try_from(stale_index).unwrap_or(u32::MAX),
                            );
                        }
                        changed = true;
                    }
                }
            }
            if let Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                owner,
                from,
                to: None,
                to_owner: None,
                ..
            }) = &instruction
            {
                if live.get(owner) == Some(from) {
                    terminal_sources.insert(*from, *owner);
                }
            }
            let definition_place = match &instruction {
                Instr::OwnershipEvent(
                    crate::model::OwnershipEvent::Mint { place, .. }
                    | crate::model::OwnershipEvent::Reset { place, .. }
                    | crate::model::OwnershipEvent::Rearm { place, .. }
                    | crate::model::OwnershipEvent::Join { place, .. }
                    | crate::model::OwnershipEvent::Transfer {
                        to: Some(place),
                        to_owner: Some(_),
                        ..
                    }
                    | crate::model::OwnershipEvent::Relocate { to: place, .. },
                ) => Some(*place),
                _ => None,
            };
            if let Some(place) = definition_place {
                terminal_sources.remove(&place);
            }
            drop_plan::apply_exact_owner_ops(std::slice::from_ref(&instruction), &mut live);
            instruction_index += 1;
        }
    }
    changed
}

/// Split a typed produced-value adoption into the two ownership facts it
/// actually represents: the temporary's terminal handoff and the target
/// binding's generation transition.
///
/// Assignment lowering first knows the physical `Move`, then publishes the
/// producer temporary's successor Transfer into the target slot.  That is
/// sufficient for a fresh `let`, but an existing `var` slot also has a
/// same-binding predecessor that must end at the assignment.  Replaying the
/// completed block makes that distinction explicit without asking the
/// Builder's generation cursor which historical owner it happened to reserve.
/// A conditional predecessor is legal only when its already-published guard
/// is the verifier's authority for the Reset.
#[allow(
    clippy::too_many_lines,
    reason = "the source terminal transfer and target lifecycle reservation must be published atomically"
)]
fn materialize_replayed_assignment_adoptions(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
) -> bool {
    let exact_states = drop_plan::exact_owner_states(blocks);
    let maybe_states = drop_plan::maybe_owner_states(blocks);
    let guards = replayed_owner_guards(blocks);
    let mut changed = false;

    for block in blocks {
        let mut exact = exact_states.0.get(&block.id).cloned().unwrap_or_default();
        let mut maybe = maybe_states.0.get(&block.id).cloned().unwrap_or_default();
        let mut physical_handoffs = HashSet::<(Place, Place)>::new();
        let mut instruction_index = 0;
        while instruction_index < block.instructions.len() {
            let instruction = block.instructions[instruction_index].clone();
            match &instruction {
                Instr::Move { src, dest } | Instr::WitnessMove { src, dest, .. } => {
                    physical_handoffs.insert((*src, *dest));
                }
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                    owner: source,
                    from,
                    to: Some(destination),
                    to_owner: Some(replacement),
                    to_ty: Some(ty),
                }) if source.binding != replacement.binding
                    && physical_handoffs.contains(&(*from, *destination)) =>
                {
                    let mut predecessors = maybe
                        .iter()
                        .filter_map(|(owner, place)| {
                            (owner.binding == replacement.binding
                                && *place == *destination
                                && owner.generation.checked_add(1) == Some(replacement.generation))
                            .then_some(*owner)
                        })
                        .collect::<Vec<_>>();
                    predecessors.sort_unstable();
                    let [previous] = predecessors.as_slice() else {
                        drop_plan::apply_exact_owner_ops(
                            std::slice::from_ref(&instruction),
                            &mut exact,
                        );
                        drop_plan::apply_maybe_owner_ops(
                            std::slice::from_ref(&instruction),
                            &mut maybe,
                        );
                        instruction_index += 1;
                        continue;
                    };
                    if !exact.contains_key(previous) && !guards.contains_key(previous) {
                        drop_plan::apply_exact_owner_ops(
                            std::slice::from_ref(&instruction),
                            &mut exact,
                        );
                        drop_plan::apply_maybe_owner_ops(
                            std::slice::from_ref(&instruction),
                            &mut maybe,
                        );
                        instruction_index += 1;
                        continue;
                    }
                    let terminal = Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                        owner: *source,
                        from: *from,
                        to: Some(*destination),
                        to_owner: None,
                        to_ty: None,
                    });
                    // This is a neutral reservation only.  The enclosing
                    // lifecycle replay immediately re-visits it and the one
                    // `replayed_edge_lifecycle_transition` producer selects
                    // Mint, Reset, or Rearm from the completed edge facts.
                    let transition = Instr::OwnershipEvent(crate::model::OwnershipEvent::Reset {
                        previous: *previous,
                        replacement: *replacement,
                        place: *destination,
                        ty: ty.clone(),
                    });
                    if std::env::var_os("HEW_DEBUG_OWNER_JOIN").is_some() {
                        eprintln!(
                            "HEW_DEBUG_OWNER_JOIN assignment-adoption bb{} i{} temporary={source:?}@{from:?} transition={previous:?}->{replacement:?}@{destination:?}",
                            block.id, instruction_index
                        );
                    }
                    block.instructions[instruction_index] = terminal.clone();
                    shift_instr_spans_on_insert(
                        &mut builder.instr_spans,
                        block.id,
                        u32::try_from(instruction_index + 1).unwrap_or(u32::MAX),
                    );
                    block
                        .instructions
                        .insert(instruction_index + 1, transition.clone());
                    drop_plan::apply_exact_owner_ops(
                        &[terminal.clone(), transition.clone()],
                        &mut exact,
                    );
                    drop_plan::apply_maybe_owner_ops(&[terminal, transition], &mut maybe);
                    changed = true;
                    instruction_index += 2;
                    continue;
                }
                _ => {}
            }
            drop_plan::apply_exact_owner_ops(std::slice::from_ref(&instruction), &mut exact);
            drop_plan::apply_maybe_owner_ops(std::slice::from_ref(&instruction), &mut maybe);
            instruction_index += 1;
        }
    }

    changed
}

/// Re-derive a static assignment Reset after a dominating Join has given the
/// loop header a later generation.
///
/// The assignment instruction is shared by every trip around a loop. Its
/// construction-time successor can therefore become historical after the
/// header Join is inserted, even though the physical store is still the next
/// generation boundary on every trip. The replay has one exact owner at the
/// Reset place; when its next unused generation is available, re-publish the
/// Reset from that owner and rename the uniquely-defined static successor.
/// This is a producer rewrite over immutable owner facts, not a Builder
/// generation-ledger repair.
#[allow(
    clippy::too_many_lines,
    reason = "the replay-selected static successor must be renamed across its complete immutable event stream"
)]
fn rederive_join_dominated_assignment_resets(blocks: &mut [BasicBlock]) -> bool {
    let exact_states = drop_plan::exact_owner_states(blocks);
    let maybe_states = drop_plan::maybe_owner_states(blocks);
    let definition_blocks = replayed_owner_definition_blocks(blocks);
    let allocated = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(event) => Some(ownership_event_owner_ids(event)),
            _ => None,
        })
        .flatten()
        .collect::<HashSet<_>>();
    let fresh_successor = |binding: BindingId| {
        allocated
            .iter()
            .filter_map(|owner| (owner.binding == binding).then_some(owner.generation))
            .max()
            .and_then(|generation| generation.checked_add(1))
            .map(|generation| crate::model::OwnerId {
                binding,
                generation,
            })
    };

    for block_index in 0..blocks.len() {
        let block_id = blocks[block_index].id;
        let mut exact = exact_states.0.get(&block_id).cloned().unwrap_or_default();
        let mut maybe = maybe_states.0.get(&block_id).cloned().unwrap_or_default();
        let mut released_at = HashMap::<(BindingId, Place), (crate::model::OwnerId, usize)>::new();
        for instruction_index in 0..blocks[block_index].instructions.len() {
            let instruction = blocks[block_index].instructions[instruction_index].clone();
            if let Instr::OwnershipEvent(crate::model::OwnershipEvent::Release { owner, place }) =
                &instruction
            {
                let mut candidates = exact
                    .iter()
                    .filter_map(|(candidate, candidate_place)| {
                        (candidate.binding == owner.binding && *candidate_place == *place)
                            .then_some(*candidate)
                    })
                    .collect::<Vec<_>>();
                candidates.sort_unstable();
                let released = candidates.first().copied().unwrap_or(*owner);
                released_at.insert((owner.binding, *place), (released, instruction_index));
            }
            if let Instr::OwnershipEvent(crate::model::OwnershipEvent::Reset {
                previous,
                replacement,
                place,
                ty,
            }) = &instruction
            {
                let mut exact_predecessors = exact
                    .iter()
                    .filter_map(|(owner, owner_place)| {
                        (owner.binding == previous.binding && *owner_place == *place)
                            .then_some(*owner)
                    })
                    .collect::<Vec<_>>();
                exact_predecessors.sort_unstable();
                if exact_predecessors.is_empty() {
                    if let Some((released, release_index)) =
                        released_at.get(&(previous.binding, *place)).copied()
                    {
                        let unique_static_successor = definition_blocks
                            .get(replacement)
                            .is_some_and(|definitions| definitions.as_slice() == [block_id]);
                        if replacement.binding == previous.binding && unique_static_successor {
                            if let Some(successor) = fresh_successor(replacement.binding) {
                                if std::env::var_os("HEW_DEBUG_OWNER_JOIN").is_some() {
                                    eprintln!(
                                    "HEW_DEBUG_OWNER_JOIN lifecycle-mint bb{block_id} i{instruction_index} released={released:?}@{place:?} replacement={replacement:?}->{successor:?}",
                                );
                                }
                                for block in blocks.iter_mut() {
                                    for instruction in &mut block.instructions {
                                        let Instr::OwnershipEvent(event) = instruction else {
                                            continue;
                                        };
                                        rewrite_ownership_event_owner(
                                            event,
                                            *replacement,
                                            successor,
                                        );
                                    }
                                }
                                let Instr::OwnershipEvent(crate::model::OwnershipEvent::Release {
                                    owner,
                                    ..
                                }) = &mut blocks[block_index].instructions[release_index]
                                else {
                                    unreachable!("the replay-selected release remains a Release");
                                };
                                *owner = released;
                                blocks[block_index].instructions[instruction_index] =
                                    Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
                                        owner: successor,
                                        place: *place,
                                        ty: ty.clone(),
                                    });
                                return true;
                            }
                        }
                    }
                    drop_plan::apply_exact_owner_ops(
                        std::slice::from_ref(&instruction),
                        &mut exact,
                    );
                    drop_plan::apply_maybe_owner_ops(
                        std::slice::from_ref(&instruction),
                        &mut maybe,
                    );
                    continue;
                }
                let [replayed_previous] = exact_predecessors.as_slice() else {
                    drop_plan::apply_exact_owner_ops(
                        std::slice::from_ref(&instruction),
                        &mut exact,
                    );
                    drop_plan::apply_maybe_owner_ops(
                        std::slice::from_ref(&instruction),
                        &mut maybe,
                    );
                    continue;
                };
                let Some(next_generation) = replayed_previous.generation.checked_add(1) else {
                    drop_plan::apply_exact_owner_ops(
                        std::slice::from_ref(&instruction),
                        &mut exact,
                    );
                    drop_plan::apply_maybe_owner_ops(
                        std::slice::from_ref(&instruction),
                        &mut maybe,
                    );
                    continue;
                };
                let successor = crate::model::OwnerId {
                    binding: replacement.binding,
                    generation: next_generation,
                };
                let unique_static_successor = definition_blocks
                    .get(replacement)
                    .is_some_and(|definitions| definitions.as_slice() == [block_id]);
                let predecessor_is_possible = maybe.iter().any(|(owner, owner_place)| {
                    *owner == *replayed_previous && *owner_place == *place
                });
                if *replayed_previous != *previous
                    && replacement.binding == previous.binding
                    && successor != *replacement
                    && !allocated.contains(&successor)
                    && unique_static_successor
                    && predecessor_is_possible
                {
                    if std::env::var_os("HEW_DEBUG_OWNER_JOIN").is_some() {
                        eprintln!(
                            "HEW_DEBUG_OWNER_JOIN lifecycle-reset bb{block_id} i{instruction_index} {previous:?}->{replacement:?} replayed={replayed_previous:?}->{successor:?}@{place:?}",
                        );
                    }
                    for block in blocks.iter_mut() {
                        for instruction in &mut block.instructions {
                            let Instr::OwnershipEvent(event) = instruction else {
                                continue;
                            };
                            rewrite_ownership_event_owner(event, *replacement, successor);
                        }
                    }
                    let Instr::OwnershipEvent(crate::model::OwnershipEvent::Reset {
                        previous,
                        replacement,
                        ..
                    }) = &mut blocks[block_index].instructions[instruction_index]
                    else {
                        unreachable!("the replay-selected instruction remains a Reset");
                    };
                    *previous = *replayed_previous;
                    *replacement = successor;
                    return true;
                }
            }
            drop_plan::apply_exact_owner_ops(std::slice::from_ref(&instruction), &mut exact);
            drop_plan::apply_maybe_owner_ops(std::slice::from_ref(&instruction), &mut maybe);
        }
    }
    false
}

pub(super) fn materialize_edge_lifecycle_owner_transitions(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
) {
    let _timing = crate::timing::stage("materialize_edge_lifecycle_owner_transitions");
    loop {
        if materialize_replayed_assignment_adoptions(blocks, builder) {
            continue;
        }
        if rederive_join_dominated_assignment_resets(blocks) {
            continue;
        }
        // Use the same two raw lattices that Checked MIR validates.  Lexical
        // scope bookkeeping may be useful to derive a later exit plan, but it
        // is not authority to resurrect a predecessor for an assignment
        // transition.
        let exact_states = drop_plan::exact_owner_states(blocks);
        let maybe_states = drop_plan::maybe_owner_states(blocks);
        let exact_entries = &exact_states.0;
        let maybe_entries = &maybe_states.0;
        let guards = replayed_owner_guards(blocks);
        let mut changed = false;
        for block in blocks.iter_mut() {
            let mut exact = exact_entries.get(&block.id).cloned().unwrap_or_default();
            let mut maybe = maybe_entries.get(&block.id).cloned().unwrap_or_default();
            for (instruction_index, instruction) in block.instructions.iter_mut().enumerate() {
                let Some(event) = (match instruction {
                    Instr::OwnershipEvent(event) => Some(event),
                    _ => None,
                }) else {
                    drop_plan::apply_exact_owner_ops(std::slice::from_ref(instruction), &mut exact);
                    drop_plan::apply_maybe_owner_ops(std::slice::from_ref(instruction), &mut maybe);
                    continue;
                };
                if let Some(next) =
                    replayed_edge_lifecycle_transition(event, &exact, &maybe, &guards)
                {
                    if *event != next {
                        if std::env::var_os("HEW_DEBUG_OWNER_JOIN").is_some() {
                            eprintln!(
                                "HEW_DEBUG_OWNER_JOIN lifecycle bb{} i{} {:?} -> {:?}",
                                block.id, instruction_index, event, next
                            );
                        }
                        *event = next;
                        changed = true;
                    }
                }
                drop_plan::apply_exact_owner_ops(std::slice::from_ref(instruction), &mut exact);
                drop_plan::apply_maybe_owner_ops(std::slice::from_ref(instruction), &mut maybe);
            }
        }
        changed |= prune_replayed_terminal_handoff_duplicates(blocks, builder);
        if !changed {
            break;
        }
    }
}

/// Publish one exact owner generation at a CFG join whose incoming edges carry
/// different generations of the same binding in the same physical slot.
///
/// Mutable loop-carried values are the canonical shape: the preheader carries
/// the initial generation while the back edge carries the reassignment
/// generation. Intersecting those IDs would erase ownership at the loop header;
/// reusing either predecessor ID would make the other edge stale. Each
/// single-successor edge therefore transfers its incoming `OwnerId` to one shared
/// successor `OwnerId`. Checked-MIR dataflow then meets the identical generation
/// at the header without consulting Builder generation ledgers.
#[allow(
    clippy::type_complexity,
    reason = "the four returned maps are the exact/maybe/must lattices consumed together by join sealing"
)]
pub(super) fn ownership_join_states(
    blocks: &[BasicBlock],
    binding_scopes: &HashMap<BindingId, ScopeId>,
    scope_info: &HashMap<ScopeId, ScopeInfoEntry>,
) -> (
    HashMap<u32, drop_plan::ExactOwnerState>,
    HashMap<u32, drop_plan::ExactOwnerState>,
    HashMap<u32, drop_plan::MaybeOwnerState>,
    HashMap<u32, drop_plan::MustBindingOwnerState>,
) {
    let _timing = crate::timing::stage("ownership_join_states");
    let mut exact_entries = HashMap::from([(ENTRY_BLOCK_ID, drop_plan::ExactOwnerState::new())]);
    let mut exact_exits = HashMap::new();
    let mut maybe_entries = HashMap::from([(ENTRY_BLOCK_ID, drop_plan::MaybeOwnerState::new())]);
    let mut maybe_exits = HashMap::new();
    let mut must_entries =
        HashMap::from([(ENTRY_BLOCK_ID, drop_plan::MustBindingOwnerState::new())]);
    let mut queue = std::collections::VecDeque::from([ENTRY_BLOCK_ID]);
    while let Some(block_id) = queue.pop_front() {
        let Some(block) = blocks.iter().find(|block| block.id == block_id) else {
            continue;
        };
        let mut exact = exact_entries.get(&block_id).cloned().unwrap_or_default();
        let mut maybe = maybe_entries.get(&block_id).cloned().unwrap_or_default();
        let mut must = must_entries.get(&block_id).cloned().unwrap_or_default();
        for instruction in &block.instructions {
            apply_lexical_scope_exit_to_exact_state(
                instruction,
                binding_scopes,
                scope_info,
                &mut exact,
            );
            apply_lexical_scope_exit_to_maybe_state(
                instruction,
                binding_scopes,
                scope_info,
                &mut maybe,
            );
        }
        apply_lexical_scope_exit_to_must_state(
            &block.instructions,
            binding_scopes,
            scope_info,
            &mut must,
        );
        exact_exits.insert(block_id, exact.clone());
        maybe_exits.insert(block_id, maybe.clone());
        for successor in block.successors() {
            let exact_changed = if let Some(existing) = exact_entries.get_mut(&successor) {
                let joined = existing
                    .iter()
                    .filter_map(|(owner, place)| {
                        (exact.get(owner) == Some(place)).then_some((*owner, *place))
                    })
                    .collect();
                if *existing == joined {
                    false
                } else {
                    *existing = joined;
                    true
                }
            } else {
                exact_entries.insert(successor, exact.clone());
                true
            };
            let maybe_changed = if let Some(existing) = maybe_entries.get_mut(&successor) {
                let before = existing.len();
                existing.extend(maybe.iter().copied());
                existing.len() != before
            } else {
                maybe_entries.insert(successor, maybe.clone());
                true
            };
            let must_changed = if let Some(existing) = must_entries.get_mut(&successor) {
                let joined = existing
                    .iter()
                    .filter_map(|(binding, place)| {
                        (must.get(binding) == Some(place)).then_some((*binding, *place))
                    })
                    .collect();
                if *existing == joined {
                    false
                } else {
                    *existing = joined;
                    true
                }
            } else {
                must_entries.insert(successor, must.clone());
                true
            };
            if exact_changed || maybe_changed || must_changed {
                queue.push_back(successor);
            }
        }
    }
    (exact_entries, exact_exits, maybe_exits, must_entries)
}
#[allow(
    clippy::match_same_arms,
    clippy::too_many_lines,
    reason = "ownership SSA join construction validates predecessors, splits edges, and renames one generation atomically"
)]
pub(super) fn materialize_exact_owner_join_transfers(
    blocks: &mut Vec<BasicBlock>,
    builder: &mut Builder,
) {
    let _timing = crate::timing::stage("materialize_exact_owner_join_transfers");
    loop {
        if materialize_replayed_cyclic_join_latches(blocks) {
            continue;
        }
        let (entries, exits, maybe_exits, must_entries) =
            ownership_join_states(blocks, &builder.binding_scope, &builder.scope_info);
        // Position of every block by id. The phi derivation resolves a
        // predecessor, a target, and a generation's definition block by id
        // inside per-binding loops; scanning `blocks` for each made the pass
        // quadratic in block count. Kept current below when an edge block is
        // appended, so a lookup never misses a block the pass itself created.
        let mut block_positions: HashMap<u32, usize> = blocks
            .iter()
            .enumerate()
            .map(|(position, block)| (block.id, position))
            .collect();
        let owner_types: HashMap<crate::model::OwnerId, ResolvedTy> = blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .filter_map(|instruction| match instruction {
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint { owner, ty, .. }) => {
                    Some((*owner, ty.clone()))
                }
                Instr::OwnershipEvent(
                    crate::model::OwnershipEvent::Reset {
                        replacement, ty, ..
                    }
                    | crate::model::OwnershipEvent::Rearm {
                        replacement, ty, ..
                    },
                ) => Some((*replacement, ty.clone())),
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                    to_owner: Some(owner),
                    to_ty: Some(ty),
                    ..
                }) => Some((*owner, ty.clone())),
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Join {
                    replacement,
                    ty,
                    ..
                }) => Some((*replacement, ty.clone())),
                _ => None,
            })
            .collect();
        let mut predecessors: HashMap<u32, Vec<u32>> = HashMap::new();
        for block in blocks.iter() {
            for successor in block.successors() {
                predecessors.entry(successor).or_default().push(block.id);
            }
        }
        for incoming in predecessors.values_mut() {
            incoming.sort_unstable();
            incoming.dedup();
        }
        let edge_dominators = block_dominators(blocks);
        let definition_blocks = replayed_owner_definition_blocks(blocks);
        if refresh_replayed_join_inputs(
            blocks,
            &predecessors,
            &exits,
            &maybe_exits,
            &definition_blocks,
            &edge_dominators,
        ) {
            continue;
        }
        let mut max_generation = HashMap::<BindingId, u32>::new();
        for owner in owner_types.keys() {
            max_generation
                .entry(owner.binding)
                .and_modify(|generation| *generation = (*generation).max(owner.generation))
                .or_insert(owner.generation);
        }

        // Exact OwnerId intersection intentionally erases a binding when a
        // cyclic join receives different generations. Generation-erased
        // must-own replay distinguishes that valid ownership SSA shape from a
        // path where the owner is genuinely absent. Publish a first-class Join
        // block parameter only when every predecessor must own the same
        // binding/place; its incoming set is the explicit set of generations
        // that may reach those edges.
        let mut join_parameter = None;
        let mut join_targets = predecessors.keys().copied().collect::<Vec<_>>();
        join_targets.sort_by(|left, right| {
            let left_depth = edge_dominators.get(left).map_or(0, HashSet::len);
            let right_depth = edge_dominators.get(right).map_or(0, HashSet::len);
            right_depth.cmp(&left_depth).then_with(|| left.cmp(right))
        });
        'join_targets: for target in join_targets {
            let incoming_blocks = &predecessors[&target];
            if incoming_blocks.len() < 2 {
                continue;
            }
            let Some(must_live) = must_entries.get(&target) else {
                continue;
            };
            let Some(target_block) = block_positions
                .get(&target)
                .map(|position| &blocks[*position])
            else {
                continue;
            };
            let mut bindings = must_live.iter().collect::<Vec<_>>();
            bindings.sort_by_key(|(binding, _)| binding.0);
            for (binding, place) in bindings {
                if target_block.instructions.iter().any(|instruction| {
                    matches!(
                        instruction,
                        Instr::OwnershipEvent(crate::model::OwnershipEvent::Join {
                            replacement,
                            ..
                        }) if replacement.binding == *binding
                    )
                }) {
                    continue;
                }
                let Some(edge_inputs) = replayed_edge_owner_inputs(
                    incoming_blocks,
                    *binding,
                    *place,
                    &exits,
                    &maybe_exits,
                    &definition_blocks,
                    &edge_dominators,
                ) else {
                    continue;
                };
                let mut incoming = edge_inputs.values().copied().collect::<Vec<_>>();
                incoming.sort_by_key(|owner| owner.generation);
                incoming.dedup();
                if incoming.len() < 2 {
                    // An explicit map with one distinct value is already an
                    // SSA identity. Do not invent a one-input Join merely to
                    // rename it: that can feed its successor back around a
                    // loop without adding any edge fact.
                    continue;
                }
                let Some(ty) = incoming
                    .first()
                    .and_then(|owner| owner_types.get(owner))
                    .cloned()
                else {
                    continue;
                };
                if incoming
                    .iter()
                    .any(|owner| owner_types.get(owner) != Some(&ty))
                {
                    continue;
                }
                let Some(generation) = max_generation
                    .get(binding)
                    .copied()
                    .unwrap_or(0)
                    .checked_add(1)
                else {
                    continue;
                };
                let replacement = crate::model::OwnerId {
                    binding: *binding,
                    generation,
                };
                if incoming.contains(&replacement) {
                    continue;
                }
                if std::env::var_os("HEW_DEBUG_OWNER_JOIN").is_some() {
                    eprintln!(
                        "HEW_DEBUG_OWNER_JOIN join target=bb{target} binding={binding:?} place={place:?} edges={edge_inputs:?} replacement={replacement:?}"
                    );
                }
                join_parameter = Some((target, incoming, replacement, *place, ty));
                break 'join_targets;
            }
        }
        if let Some((target, incoming, replacement, place, ty)) = join_parameter {
            let Some(block) = blocks.iter_mut().find(|block| block.id == target) else {
                continue;
            };
            block.instructions.insert(
                0,
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Join {
                    incoming,
                    replacement,
                    place,
                    ty,
                }),
            );
            // Freeze the Join's inherited physical guard before a later
            // replay derives any sequential replacement.  The owner facts,
            // rather than the construction cursor, remain the authority for
            // that successor's identity.
            drop_plan::materialize_successor_guard_authority(blocks);
            // Introducing a block parameter changes the exact generation
            // seen by static overwrite/consume operations in its dominated
            // region. Re-key those terminal operations immediately, before
            // another join-search iteration can mistake the stale identity
            // for a second live generation.
            deduplicate_ownership_spines(blocks, builder);
            canonicalize_terminal_transfer_owner_ids(blocks);
            canonicalize_release_owner_ids(blocks);
            continue;
        }

        // The join-parameter phase above `continue`s the fixpoint whenever it
        // changes anything, so `blocks` is frozen from here to the mutation at
        // the end of this iteration. Dominance is therefore derived at most
        // once here and read by both the back-edge admission below and the
        // rename that follows a selection. Deriving it inside the per-binding
        // admission recomputed the whole dominator fixpoint per candidate;
        // deriving it unconditionally paid for the fixpoint on the many
        // iterations that select nothing needing it.
        let mut join_dominators: Option<HashMap<u32, HashSet<u32>>> = None;
        let mut targets = predecessors.keys().copied().collect::<Vec<_>>();
        targets.sort_unstable();
        let mut selected = None;
        'targets: for target in targets {
            let incoming = &predecessors[&target];
            if incoming.len() < 2
                || incoming.iter().any(|predecessor| {
                    block_positions
                        .get(predecessor)
                        .map(|position| &blocks[*position])
                        .is_none_or(|block| {
                            let successors = block.successors();
                            successors != vec![target]
                                && !matches!(
                                    block.terminator,
                                    Terminator::Branch {
                                        then_target,
                                        else_target,
                                        ..
                                    } if then_target == target || else_target == target
                                )
                        })
                })
            {
                continue;
            }
            let Some(first_state) = exits.get(&incoming[0]) else {
                continue;
            };
            let mut bindings = first_state
                .keys()
                .map(|owner| owner.binding)
                .collect::<Vec<_>>();
            bindings.sort_by_key(|binding| binding.0);
            bindings.dedup();
            for binding in bindings {
                // A first-class block Join already is the ownership-SSA
                // parameter for this binding.  Do not stack the older
                // edge-transfer phi on top of it: doing so creates two live
                // generations at the block entry and makes the later static
                // overwrite name whichever duplicate happened to be visited
                // last.  The Join interpreter canonicalizes every incoming
                // generation of the binding to its one replacement.
                if block_positions
                    .get(&target)
                    .map(|position| &blocks[*position])
                    .is_some_and(|block| {
                        block.instructions.iter().any(|instruction| {
                            matches!(
                                instruction,
                                Instr::OwnershipEvent(
                                    crate::model::OwnershipEvent::Join { replacement, .. }
                                ) if replacement.binding == binding
                            )
                        })
                    })
                {
                    continue;
                }
                let mut first_matching = first_state
                    .iter()
                    .filter_map(|(owner, place)| {
                        (owner.binding == binding).then_some((*owner, *place))
                    })
                    .collect::<Vec<_>>();
                first_matching.sort_by_key(|(owner, _)| *owner);
                let [(_, place)] = first_matching.as_slice() else {
                    continue;
                };
                let Some(edge_inputs) = replayed_edge_owner_inputs(
                    incoming,
                    binding,
                    *place,
                    &exits,
                    &maybe_exits,
                    &definition_blocks,
                    &edge_dominators,
                ) else {
                    continue;
                };
                let owners = edge_inputs
                    .iter()
                    .map(|(predecessor, owner)| (*predecessor, *owner, *place))
                    .collect::<Vec<_>>();
                if owners.windows(2).all(|pair| pair[0].1 == pair[1].1) {
                    continue;
                }
                let Some(ty) = owners
                    .first()
                    .and_then(|(_, owner, _)| owner_types.get(owner))
                    .cloned()
                else {
                    continue;
                };
                if owners
                    .iter()
                    .any(|(_, owner, _)| owner_types.get(owner) != Some(&ty))
                {
                    continue;
                }
                // A backedge may carry either an edge-local definition or a
                // successor generation minted earlier in the loop after the
                // header generation was terminally consumed. The latter is
                // the ordinary `iter = iter.next().1` shape. Admit it only
                // when its unique definition dominates the backedge and the
                // exact state immediately before that definition contains no
                // live generation of the same binding.
                let dominators = join_dominators.get_or_insert_with(|| block_dominators(blocks));
                let backedge_owner_is_exact_successor =
                    owners.iter().all(|(predecessor, owner, _)| {
                        let is_back_edge = dominators
                            .get(predecessor)
                            .is_some_and(|set| set.contains(&target));
                        if !is_back_edge {
                            return true;
                        }
                        let definitions = blocks
                            .iter()
                            .flat_map(|block| {
                                block.instructions.iter().enumerate().filter_map(
                                    move |(instruction_index, instruction)| {
                                        matches!(
                                            instruction,
                                            Instr::OwnershipEvent(
                                                crate::model::OwnershipEvent::Mint {
                                                    owner: definition,
                                                    ..
                                                }
                                                    | crate::model::OwnershipEvent::Reset {
                                                        replacement: definition,
                                                        ..
                                                    }
                                                    | crate::model::OwnershipEvent::Rearm {
                                                        replacement: definition,
                                                        ..
                                                    }
                                                    | crate::model::OwnershipEvent::Transfer {
                                                        to_owner: Some(definition),
                                                        ..
                                                    }
                                            ) if definition == owner
                                        )
                                        .then_some((block.id, instruction_index))
                                    },
                                )
                            })
                            .collect::<Vec<_>>();
                        // A generation produced by an earlier CFG phi has one
                        // explicit Transfer definition on every incoming edge,
                        // not one global definition. Treat those edge-local
                        // writes as one definition when they all converge on a
                        // common join that dominates this backedge. This keeps
                        // nested branch/join ownership composable: the inner
                        // phi is an ordinary exact generation at the outer loop
                        // header, without falling back to Builder ancestry.
                        let is_prior_join_phi = definitions.len() >= 2
                            && blocks.iter().any(|join| {
                                definitions.iter().all(|(definition_block, index)| {
                                    let Some(definition) = block_positions
                                        .get(definition_block)
                                        .map(|position| &blocks[*position])
                                    else {
                                        return false;
                                    };
                                    definition.successors().contains(&join.id)
                                        && matches!(
                                            definition.instructions.get(*index),
                                            Some(Instr::OwnershipEvent(
                                                crate::model::OwnershipEvent::Transfer {
                                                    to_owner: Some(next),
                                                    to: Some(_),
                                                    ..
                                                }
                                            )) if *next == *owner
                                        )
                                }) && dominators
                                    .get(&join.id)
                                    .is_some_and(|set| set.contains(&target))
                                    && dominators
                                        .get(predecessor)
                                        .is_some_and(|set| set.contains(&join.id))
                            });
                        if is_prior_join_phi {
                            return true;
                        }
                        let [(definition_block, instruction_index)] = definitions.as_slice() else {
                            return false;
                        };
                        if !dominators
                            .get(definition_block)
                            .is_some_and(|set| set.contains(&target))
                            || !dominators
                                .get(predecessor)
                                .is_some_and(|set| set.contains(definition_block))
                        {
                            return false;
                        }
                        let Some(definition) = block_positions
                            .get(definition_block)
                            .map(|position| &blocks[*position])
                        else {
                            return false;
                        };
                        let Some(mut live_before_definition) =
                            entries.get(definition_block).cloned()
                        else {
                            return false;
                        };
                        drop_plan::apply_exact_owner_ops(
                            &definition.instructions[..*instruction_index],
                            &mut live_before_definition,
                        );
                        !live_before_definition
                            .keys()
                            .any(|candidate| candidate.binding == binding)
                    });
                if !backedge_owner_is_exact_successor {
                    continue;
                }
                let Some(generation) = max_generation
                    .get(&binding)
                    .copied()
                    .unwrap_or(0)
                    .checked_add(1)
                else {
                    continue;
                };
                let replacement = crate::model::OwnerId {
                    binding,
                    generation,
                };
                if owners.iter().any(|(_, owner, _)| *owner == replacement) {
                    continue;
                }
                selected = Some((target, owners, replacement, ty));
                break 'targets;
            }
        }
        let Some((target, owners, replacement, ty)) = selected else {
            break;
        };
        if std::env::var_os("HEW_DEBUG_OWNER_JOIN").is_some() {
            let edges = owners
                .iter()
                .map(|(predecessor, owner, _)| (*predecessor, *owner))
                .collect::<EdgeOwnerInputs>();
            eprintln!(
                "HEW_DEBUG_OWNER_JOIN transfer-phi target=bb{target} edges={edges:?} replacement={replacement:?} ty={ty:?}"
            );
        }
        let dominators = join_dominators.get_or_insert_with(|| block_dominators(blocks));
        let rewrite_after_join = owners
            .iter()
            .filter_map(|(predecessor, owner, _)| {
                let is_back_edge = dominators
                    .get(predecessor)
                    .is_some_and(|set| set.contains(&target));
                (!is_back_edge).then_some((*owner, replacement))
            })
            .collect::<HashMap<_, _>>();
        for (predecessor, owner, place) in &owners {
            let transfer = Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                owner: *owner,
                from: *place,
                to: Some(*place),
                to_owner: Some(replacement),
                to_ty: Some(ty.clone()),
            });
            let Some(predecessor_index) = block_positions.get(predecessor).copied() else {
                continue;
            };
            if blocks[predecessor_index].successors() == vec![target] {
                blocks[predecessor_index].instructions.push(transfer);
                continue;
            }

            // Checked arithmetic and other trapping operations commonly make
            // a loop latch a two-way Branch: one edge returns to the header,
            // while the other traps.  The ownership phi belongs only to the
            // header edge.  Split that edge instead of either dropping the
            // transfer or publishing it before the Branch (which would rename
            // ownership on the unrelated exit as well).
            let edge_id = blocks
                .iter()
                .map(|block| block.id)
                .max()
                .unwrap_or(ENTRY_BLOCK_ID)
                .saturating_add(1);
            let Terminator::Branch {
                then_target,
                else_target,
                ..
            } = &mut blocks[predecessor_index].terminator
            else {
                continue;
            };
            if *then_target == target {
                *then_target = edge_id;
            }
            if *else_target == target {
                *else_target = edge_id;
            }
            block_positions.insert(edge_id, blocks.len());
            blocks.push(BasicBlock {
                id: edge_id,
                statements: Vec::new(),
                instructions: vec![transfer],
                terminator: Terminator::Goto { target },
            });
        }
        // Static instructions in a loop body are shared by the first and every
        // subsequent iteration.  Rewrite references to the non-backedge
        // incoming identity to the phi identity throughout the join-dominated
        // region; backedge definitions remain branch-local until their edge
        // transfer above.  This is ordinary SSA renaming over explicit MIR
        // owner operations, not retrospective move ancestry.
        for block in blocks.iter_mut().filter(|block| {
            dominators
                .get(&block.id)
                .is_some_and(|set| set.contains(&target))
        }) {
            for instruction in &mut block.instructions {
                let Instr::OwnershipEvent(event) = instruction else {
                    continue;
                };
                match event {
                    crate::model::OwnershipEvent::Transfer {
                        owner, to_owner, ..
                    } => {
                        if *to_owner != Some(replacement) {
                            if let Some(next) = rewrite_after_join.get(owner) {
                                *owner = *next;
                            }
                        }
                    }
                    crate::model::OwnershipEvent::Relocate { owner, .. }
                    | crate::model::OwnershipEvent::Release { owner, .. }
                    | crate::model::OwnershipEvent::GuardedRelease { owner, .. }
                    | crate::model::OwnershipEvent::DemoteToAlias { owner, .. }
                    | crate::model::OwnershipEvent::Guard { owner, .. } => {
                        if let Some(next) = rewrite_after_join.get(owner) {
                            *owner = *next;
                        }
                    }
                    crate::model::OwnershipEvent::Reset { previous, .. }
                    | crate::model::OwnershipEvent::Rearm { previous, .. } => {
                        if let Some(next) = rewrite_after_join.get(previous) {
                            *previous = *next;
                        }
                    }
                    crate::model::OwnershipEvent::Mint { .. }
                    | crate::model::OwnershipEvent::DropRecipe { .. }
                    | crate::model::OwnershipEvent::Join { .. }
                    | crate::model::OwnershipEvent::InteriorAlias { .. }
                    | crate::model::OwnershipEvent::AliasRelocate { .. }
                    | crate::model::OwnershipEvent::AliasEnd { .. }
                    | crate::model::OwnershipEvent::EdgeCarry { .. }
                    | crate::model::OwnershipEvent::ScopeExit { .. } => {}
                }
            }
        }
        // Installing this phi can make an overwrite's formerly provisional
        // Release name resolvable at its exact entry state. Normalize that
        // physical release before searching for another phi; otherwise the
        // stale identity fails to end the incoming generation and the next
        // iteration invents a second, spurious join generation from it.
        deduplicate_ownership_spines(blocks, builder);
        canonicalize_terminal_transfer_owner_ids(blocks);
        canonicalize_release_owner_ids(blocks);
    }
}
