//! Path-exact release of owners consumed on only some paths into a join.
//!
//! Source lowering ends an owner's generation where the value is moved
//! (returned, stored into an aggregate, sent, handed to a consuming callee).
//! When that move sits on only one path into a join, the sibling path still
//! owns the value: no static cleanup after the join is admissible (it would
//! double-free the moved path), and the owner is unusable after the join
//! (the move checker rejects a use after a `MaybeConsumed` merge). The value
//! is therefore dead on the owning path exactly at the join edge, so its
//! release belongs on that edge. This pass materializes it from ownership
//! replay: for every join, every unguarded generation that is possibly live
//! but not exactly live on entry, and was ended on at least one incoming
//! path, is dropped on each predecessor edge where it is still exactly live.
//!
//! Guarded generations are excluded: their runtime flag already selects the
//! release path. Generations that are merely absent on some paths (never
//! minted there) owe nothing and are left alone. A generation renamed by a
//! lineage `Join` at the join block belongs to that rule. And a release is
//! only placed where the owning storage is physically dead on entry to the
//! join (no read reaches it before a redefinition): a producer that moves a
//! value without publishing the Transfer keeps its leak reported by the
//! verifier rather than turned into a use-after-free here.
use super::{
    exact_owner_states, inline_drop_spec_for_recipe, maybe_ended_owner_states, maybe_owner_states,
    place_refs_local, terminator_source_places, BasicBlock, Builder, HashMap, HashSet, Instr,
    Place, SuspendKind, Terminator,
};
use crate::lower::cfg_util::shift_instr_spans_on_insert;
use crate::lower::redirect_terminator_successor;
use crate::lower::suspend_places::instr_source_places;
use crate::model::{OwnerDropRecipe, OwnerId, OwnershipEvent};

/// Whether `local` may still be read on some path from the entry of `block`
/// before an instruction fully redefines it. Backward liveness over the CFG;
/// a full redefinition is a whole-local `Move` destination, a whole-local
/// `NeutralizePayloadSlot`, or a call destination. Partial writes (field or
/// variant stores) count as reads, so the answer errs towards "live".
fn local_live_at_block_entry(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    block: u32,
    local: u32,
) -> bool {
    let whole = Place::Local(local);
    let mut block_reads = HashMap::<u32, bool>::new();
    let mut block_defs = HashMap::<u32, bool>::new();
    for candidate in blocks {
        let mut reads_first = false;
        let mut defined = false;
        for instruction in &candidate.instructions {
            if instruction_reads_local(instruction, local) {
                reads_first = true;
                break;
            }
            let redefines = match instruction {
                Instr::Move { dest, .. } => *dest == whole,
                Instr::NeutralizePayloadSlot { place, .. } => *place == whole,
                _ => false,
            };
            if redefines {
                defined = true;
                break;
            }
        }
        if !reads_first && !defined {
            if terminator_source_places(&candidate.terminator, suspend_kinds.get(&candidate.id))
                .into_iter()
                .any(|place| place_refs_local(place, local))
            {
                reads_first = true;
            } else if matches!(&candidate.terminator, Terminator::Call { dest: Some(dest), .. } if *dest == whole)
            {
                defined = true;
            }
        }
        block_reads.insert(candidate.id, reads_first);
        block_defs.insert(candidate.id, defined);
    }
    let mut live_in = HashMap::<u32, bool>::new();
    let mut changed = true;
    while changed {
        changed = false;
        for candidate in blocks {
            let live_out = candidate
                .successors()
                .into_iter()
                .any(|successor| live_in.get(&successor).copied().unwrap_or(false));
            let next = block_reads[&candidate.id] || (!block_defs[&candidate.id] && live_out);
            if live_in.get(&candidate.id).copied().unwrap_or(false) != next {
                live_in.insert(candidate.id, next);
                changed = true;
            }
        }
    }
    live_in.get(&block).copied().unwrap_or(false)
}

fn instruction_reads_local(instruction: &Instr, local: u32) -> bool {
    instr_source_places(instruction)
        .into_iter()
        .any(|place| place_refs_local(place, local))
        || match instruction {
            // A partial write into the local's storage keeps the rest of the
            // value alive; only a whole-local write is a redefinition.
            Instr::Move { dest, .. } => {
                *dest != Place::Local(local) && place_refs_local(*dest, local)
            }
            _ => false,
        }
}

/// One release scheduled on the `predecessor -> target` edge.
#[derive(Debug, Clone, PartialEq)]
pub(in crate::lower) struct EdgeRelease {
    pub predecessor: u32,
    pub target: u32,
    pub owner: OwnerId,
    pub place: Place,
    pub recipe: OwnerDropRecipe,
}

/// Derive the edge releases from replay without mutating the CFG.
pub(in crate::lower) fn conditional_consume_edge_releases(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
) -> Vec<EdgeRelease> {
    let (exact_entries, exact_exits) = exact_owner_states(blocks);
    let (maybe_entries, _) = maybe_owner_states(blocks);
    let (ended_entries, _) = maybe_ended_owner_states(blocks);

    let mut recipes = HashMap::<OwnerId, Vec<OwnerDropRecipe>>::new();
    // Guarded generations select their release path at runtime and stay
    // with the guard rules.
    let mut guarded = HashSet::new();
    // Generations a lineage `Join` renames at a block are consumed by that
    // rename, never by an edge drop into the same block.
    let mut joined_at = HashMap::<u32, HashSet<OwnerId>>::new();
    for block in blocks {
        for instruction in &block.instructions {
            match instruction {
                Instr::OwnershipEvent(OwnershipEvent::DropRecipe { owner, recipe }) => {
                    recipes.entry(*owner).or_default().push(recipe.clone());
                }
                Instr::OwnershipEvent(OwnershipEvent::Guard { owner, .. }) => {
                    guarded.insert(*owner);
                }
                Instr::OwnershipEvent(OwnershipEvent::Join { incoming, .. }) => {
                    joined_at
                        .entry(block.id)
                        .or_default()
                        .extend(incoming.iter().copied());
                }
                _ => {}
            }
        }
    }

    let mut predecessors = HashMap::<u32, Vec<u32>>::new();
    for block in blocks {
        for successor in block.successors() {
            let entry = predecessors.entry(successor).or_default();
            if !entry.contains(&block.id) {
                entry.push(block.id);
            }
        }
    }

    let mut releases = Vec::new();
    let mut targets = predecessors.keys().copied().collect::<Vec<_>>();
    targets.sort_unstable();
    for target in targets {
        let incoming = &predecessors[&target];
        if incoming.len() < 2 {
            continue;
        }
        let exact = exact_entries.get(&target).cloned().unwrap_or_default();
        let ended = ended_entries.get(&target).cloned().unwrap_or_default();
        let joined = joined_at.get(&target);
        let mut candidates = maybe_entries
            .get(&target)
            .into_iter()
            .flatten()
            .filter_map(|(owner, _)| {
                (!exact.contains_key(owner)
                    && ended.contains(owner)
                    && !guarded.contains(owner)
                    && joined.is_none_or(|joined| !joined.contains(owner)))
                .then_some(*owner)
            })
            .collect::<Vec<_>>();
        candidates.sort_unstable();
        candidates.dedup();
        for owner in candidates {
            let Some([recipe]) = recipes.get(&owner).map(Vec::as_slice) else {
                continue;
            };
            if inline_drop_spec_for_recipe(recipe).is_none() {
                continue;
            }
            for predecessor in incoming {
                let Some(place) = exact_exits
                    .get(predecessor)
                    .and_then(|state| state.get(&owner))
                    .copied()
                else {
                    continue;
                };
                let Place::Local(local) = place else {
                    continue;
                };
                if local_live_at_block_entry(blocks, suspend_kinds, target, local) {
                    continue;
                }
                releases.push(EdgeRelease {
                    predecessor: *predecessor,
                    target,
                    owner,
                    place,
                    recipe: recipe.clone(),
                });
            }
        }
    }
    releases
}

/// Materialize every replay-derived edge release into the Raw-MIR CFG.
///
/// A `Goto` predecessor receives the drop inline before its terminator; any
/// other predecessor has the edge split through a fresh block so the release
/// runs after the terminator's own effects (a call whose argument is the
/// released place must complete first). Goto edge witnesses are republished
/// by the caller after this pass, so no `EdgeCarry` is authored here.
pub(in crate::lower) fn materialize_conditional_consume_releases(
    blocks: &mut Vec<BasicBlock>,
    builder: &mut Builder,
) {
    let releases = conditional_consume_edge_releases(blocks, &builder.suspend_kinds);
    if releases.is_empty() {
        return;
    }
    // Group per edge so one split block serves every owner released there.
    let mut per_edge = HashMap::<(u32, u32), Vec<EdgeRelease>>::new();
    let mut edge_order = Vec::new();
    for release in releases {
        let key = (release.predecessor, release.target);
        if !per_edge.contains_key(&key) {
            edge_order.push(key);
        }
        per_edge.entry(key).or_default().push(release);
    }
    for key in edge_order {
        let mut edge_releases = per_edge.remove(&key).unwrap_or_default();
        // Later declarations release first, matching lexical LIFO cleanup.
        edge_releases.sort_by(|left, right| {
            right
                .recipe
                .declaration_order
                .cmp(&left.recipe.declaration_order)
                .then_with(|| right.owner.cmp(&left.owner))
        });
        let instructions = edge_releases
            .iter()
            .flat_map(|release| {
                let drop_fn = inline_drop_spec_for_recipe(&release.recipe)
                    .expect("edge releases are derived only for inline-droppable recipes");
                [
                    Instr::Drop {
                        place: release.place,
                        ty: release.recipe.ty.clone(),
                        drop_fn: Some(drop_fn),
                    },
                    Instr::OwnershipEvent(OwnershipEvent::Release {
                        owner: release.owner,
                        place: release.place,
                    }),
                ]
            })
            .collect::<Vec<_>>();
        let (predecessor, target) = key;
        let Some(block_index) = blocks.iter().position(|block| block.id == predecessor) else {
            continue;
        };
        if matches!(blocks[block_index].terminator, Terminator::Goto { .. }) {
            let block = &mut blocks[block_index];
            for instruction in instructions {
                let at = block.instructions.len();
                block.instructions.push(instruction);
                shift_instr_spans_on_insert(
                    &mut builder.instr_spans,
                    predecessor,
                    u32::try_from(at).unwrap_or(u32::MAX),
                );
            }
            continue;
        }
        let split = builder.alloc_block();
        assert!(
            blocks.iter().all(|block| block.id != split),
            "block allocator handed out an id already present in the CFG"
        );
        let redirected =
            redirect_terminator_successor(&mut blocks[block_index].terminator, target, split);
        assert!(
            redirected,
            "predecessor {predecessor} no longer reaches join {target}"
        );
        blocks.push(BasicBlock {
            id: split,
            statements: Vec::new(),
            instructions,
            terminator: Terminator::Goto { target },
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lower::drop_plan::validate_ownership_events;
    use crate::model::{DropFnSpec, DropKind, OwnershipGuardKind};
    use crate::CheckedMirFunction;
    use hew_hir::BindingId;
    use hew_types::ResolvedTy;

    fn owner(binding: u32) -> OwnerId {
        OwnerId {
            binding: BindingId(binding),
            generation: 0,
        }
    }

    fn string_recipe(declaration_order: u32) -> OwnerDropRecipe {
        OwnerDropRecipe {
            ty: ResolvedTy::String,
            drop_fn: None,
            kind: DropKind::CowHeap {
                release: crate::ownership::CowHeapRelease::String,
            },
            declaration_order,
        }
    }

    fn mint(owner: OwnerId, place: Place, declaration_order: u32) -> [Instr; 2] {
        [
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner,
                place,
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                owner,
                recipe: string_recipe(declaration_order),
            }),
        ]
    }

    fn consume(owner: OwnerId, place: Place) -> Instr {
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner,
            from: place,
            to: Some(Place::ReturnSlot),
            to_owner: None,
            to_ty: None,
        })
    }

    fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![],
            instructions,
            terminator,
        }
    }

    fn branch_to(then_target: u32, else_target: u32) -> Terminator {
        Terminator::Branch {
            cond: Place::Local(9),
            then_target,
            else_target,
        }
    }

    fn builder() -> Builder {
        Builder {
            next_block_id: 100,
            ..Builder::default()
        }
    }

    fn checked(blocks: Vec<BasicBlock>) -> CheckedMirFunction {
        let (_, exits) = crate::lower::drop_plan::enumerate_exits(&blocks, &HashSet::new());
        let drop_plans =
            crate::lower::drop_plan::derive_drop_plans_from_replay(&blocks, &[], exits);
        CheckedMirFunction {
            name: "edge_release".to_owned(),
            return_ty: ResolvedTy::Unit,
            blocks,
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
            ownership_elaboration: Some(Box::new(crate::ElaboratedMirFunction {
                name: "edge_release".to_owned(),
                return_ty: ResolvedTy::Unit,
                statements: vec![],
                decisions: vec![],
                blocks: vec![],
                drop_plans,
                coroutine: None,
                lambda_captures: vec![],
            })),
        }
    }

    fn drops_of(block: &BasicBlock) -> Vec<Place> {
        block
            .instructions
            .iter()
            .filter_map(|instruction| match instruction {
                Instr::Drop { place, drop_fn, .. }
                    if *drop_fn == Some(DropFnSpec::Release("hew_string_drop")) =>
                {
                    Some(*place)
                }
                _ => None,
            })
            .collect()
    }

    /// `a` minted in block 0, consumed on the block-1 path, still owned on the
    /// block-2 path; both paths reach the returning block 3.
    fn conditionally_consumed(a: OwnerId, place: Place) -> Vec<BasicBlock> {
        vec![
            block(0, mint(a, place, 0).to_vec(), branch_to(1, 2)),
            block(1, vec![consume(a, place)], Terminator::Goto { target: 3 }),
            block(2, vec![], Terminator::Goto { target: 3 }),
            block(3, vec![], Terminator::Return),
        ]
    }

    #[test]
    fn owning_goto_edge_receives_the_release_inline() {
        let a = owner(1);
        let place = Place::Local(4);
        let mut blocks = conditionally_consumed(a, place);
        let mut builder = builder();
        materialize_conditional_consume_releases(&mut blocks, &mut builder);
        assert_eq!(blocks.len(), 4, "a Goto predecessor is not split");
        assert_eq!(drops_of(&blocks[2]), vec![place]);
        assert!(
            blocks[2].instructions.iter().any(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Release { owner, place: released })
                    if *owner == a && *released == place
            )),
            "{:?}",
            blocks[2].instructions
        );
        assert!(
            drops_of(&blocks[1]).is_empty(),
            "the consumed path owes nothing"
        );
        let findings = validate_ownership_events(&checked(blocks));
        assert!(findings.is_empty(), "{findings:?}");
    }

    #[test]
    fn owning_call_edge_is_split_so_the_release_follows_the_call() {
        let a = owner(1);
        let place = Place::Local(4);
        let mut blocks = conditionally_consumed(a, place);
        blocks[2].terminator = Terminator::Call {
            callee: "callee".to_owned(),
            authority: crate::model::CallAuthority::Direct,
            args: vec![place],
            dest: None,
            next: 3,
        };
        let mut builder = builder();
        materialize_conditional_consume_releases(&mut blocks, &mut builder);
        assert_eq!(
            blocks.len(),
            5,
            "the call edge is split through a fresh block"
        );
        assert!(
            drops_of(&blocks[2]).is_empty(),
            "the call still reads the place; nothing is dropped before it"
        );
        let Terminator::Call { next, .. } = blocks[2].terminator else {
            panic!("predecessor terminator must remain a call");
        };
        let split = &blocks[4];
        assert_eq!(split.id, next);
        assert_eq!(drops_of(split), vec![place]);
        assert_eq!(split.terminator, Terminator::Goto { target: 3 });
        let findings = validate_ownership_events(&checked(blocks));
        assert!(findings.is_empty(), "{findings:?}");
    }

    #[test]
    fn place_read_after_the_join_is_not_released() {
        // The owning path physically reads the place after the join: the
        // producer failed to publish the consuming Transfer, so an edge drop
        // would free the value it is about to return.
        let a = owner(1);
        let place = Place::Local(4);
        let mut blocks = conditionally_consumed(a, place);
        blocks[3].instructions = vec![Instr::Move {
            dest: Place::ReturnSlot,
            src: place,
        }];
        let mut builder = builder();
        materialize_conditional_consume_releases(&mut blocks, &mut builder);
        assert!(
            blocks.iter().all(|block| drops_of(block).is_empty()),
            "{blocks:?}"
        );
        let findings = validate_ownership_events(&checked(blocks));
        assert!(
            findings.iter().any(|finding| matches!(
                finding,
                crate::MirCheck::ObligationUnderReleased { reason, .. }
                    if reason.contains("consumed on some paths")
            )),
            "the leak stays reported instead of becoming a use-after-free: {findings:?}"
        );
    }

    #[test]
    fn place_redefined_before_its_next_read_is_released() {
        // A loop-shaped read: the place is read again only after a whole-local
        // redefinition, so the current value is dead at the join.
        let a = owner(1);
        let place = Place::Local(4);
        let mut blocks = conditionally_consumed(a, place);
        blocks[3].instructions = vec![
            Instr::Move {
                dest: place,
                src: Place::Local(7),
            },
            Instr::Move {
                dest: Place::Local(8),
                src: place,
            },
        ];
        let mut builder = builder();
        materialize_conditional_consume_releases(&mut blocks, &mut builder);
        assert_eq!(drops_of(&blocks[2]), vec![place]);
    }

    #[test]
    fn generation_joined_at_the_target_is_left_to_the_lineage_join() {
        let old = owner(1);
        let replacement = OwnerId {
            binding: BindingId(1),
            generation: 1,
        };
        let joined = OwnerId {
            binding: BindingId(1),
            generation: 2,
        };
        let place = Place::Local(4);
        let mut blocks = vec![
            block(0, mint(old, place, 0).to_vec(), branch_to(1, 2)),
            // Overwrite protocol: release the old generation, mint a fresh
            // value and transfer it onto the binding.
            block(
                1,
                vec![
                    Instr::OwnershipEvent(OwnershipEvent::Release { owner: old, place }),
                    Instr::OwnershipEvent(OwnershipEvent::Mint {
                        owner: owner(50),
                        place: Place::Local(5),
                        ty: ResolvedTy::String,
                    }),
                    Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                        owner: owner(50),
                        recipe: string_recipe(1),
                    }),
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        owner: owner(50),
                        from: Place::Local(5),
                        to: Some(place),
                        to_owner: Some(replacement),
                        to_ty: Some(ResolvedTy::String),
                    }),
                    Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                        owner: replacement,
                        recipe: string_recipe(0),
                    }),
                ],
                Terminator::Goto { target: 3 },
            ),
            block(2, vec![], Terminator::Goto { target: 3 }),
            block(
                3,
                vec![
                    Instr::OwnershipEvent(OwnershipEvent::Join {
                        incoming: vec![old, replacement],
                        replacement: joined,
                        place,
                        ty: ResolvedTy::String,
                    }),
                    Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                        owner: joined,
                        recipe: string_recipe(0),
                    }),
                ],
                Terminator::Return,
            ),
        ];
        let mut builder = builder();
        materialize_conditional_consume_releases(&mut blocks, &mut builder);
        assert!(
            blocks.iter().all(|block| drops_of(block).is_empty()),
            "a Join input is renamed, never dropped on its edge: {blocks:?}"
        );
    }

    #[test]
    fn guarded_generation_keeps_its_runtime_flag_authority() {
        let a = owner(1);
        let place = Place::Local(4);
        let mut blocks = conditionally_consumed(a, place);
        blocks[0]
            .instructions
            .push(Instr::OwnershipEvent(OwnershipEvent::Guard {
                owner: a,
                flag: Place::Local(6),
                kind: OwnershipGuardKind::Collection,
            }));
        let mut builder = builder();
        materialize_conditional_consume_releases(&mut blocks, &mut builder);
        assert!(
            blocks.iter().all(|block| drops_of(block).is_empty()),
            "{blocks:?}"
        );
    }
}
