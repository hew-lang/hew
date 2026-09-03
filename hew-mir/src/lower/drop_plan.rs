#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    base_local, check_duplex_split_state, close_obligated_borrow_alias_violations,
    collection_borrow_getter_alias_locals, interior_alias_receiver_violations, place_refs_local,
    retained_string_terminator_drop_safe, short_name, string_call_borrows,
    terminator_source_places, user_record_layout_key, BTreeMap, BasicBlock, BindingId, BlockKind,
    Builder, BuiltinType, CheckedMirFunction, ClosurePairRhs, DropKind, DropPlan, ElabBlock,
    ElabDrop, ElaboratedMirFunction, ExitPath, HashMap, HashSet, HirExpr, HirExprKind, Instr,
    IntentKind, LambdaCapture, MirCheck, MirDiagnostic, MirDiagnosticKind, MirStatement,
    ParamCrashCleanupKind, Place, RawMirFunction, ResolvedRef, ResolvedTy, SiteId, SuspendKind,
    Terminator, TraitObjectStorage, ValueClass, ENTRY_BLOCK_ID,
};
use crate::model::CooperateSite;
#[cfg(test)]
use hew_hir::ResourceMarker;

mod conditional_consume_release;
mod diagnostic_projection;
mod owner_state;

pub(super) use conditional_consume_release::materialize_conditional_consume_releases;
pub(super) use diagnostic_projection::project_findings;
pub(super) use owner_state::{
    apply_exact_owner_ops, apply_maybe_owner_ops, debug_assert_exact_entries_current,
    debug_assert_maybe_entries_current, exact_owner_states, maybe_ended_owner_states,
    maybe_owner_states, ExactOwnerState, MaybeOwnerState, MustBindingOwnerState,
};

/// Drop-elaboration pass over a `CheckedMirFunction`.
///
/// Produces an `ElaboratedMirFunction` whose `blocks` + `drop_plans`
/// describe, structurally, what drops fire on every exit edge of the
/// function. The pass is intraprocedural and uses the
/// `DecisionFact::value_class` data already on the checked MIR (no
/// cross-join coalescing — council R-C3.1 / plan §5 commit 2: drops
/// fire at each exit independently; full NLL precision is deferred to
/// v0.6).
///
/// Algorithm per HEW-SPEC §3.7.8.4 (lexical scope teardown):
///   1. Replay explicit Checked-MIR ownership events to obtain each typed
///      `OwnerId`, its physical `Place`, generation, transfers, and guard.
///      Builder ownership ledgers are lowering cursors only and are not
///      cleanup-admission authority after the MIR stream is sealed.
///   2. `enumerate_exits` names every exit edge of the CFG (`Return`,
///      `Trap`/`Panic`, `Cancel`, `Suspend`, call unwind edges, ...) and
///      the `BlockKind::Cleanup` projection for each `Trap`.
///   3. `derive_drop_plans_from_replay` fills each exit's `DropPlan` with
///      one `ElabDrop` per owner generation live at that exit whose inline
///      release does not dominate it, built from the owner's definition-site
///      `DropRecipe` and `Guard`, in reverse declaration order
///      (`OwnerDropRecipe::declaration_order`).
///   4. `validate_ownership_events` replays the same stream and rejects a
///      plan that drops a place with no live owner, omits a live owner's
///      cleanup, or disagrees with the recipe/guard/one-owner-per-place
///      invariants.
///
/// Drop classification:
///   - `ValueClass::AffineResource` -> `ElabDrop { drop_fn: Some("<TypeName>::close") }`
///     (synthesised name; once `@resource` types reach the spine subset,
///     this is replaced by the resolved `FnId` of the type's `close`
///     consuming method).
///   - `ValueClass::Linear` -> NO implicit drop emitted. The move-checker
///     is the proof-of-consume; an unconsumed `Linear` binding has
///     already been rejected as `MirCheck::MustConsume` upstream.
///   - All other classes -> no drop emitted (`BitCopy`, `CowValue`, `View`,
///     `PersistentShare`, `Unknown` — `Unknown` is itself an upstream
///     rejection).
#[allow(
    clippy::too_many_arguments,
    reason = "the derivation threads the sealed Checked-MIR inputs plus the \
              callable's presentation name and its resolver-anchored identity \
              key, which the elaborated function carries side by side"
)]
pub(super) fn derive_elaboration(
    name: &str,
    key: &crate::model::MirCallableKey,
    return_ty: &ResolvedTy,
    blocks: &[BasicBlock],
    cooperate_sites: &[CooperateSite],
    decisions: &[super::DecisionFact],
    builder: &Builder,
    flat_statements: &[MirStatement],
) -> (ElaboratedMirFunction, Vec<MirDiagnostic>) {
    let _timing = crate::timing::stage("derive_elaboration");
    let mut elaboration_diagnostics = Vec::new();
    // Statements stream: retained for snapshot/compat continuity with the
    // pre-Cluster-3 elaborator. Every owner binding gets a checker-stream
    // `Drop` entry in reverse-definition order; the structural drop plan in
    // `drop_plans` is the authoritative per-`ExitPath` answer. The flat
    // stream is the union of every block's `statements` in construction
    // order.
    let mut elaborated_statements: Vec<MirStatement> = flat_statements.to_vec();
    let binding_names: HashMap<BindingId, String> = flat_statements
        .iter()
        .filter_map(|statement| match statement {
            MirStatement::Bind { binding, name, .. } => Some((*binding, name.clone())),
            _ => None,
        })
        .collect();
    let owned_locals_snapshot = owner_definition_snapshot(blocks, &binding_names);
    for (binding, name, ty) in owned_locals_snapshot.iter().rev() {
        if builder.back_edge_only_iteration_owners.contains(binding) {
            continue;
        }
        elaborated_statements.push(MirStatement::Drop {
            binding: *binding,
            name: name.clone(),
            ty: ty.clone(),
        });
    }

    // Every receiver-interior alias is ownerless: the collection remains the
    // sole destructor authority for ordinary heap values, nested collections,
    // and close-obligated handles alike. The escape/use proof below is needed
    // only for the close-obligated subset.
    let borrow_getter_aliases = collection_borrow_getter_alias_locals(blocks);
    let close_obligated_borrow_aliases =
        close_obligated_borrow_alias_locals(&borrow_getter_aliases, &builder.locals);
    // Fail-closed floor for the suppression: every use of a close-obligated
    // borrow must be a proven-safe read. An escape (return/store), a consume
    // (`e.close()`, `w.push(e)`, any call argument), or a reassignment refuses
    // the function -- suppression without this proof converted a silent
    // double-close into a use-after-close (the alias outliving the
    // collection's release) and stays structurally unreachable only by
    // rejecting the unprovable shapes.
    for violation in close_obligated_borrow_alias_violations(
        blocks,
        &close_obligated_borrow_aliases,
        &tracked_obligation_locals(builder, blocks),
    ) {
        elaboration_diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::DropPlanUndetermined {
                block: ENTRY_BLOCK_ID,
                reason: violation,
            },
            note: "a borrowed collection element has exactly one release authority (the \
 collection); read fields through it, or restructure so the element is \
 owned outside the collection"
                .to_string(),
        });
    }
    for violation in interior_alias_receiver_violations(blocks) {
        elaboration_diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::DropPlanUndetermined {
                block: ENTRY_BLOCK_ID,
                reason: violation,
            },
            note: "an interior collection alias may be used only while its exact receiver generation remains live and unmutated"
                .to_string(),
        });
    }

    let cancellation_blocks = cooperate_sites
        .iter()
        .map(|site| site.bb_id)
        .collect::<HashSet<_>>();
    let (elab_blocks, exits) = enumerate_exits(blocks, &cancellation_blocks);
    let drop_plans = derive_drop_plans_from_replay(blocks, decisions, exits);

    (
        ElaboratedMirFunction {
            name: name.to_owned(),
            key: key.clone(),
            return_ty: return_ty.clone(),
            statements: elaborated_statements,
            decisions: builder.decisions.clone(),
            blocks: elab_blocks,
            drop_plans,
            coroutine: None,
            // Lambda-actor capture set, populated by the MIR producer at
            // each `HirExprKind::SpawnLambdaActor` site (see
            // `Builder::lower_spawn_lambda_actor`). The structural
            // fail-closed checker `validate_lambda_captures` enforces the
            // invariants (Weak attaches to LambdaActorHandle; at most one
            // Weak per actor handle) on the emitted ledger.
            lambda_captures: builder.lambda_captures.clone(),
        },
        elaboration_diagnostics,
    )
}

/// Every owner binding named by the Checked-MIR definition events, in first
/// definition order, with its type. No Builder ledger names an owner here.
///
/// A same-place transfer replaces a provisional publication identity with
/// its named owner without changing physical storage. Both `OwnerId`s remain
/// in the immutable history, but there is only one destructor slot, so the
/// statements stream names the adopted identity only.
fn owner_definition_snapshot(
    blocks: &[BasicBlock],
    binding_names: &HashMap<BindingId, String>,
) -> Vec<(BindingId, String, ResolvedTy)> {
    use crate::model::OwnershipEvent;

    let mut snapshot: Vec<(BindingId, String, ResolvedTy)> = Vec::new();
    let mut binding_places: HashMap<BindingId, Place> = HashMap::new();
    let mut adopted_owner_by_place: HashMap<Place, BindingId> = HashMap::new();
    for event in blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(event) => Some(event),
            _ => None,
        })
    {
        let (owner, place, ty) = match event {
            OwnershipEvent::Mint { owner, place, ty }
            | OwnershipEvent::Transfer {
                to: Some(place),
                to_owner: Some(owner),
                to_ty: Some(ty),
                ..
            } => (*owner, *place, ty),
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
            }
            | OwnershipEvent::Join {
                replacement,
                place,
                ty,
                ..
            } => (*replacement, *place, ty),
            OwnershipEvent::DropRecipe { .. }
            | OwnershipEvent::Transfer { .. }
            | OwnershipEvent::Relocate { .. }
            | OwnershipEvent::Release { .. }
            | OwnershipEvent::GuardedRelease { .. }
            | OwnershipEvent::DemoteToAlias { .. }
            | OwnershipEvent::Guard { .. }
            | OwnershipEvent::InteriorAlias { .. }
            | OwnershipEvent::AliasRelocate { .. }
            | OwnershipEvent::AliasEnd { .. }
            | OwnershipEvent::EdgeCarry { .. }
            | OwnershipEvent::ScopeExit { .. } => continue,
        };
        if let OwnershipEvent::Transfer {
            from, to: Some(to), ..
        } = event
        {
            if from == to {
                adopted_owner_by_place.insert(*to, owner.binding);
            }
        }
        let first_definition = binding_places.insert(owner.binding, place).is_none();
        if first_definition {
            let name = binding_names
                .get(&owner.binding)
                .cloned()
                .unwrap_or_else(|| format!("__hew_owner_{}", owner.binding.0));
            snapshot.push((owner.binding, name, ty.clone()));
        }
    }
    snapshot.retain(|(binding, _, _)| {
        binding_places.get(binding).is_some_and(|place| {
            adopted_owner_by_place
                .get(place)
                .is_none_or(|adopted| binding == adopted)
        })
    });
    snapshot
}

pub(super) fn owner_definition_drop_recipe(
    builder: &Builder,
    owner: crate::model::OwnerId,
    place: Place,
    ty: &ResolvedTy,
    declaration_order: u32,
) -> Option<crate::model::OwnerDropRecipe> {
    let (kind, drop_fn) = if let ResolvedTy::Named {
        args,
        builtin: Some(BuiltinType::Vec),
        ..
    } = ty
    {
        let element = args
            .first()
            .unwrap_or_else(|| panic!("owner {owner} defines Vec without an element type"));
        let release = match builder.classify_vec_element_release(element) {
            crate::ownership::VecElementRelease::Plain => {
                crate::ownership::CowHeapRelease::VecPlain
            }
            crate::ownership::VecElementRelease::OwnedElement => {
                crate::ownership::CowHeapRelease::VecOwnedElement
            }
            crate::ownership::VecElementRelease::ClosurePair => {
                crate::ownership::CowHeapRelease::VecClosurePairs
            }
            crate::ownership::VecElementRelease::Unsupported(
                crate::ownership::FailClosedReason::NoReleaseProtocol,
            ) if builder.elem_is_owned_abi_releasable(element) => {
                crate::ownership::CowHeapRelease::VecOwnedElement
            }
            // No wired per-element release, or an element whose ownership the
            // registries cannot see: publish no recipe rather than a
            // buffer-only free that would leak every element node.
            crate::ownership::VecElementRelease::Unsupported(_) => return None,
        };
        (DropKind::CowHeap { release }, None)
    } else if let Some(release) = builder.vec_iter_cursor_release_protocol(ty) {
        (DropKind::VecIterCursor { release }, None)
    } else if ty_is_closure_pair(ty) {
        (DropKind::ClosurePair, None)
    } else if matches!(ty, ResolvedTy::TraitObject { .. }) {
        let storage = builder
            .dyn_trait_storage
            .get(&owner.binding)
            .copied()
            .unwrap_or_else(|| {
                panic!("owner {owner} defines dyn Trait without a storage discriminator")
            });
        (DropKind::TraitObject { storage }, None)
    } else if ty_is_indirect_enum(ty, &builder.enum_layouts) {
        (DropKind::IndirectEnum, None)
    } else if ty_is_heap_owning_enum_composite(
        ty,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
    ) {
        (DropKind::EnumInPlace, None)
    } else if ty_is_heap_owning_tuple(
        ty,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
    ) {
        (DropKind::TupleInPlace, None)
    } else if builder.is_owned_aggregate_record_ty(ty) {
        (DropKind::RecordInPlace, None)
    } else {
        let kind = drop_kind_for(
            place,
            ty,
            matches!(ty, ResolvedTy::TraitObject { .. })
                .then(|| builder.dyn_trait_storage.get(&owner.binding).copied())
                .flatten(),
        );
        let drop_fn = place_aware_drop_fn(place, resource_drop_fn(ty, &builder.type_classes));
        (kind, drop_fn)
    };
    Some(crate::model::OwnerDropRecipe {
        ty: ty.clone(),
        drop_fn,
        kind,
        declaration_order,
    })
}

/// Carry a physical cleanup guard across an explicit owner-generation
/// successor. A returns-receiver call is the canonical shape: the handle is
/// the same dynamic owner at a new place/generation, so its affine flag and
/// semantic guard kind are part of the transferred authority tuple.
///
/// Collection and overwrite guards have different transfer semantics: each
/// belongs to a conditionally-moved SOURCE storage slot and is set to one on
/// the exact edge that hands the value to a new lexical binding. That consumed
/// bit cannot guard the destination owner -- doing so would skip the
/// destination's only release. Preserve these storage-bound guards only
/// through same-binding generation changes; a binding-changing transfer leaves
/// the destination unguarded and path-exact.
///
/// This operates only on explicit MIR definitions. Conflicting/missing input
/// guards are left untouched so sealing fails closed; no Builder flag map is
/// consulted.
pub(super) fn materialize_successor_guard_authority(blocks: &mut [BasicBlock]) {
    use crate::model::OwnershipEvent;
    let _timing = crate::timing::stage("materialize_successor_guard_authority");

    loop {
        let mut guards = HashMap::new();
        let mut conflicts = HashSet::new();
        for instruction in blocks.iter().flat_map(|block| &block.instructions) {
            let Instr::OwnershipEvent(OwnershipEvent::Guard { owner, flag, kind }) = instruction
            else {
                continue;
            };
            if guards
                .insert(*owner, (*flag, *kind))
                .is_some_and(|prior| prior != (*flag, *kind))
            {
                conflicts.insert(*owner);
            }
        }
        let mut changed = false;
        for block in blocks.iter_mut() {
            let mut insertions = Vec::new();
            for (index, instruction) in block.instructions.iter().enumerate() {
                let successor_guard = match instruction {
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        owner,
                        to_owner: Some(successor),
                        ..
                    }) if !conflicts.contains(owner) => {
                        guards.get(owner).copied().and_then(|guard @ (_, kind)| {
                            let storage_bound = matches!(
                                kind,
                                crate::model::OwnershipGuardKind::Collection
                                    | crate::model::OwnershipGuardKind::Overwrite
                            );
                            (!storage_bound || successor.binding == owner.binding)
                                .then_some((*successor, guard))
                        })
                    }
                    Instr::OwnershipEvent(
                        OwnershipEvent::Reset {
                            previous,
                            replacement,
                            ..
                        }
                        | OwnershipEvent::Rearm {
                            previous,
                            replacement,
                            ..
                        },
                    ) if !conflicts.contains(previous) => guards
                        .get(previous)
                        .copied()
                        .map(|guard| (*replacement, guard)),
                    Instr::OwnershipEvent(OwnershipEvent::Join {
                        incoming,
                        replacement,
                        ..
                    }) => {
                        let inherited = incoming
                            .iter()
                            .filter_map(|owner| guards.get(owner).copied())
                            .collect::<HashSet<_>>();
                        (inherited.len() == 1
                            && incoming.iter().all(|owner| guards.contains_key(owner)))
                        .then(|| (*replacement, *inherited.iter().next().unwrap()))
                    }
                    _ => None,
                };
                let Some((successor, (flag, kind))) = successor_guard else {
                    continue;
                };
                if guards.contains_key(&successor) || conflicts.contains(&successor) {
                    continue;
                }
                insertions.push((index + 1, successor, flag, kind));
            }
            for (index, owner, flag, kind) in insertions.into_iter().rev() {
                block.instructions.insert(
                    index,
                    Instr::OwnershipEvent(OwnershipEvent::Guard { owner, flag, kind }),
                );
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

#[cfg(test)]
fn successor_guard_test_blocks(
    source: crate::model::OwnerId,
    successor: crate::model::OwnerId,
    guards: &[(Place, crate::model::OwnershipGuardKind)],
) -> Vec<BasicBlock> {
    use crate::model::OwnershipEvent;

    let mut instructions = vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
        owner: source,
        place: Place::Local(1),
        ty: ResolvedTy::String,
    })];
    instructions.extend(guards.iter().map(|(flag, kind)| {
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner: source,
            flag: *flag,
            kind: *kind,
        })
    }));
    instructions.push(Instr::OwnershipEvent(OwnershipEvent::Transfer {
        owner: source,
        from: Place::Local(1),
        to: Some(Place::Local(2)),
        to_owner: Some(successor),
        to_ty: Some(ResolvedTy::String),
    }));
    vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions,
        terminator: Terminator::Return,
    }]
}

#[cfg(test)]
fn successor_published_guards(
    blocks: &[BasicBlock],
    successor: crate::model::OwnerId,
) -> Vec<(Place, crate::model::OwnershipGuardKind)> {
    blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard { owner, flag, kind })
                if *owner == successor =>
            {
                Some((*flag, *kind))
            }
            _ => None,
        })
        .collect()
}

#[test]
fn successor_collection_guard_stays_with_same_binding_lineage() {
    use crate::model::{OwnerId, OwnershipGuardKind};

    let source = OwnerId {
        binding: BindingId(200),
        generation: 0,
    };
    let successor = OwnerId {
        binding: source.binding,
        generation: 1,
    };
    let expected = (Place::Local(3), OwnershipGuardKind::Collection);
    let mut blocks = successor_guard_test_blocks(source, successor, &[expected]);

    materialize_successor_guard_authority(&mut blocks);

    assert_eq!(
        successor_published_guards(&blocks, successor),
        vec![expected]
    );
}

#[test]
fn successor_overwrite_guard_stays_with_same_binding_lineage() {
    use crate::model::{OwnerId, OwnershipGuardKind};

    let source = OwnerId {
        binding: BindingId(205),
        generation: 0,
    };
    let successor = OwnerId {
        binding: source.binding,
        generation: 1,
    };
    let expected = (Place::Local(3), OwnershipGuardKind::Overwrite);
    let mut blocks = successor_guard_test_blocks(source, successor, &[expected]);

    materialize_successor_guard_authority(&mut blocks);

    assert_eq!(
        successor_published_guards(&blocks, successor),
        vec![expected]
    );
}

#[test]
fn successor_non_collection_guard_stays_with_dynamic_owner_transfer() {
    use crate::model::{OwnerId, OwnershipGuardKind};

    let source = OwnerId {
        binding: BindingId(203),
        generation: 0,
    };
    let successor = OwnerId {
        binding: BindingId(204),
        generation: 0,
    };
    let expected = (Place::Local(3), OwnershipGuardKind::AffineRelease);
    let mut blocks = successor_guard_test_blocks(source, successor, &[expected]);

    materialize_successor_guard_authority(&mut blocks);

    assert_eq!(
        successor_published_guards(&blocks, successor),
        vec![expected]
    );
}

#[test]
fn successor_collection_guard_rejects_binding_changing_or_ambiguous_authority() {
    use crate::model::{OwnerId, OwnershipGuardKind};

    let source = OwnerId {
        binding: BindingId(201),
        generation: 0,
    };
    let rebound = OwnerId {
        binding: BindingId(202),
        generation: 0,
    };
    let mut rebound_blocks = successor_guard_test_blocks(
        source,
        rebound,
        &[(Place::Local(3), OwnershipGuardKind::Collection)],
    );
    materialize_successor_guard_authority(&mut rebound_blocks);
    assert!(
        successor_published_guards(&rebound_blocks, rebound).is_empty(),
        "a consumed source collection bit cannot suppress its new binding owner's release"
    );

    let same_binding = OwnerId {
        binding: source.binding,
        generation: 1,
    };
    let mut ambiguous_blocks = successor_guard_test_blocks(
        source,
        same_binding,
        &[
            (Place::Local(3), OwnershipGuardKind::Collection),
            (Place::Local(4), OwnershipGuardKind::Collection),
        ],
    );
    materialize_successor_guard_authority(&mut ambiguous_blocks);
    assert!(
        successor_published_guards(&ambiguous_blocks, same_binding).is_empty(),
        "conflicting physical guard authority must remain validator-visible"
    );
}

#[test]
fn successor_overwrite_guard_rejects_binding_change_and_ambiguity() {
    use crate::model::{OwnerId, OwnershipGuardKind};

    let source = OwnerId {
        binding: BindingId(206),
        generation: 0,
    };
    let rebound = OwnerId {
        binding: BindingId(207),
        generation: 0,
    };
    let mut rebound_blocks = successor_guard_test_blocks(
        source,
        rebound,
        &[(Place::Local(3), OwnershipGuardKind::Overwrite)],
    );
    materialize_successor_guard_authority(&mut rebound_blocks);
    assert!(
        successor_published_guards(&rebound_blocks, rebound).is_empty(),
        "a consumed overwrite bit cannot suppress a new binding owner's release"
    );

    let same_binding = OwnerId {
        binding: source.binding,
        generation: 1,
    };
    let mut ambiguous_blocks = successor_guard_test_blocks(
        source,
        same_binding,
        &[
            (Place::Local(3), OwnershipGuardKind::Overwrite),
            (Place::Local(4), OwnershipGuardKind::Overwrite),
        ],
    );
    materialize_successor_guard_authority(&mut ambiguous_blocks);
    assert!(
        successor_published_guards(&ambiguous_blocks, same_binding).is_empty(),
        "conflicting overwrite flags must remain validator-visible"
    );
}

/// Publish the destructor ritual immediately beside every owner definition.
/// The recipe is derived only from that definition's typed `(OwnerId, Place,
/// Ty)` fact and the canonical type/layout registry. No LIFO template, exit
/// plan, Builder ownership ledger, or move ancestry participates.
pub(super) fn materialize_definition_site_drop_recipes(
    blocks: &mut [BasicBlock],
    builder: &Builder,
) {
    use crate::model::OwnershipEvent;
    let _timing = crate::timing::stage("materialize_definition_site_drop_recipes");

    let mut binding_order = HashMap::<BindingId, u32>::new();
    let mut next_order = 0_u32;
    for block in blocks {
        let instructions = std::mem::take(&mut block.instructions);
        let mut rewritten = Vec::with_capacity(instructions.len());
        for instruction in instructions {
            let definition = match &instruction {
                Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, ty }) => {
                    Some((*owner, *place, ty.clone()))
                }
                Instr::OwnershipEvent(
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
                    }
                    | OwnershipEvent::Join {
                        replacement,
                        place,
                        ty,
                        ..
                    },
                ) => Some((*replacement, *place, ty.clone())),
                Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    to: Some(place),
                    to_owner: Some(owner),
                    to_ty: Some(ty),
                    ..
                }) => Some((*owner, *place, ty.clone())),
                Instr::OwnershipEvent(OwnershipEvent::DropRecipe { .. }) => {
                    panic!("drop recipe was published before its owner-definition site")
                }
                _ => None,
            };
            rewritten.push(instruction);
            if let Some((owner, place, ty)) = definition {
                let order = *binding_order.entry(owner.binding).or_insert_with(|| {
                    let order = next_order;
                    next_order = next_order.saturating_add(1);
                    order
                });
                if let Some(recipe) =
                    owner_definition_drop_recipe(builder, owner, place, &ty, order)
                {
                    rewritten.push(Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                        owner,
                        recipe,
                    }));
                }
            }
        }
        block.instructions = rewritten;
    }
}

/// Derive every exit's cleanup plan from Checked-MIR ownership replay.
///
/// `required(exit)` is the exact set of owner generations live at that exit
/// whose inline release does not dominate it — the same
/// `guarded_required_owners_for_exit` rule `validate_ownership_events`
/// replays — and each becomes one [`ElabDrop`] through the owner's
/// definition-site [`OwnershipEvent::DropRecipe`] and `Guard`. No Builder
/// ledger, allow-set, or LIFO template participates: a value the compiler
/// cannot safely drop must not mint an owner, and a value that mints is
/// dropped on every exit where it is still owned.
///
/// Function-entry cancellation is the sole exception: parameter storage is
/// live before MIR's leading Mint and is admitted by typed boundary facts.
pub(super) fn derive_drop_plans_from_replay(
    blocks: &[BasicBlock],
    decisions: &[super::DecisionFact],
    exits: Vec<ExitPath>,
) -> Vec<(ExitPath, DropPlan)> {
    use crate::model::OwnershipEvent;

    let mut recipes = HashMap::new();
    for (owner, recipe) in blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe { owner, recipe }) => {
                Some((*owner, recipe.clone()))
            }
            _ => None,
        })
    {
        assert!(
            recipes.insert(owner, recipe).is_none(),
            "owner {owner} publishes more than one destructor recipe"
        );
    }
    let guards = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Guard { owner, flag, .. }) => {
                Some((*owner, *flag))
            }
            _ => None,
        })
        .collect::<HashMap<_, _>>();
    let guarded_owners = guards.keys().copied().collect::<HashSet<_>>();
    let exact_states = exact_owner_states(blocks);
    let entries = &exact_states.0;
    let exit_states = &exact_states.1;
    let maybe_states = maybe_owner_states(blocks);
    let maybe_entries = &maybe_states.0;
    let maybe_exits = &maybe_states.1;
    let entry_parameter_owners = entry_cancel_parameter_owners(blocks, decisions);
    exits
        .into_iter()
        .map(|exit| {
            let is_entry_cancel =
                matches!(exit, ExitPath::Cancel { block } if block == ENTRY_BLOCK_ID);
            let required = if is_entry_cancel {
                entry_parameter_owners.clone()
            } else {
                guarded_required_owners_for_exit(
                    &exit,
                    blocks,
                    entries,
                    exit_states,
                    maybe_entries,
                    maybe_exits,
                    &guarded_owners,
                )
            };
            let mut synthesized = required
                .iter()
                .filter_map(|(owner, place)| {
                    let recipe = recipes.get(owner)?;
                    Some((
                        recipe.declaration_order,
                        *owner,
                        ElabDrop {
                            place: *place,
                            ty: recipe.ty.clone(),
                            drop_fn: recipe.drop_fn.clone(),
                            kind: recipe.kind,
                            // Codegen injects FunctionEntry cancellation after
                            // storing ABI parameters but before executing MIR's
                            // parameter guard initialisers. The incoming owner
                            // is live there, but its sidecar is not readable yet.
                            guard: if is_entry_cancel {
                                None
                            } else {
                                guards
                                    .get(owner)
                                    .copied()
                                    .map(|flag| crate::model::ElabDropGuard {
                                        owner: *owner,
                                        flag,
                                    })
                            },
                        },
                    ))
                })
                .collect::<Vec<_>>();
            synthesized
                .sort_by(|left, right| right.0.cmp(&left.0).then_with(|| right.1.cmp(&left.1)));
            let drops = synthesized
                .into_iter()
                .map(|(_, _, drop)| drop)
                .collect::<Vec<_>>();
            (exit, DropPlan { drops })
        })
        .collect()
}

/// Exact owner generations already present when codegen's synthesized
/// function-entry cancellation branch runs.
///
/// Parameter owner mints are represented as leading MIR operations so normal
/// Checked-MIR replay can validate their later transfers. Physically, however,
/// codegen stores every ABI argument before injecting `hew_actor_cooperate`.
/// Therefore an owning parameter is live on that exceptional edge even though
/// the ordinary entry-state lattice has not replayed its Mint instruction yet.
/// The typed parameter-boundary fact is the authority: a body-local Mint and a
/// borrowed/representation-loan parameter can never enter this set merely by
/// sharing a low-numbered local.
///
/// The event stream cannot replace the fact. Measured over the repro corpus,
/// every vertical-slice fixture, and the goobers modules (~28,700 functions):
/// "every unguarded `Mint` over a parameter local in the entry block" misses
/// the ~400 owned parameters whose `Mint` is guarded (they are dropped
/// UNGUARDED here, before their sidecar exists), and "every `Mint` over a
/// parameter local" additionally admits the guarded cursor `Mint` that
/// `VecIter::next`'s `var self` publishes over a `BorrowReadOnly` parameter —
/// which the caller still owns. Only the boundary mode separates those two.
fn entry_cancel_parameter_owners(
    blocks: &[BasicBlock],
    decisions: &[super::DecisionFact],
) -> ExactOwnerState {
    let owned_parameters = decisions
        .iter()
        .filter_map(|decision| {
            let super::Strategy::ParamBoundary(fact) = decision.strategy else {
                return None;
            };
            matches!(
                fact.mode,
                super::ParamBoundaryMode::TransferResource
                    | super::ParamBoundaryMode::OwnedMessage
                    | super::ParamBoundaryMode::OwnedCarrier
                    | super::ParamBoundaryMode::OwnedCursor
            )
            .then_some(fact.param_index)
        })
        .collect::<HashSet<_>>();
    let Some(entry) = blocks.iter().find(|block| block.id == ENTRY_BLOCK_ID) else {
        return ExactOwnerState::new();
    };
    entry
        .instructions
        .iter()
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
                owner,
                place: place @ Place::Local(local),
                ..
            }) if owned_parameters.contains(local) => Some((*owner, *place)),
            _ => None,
        })
        .collect()
}

#[cfg(test)]
#[test]
fn entry_cancel_owner_requires_typed_owned_parameter_boundary() {
    let owner = crate::model::OwnerId {
        binding: BindingId(91),
        generation: 0,
    };
    let blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(0),
            ty: ResolvedTy::Bytes,
        })],
        terminator: Terminator::Return,
    }];
    let boundary = |mode| super::DecisionFact {
        site: SiteId(0),
        value_class: ValueClass::CowValue,
        ty: ResolvedTy::Bytes,
        intent: IntentKind::Unknown,
        strategy: super::Strategy::ParamBoundary(super::ParamBoundaryFact {
            param_index: 0,
            param_count: 1,
            caller_visible_projection: false,
            mode,
        }),
        why: "entry-cancel owner unit fixture".to_owned(),
    };

    assert_eq!(
        entry_cancel_parameter_owners(&blocks, &[boundary(super::ParamBoundaryMode::OwnedMessage)]),
        HashMap::from([(owner, Place::Local(0))]),
        "an owned message is physically live after ABI ingress and before MIR replay"
    );
    assert_eq!(
        entry_cancel_parameter_owners(&blocks, &[boundary(super::ParamBoundaryMode::OwnedCursor)]),
        HashMap::from([(owner, Place::Local(0))]),
        "an OwnedCursor parameter is physically live after ABI ingress and before its guard exists"
    );
    assert!(
        entry_cancel_parameter_owners(
            &blocks,
            &[boundary(super::ParamBoundaryMode::BorrowReadOnly)]
        )
        .is_empty(),
        "a low-numbered Mint without typed owned-boundary authority must stay excluded"
    );
}

/// Keep the legacy cleanup-block projection byte-for-byte synchronized with
/// the exact OwnerId-derived panic plans. `drop_plans` is the sole authority;
/// `ElabBlock::drops` remains only for dump/API compatibility until that field
/// is removed from the public MIR model.
fn synchronize_cleanup_blocks(elaboration: &mut ElaboratedMirFunction) {
    let panic_drops = elaboration
        .drop_plans
        .iter()
        .filter_map(|(exit, plan)| matches!(exit, ExitPath::Panic { .. }).then_some(&plan.drops))
        .collect::<Vec<_>>();
    let cleanup_blocks = elaboration
        .blocks
        .iter_mut()
        .filter(|block| block.kind == BlockKind::Cleanup)
        .collect::<Vec<_>>();
    assert_eq!(
        cleanup_blocks.len(),
        panic_drops.len(),
        "cleanup-block compatibility projection diverged from exact panic plans"
    );
    for (block, drops) in cleanup_blocks.into_iter().zip(panic_drops) {
        block.drops.clone_from(drops);
    }
}

pub(super) fn inline_drop_spec_for_recipe(
    recipe: &crate::model::OwnerDropRecipe,
) -> Option<crate::model::DropFnSpec> {
    if let Some(spec) = &recipe.drop_fn {
        return Some(spec.clone());
    }
    match recipe.kind {
        crate::model::DropKind::CowHeap { release } => {
            Some(crate::model::DropFnSpec::Release(release.release_symbol()))
        }
        crate::model::DropKind::RecordInPlace => Some(crate::model::DropFnSpec::InPlace(
            crate::ownership::InPlaceReleaseKind::Record,
        )),
        crate::model::DropKind::AggregateRecursive | crate::model::DropKind::TupleInPlace => {
            Some(crate::model::DropFnSpec::InPlace(
                crate::ownership::InPlaceReleaseKind::AggregateRecursive,
            ))
        }
        crate::model::DropKind::EnumInPlace => Some(crate::model::DropFnSpec::InPlace(
            crate::ownership::InPlaceReleaseKind::Enum,
        )),
        crate::model::DropKind::Resource
        | crate::model::DropKind::RcRelease
        | crate::model::DropKind::WeakRelease
        | crate::model::DropKind::DuplexClose
        | crate::model::DropKind::DuplexHalfClose(_)
        | crate::model::DropKind::LambdaActorRelease
        | crate::model::DropKind::TraitObject { .. }
        | crate::model::DropKind::VecIterCursor { .. }
        | crate::model::DropKind::ClosurePair
        | crate::model::DropKind::IndirectEnum => None,
    }
}

/// Materialise replacement drops from immutable owner state and recipes.
///
/// Assignment lowering can no longer use its mutable `owned_locals`
/// disposition as cleanup authority: a binding consumed on one branch may be
/// live on this exact successor, and a loop Join may have renamed its current
/// generation after source lowering.  At sealing time the complete answer is
/// available in Checked-MIR form.  A typed provisional-to-binding Transfer
/// whose destination already carries one exact generation of that binding is
/// an overwrite. Drop/release that old generation immediately before the
/// physical Move, preserving it on the call's unwind edge and installing the
/// new generation only on normal success.
#[allow(
    clippy::too_many_lines,
    reason = "overwrite retirement replays one exact owner-state transition and its physical cleanup atomically"
)]
pub(super) fn materialize_exact_overwrite_releases(
    blocks: &mut [BasicBlock],
    builder: Option<&Builder>,
) {
    use crate::model::OwnershipEvent;
    let _timing = crate::timing::stage("materialize_exact_overwrite_releases");

    let mut recipes = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe { owner, recipe }) => {
                Some((*owner, recipe.clone()))
            }
            _ => None,
        })
        .collect::<HashMap<_, _>>();
    // The pass also runs before definition-site recipes are published, so the
    // replaced generation is ended in the event stream as soon as the
    // ownership phis are sealed and no later pass replays two live generations
    // of one binding at the assignment slot. Derive the physical cleanup from
    // the old owner's definition exactly as `materialize_definition_site_drop_recipes`
    // will; the `declaration_order` is irrelevant to an inline release.
    if let Some(builder) = builder {
        for instruction in blocks.iter().flat_map(|block| &block.instructions) {
            let definition = match instruction {
                Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, ty }) => {
                    Some((*owner, *place, ty))
                }
                Instr::OwnershipEvent(
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
                    }
                    | OwnershipEvent::Join {
                        replacement,
                        place,
                        ty,
                        ..
                    },
                ) => Some((*replacement, *place, ty)),
                Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    to: Some(place),
                    to_owner: Some(owner),
                    to_ty: Some(ty),
                    ..
                }) => Some((*owner, *place, ty)),
                _ => None,
            };
            if let Some((owner, place, ty)) = definition {
                if let std::collections::hash_map::Entry::Vacant(entry) = recipes.entry(owner) {
                    if let Some(recipe) = owner_definition_drop_recipe(builder, owner, place, ty, 0)
                    {
                        entry.insert(recipe);
                    }
                }
            }
        }
    }
    let guarded = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Guard { owner, .. }) => Some(*owner),
            _ => None,
        })
        .collect::<HashSet<_>>();
    loop {
        let exact_states = exact_owner_states(blocks);
        let entries = &exact_states.0;
        let mut inserted_any = false;
        for block in blocks.iter_mut() {
            let mut live = entries.get(&block.id).cloned().unwrap_or_default();
            let mut insertions = Vec::new();
            for (index, instruction) in block.instructions.iter().enumerate() {
                if let Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    owner: source_owner,
                    from: source,
                    to: Some(destination),
                    to_owner: Some(successor),
                    ..
                }) = instruction
                {
                    let old = live
                        .iter()
                        .filter_map(|(candidate, place)| {
                            (candidate.binding == successor.binding
                                && candidate != successor
                                && candidate != source_owner
                                && *place == *destination)
                                .then_some(*candidate)
                        })
                        .collect::<Vec<_>>();
                    let move_index = index.checked_sub(2).filter(|move_index| {
                        matches!(
                            (
                                block.instructions.get(*move_index),
                                block.instructions.get(*move_index + 1),
                            ),
                            (
                                Some(Instr::Move { dest, src }),
                                Some(Instr::NeutralizePayloadSlot {
                                    place,
                                    transferee: Some(transferee),
                                    ..
                                })
                            ) if dest == destination
                                && src == source
                                && place == source
                                && transferee == destination
                        )
                    });
                    // An assignment that emitted its own overwrite release
                    // at lowering time named the Builder cursor's generation,
                    // which is stale on a sibling branch or inside a loop and
                    // is re-keyed only after the phis are sealed; the replay
                    // here still shows the old generation live. Leave that
                    // slot to the re-keying rather than release it twice.
                    let lowering_released_here =
                        block.instructions[..index].iter().any(|instruction| {
                            matches!(
                                instruction,
                                Instr::OwnershipEvent(OwnershipEvent::Release { owner, place })
                                    if owner.binding == successor.binding && place == destination
                            )
                        });
                    if let ([old_owner], Some(move_index), false) =
                        (old.as_slice(), move_index, lowering_released_here)
                    {
                        if !guarded.contains(old_owner) {
                            if let Some(recipe) = recipes.get(old_owner) {
                                if let Some(drop_fn) = inline_drop_spec_for_recipe(recipe) {
                                    let drop = if matches!(
                                        recipe.kind,
                                        crate::model::DropKind::RecordInPlace
                                            | crate::model::DropKind::EnumInPlace
                                    ) {
                                        Instr::AggregateOverwriteRelease {
                                            old: *destination,
                                            replacement: *source,
                                            ty: recipe.ty.clone(),
                                        }
                                    } else {
                                        Instr::Drop {
                                            place: *destination,
                                            ty: recipe.ty.clone(),
                                            drop_fn: Some(drop_fn),
                                        }
                                    };
                                    insertions.push((
                                        move_index,
                                        drop,
                                        Instr::OwnershipEvent(OwnershipEvent::Release {
                                            owner: *old_owner,
                                            place: *destination,
                                        }),
                                    ));
                                    live.remove(old_owner);
                                }
                            }
                        }
                    }
                }
                apply_exact_owner_ops(std::slice::from_ref(instruction), &mut live);
            }
            if !insertions.is_empty() {
                inserted_any = true;
            }
            for (index, drop, release) in insertions.into_iter().rev() {
                block.instructions.insert(index, release);
                block.instructions.insert(index, drop);
            }
        }
        if !inserted_any {
            break;
        }
    }
}

/// Seal the ownership portion of Checked MIR in the same transaction that
/// creates the immutable function. Owner identity, generation, storage,
/// transfer, release, and conditional guard authority come exclusively from
/// the MIR operation stream. The remaining Builder reads are type/layout and
/// physical-cleanup metadata used to select a destructor, never an ownership
/// ledger or a retrospective move ancestry. Every later consumer replays and
/// independently validates the frozen plan through [`elaborate`].
#[allow(
    clippy::too_many_arguments,
    reason = "sealing must atomically freeze the complete Checked-MIR ownership authority"
)]
pub(super) fn seal_checked(
    name: String,
    key: crate::model::MirCallableKey,
    return_ty: ResolvedTy,
    blocks: Vec<BasicBlock>,
    _raw: &RawMirFunction,
    decisions: Vec<super::DecisionFact>,
    checks: Vec<MirCheck>,
    cooperate_sites: Vec<CooperateSite>,
    builder: &Builder,
    flat_statements: &[MirStatement],
) -> (CheckedMirFunction, Vec<MirDiagnostic>) {
    let _timing = crate::timing::stage("seal_checked");
    let (mut ownership_elaboration, mut diagnostics) = derive_elaboration(
        &name,
        &key,
        &return_ty,
        &blocks,
        &cooperate_sites,
        &decisions,
        builder,
        flat_statements,
    );
    synchronize_cleanup_blocks(&mut ownership_elaboration);
    let field_drop_admissible = |ty: &ResolvedTy| builder.field_drop_in_place_admissible(ty);
    for check in validate_field_drop_in_place(
        &blocks,
        &ownership_elaboration,
        &builder.locals,
        &builder.enum_layouts,
        &field_drop_admissible,
    ) {
        diagnostics.extend(project_findings(std::slice::from_ref(&check)));
    }
    let checked = CheckedMirFunction {
        name,
        key,
        return_ty,
        blocks,
        decisions,
        checks,
        cooperate_sites,
        ownership_elaboration: Some(Box::new(ownership_elaboration)),
    };
    // Final obligation proof runs immediately after sealing through
    // `validate_ownership_events`.  It consumes this immutable Checked-MIR
    // stream and the frozen elaboration only; the legacy local-count balance
    // model remains available to its hand-built unit fixtures but is no
    // longer production ownership authority.
    (checked, diagnostics)
}

/// Replay ownership elaboration using Checked MIR as the sole authority.
/// Absence is permitted only for hand-built analysis fixtures that never ask
/// for code generation, so reaching this function without a frozen plan is an
/// internal compiler invariant violation.
pub(super) fn elaborate(checked: &CheckedMirFunction) -> ElaboratedMirFunction {
    let _timing = crate::timing::stage("elaborate");
    checked
        .ownership_elaboration
        .as_deref()
        .cloned()
        .expect("Checked MIR reached ownership elaboration without a sealed ownership plan")
}

/// Recover one unambiguous lexical Place per binding from owner definitions.
fn unique_binding_definition_places(
    definition_places: &HashMap<crate::model::OwnerId, Place>,
) -> HashMap<BindingId, Place> {
    let mut places_by_binding = HashMap::<BindingId, HashSet<Place>>::new();
    for (owner, place) in definition_places {
        places_by_binding
            .entry(owner.binding)
            .or_default()
            .insert(*place);
    }
    places_by_binding
        .into_iter()
        .filter_map(|(binding, places)| {
            if places.len() != 1 {
                return None;
            }
            places.into_iter().next().map(|place| (binding, place))
        })
        .collect()
}

/// Anchor each definition-place relocation at its binding use in that block.
fn definition_relocation_sites(
    checked: &CheckedMirFunction,
    definition_places: &HashMap<crate::model::OwnerId, Place>,
) -> HashMap<BindingId, SiteId> {
    use crate::model::OwnershipEvent;

    let mut relocation_sites = HashMap::<BindingId, SiteId>::new();
    for block in &checked.blocks {
        let relocated = block
            .instructions
            .iter()
            .filter_map(|instruction| match instruction {
                Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, from, to })
                    if from != to && definition_places.get(owner) == Some(from) =>
                {
                    Some(owner.binding)
                }
                _ => None,
            })
            .collect::<HashSet<_>>();
        for statement in &block.statements {
            let MirStatement::Use { binding, site, .. } = statement else {
                continue;
            };
            if relocated.contains(binding) {
                relocation_sites.entry(*binding).or_insert(*site);
            }
        }
    }
    relocation_sites
}

/// Bindings defined in a block cannot be judged from its entry owner state.
fn block_defined_bindings(block: &BasicBlock) -> HashSet<BindingId> {
    use crate::model::OwnershipEvent;

    block
        .instructions
        .iter()
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(
                OwnershipEvent::Mint { owner, .. }
                | OwnershipEvent::Transfer {
                    to_owner: Some(owner),
                    ..
                },
            ) => Some(owner.binding),
            Instr::OwnershipEvent(
                OwnershipEvent::Reset { replacement, .. }
                | OwnershipEvent::Rearm { replacement, .. }
                | OwnershipEvent::Join { replacement, .. },
            ) => Some(replacement.binding),
            _ => None,
        })
        .collect()
}

/// Reject a lexical binding read when immutable owner state proves that its
/// owner was physically relocated away from the binding's definition place on
/// at least one reaching path.
///
/// `MirStatement::Use` carries binding identity rather than a backend Place.
/// The definition-site `Mint`/successor facts provide that Place, while the
/// exact owner-state intersection proves whether every path still owns the
/// binding there. A conditional selection (`a` on one arm, `b` on the other)
/// relocates `a` only on the selected arm; at the shared continuation there is
/// therefore no one exact `a` owner at its lexical slot, and `a.len()` is a
/// use-after-move. A clone publishes no source relocation and is untouched.
///
/// Admission is fail-closed but narrow: require one unambiguous definition
/// place for the binding and an explicit Relocate from that place. Bindings
/// with conflicting definition places remain the general ownership
/// validator's responsibility rather than receiving a guessed diagnostic.
fn relocated_binding_use_checks(
    checked: &CheckedMirFunction,
    entries: &HashMap<u32, ExactOwnerState>,
    definition_places: &HashMap<crate::model::OwnerId, Place>,
) -> Vec<MirCheck> {
    let lexical_places = unique_binding_definition_places(definition_places);
    let relocation_sites = definition_relocation_sites(checked, definition_places);
    if relocation_sites.is_empty() {
        return Vec::new();
    }

    let mut findings = Vec::new();
    let mut seen = HashSet::new();
    for block in &checked.blocks {
        let Some(entry) = entries.get(&block.id) else {
            continue;
        };
        let defined_here = block_defined_bindings(block);
        for statement in &block.statements {
            let MirStatement::Use {
                binding,
                name,
                site,
                ..
            } = statement
            else {
                continue;
            };
            let (Some(consumed_at), Some(lexical_place)) = (
                relocation_sites.get(binding).copied(),
                lexical_places.get(binding).copied(),
            ) else {
                continue;
            };
            if *site == consumed_at || defined_here.contains(binding) {
                continue;
            }
            let exact = entry
                .iter()
                .filter(|(owner, _)| owner.binding == *binding)
                .collect::<Vec<_>>();
            if matches!(exact.as_slice(), [(owner, place)] if **place == lexical_place
                && definition_places.get(owner) == Some(&lexical_place))
            {
                continue;
            }
            if seen.insert((*binding, *site)) {
                findings.push(MirCheck::UseAfterConsume {
                    binding: *binding,
                    name: name.clone(),
                    consumed_at,
                    used_at: *site,
                });
            }
        }
    }
    findings
}

/// Validate the generation-aware ownership program carried by Checked MIR.
/// The meet is intersection: an owner is usable after a join only when every
/// predecessor still owns that exact generation. This rejects stale/reused
/// generations and branch-dependent phantom ownership before codegen.
#[must_use]
/// Every exact generation live on a `Goto` edge must be named by a source-side
/// `EdgeCarry`, and every `EdgeCarry` to that target must name a generation
/// that is live there. Replay carries the whole exit state into the target,
/// and a `Goto` plan never discharges, so a missing carry is the one place a
/// silently unreleased generation could cross a join: it is rejected here
/// rather than converted into an executable drop the target would repeat.
fn goto_edge_carry_checks(
    checked: &CheckedMirFunction,
    block: &BasicBlock,
    exit_state: &ExactOwnerState,
) -> Vec<MirCheck> {
    use crate::model::OwnershipEvent;

    let Terminator::Goto { target } = block.terminator else {
        return Vec::new();
    };
    let carried = block
        .instructions
        .iter()
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
                owner,
                place,
                target: carried_target,
            }) if *carried_target == target => Some((*owner, *place)),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let mut findings = Vec::new();
    let mut live = exit_state
        .iter()
        .filter(|(_, place)| !matches!(place, Place::ReturnSlot))
        .map(|(owner, place)| (*owner, *place))
        .collect::<Vec<_>>();
    live.sort_by_key(|(owner, _)| *owner);
    for (owner, place) in live {
        if !carried.contains(&(owner, place)) {
            findings.push(MirCheck::DischargeAuthorityDrift {
                function: checked.name.clone(),
                block: block.id,
                name: "edge-carry".to_owned(),
                reason: format!(
                    "owner {owner} at {place:?} is live on the Goto edge to block {target} but no EdgeCarry preserves it"
                ),
            });
        }
    }
    let mut stale = carried
        .into_iter()
        .filter(|(owner, place)| exit_state.get(owner) != Some(place))
        .collect::<Vec<_>>();
    stale.sort_by_key(|(owner, _)| *owner);
    for (owner, place) in stale {
        findings.push(MirCheck::DischargeAuthorityDrift {
            function: checked.name.clone(),
            block: block.id,
            name: "edge-carry".to_owned(),
            reason: format!(
                "EdgeCarry names {owner} at {place:?} on the Goto edge to block {target}, but that generation is not live there"
            ),
        });
    }
    findings
}

#[allow(
    clippy::match_same_arms,
    clippy::too_many_lines,
    reason = "the generation-aware transfer function and its diagnostics form one validator invariant"
)]
pub(super) fn validate_ownership_events(checked: &CheckedMirFunction) -> Vec<MirCheck> {
    use crate::model::{OwnerId, OwnershipEvent};
    let _timing = crate::timing::stage("validate_ownership_events");

    let exact_states = exact_owner_states(&checked.blocks);
    let entries = &exact_states.0;
    let exits = &exact_states.1;
    let maybe_states = maybe_owner_states(&checked.blocks);
    let maybe_entries = &maybe_states.0;
    let maybe_exits = &maybe_states.1;
    let (must_binding_entries, _) = owner_state::must_binding_owner_states(&checked.blocks);
    let entry_parameter_owners = entry_cancel_parameter_owners(&checked.blocks, &checked.decisions);

    let mut findings = Vec::new();
    let mut published_guards = HashMap::new();
    let mut definition_types = HashMap::<OwnerId, ResolvedTy>::new();
    let mut definition_places = HashMap::<OwnerId, Place>::new();
    let mut recipes_by_owner = HashMap::<OwnerId, Vec<crate::model::OwnerDropRecipe>>::new();
    for block in &checked.blocks {
        for instruction in &block.instructions {
            let definition = match instruction {
                Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, ty }) => {
                    definition_places.insert(*owner, *place);
                    Some((*owner, ty))
                }
                Instr::OwnershipEvent(
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
                    }
                    | OwnershipEvent::Join {
                        replacement,
                        place,
                        ty,
                        ..
                    },
                ) => {
                    definition_places.insert(*replacement, *place);
                    Some((*replacement, ty))
                }
                Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    to: Some(place),
                    to_owner: Some(owner),
                    to_ty: Some(ty),
                    ..
                }) => {
                    definition_places.insert(*owner, *place);
                    Some((*owner, ty))
                }
                _ => None,
            };
            if let Some((owner, ty)) = definition {
                if definition_types.insert(owner, ty.clone()).is_some() {
                    findings.push(MirCheck::DischargeAuthorityDrift {
                        function: checked.name.clone(),
                        block: block.id,
                        name: "ownership-recipe".to_owned(),
                        reason: format!("owner {owner} has more than one definition site"),
                    });
                }
            }
            if let Instr::OwnershipEvent(OwnershipEvent::DropRecipe { owner, recipe }) = instruction
            {
                recipes_by_owner
                    .entry(*owner)
                    .or_default()
                    .push(recipe.clone());
            }
            let Instr::OwnershipEvent(OwnershipEvent::Guard { owner, flag, kind }) = instruction
            else {
                continue;
            };
            if let Some((prior_flag, prior_kind)) = published_guards.insert(*owner, (*flag, *kind))
            {
                if prior_flag != *flag || prior_kind != *kind {
                    findings.push(MirCheck::DischargeAuthorityDrift {
                        function: checked.name.clone(),
                        block: block.id,
                        name: "ownership-generation".to_owned(),
                        reason: format!(
                            "owner {owner} publishes conflicting cleanup guards: {prior_kind:?}@{prior_flag:?} and {kind:?}@{flag:?}"
                        ),
                    });
                }
            }
        }
    }
    if checked.ownership_elaboration.is_some() {
        // `definition_types` is a HashMap; report recipe findings in source
        // order (the binding's `Bind` site), then by owner, so a missing or
        // drifted recipe is diagnosed deterministically and once per owner.
        let binding_sites: HashMap<BindingId, SiteId> = checked
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .filter_map(|statement| match statement {
                MirStatement::Bind { binding, site, .. } => Some((*binding, *site)),
                _ => None,
            })
            .collect();
        let mut definitions = definition_types.iter().collect::<Vec<_>>();
        definitions.sort_by_key(|(owner, _)| {
            let site = binding_sites.get(&owner.binding).copied();
            (site.is_none(), site.unwrap_or(SiteId(u32::MAX)), **owner)
        });
        for (owner, ty) in definitions {
            match recipes_by_owner.get(owner).map(Vec::as_slice) {
                Some([recipe]) if &recipe.ty == ty => {}
                Some([recipe]) => findings.push(MirCheck::DischargeAuthorityDrift {
                    function: checked.name.clone(),
                    block: ENTRY_BLOCK_ID,
                    name: "ownership-recipe".to_owned(),
                    reason: format!(
                        "owner {owner} is defined as {ty}, but its destructor recipe names {}",
                        recipe.ty
                    ),
                }),
                Some(recipes) => findings.push(MirCheck::DischargeAuthorityDrift {
                    function: checked.name.clone(),
                    block: ENTRY_BLOCK_ID,
                    name: "ownership-recipe".to_owned(),
                    reason: format!(
                        "owner {owner} must publish exactly one destructor recipe, found {}",
                        recipes.len()
                    ),
                }),
                None => findings.push(MirCheck::DischargeAuthorityDrift {
                    function: checked.name.clone(),
                    block: ENTRY_BLOCK_ID,
                    name: "ownership-recipe".to_owned(),
                    reason: format!("owner {owner} has no definition-site destructor recipe"),
                }),
            }
        }
        for owner in recipes_by_owner.keys() {
            if !definition_types.contains_key(owner) {
                findings.push(MirCheck::DischargeAuthorityDrift {
                    function: checked.name.clone(),
                    block: ENTRY_BLOCK_ID,
                    name: "ownership-recipe".to_owned(),
                    reason: format!(
                        "destructor recipe for {owner} has no matching owner definition"
                    ),
                });
            }
        }
    }
    findings.extend(relocated_binding_use_checks(
        checked,
        entries,
        &definition_places,
    ));
    let guarded_owners = published_guards.keys().copied().collect::<HashSet<_>>();
    let mut reported_shared_places = HashSet::<(Place, Vec<OwnerId>)>::new();
    for block in &checked.blocks {
        let Some(mut live) = entries.get(&block.id).cloned() else {
            continue;
        };
        let mut maybe_live = maybe_entries.get(&block.id).cloned().unwrap_or_default();
        let mut pending_relocations: HashMap<OwnerId, (Place, Place)> = HashMap::new();
        for (instruction_index, instruction) in block.instructions.iter().enumerate() {
            let Instr::OwnershipEvent(event) = instruction else {
                if let Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. } =
                    instruction
                {
                    for owner in live
                        .iter()
                        .filter_map(|(owner, place)| (*place == *src).then_some(*owner))
                    {
                        pending_relocations.insert(owner, (*src, *dest));
                    }
                }
                apply_exact_owner_ops(std::slice::from_ref(instruction), &mut live);
                apply_maybe_owner_ops(std::slice::from_ref(instruction), &mut maybe_live);
                continue;
            };
            let defect = match event {
                OwnershipEvent::Mint { owner, .. } => maybe_live
                    .iter()
                    .any(|(live_owner, _)| live_owner.binding == owner.binding)
                    .then(|| {
                        format!(
                            "owner {owner} is minted while another generation of that binding is already live"
                        )
                    }),
                OwnershipEvent::Transfer {
                    owner,
                    from,
                    to,
                    to_owner,
                    to_ty,
                } => match live.get(owner) {
                    None => Some(format!(
                        "owner {owner} is transferred after its generation ended"
                    )),
                    Some(actual) if actual != from => {
                        let paired_relocation = to == &Some(*actual)
                            && pending_relocations.get(owner) == Some(&(*from, *actual));
                        if paired_relocation {
                            None
                        } else {
                            Some(format!(
                                "owner {owner} is transferred from {from:?}, but Checked MIR records its current place as {actual:?}"
                            ))
                        }
                    }
                    Some(_) if to_owner.is_some() && to.is_none() => Some(format!(
                        "owner {owner} transfers to a new owner without a destination place"
                    )),
                    Some(_) if to_owner.is_some() != to_ty.is_some() => Some(format!(
                        "owner {owner} transfer successor identity/type authority is incomplete"
                    )),
                    Some(_) if to_owner.is_some_and(|next| live.contains_key(&next)) => {
                        Some(format!(
                            "owner {owner} transfers into an already-live owner generation {}",
                            owner_list(to_owner.iter())
                        ))
                    }
                    Some(_) => None,
                },
                OwnershipEvent::Relocate { owner, from, to } => match live.get(owner) {
                    None if maybe_live.contains(&(*owner, *from))
                        || maybe_live.contains(&(*owner, *to)) =>
                    {
                        None
                    }
                    None => Some(format!(
                        "owner {owner} is relocated after its generation ended"
                    )),
                    Some(actual) if actual != from && actual != to => Some(format!(
                        "owner {owner} is relocated from {from:?}, but Checked MIR records its current place as {actual:?}"
                    )),
                    Some(_) => None,
                },
                OwnershipEvent::Release { owner, place } => match live.get(owner) {
                    None => Some(format!(
                        "owner {owner} is released after its generation ended"
                    )),
                    Some(actual) if actual != place => Some(format!(
                        "owner {owner} is released from {place}, but Checked MIR records its current place as {actual}"
                    )),
                    Some(_) => None,
                },
                OwnershipEvent::GuardedRelease { owner, place, flag } => {
                    if published_guards.get(owner).map(|(published, _)| published) != Some(flag) {
                        Some(format!(
                            "guarded release for {owner} names {flag}, but that owner publishes {}",
                            describe_optional_place(
                                published_guards.get(owner).map(|(published, _)| published)
                            )
                        ))
                    } else if live.get(owner).is_some_and(|actual| actual != place) {
                        Some(format!(
                            "guarded release for {owner} names {place}, but Checked MIR records its current place as {}",
                            describe_optional_place(live.get(owner))
                        ))
                    } else {
                        None
                    }
                }
                OwnershipEvent::DemoteToAlias { owner, place } => match live.get(owner) {
                    None => Some(format!(
                        "owner {owner} is demoted after its generation ended"
                    )),
                    Some(actual) if actual != place => Some(format!(
                        "owner {owner} is demoted at {place:?}, but Checked MIR records its current place as {actual:?}"
                    )),
                    Some(_) => None,
                },
                OwnershipEvent::Reset {
                    previous,
                    replacement,
                    place,
                    ..
                } => {
                    if replacement.binding != previous.binding
                        || replacement.generation != previous.generation.saturating_add(1)
                    {
                        Some(format!(
                            "reset {previous} -> {replacement} is not the next generation"
                        ))
                    } else if !(live.contains_key(previous)
                        || (published_guards.contains_key(previous)
                            && maybe_entries
                                .get(&block.id)
                                .is_some_and(|state| state.contains(&(*previous, *place)))))
                    {
                        Some(format!(
                            "reset source {previous} is neither live on every incoming path nor conditionally live under its published guard"
                        ))
                    } else if live.get(previous).is_some_and(|actual| actual != place) {
                        Some(format!(
                            "reset source {previous} names {place}, but Checked MIR records its current place as {}",
                            describe_optional_place(live.get(previous))
                        ))
                    } else if live.contains_key(replacement) {
                        Some(format!("reset replacement {replacement} is already live"))
                    } else {
                        None
                    }
                }
                OwnershipEvent::Rearm {
                    previous,
                    replacement,
                    place,
                    ty,
                } => {
                    let maybe_same_binding = maybe_live
                        .iter()
                        .filter(|(owner, _)| owner.binding == previous.binding)
                        .copied()
                        .collect::<Vec<_>>();
                    let exact_same_binding = live
                        .iter()
                        .filter(|(owner, _)| owner.binding == previous.binding)
                        .map(|(owner, actual)| (*owner, *actual))
                        .collect::<Vec<_>>();
                    if replacement.binding != previous.binding
                        || replacement.generation != previous.generation.saturating_add(1)
                    {
                        Some(format!(
                            "rearm {previous} -> {replacement} is not the next generation"
                        ))
                    } else if maybe_same_binding.as_slice() != [(*previous, *place)] {
                        Some(format!(
                            "rearm source {previous} is not the sole possibly-live generation at {place:?}: {}",
                            owner_place_list(maybe_same_binding.iter())
                        ))
                    } else if !exact_same_binding.is_empty()
                        && exact_same_binding.as_slice() != [(*previous, *place)]
                    {
                        Some(format!(
                            "rearm source {previous} has ambiguous exact generations/places {}",
                            owner_place_list(exact_same_binding.iter())
                        ))
                    } else if definition_places.get(previous) != Some(place)
                        || definition_places.get(replacement) != Some(place)
                    {
                        Some(format!(
                            "rearm {previous} -> {replacement} does not preserve its definition place {place:?}"
                        ))
                    } else if definition_types.get(previous) != Some(ty)
                        || definition_types.get(replacement) != Some(ty)
                    {
                        Some(format!(
                            "rearm {previous} -> {replacement} does not preserve its definition type {ty}"
                        ))
                    } else if !published_guards.contains_key(previous)
                        || published_guards.get(previous) != published_guards.get(replacement)
                    {
                        Some(format!(
                            "rearm {previous} -> {replacement} does not preserve one exact cleanup guard"
                        ))
                    } else if recipes_by_owner.get(previous).map(Vec::as_slice)
                        != recipes_by_owner.get(replacement).map(Vec::as_slice)
                        || recipes_by_owner
                            .get(previous)
                            .is_none_or(|recipes| recipes.len() != 1)
                    {
                        Some(format!(
                            "rearm {previous} -> {replacement} does not preserve one exact destructor recipe"
                        ))
                    } else if maybe_live.contains(&(*replacement, *place))
                        || live.contains_key(replacement)
                    {
                        Some(format!(
                            "rearm replacement {replacement} is already live before its lineage event"
                        ))
                    } else {
                        None
                    }
                }
                OwnershipEvent::Guard { owner, flag, .. } => {
                    (!live.contains_key(owner)).then(|| {
                        format!("cleanup guard {flag:?} is attached after owner {owner} ended")
                    })
                }
                OwnershipEvent::Join {
                    incoming,
                    replacement,
                    place,
                    ty,
                } => {
                    let same_binding = incoming
                        .iter()
                        .all(|owner| owner.binding == replacement.binding);
                    let distinct = incoming.iter().all(|owner| owner != replacement)
                        && incoming.iter().copied().collect::<HashSet<_>>().len()
                            == incoming.len();
                    let declared = incoming
                        .iter()
                        .copied()
                        .map(|owner| (owner, *place))
                        .collect::<HashSet<_>>();
                    let possible = maybe_live
                        .iter()
                        .filter(|(owner, _)| owner.binding == replacement.binding)
                        .copied()
                        .collect::<HashSet<_>>();
                    let exact_same_binding = live
                        .iter()
                        .filter(|(owner, _)| owner.binding == replacement.binding)
                        .map(|(owner, current)| (*owner, *current))
                        .collect::<HashSet<_>>();
                    let metadata_matches = incoming
                        .iter()
                        .chain(std::iter::once(replacement))
                        .all(|owner| {
                            definition_places.get(owner) == Some(place)
                                && definition_types.get(owner) == Some(ty)
                                && published_guards.get(owner)
                                    == published_guards.get(replacement)
                                && recipes_by_owner.get(owner).map(Vec::as_slice)
                                    == recipes_by_owner.get(replacement).map(Vec::as_slice)
                                && recipes_by_owner
                                    .get(owner)
                                    .is_some_and(|recipes| recipes.len() == 1)
                        });
                    let incoming_owners = incoming;
                    let incoming = owner_list(incoming_owners.iter());
                    if incoming_owners.is_empty() || !same_binding || !distinct {
                        Some(format!(
                            "ownership join {incoming} -> {replacement} is not a non-empty set of distinct same-binding predecessors and successor"
                        ))
                    } else if possible != declared {
                        Some(format!(
                            "ownership join {incoming} -> {replacement} does not enumerate its exact possible incoming owners at {place:?}: {}",
                            owner_place_list(possible.iter())
                        ))
                    } else if !exact_same_binding.is_subset(&declared) {
                        Some(format!(
                            "ownership join {incoming} -> {replacement} has ambiguous exact incoming owners {}",
                            owner_place_list(exact_same_binding.iter())
                        ))
                    } else if must_binding_entries
                        .get(&block.id)
                        .and_then(|state| state.get(&replacement.binding))
                        != Some(place)
                    {
                        Some(format!(
                            "ownership join {incoming} -> {replacement} has an ownerless or wrong-place incoming path"
                        ))
                    } else if live.contains_key(replacement)
                        || maybe_live.contains(&(*replacement, *place))
                    {
                        Some(format!(
                            "ownership join replacement {replacement} is already live before convergence"
                        ))
                    } else if !metadata_matches {
                        Some(format!(
                            "ownership join {incoming} -> {replacement} does not preserve one exact place/type/guard/recipe"
                        ))
                    } else {
                        None
                    }
                }
                OwnershipEvent::DropRecipe { .. }
                | OwnershipEvent::InteriorAlias { .. }
                | OwnershipEvent::AliasRelocate { .. }
                | OwnershipEvent::AliasEnd { .. }
                | OwnershipEvent::EdgeCarry { .. }
                | OwnershipEvent::ScopeExit { .. } => None,
            };
            if let Some(reason) = defect {
                findings.push(MirCheck::DischargeAuthorityDrift {
                    function: checked.name.clone(),
                    block: block.id,
                    name: "ownership-generation".to_owned(),
                    reason: format!("{reason} at instruction {instruction_index}"),
                });
            }
            apply_exact_owner_ops(std::slice::from_ref(instruction), &mut live);
            apply_maybe_owner_ops(std::slice::from_ref(instruction), &mut maybe_live);
            for (place, owners) in shared_exact_owner_places(&live) {
                if !reported_shared_places.insert((place, owners.clone())) {
                    continue;
                }
                findings.push(MirCheck::DischargeAuthorityDrift {
                    function: checked.name.clone(),
                    block: block.id,
                    name: "ownership-place".to_owned(),
                    reason: format!(
                        "place {place} has more than one live exact owner generation {} after instruction {instruction_index}",
                        owner_list(owners.iter())
                    ),
                });
            }
            match event {
                OwnershipEvent::Mint { owner, .. }
                | OwnershipEvent::Transfer { owner, .. }
                | OwnershipEvent::Relocate { owner, .. }
                | OwnershipEvent::Release { owner, .. }
                | OwnershipEvent::GuardedRelease { owner, .. }
                | OwnershipEvent::DemoteToAlias { owner, .. }
                | OwnershipEvent::Guard { owner, .. } => {
                    pending_relocations.remove(owner);
                }
                OwnershipEvent::Reset {
                    previous,
                    replacement,
                    ..
                } => {
                    pending_relocations.remove(previous);
                    pending_relocations.remove(replacement);
                }
                OwnershipEvent::Rearm {
                    previous,
                    replacement,
                    ..
                } => {
                    pending_relocations.remove(previous);
                    pending_relocations.remove(replacement);
                }
                OwnershipEvent::Join {
                    incoming,
                    replacement,
                    ..
                } => {
                    for owner in incoming {
                        pending_relocations.remove(owner);
                    }
                    pending_relocations.remove(replacement);
                }
                OwnershipEvent::DropRecipe { .. }
                | OwnershipEvent::InteriorAlias { .. }
                | OwnershipEvent::AliasRelocate { .. }
                | OwnershipEvent::AliasEnd { .. }
                | OwnershipEvent::EdgeCarry { .. }
                | OwnershipEvent::ScopeExit { .. } => {}
            }
        }
        findings.extend(goto_edge_carry_checks(checked, block, &live));
    }
    // Revalidate every unconditional cleanup against the same Checked-MIR
    // program. This is deliberately downstream of sealing and accepts no
    // Builder: codegen cannot consume a frozen drop plan whose owner/place is
    // not reproducible from the immutable event/dataflow stream.
    if let Some(elaboration) = checked.ownership_elaboration.as_deref() {
        let owner_types = &definition_types;
        let owner_guards: HashMap<OwnerId, Place> = checked
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .filter_map(|instruction| match instruction {
                Instr::OwnershipEvent(OwnershipEvent::Guard { owner, flag, .. }) => {
                    Some((*owner, *flag))
                }
                _ => None,
            })
            .collect();
        let binding_metadata = binding_definition_metadata(&checked.blocks);
        for (exit, plan) in &elaboration.drop_plans {
            let block_id = exit_block_id(exit);
            let Some(_block) = checked.blocks.iter().find(|block| block.id == block_id) else {
                continue;
            };
            let live = exact_owner_state_for_exit(exit, &checked.blocks, entries, exits);
            let required = if matches!(exit, ExitPath::Cancel { block } if *block == ENTRY_BLOCK_ID)
            {
                entry_parameter_owners.clone()
            } else {
                guarded_required_owners_for_exit(
                    exit,
                    &checked.blocks,
                    entries,
                    exits,
                    maybe_entries,
                    maybe_exits,
                    &guarded_owners,
                )
            };
            for (binding, candidates) in ambiguous_guarded_owners_for_exit(
                exit,
                &checked.blocks,
                maybe_entries,
                maybe_exits,
                &guarded_owners,
            ) {
                if required.keys().any(|owner| owner.binding == binding) {
                    continue;
                }
                let local_ty = candidates
                    .first()
                    .and_then(|(owner, _)| definition_types.get(owner))
                    .map_or_else(|| "<unknown>".to_owned(), ToString::to_string);
                let Some((name, site)) = binding_metadata.get(&binding).cloned() else {
                    findings.push(unnameable_obligation_subject(
                        &checked.name,
                        block_id,
                        format!(
                            "conditionally live owner {binding} has no definition-site binding on \
                             {}",
                            exit_path_user_label(exit)
                        ),
                    ));
                    continue;
                };
                findings.push(MirCheck::ObligationUnderReleased {
                    function: checked.name.clone(),
                    blocks: vec![block_id],
                    site,
                    name,
                    local_ty,
                    mint_provenance: crate::model::ObligationMintProvenance::Ordinary,
                    reason: format!(
                        "no cleanup can be admitted on {}: the value is only conditionally live there",
                        exit_path_user_label(exit)
                    ),
                });
            }
            let mut matched = HashSet::new();
            for drop in &plan.drops {
                if let Some(guard) = drop.guard {
                    let exact_live_owners: Vec<OwnerId> = live
                        .iter()
                        .filter_map(|(owner, place)| (*place == drop.place).then_some(*owner))
                        .collect();
                    if let [owner] = exact_live_owners.as_slice() {
                        if *owner != guard.owner || owner_guards.get(owner) != Some(&guard.flag) {
                            findings.push(MirCheck::DischargeAuthorityDrift {
                                function: checked.name.clone(),
                                block: block_id,
                                name: "checked-ownership-plan".to_owned(),
                                reason: format!(
                                    "cleanup at {} is guarded by {}@{}, but live generation {owner} carries {}",
                                    drop.place,
                                    guard.owner,
                                    guard.flag,
                                    describe_optional_place(owner_guards.get(owner))
                                ),
                            });
                        }
                    }
                }
                let candidates: Vec<OwnerId> = required
                    .iter()
                    .filter_map(|(owner, place)| {
                        let guard_matches = drop.guard.is_none_or(|guard| guard.owner == *owner);
                        (guard_matches
                            && *place == drop.place
                            && owner_types.get(owner) == Some(&drop.ty))
                        .then_some(*owner)
                    })
                    .collect();
                match candidates.as_slice() {
                    [owner]
                        if recipes_by_owner.get(owner).is_some_and(|recipes| {
                            matches!(recipes.as_slice(), [recipe]
                                if recipe.ty == drop.ty
                                    && recipe.kind == drop.kind
                                    && recipe.drop_fn == drop.drop_fn)
                        }) && matched.insert(*owner) => {}
                    [owner]
                        if recipes_by_owner.get(owner).is_some_and(|recipes| {
                            matches!(recipes.as_slice(), [recipe]
                                if recipe.ty != drop.ty
                                    || recipe.kind != drop.kind
                                    || recipe.drop_fn != drop.drop_fn)
                        }) => findings.push(MirCheck::DischargeAuthorityDrift {
                            function: checked.name.clone(),
                            block: block_id,
                            name: "ownership-recipe".to_owned(),
                            reason: format!(
                                "cleanup ritual at {} does not equal the definition-site recipe for {owner}",
                                drop.place
                            ),
                        }),
                    [owner] => findings.push(match binding_metadata.get(&owner.binding) {
                        Some((name, site)) => MirCheck::ObligationOverReleased {
                            function: checked.name.clone(),
                            blocks: vec![block_id],
                            site: *site,
                            name: name.clone(),
                            reason: format!(
                                "the exit plan for {} releases it more than once",
                                exit_path_user_label(exit)
                            ),
                        },
                        None => unnameable_obligation_subject(
                            &checked.name,
                            block_id,
                            format!(
                                "the exit plan for {} releases owner {owner} twice, and that \
                                 owner has no definition-site binding",
                                exit_path_user_label(exit)
                            ),
                        ),
                    }),
                    [] => findings.push(unnameable_obligation_subject(
                        &checked.name,
                        block_id,
                        format!(
                            "the exit plan for {} releases a `{}` that the replayed event stream \
                             no longer holds there",
                            exit_path_user_label(exit),
                            drop.ty
                        ),
                    )),
                    _ => findings.push(MirCheck::DischargeAuthorityDrift {
                        function: checked.name.clone(),
                        block: block_id,
                        name: "checked-ownership-plan".to_owned(),
                        reason: format!(
                            "cleanup at {} ambiguously matches multiple exact owner generations {}",
                            drop.place,
                            owner_list(candidates.iter())
                        ),
                    }),
                }
            }
            // The inverse direction: every required owner with a usable
            // definition-site recipe must be discharged by exactly one plan
            // entry. An owner without a single recipe is already reported at
            // its definition site, and a place shared by two required
            // generations is already the `ownership-place` finding, so only
            // sole-owner, recipe-bearing omissions are new.
            let mut omitted = required
                .iter()
                .filter(|(owner, place)| {
                    !matched.contains(*owner)
                        && recipes_by_owner
                            .get(*owner)
                            .is_some_and(|recipes| recipes.len() == 1)
                        && required.values().filter(|other| *other == *place).count() == 1
                })
                .collect::<Vec<_>>();
            omitted.sort_by_key(|(owner, _)| (owner.binding.0, owner.generation));
            for (owner, _place) in omitted {
                let Some((name, site)) = binding_metadata.get(&owner.binding).cloned() else {
                    findings.push(unnameable_obligation_subject(
                        &checked.name,
                        block_id,
                        format!(
                            "the exit plan for {} omits cleanup for owner {owner}, and that owner \
                             has no definition-site binding",
                            exit_path_user_label(exit)
                        ),
                    ));
                    continue;
                };
                findings.push(MirCheck::ObligationUnderReleased {
                    function: checked.name.clone(),
                    blocks: vec![block_id],
                    site,
                    name,
                    local_ty: definition_types
                        .get(owner)
                        .map_or_else(|| "<unknown>".to_owned(), ToString::to_string),
                    mint_provenance: crate::model::ObligationMintProvenance::Ordinary,
                    reason: format!(
                        "the exit plan for {} omits its cleanup",
                        exit_path_user_label(exit)
                    ),
                });
            }
        }
    }
    findings
}

/// An obligation imbalance whose subject the user cannot name is a lowering
/// defect, not a user error. Two cases reach here: the owner's binding has no
/// `Bind` statement anywhere in the function (a lowering-synthesized temp, so
/// there is no source value to report), and a frozen plan entry that matches
/// no required owner at all (the plan and the replayed event stream disagree).
/// Both go to the internal-error channel rather than inventing a value name
/// out of the `OwnerId` and anchoring it at `SiteId(0)`, which renders a caret
/// on the module's first construct.
fn unnameable_obligation_subject(function: &str, block: u32, reason: String) -> MirCheck {
    MirCheck::DischargeAuthorityDrift {
        function: function.to_owned(),
        block,
        name: "ownership-obligation-subject".to_owned(),
        reason,
    }
}

/// Render owner generations for a verifier finding as `[b3#0, b3#1]`.
/// Findings are surfaced to users through the internal-error channel, so
/// they carry the `OwnerId` `Display` form, never its `Debug` payload.
fn owner_list<'a>(owners: impl IntoIterator<Item = &'a crate::model::OwnerId>) -> String {
    let rendered = owners
        .into_iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>();
    format!("[{}]", rendered.join(", "))
}

/// Render `(owner, place)` pairs for a verifier finding as `[b3#0@Local(2)]`.
fn owner_place_list<'a>(
    pairs: impl IntoIterator<Item = &'a (crate::model::OwnerId, Place)>,
) -> String {
    let mut rendered = pairs
        .into_iter()
        .map(|(owner, place)| format!("{owner}@{place:?}"))
        .collect::<Vec<_>>();
    rendered.sort();
    format!("[{}]", rendered.join(", "))
}

/// Every place that currently carries more than one live exact owner
/// generation, with those generations in a canonical order.
///
/// Checked MIR admits at most one live exact owner per `Place` at any program
/// point: a second generation over the same bytes makes every later cleanup
/// match the place ambiguously and the balance fixpoint fail closed. Lowering
/// hands a value from one generation to the next with `Transfer`/`Reset`/
/// `Rearm`/`Join`; a bare second `Mint` or `Relocate` onto a live place is a
/// lowering defect, never a user error.
fn shared_exact_owner_places(live: &ExactOwnerState) -> Vec<(Place, Vec<crate::model::OwnerId>)> {
    let mut by_place = HashMap::<Place, Vec<crate::model::OwnerId>>::new();
    for (owner, place) in live {
        by_place.entry(*place).or_default().push(*owner);
    }
    let mut shared = by_place
        .into_iter()
        .filter(|(_, owners)| owners.len() > 1)
        .collect::<Vec<_>>();
    for (_, owners) in &mut shared {
        owners.sort_by_key(|owner| (owner.binding.0, owner.generation));
    }
    shared.sort_by_key(|(place, _)| format!("{place:?}"));
    shared
}

#[cfg(test)]
fn one_owner_per_place_findings(checked: &CheckedMirFunction) -> Vec<String> {
    validate_ownership_events(checked)
        .into_iter()
        .filter_map(|finding| match finding {
            MirCheck::DischargeAuthorityDrift { name, reason, .. } if name == "ownership-place" => {
                Some(reason)
            }
            _ => None,
        })
        .collect()
}

#[test]
fn raw_duplicate_mint_into_live_place_is_rejected_by_checked_mir_verification() {
    use crate::model::{OwnerId, OwnershipEvent};

    let payload = OwnerId {
        binding: BindingId(2),
        generation: 0,
    };
    let temp = OwnerId {
        binding: BindingId(4_294_967_226),
        generation: 0,
    };
    let slot = Place::Local(6);
    let recipe = checked_test_string_recipe();
    // The pre-fix `quote(when()?)` stream: the `__try_ok` payload owner is
    // relocated into the call-argument slot and the join then mints the
    // argument temp over the same slot.
    let raw_duplicate_mint_stream = vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: payload,
            place: Place::Local(13),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: payload,
            recipe: recipe.clone(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: payload,
            from: Place::Local(13),
            to: slot,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: temp,
            place: slot,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: temp,
            recipe,
        }),
    ];
    let checked = checked_with_ownership_events(raw_duplicate_mint_stream);
    let findings = validate_ownership_events(&checked)
        .into_iter()
        .filter_map(|finding| match finding {
            MirCheck::DischargeAuthorityDrift { name, reason, .. } if name == "ownership-place" => {
                Some(reason)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        findings.len(),
        1,
        "one finding per shared place, not one per later instruction: {findings:?}"
    );
    assert!(
        findings[0].contains("_6")
            && findings[0].contains("b2#0")
            && findings[0].contains("b4294967226#0")
            && findings[0].contains("after instruction 3"),
        "{findings:?}"
    );
}

/// The verifier's findings are lowering invariants: they reach the user
/// through the internal-compiler-error channel, once per function, and they
/// name owners by their `Display` identity — the derived `Debug` payload
/// (`OwnerId { binding: BindingId(..) .. }`) never appears.
#[test]
fn verifier_findings_project_to_one_internal_error_without_debug_identities() {
    use crate::model::{MirDiagnosticKind, OwnerId, OwnershipEvent};

    let payload = OwnerId {
        binding: BindingId(2),
        generation: 0,
    };
    let temp = OwnerId {
        binding: BindingId(4_294_967_226),
        generation: 0,
    };
    let slot = Place::Local(6);
    let recipe = checked_test_string_recipe();
    // Two defects in one function: a second mint over a live place, and a
    // release of an owner whose generation never existed.
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: payload,
            place: slot,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: payload,
            recipe: recipe.clone(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: temp,
            place: slot,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: temp,
            recipe,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Release {
            owner: OwnerId {
                binding: BindingId(9),
                generation: 4,
            },
            place: Place::Local(1),
        }),
    ]);
    let findings = validate_ownership_events(&checked);
    assert!(findings.len() >= 2, "{findings:?}");
    for finding in &findings {
        let MirCheck::DischargeAuthorityDrift { reason, .. } = finding else {
            panic!("unexpected finding {finding:?}");
        };
        assert!(
            !reason.contains("OwnerId {") && !reason.contains("BindingId("),
            "finding leaks Debug identities: {reason}"
        );
    }
    let diagnostics = project_findings(&findings);
    let [diagnostic] = diagnostics.as_slice() else {
        panic!("one internal error per function, got {diagnostics:#?}");
    };
    let MirDiagnosticKind::LoweringInvariant {
        function,
        rule,
        detail,
        ..
    } = &diagnostic.kind
    else {
        panic!("expected LoweringInvariant, got {diagnostic:#?}");
    };
    assert_eq!(function, &checked.name);
    assert_eq!(rule, "ownership-place");
    assert!(
        detail.contains("b2#0") && detail.contains("b4294967226#0"),
        "{detail}"
    );
}

#[test]
fn generation_ending_transfer_before_join_mint_is_one_owner_per_place() {
    use crate::model::{OwnerId, OwnershipEvent};

    let payload = OwnerId {
        binding: BindingId(2),
        generation: 0,
    };
    let temp = OwnerId {
        binding: BindingId(4_294_967_226),
        generation: 0,
    };
    let slot = Place::Local(6);
    let recipe = checked_test_string_recipe();
    // The fixed stream: the arm's Move hands the payload generation off
    // (`Transfer` to the slot with no successor owner) before the join mints
    // the argument temp there.
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: payload,
            place: Place::Local(13),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: payload,
            recipe: recipe.clone(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: payload,
            from: Place::Local(13),
            to: Some(slot),
            to_owner: None,
            to_ty: None,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: temp,
            place: slot,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: temp,
            recipe,
        }),
    ]);
    assert_eq!(one_owner_per_place_findings(&checked), Vec::<String>::new());
}

/// Lower the `quote(when()?)` shape end to end: a `?`-payload owner
/// (`__try_ok`) is the source of one arm's Move into the call-argument slot,
/// and the join publication mints the argument temp over that same slot. The
/// producer must end the payload generation at its Move; this pins that
/// `composite_join_predecessor_move_sources` keeps doing so rather than the
/// hand-built streams above, which insert the Transfer themselves.
#[test]
fn try_payload_forwarded_into_a_call_argument_join_has_one_owner_per_place() {
    let source = r#"
fn quote(s: string) -> string { "\"" + s + "\"" }
fn when() -> Result<string, string> { Ok("now") }

fn build(kind: string) -> Result<string, string> {
    let event = "{\"type\":" + quote(kind) + ",\"time\":" + quote(when()?) + "}";
    Ok(event)
}
"#;
    let module = crate::return_provenance::tests::lower_source(source);
    let pipeline = crate::lower_hir_module(&module);
    let checked = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "build")
        .expect("checked build");
    // Mutation oracle: with the join scan disabled this reports
    // "place Local(N) has more than one live exact owner generation".
    assert_eq!(one_owner_per_place_findings(checked), Vec::<String>::new());
    assert!(
        pipeline.diagnostics.is_empty(),
        "fixture must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn distinct_places_with_one_owner_each_are_not_shared() {
    use crate::model::{OwnerId, OwnershipEvent};

    let recipe = checked_test_string_recipe();
    let mut events = Vec::new();
    for (binding, local) in [(BindingId(3), 7), (BindingId(4), 8)] {
        let owner = OwnerId {
            binding,
            generation: 0,
        };
        events.push(Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner,
            place: Place::Local(local),
            ty: ResolvedTy::String,
        }));
        events.push(Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner,
            recipe: recipe.clone(),
        }));
    }
    let checked = checked_with_ownership_events(events);
    assert_eq!(one_owner_per_place_findings(&checked), Vec::<String>::new());
}

#[cfg(test)]
fn checked_with_ownership_events(events: Vec<Instr>) -> CheckedMirFunction {
    CheckedMirFunction {
        name: "ownership_event_falsifier".to_owned(),
        key: crate::model::MirCallableKey::for_test("ownership_event_falsifier"),
        return_ty: ResolvedTy::Unit,
        blocks: vec![BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: events,
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: None,
    }
}

#[cfg(test)]
fn checked_test_string_recipe() -> crate::model::OwnerDropRecipe {
    crate::model::OwnerDropRecipe {
        ty: ResolvedTy::String,
        drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::String,
        },
        declaration_order: 0,
    }
}

#[cfg(test)]
fn checked_recipe_fixture(
    recipes: Vec<crate::model::OwnerDropRecipe>,
    drops: Vec<ElabDrop>,
) -> CheckedMirFunction {
    let owner = crate::model::OwnerId {
        binding: BindingId(177),
        generation: 0,
    };
    let place = Place::Local(7);
    let mut instructions = vec![Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
        owner,
        place,
        ty: ResolvedTy::String,
    })];
    instructions.extend(recipes.into_iter().map(|recipe| {
        Instr::OwnershipEvent(crate::model::OwnershipEvent::DropRecipe { owner, recipe })
    }));
    CheckedMirFunction {
        name: "recipe_invariant".to_owned(),
        key: crate::model::MirCallableKey::for_test("recipe_invariant"),
        return_ty: ResolvedTy::Unit,
        blocks: vec![BasicBlock {
            id: ENTRY_BLOCK_ID,
            // The owner has a source-level `let`, so an imbalance over it is
            // the user's error and names the value the way the source does.
            statements: vec![MirStatement::Bind {
                binding: owner.binding,
                name: "greeting".to_owned(),
                site: SiteId(12),
                ty: ResolvedTy::String,
            }],
            instructions,
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: Some(Box::new(ElaboratedMirFunction {
            name: "recipe_invariant".to_owned(),
            key: crate::model::MirCallableKey::for_test("recipe_invariant"),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans: vec![(
                ExitPath::Return {
                    block: ENTRY_BLOCK_ID,
                },
                DropPlan { drops },
            )],
            coroutine: None,
            lambda_captures: vec![],
        })),
    }
}

#[cfg(test)]
fn checked_rearm_fixture(rearm_place: Place, replacement_flag: Place) -> CheckedMirFunction {
    use crate::model::{OwnerId, OwnershipEvent, OwnershipGuardKind};

    let old = OwnerId {
        binding: BindingId(178),
        generation: 0,
    };
    let replacement = OwnerId {
        binding: old.binding,
        generation: 1,
    };
    let place = Place::Local(7);
    let flag = Place::Local(8);
    let recipe = checked_test_string_recipe();
    checked_with_ownership_events(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: old,
            place,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: old,
            recipe: recipe.clone(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner: old,
            flag,
            kind: OwnershipGuardKind::Overwrite,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Rearm {
            previous: old,
            replacement,
            place: rearm_place,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: replacement,
            recipe,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner: replacement,
            flag: replacement_flag,
            kind: OwnershipGuardKind::Overwrite,
        }),
    ])
}

#[test]
fn sequential_same_slot_rearm_is_exact() {
    let checked = checked_rearm_fixture(Place::Local(7), Place::Local(8));
    assert_eq!(validate_ownership_events(&checked), []);
}

#[test]
fn relocated_use_does_not_guess_an_ambiguous_definition_place() {
    use crate::model::{OwnerId, OwnershipEvent};

    let binding = BindingId(205);
    let first = OwnerId {
        binding,
        generation: 0,
    };
    let conflicting = OwnerId {
        binding,
        generation: 1,
    };
    let mut checked = checked_with_ownership_events(vec![]);
    checked.blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![
                MirStatement::Bind {
                    binding,
                    name: "ambiguous".to_owned(),
                    site: SiteId(1),
                    ty: ResolvedTy::String,
                },
                MirStatement::Use {
                    binding,
                    name: "ambiguous".to_owned(),
                    site: SiteId(2),
                    ty: ResolvedTy::String,
                    intent: IntentKind::Read,
                },
            ],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner: first,
                    place: Place::Local(1),
                    ty: ResolvedTy::String,
                }),
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner: conflicting,
                    place: Place::Local(2),
                    ty: ResolvedTy::String,
                }),
                Instr::Move {
                    dest: Place::Local(3),
                    src: Place::Local(1),
                },
                Instr::OwnershipEvent(OwnershipEvent::Relocate {
                    owner: first,
                    from: Place::Local(1),
                    to: Place::Local(3),
                }),
            ],
            terminator: Terminator::Goto { target: 1 },
        },
        BasicBlock {
            id: 1,
            statements: vec![MirStatement::Use {
                binding,
                name: "ambiguous".to_owned(),
                site: SiteId(3),
                ty: ResolvedTy::String,
                intent: IntentKind::Read,
            }],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];

    let findings = validate_ownership_events(&checked);
    assert!(findings.iter().any(|finding| matches!(
        finding,
        MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("minted while another generation")
    )));
    assert!(
        findings
            .iter()
            .all(|finding| !matches!(finding, MirCheck::UseAfterConsume { .. })),
        "conflicting definition places stay the ownership validator's ambiguity instead of receiving a guessed use-after-move: {findings:?}"
    );
}

#[test]
fn rearm_rejects_wrong_place_or_guard() {
    let wrong_place =
        validate_ownership_events(&checked_rearm_fixture(Place::Local(9), Place::Local(8)));
    assert!(wrong_place.iter().any(|finding| matches!(
        finding,
        MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("rearm source") && reason.contains("sole possibly-live")
    )));

    let wrong_guard =
        validate_ownership_events(&checked_rearm_fixture(Place::Local(7), Place::Local(9)));
    assert!(wrong_guard.iter().any(|finding| matches!(
        finding,
        MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("rearm") && reason.contains("guard")
    )));
}

#[test]
fn rearm_rejects_co_live_generation() {
    use crate::model::{OwnerId, OwnershipEvent, OwnershipGuardKind};

    let mut checked = checked_rearm_fixture(Place::Local(7), Place::Local(8));
    let other = OwnerId {
        binding: BindingId(178),
        generation: 2,
    };
    checked.blocks[0].instructions.splice(
        3..3,
        [
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: other,
                place: Place::Local(10),
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                owner: other,
                recipe: checked_test_string_recipe(),
            }),
            Instr::OwnershipEvent(OwnershipEvent::Guard {
                owner: other,
                flag: Place::Local(11),
                kind: OwnershipGuardKind::Overwrite,
            }),
        ],
    );
    let findings = validate_ownership_events(&checked);
    assert!(findings.iter().any(|finding| matches!(
        finding,
        MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("rearm source") && reason.contains("sole possibly-live")
    )));
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the branch fixture exposes both possible predecessor generations explicitly"
)]
fn rearm_rejects_branch_ambiguous_generation() {
    use crate::model::{OwnerId, OwnershipEvent, OwnershipGuardKind};

    let binding = BindingId(179);
    let old = OwnerId {
        binding,
        generation: 0,
    };
    let branch_owner = OwnerId {
        binding,
        generation: 1,
    };
    let replacement = OwnerId {
        binding,
        generation: 2,
    };
    let place = Place::Local(12);
    let flag = Place::Local(13);
    let recipe = checked_test_string_recipe();
    let blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner: old,
                    place,
                    ty: ResolvedTy::String,
                }),
                Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                    owner: old,
                    recipe: recipe.clone(),
                }),
                Instr::OwnershipEvent(OwnershipEvent::Guard {
                    owner: old,
                    flag,
                    kind: OwnershipGuardKind::Overwrite,
                }),
            ],
            terminator: Terminator::Branch {
                cond: Place::Local(14),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Reset {
                    previous: old,
                    replacement: branch_owner,
                    place,
                    ty: ResolvedTy::String,
                }),
                Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                    owner: branch_owner,
                    recipe: recipe.clone(),
                }),
                Instr::OwnershipEvent(OwnershipEvent::Guard {
                    owner: branch_owner,
                    flag,
                    kind: OwnershipGuardKind::Overwrite,
                }),
            ],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Rearm {
                    previous: branch_owner,
                    replacement,
                    place,
                    ty: ResolvedTy::String,
                }),
                Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                    owner: replacement,
                    recipe,
                }),
                Instr::OwnershipEvent(OwnershipEvent::Guard {
                    owner: replacement,
                    flag,
                    kind: OwnershipGuardKind::Overwrite,
                }),
            ],
            terminator: Terminator::Return,
        },
    ];
    let mut checked = checked_with_ownership_events(vec![]);
    checked.blocks = blocks;
    let findings = validate_ownership_events(&checked);
    assert!(findings.iter().any(|finding| matches!(
        finding,
        MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("rearm source") && reason.contains("sole possibly-live")
    )));
}

#[cfg(test)]
fn checked_join_rearm_fixture() -> CheckedMirFunction {
    use crate::model::{OwnerId, OwnershipEvent, OwnershipGuardKind};

    let binding = BindingId(180);
    let generation = |generation| OwnerId {
        binding,
        generation,
    };
    let (initial, branch, joined, rearmed) =
        (generation(0), generation(1), generation(2), generation(3));
    let place = Place::Local(15);
    let flag = Place::Local(16);
    let recipe = checked_test_string_recipe();
    let publish = |owner| {
        [
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                owner,
                recipe: recipe.clone(),
            }),
            Instr::OwnershipEvent(OwnershipEvent::Guard {
                owner,
                flag,
                kind: OwnershipGuardKind::Overwrite,
            }),
        ]
    };
    let mut entry_instructions = vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
        owner: initial,
        place,
        ty: ResolvedTy::String,
    })];
    entry_instructions.extend(publish(initial));
    let mut reset_instructions = vec![Instr::OwnershipEvent(OwnershipEvent::Reset {
        previous: initial,
        replacement: branch,
        place,
        ty: ResolvedTy::String,
    })];
    reset_instructions.extend(publish(branch));
    // Each predecessor witnesses the generation it carries into the join.
    let carry = |owner| {
        Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
            owner,
            place,
            target: 3,
        })
    };
    reset_instructions.push(carry(branch));
    let passthrough_instructions = vec![carry(initial)];
    let mut join_instructions = vec![Instr::OwnershipEvent(OwnershipEvent::Join {
        incoming: vec![initial, branch],
        replacement: joined,
        place,
        ty: ResolvedTy::String,
    })];
    join_instructions.extend(publish(joined));
    join_instructions.push(Instr::OwnershipEvent(OwnershipEvent::Rearm {
        previous: joined,
        replacement: rearmed,
        place,
        ty: ResolvedTy::String,
    }));
    join_instructions.extend(publish(rearmed));
    let mut checked = checked_with_ownership_events(vec![]);
    checked.blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: entry_instructions,
            terminator: Terminator::Branch {
                cond: Place::Local(17),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: reset_instructions,
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: passthrough_instructions,
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: join_instructions,
            terminator: Terminator::Return,
        },
    ];
    checked
}

#[test]
fn exact_join_then_rearm_lineage_is_valid() {
    assert_eq!(validate_ownership_events(&checked_join_rearm_fixture()), []);
}

#[test]
fn join_lineage_rejects_missing_incoming_and_ambiguous_owner() {
    use crate::model::{OwnerId, OwnershipEvent};

    let mut missing = checked_join_rearm_fixture();
    let join = missing.blocks[3]
        .instructions
        .iter_mut()
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Join { incoming, .. }) => Some(incoming),
            _ => None,
        })
        .expect("fixture contains a Join");
    join.pop();
    assert!(validate_ownership_events(&missing)
        .iter()
        .any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { reason, .. }
                if reason.contains("does not enumerate its exact possible incoming owners")
        )));

    let mut ambiguous = checked_join_rearm_fixture();
    let extra = OwnerId {
        binding: BindingId(180),
        generation: 4,
    };
    ambiguous.blocks[1]
        .instructions
        .push(Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: extra,
            place: Place::Local(15),
            ty: ResolvedTy::String,
        }));
    assert!(validate_ownership_events(&ambiguous)
        .iter()
        .any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { reason, .. }
                if reason.contains("does not enumerate its exact possible incoming owners")
                    || reason.contains("ambiguous exact incoming owners")
        )));
}

#[test]
fn join_lineage_rejects_its_successor_as_an_incoming_owner() {
    use crate::model::OwnershipEvent;
    let mut checked = checked_join_rearm_fixture();
    let (incoming, replacement) = checked.blocks[3]
        .instructions
        .iter_mut()
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Join {
                incoming,
                replacement,
                ..
            }) => Some((incoming, *replacement)),
            _ => None,
        })
        .expect("fixture contains a Join");
    incoming.push(replacement);
    assert!(validate_ownership_events(&checked)
        .iter()
        .any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { reason, .. }
                if reason.contains("not a non-empty set of distinct same-binding predecessors and successor")
        )));
}

#[test]
fn join_lineage_rejects_wrong_guard_or_place() {
    use crate::model::OwnershipEvent;

    let mut wrong_guard = checked_join_rearm_fixture();
    let changed_guard = wrong_guard.blocks[3]
        .instructions
        .iter_mut()
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Guard { owner, flag, .. })
                if owner.generation == 2 =>
            {
                Some(flag)
            }
            _ => None,
        })
        .expect("fixture publishes the Join guard");
    *changed_guard = Place::Local(18);
    assert!(validate_ownership_events(&wrong_guard)
        .iter()
        .any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { reason, .. }
                if reason.contains("does not preserve one exact place/type/guard/recipe")
        )));

    let mut wrong_place = checked_join_rearm_fixture();
    let join_place = wrong_place.blocks[3]
        .instructions
        .iter_mut()
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Join { place, .. }) => Some(place),
            _ => None,
        })
        .expect("fixture contains a Join");
    *join_place = Place::Local(19);
    assert!(validate_ownership_events(&wrong_place)
        .iter()
        .any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { reason, .. }
                if reason.contains("does not enumerate its exact possible incoming owners")
                    || reason.contains("wrong-place incoming path")
                    || reason.contains("does not preserve one exact place/type/guard/recipe")
        )));
}

#[test]
fn checked_owner_definition_rejects_multiple_drop_recipes() {
    let recipe = checked_test_string_recipe();
    let checked = checked_recipe_fixture(vec![recipe.clone(), recipe], vec![]);
    assert!(validate_ownership_events(&checked).iter().any(|finding| {
        matches!(finding, MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("exactly one destructor recipe, found 2"))
    }));
}

#[test]
fn checked_owner_definition_rejects_recipe_type_drift() {
    let mut recipe = checked_test_string_recipe();
    recipe.ty = ResolvedTy::Bytes;
    let checked = checked_recipe_fixture(vec![recipe], vec![]);
    assert!(validate_ownership_events(&checked).iter().any(|finding| {
        matches!(finding, MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("is defined as string") && reason.contains("names bytes"))
    }));
}

#[test]
fn checked_exit_rejects_cleanup_ritual_that_differs_from_recipe() {
    let checked = checked_recipe_fixture(
        vec![checked_test_string_recipe()],
        vec![ElabDrop {
            place: Place::Local(7),
            ty: ResolvedTy::String,
            drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
            kind: DropKind::Resource,
            guard: None,
        }],
    );
    assert!(validate_ownership_events(&checked).iter().any(|finding| {
        matches!(finding, MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("does not equal the definition-site recipe"))
    }));
}

/// Every string a user-facing obligation finding carries must be readable
/// without a compiler debugger: it names the kind of exit that leaks (ladder
/// §3.6 vocabulary) and never a `Place`, an `ExitPath` `Debug` payload, or an
/// owner generation. The engineer's view of the same fact is the
/// `HEW_DEBUG_CHECKED_FUNCTION` event dump.
#[cfg(test)]
fn assert_user_register(reason: &str) {
    for internal in ["Local(", "Return {", "Unwind {", "EnumVariant"] {
        assert!(
            !reason.contains(internal),
            "user-facing reason leaks `{internal}`: {reason}"
        );
    }
    assert!(
        !contains_owner_generation(reason),
        "user-facing reason leaks an owner generation: {reason}"
    );
}

/// The `OwnerId` `Display` form is `b<binding>#<generation>`. Match its shape
/// rather than any one fixture's binding id, so the negative control keeps
/// biting when a fixture renumbers.
#[cfg(test)]
fn contains_owner_generation(reason: &str) -> bool {
    let bytes = reason.as_bytes();
    bytes.iter().enumerate().any(|(start, byte)| {
        if *byte != b'b' {
            return false;
        }
        let mut index = start + 1;
        let digits_start = index;
        while bytes.get(index).is_some_and(u8::is_ascii_digit) {
            index += 1;
        }
        if index == digits_start || bytes.get(index) != Some(&b'#') {
            return false;
        }
        bytes.get(index + 1).is_some_and(u8::is_ascii_digit)
    })
}

#[test]
fn an_owner_generation_is_recognised_but_a_plain_word_is_not() {
    assert!(contains_owner_generation("owner b42#3 leaks"));
    assert!(!contains_owner_generation(
        "the return path omits its cleanup"
    ));
    assert!(!contains_owner_generation("bucket#3"));
    assert!(!contains_owner_generation("b42#x"));
}

/// Strip the definition-site `Bind`, leaving an owner the source cannot name.
/// Lowering-synthesized temps reach the verifier this way.
#[cfg(test)]
fn checked_recipe_fixture_without_binding(
    recipes: Vec<crate::model::OwnerDropRecipe>,
    drops: Vec<ElabDrop>,
) -> CheckedMirFunction {
    let mut checked = checked_recipe_fixture(recipes, drops);
    for block in &mut checked.blocks {
        block.statements.clear();
    }
    checked
}

#[test]
fn an_omitted_cleanup_over_an_unnamed_owner_is_an_internal_error() {
    let checked =
        checked_recipe_fixture_without_binding(vec![checked_test_string_recipe()], vec![]);
    let findings = validate_ownership_events(&checked);
    let [MirCheck::DischargeAuthorityDrift { name, reason, .. }] = findings.as_slice() else {
        panic!("expected the imbalance to route to the internal channel, got {findings:#?}");
    };
    assert_eq!(name, "ownership-obligation-subject");
    assert!(
        reason.contains("has no definition-site binding"),
        "the internal reason says why the subject is unnameable: {reason}"
    );
    assert!(
        !findings.iter().any(|finding| matches!(
            finding,
            MirCheck::ObligationUnderReleased { .. } | MirCheck::ObligationOverReleased { .. }
        )),
        "no user-register obligation names an owner the source never bound: {findings:#?}"
    );
}

#[test]
fn a_plan_entry_matching_no_live_owner_is_an_internal_error() {
    // A release of `Local(9)`, which the replayed event stream never mints.
    let checked = checked_recipe_fixture(
        vec![checked_test_string_recipe()],
        vec![ElabDrop {
            place: Place::Local(9),
            ty: ResolvedTy::String,
            drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
            kind: DropKind::Resource,
            guard: None,
        }],
    );
    let findings = validate_ownership_events(&checked);
    assert!(
        findings.iter().any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { name, reason, .. }
                if name == "ownership-obligation-subject"
                    && reason.contains("no longer holds there")
        )),
        "a plan/replay disagreement is a compiler defect, not a double free: {findings:#?}"
    );
    assert!(
        !findings
            .iter()
            .any(|finding| matches!(finding, MirCheck::ObligationOverReleased { .. })),
        "the stale plan entry must not be reported as the user's double free: {findings:#?}"
    );
}

#[test]
fn an_omitted_cleanup_names_the_exit_in_the_user_register() {
    let checked = checked_recipe_fixture(vec![checked_test_string_recipe()], vec![]);
    let findings = validate_ownership_events(&checked);
    let [MirCheck::ObligationUnderReleased { reason, .. }] = findings.as_slice() else {
        panic!("expected one under-released obligation, got {findings:#?}");
    };
    assert!(
        reason.contains("the return path"),
        "the leaking exit is named: {reason}"
    );
    assert_user_register(reason);
}

#[test]
fn a_duplicated_cleanup_names_the_exit_in_the_user_register() {
    let drop = ElabDrop {
        place: Place::Local(7),
        ty: ResolvedTy::String,
        drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::String,
        },
        guard: None,
    };
    let checked =
        checked_recipe_fixture(vec![checked_test_string_recipe()], vec![drop.clone(), drop]);
    let findings = validate_ownership_events(&checked);
    let Some(MirCheck::ObligationOverReleased { reason, .. }) = findings
        .iter()
        .find(|finding| matches!(finding, MirCheck::ObligationOverReleased { .. }))
    else {
        panic!("expected an over-released obligation, got {findings:#?}");
    };
    assert!(
        reason.contains("the return path"),
        "the double-releasing exit is named: {reason}"
    );
    assert_user_register(reason);
}

#[test]
fn generic_vec_definition_publishes_outer_buffer_recipe() {
    let owner = crate::model::OwnerId {
        binding: BindingId(178),
        generation: 0,
    };
    let ty = ResolvedTy::named_builtin(
        "Vec",
        BuiltinType::Vec,
        vec![ResolvedTy::TypeParam {
            name: "T".to_owned(),
        }],
    );
    let recipe = owner_definition_drop_recipe(&Builder::default(), owner, Place::Local(8), &ty, 0)
        .expect("a generic Vec still owns and must release its outer buffer");
    assert_eq!(
        recipe.kind,
        DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::VecPlain,
        }
    );
}

#[test]
fn unwired_vec_definition_publishes_no_unsafe_recipe() {
    let owner = crate::model::OwnerId {
        binding: BindingId(179),
        generation: 0,
    };
    let ty = ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::Bytes]);
    assert!(
        owner_definition_drop_recipe(
            &Builder::default(),
            owner,
            Place::Local(9),
            &ty,
            0,
        )
        .is_none(),
        "an unwired per-element ABI must fail sealing instead of publishing a wrong buffer-only destructor"
    );
}

#[test]
fn ownership_event_validator_rejects_reused_generation() {
    let owner = crate::model::OwnerId {
        binding: BindingId(7),
        generation: 0,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(1),
            ty: ResolvedTy::Bytes,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
            owner,
            from: Place::Local(1),
            to: Some(Place::MachineVariant {
                local: 2,
                variant_idx: 0,
                field_idx: 1,
            }),
            to_owner: None,
            to_ty: None,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
            owner,
            from: Place::Local(1),
            to: Some(Place::MachineVariant {
                local: 2,
                variant_idx: 0,
                field_idx: 2,
            }),
            to_owner: None,
            to_ty: None,
        }),
    ]);
    assert_eq!(validate_ownership_events(&checked).len(), 1);
}

#[test]
fn ownership_event_validator_rejects_stale_generation_after_overwrite() {
    let old = crate::model::OwnerId {
        binding: BindingId(9),
        generation: 0,
    };
    let new = crate::model::OwnerId {
        binding: BindingId(9),
        generation: 1,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner: old,
            place: Place::Local(3),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Reset {
            previous: old,
            replacement: new,
            place: Place::Local(3),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Release {
            owner: old,
            place: Place::Local(3),
        }),
    ]);
    assert_eq!(validate_ownership_events(&checked).len(), 1);
}

#[test]
fn ownership_event_validator_keeps_and_rejects_separated_stale_relocate() {
    let owner = crate::model::OwnerId {
        binding: BindingId(10),
        generation: 0,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(3),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Release {
            owner,
            place: Place::Local(3),
        }),
        Instr::ConstI64 {
            dest: Place::Local(9),
            value: 1,
        },
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Relocate {
            owner,
            from: Place::Local(3),
            to: Place::Local(4),
        }),
    ]);
    assert!(validate_ownership_events(&checked).iter().any(|finding| {
        matches!(finding, MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("relocated after its generation ended"))
    }));
}

#[test]
fn ownership_event_validator_rejects_transfer_from_stale_place() {
    let owner = crate::model::OwnerId {
        binding: BindingId(11),
        generation: 0,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(4),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
            owner,
            from: Place::Local(0),
            to: Some(Place::Local(5)),
            to_owner: None,
            to_ty: None,
        }),
    ]);
    assert_eq!(validate_ownership_events(&checked).len(), 1);
}

#[test]
fn ownership_event_validator_rejects_fabricated_from_even_when_to_matches_live_place() {
    let owner = crate::model::OwnerId {
        binding: BindingId(16),
        generation: 0,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(4),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
            owner,
            from: Place::Local(0),
            to: Some(Place::Local(4)),
            to_owner: None,
            to_ty: None,
        }),
    ]);
    assert_eq!(validate_ownership_events(&checked).len(), 1);
}

#[test]
fn ownership_event_validator_rejects_release_from_stale_place() {
    let owner = crate::model::OwnerId {
        binding: BindingId(12),
        generation: 0,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(6),
            ty: ResolvedTy::Bytes,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Release {
            owner,
            place: Place::Local(0),
        }),
    ]);
    assert_eq!(validate_ownership_events(&checked).len(), 1);
}

#[test]
fn ownership_event_validator_accepts_exact_place_transfer() {
    let first = crate::model::OwnerId {
        binding: BindingId(13),
        generation: 0,
    };
    let second = crate::model::OwnerId {
        binding: BindingId(14),
        generation: 0,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner: first,
            place: Place::Local(7),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
            owner: first,
            from: Place::Local(7),
            to: Some(Place::Local(8)),
            to_owner: Some(second),
            to_ty: Some(ResolvedTy::String),
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Release {
            owner: second,
            place: Place::Local(8),
        }),
    ]);
    assert!(validate_ownership_events(&checked).is_empty());
}

#[test]
fn physical_move_does_not_relocate_owner_without_explicit_event() {
    let owner = crate::model::OwnerId {
        binding: BindingId(140),
        generation: 0,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(1),
            ty: ResolvedTy::String,
        }),
        Instr::Move {
            dest: Place::Local(2),
            src: Place::Local(1),
        },
    ]);
    let exact_states = exact_owner_states(&checked.blocks);
    let exits = &exact_states.1;
    assert_eq!(
        exits
            .get(&ENTRY_BLOCK_ID)
            .and_then(|state| state.get(&owner)),
        Some(&Place::Local(1)),
        "a physical copy must not silently become ownership authority"
    );
}

#[test]
fn cleanup_block_projection_is_rebuilt_from_exact_panic_plan() {
    let exact = ElabDrop {
        place: Place::Local(4),
        ty: ResolvedTy::String,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::String,
        },
        guard: None,
    };
    let mut elaboration = ElaboratedMirFunction {
        name: "cleanup_projection".to_owned(),
        key: crate::model::MirCallableKey::for_test("cleanup_projection"),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![ElabBlock {
            id: 1,
            kind: BlockKind::Cleanup,
            drops: vec![],
            successor: None,
        }],
        drop_plans: vec![(
            ExitPath::Panic { block: 0 },
            DropPlan {
                drops: vec![exact.clone()],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    };
    synchronize_cleanup_blocks(&mut elaboration);
    assert_eq!(elaboration.blocks[0].drops, vec![exact]);
}

#[test]
fn checked_ownership_plan_replays_without_builder_state() {
    let binding = BindingId(15);
    let owner = crate::model::OwnerId {
        binding,
        generation: 0,
    };
    let mut checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(9),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::DropRecipe {
            owner,
            recipe: checked_test_string_recipe(),
        }),
    ]);
    checked.ownership_elaboration = Some(Box::new(ElaboratedMirFunction {
        name: checked.name.clone(),
        key: crate::model::MirCallableKey::for_test(&checked.name),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(
            ExitPath::Return {
                block: ENTRY_BLOCK_ID,
            },
            DropPlan {
                drops: vec![ElabDrop {
                    place: Place::Local(10),
                    ty: ResolvedTy::String,
                    drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                    kind: DropKind::CowHeap {
                        release: crate::ownership::CowHeapRelease::String,
                    },
                    guard: None,
                }],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    }));
    assert!(validate_ownership_events(&checked).iter().any(|finding| {
        matches!(finding, MirCheck::DischargeAuthorityDrift { reason, .. }
        if reason.contains(
            "releases a `string` that the replayed event stream no longer holds there"
        ))
    }));
}

#[test]
fn checked_ownership_plan_requires_explicit_guard_event() {
    let binding = BindingId(16);
    let owner = crate::model::OwnerId {
        binding,
        generation: 0,
    };
    let place = Place::Local(9);
    let flag = Place::Local(20);
    let make_checked = |publish_guard: bool| {
        let mut events = vec![
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
                owner,
                place,
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(crate::model::OwnershipEvent::DropRecipe {
                owner,
                recipe: checked_test_string_recipe(),
            }),
        ];
        if publish_guard {
            events.push(Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
                owner,
                flag,
                kind: crate::model::OwnershipGuardKind::AffineRelease,
            }));
        }
        let mut checked = checked_with_ownership_events(events);
        checked.ownership_elaboration = Some(Box::new(ElaboratedMirFunction {
            name: checked.name.clone(),
            key: crate::model::MirCallableKey::for_test(&checked.name),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans: vec![(
                ExitPath::Return {
                    block: ENTRY_BLOCK_ID,
                },
                DropPlan {
                    drops: vec![ElabDrop {
                        place,
                        ty: ResolvedTy::String,
                        drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                        kind: DropKind::CowHeap {
                            release: crate::ownership::CowHeapRelease::String,
                        },
                        guard: Some(crate::model::ElabDropGuard { owner, flag }),
                    }],
                },
            )],
            coroutine: None,
            lambda_captures: vec![],
        }));
        checked
    };

    assert_eq!(
        validate_ownership_events(&make_checked(false)).len(),
        1,
        "a physical cleanup flag without a Checked-MIR Guard event must fail closed"
    );
    assert!(
        validate_ownership_events(&make_checked(true)).is_empty(),
        "a matching OwnerId/generation Guard event must reproduce the cleanup plan without Builder state"
    );
}

#[test]
fn checked_ownership_plan_rejects_conflicting_guard_authorities() {
    let owner = crate::model::OwnerId {
        binding: BindingId(18),
        generation: 0,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(9),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
            owner,
            flag: Place::Local(20),
            kind: crate::model::OwnershipGuardKind::AffineRelease,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
            owner,
            flag: Place::Local(21),
            kind: crate::model::OwnershipGuardKind::Overwrite,
        }),
    ]);

    let findings = validate_ownership_events(&checked);
    assert_eq!(findings.len(), 1);
    assert!(matches!(
        &findings[0],
        MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("conflicting cleanup guards")
    ));
}

#[test]
fn checked_ownership_plan_rejects_stale_generation_guard() {
    let binding = BindingId(17);
    let old = crate::model::OwnerId {
        binding,
        generation: 0,
    };
    let replacement = crate::model::OwnerId {
        binding,
        generation: 1,
    };
    let flag = Place::Local(20);
    let mut checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner: old,
            place: Place::Local(9),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
            owner: old,
            flag,
            kind: crate::model::OwnershipGuardKind::ActorMessageCow,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Reset {
            previous: old,
            replacement,
            place: Place::Local(9),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::DropRecipe {
            owner: old,
            recipe: checked_test_string_recipe(),
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::DropRecipe {
            owner: replacement,
            recipe: checked_test_string_recipe(),
        }),
    ]);
    checked.ownership_elaboration = Some(Box::new(ElaboratedMirFunction {
        name: checked.name.clone(),
        key: crate::model::MirCallableKey::for_test(&checked.name),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(
            ExitPath::Return {
                block: ENTRY_BLOCK_ID,
            },
            DropPlan {
                drops: vec![ElabDrop {
                    place: Place::Local(9),
                    ty: ResolvedTy::String,
                    drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                    kind: DropKind::CowHeap {
                        release: crate::ownership::CowHeapRelease::String,
                    },
                    guard: Some(crate::model::ElabDropGuard { owner: old, flag }),
                }],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    }));
    assert!(validate_ownership_events(&checked).iter().any(|finding| {
        matches!(finding, MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains("guarded by") && reason.contains("live generation"))
    }));
}

#[cfg(test)]
fn goto_edge_fixture(source_instructions: Vec<Instr>) -> CheckedMirFunction {
    CheckedMirFunction {
        name: "goto_edge".to_owned(),
        key: crate::model::MirCallableKey::for_test("goto_edge"),
        return_ty: ResolvedTy::Unit,
        blocks: vec![
            BasicBlock {
                id: ENTRY_BLOCK_ID,
                statements: vec![],
                instructions: source_instructions,
                terminator: Terminator::Goto { target: 1 },
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ],
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: None,
    }
}

#[cfg(test)]
fn edge_carry_findings(checked: &CheckedMirFunction) -> Vec<String> {
    validate_ownership_events(checked)
        .into_iter()
        .filter_map(|finding| match finding {
            MirCheck::DischargeAuthorityDrift { name, reason, .. } if name == "edge-carry" => {
                Some(reason)
            }
            _ => None,
        })
        .collect()
}

#[test]
fn goto_plan_never_discharges_a_carried_owner() {
    use crate::model::{OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(44),
        generation: 0,
    };
    let place = Place::Local(7);
    let checked = goto_edge_fixture(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner,
            place,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner,
            recipe: checked_test_string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
            owner,
            place,
            target: 1,
        }),
    ]);
    let exact_states = exact_owner_states(&checked.blocks);
    let entries = &exact_states.0;
    let exits = &exact_states.1;
    let required = exact_required_owners_for_exit(
        &ExitPath::Goto {
            block: ENTRY_BLOCK_ID,
            target: 1,
        },
        &checked.blocks,
        entries,
        exits,
    );

    assert!(
        required.is_empty(),
        "replay carries the generation into the target"
    );
    assert_eq!(
        entries.get(&1).and_then(|state| state.get(&owner)),
        Some(&place),
        "the target's exact entry state sees the carried generation"
    );
    assert_eq!(edge_carry_findings(&checked), Vec::<String>::new());
}

#[test]
fn goto_with_live_owner_and_no_edge_carry_is_rejected_not_dropped() {
    use crate::model::{OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(45),
        generation: 0,
    };
    let place = Place::Local(2);
    let checked = goto_edge_fixture(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner,
            place,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner,
            recipe: checked_test_string_recipe(),
        }),
    ]);
    let exact_states = exact_owner_states(&checked.blocks);
    let entries = &exact_states.0;
    let exits = &exact_states.1;
    let required = exact_required_owners_for_exit(
        &ExitPath::Goto {
            block: ENTRY_BLOCK_ID,
            target: 1,
        },
        &checked.blocks,
        entries,
        exits,
    );

    assert!(
        required.is_empty(),
        "a Goto plan must not execute a drop for a generation the target still sees as live: {required:?}"
    );
    let findings = edge_carry_findings(&checked);
    assert_eq!(findings.len(), 1, "{findings:?}");
    assert!(
        findings[0].contains("no EdgeCarry preserves it"),
        "{findings:?}"
    );
}

#[test]
fn goto_edge_carry_naming_a_stale_generation_is_rejected() {
    use crate::model::{OwnerId, OwnershipEvent};

    let binding = BindingId(46);
    let old = OwnerId {
        binding,
        generation: 0,
    };
    let replacement = OwnerId {
        binding,
        generation: 1,
    };
    let place = Place::Local(3);
    let checked = goto_edge_fixture(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: old,
            place,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: old,
            recipe: checked_test_string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Reset {
            previous: old,
            replacement,
            place,
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: replacement,
            recipe: checked_test_string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
            owner: old,
            place,
            target: 1,
        }),
    ]);

    let findings = edge_carry_findings(&checked);
    assert_eq!(findings.len(), 2, "{findings:?}");
    assert!(
        findings
            .iter()
            .any(|reason| reason.contains("b46#1") && reason.contains("no EdgeCarry")),
        "the live replacement generation has no carry: {findings:?}"
    );
    assert!(
        findings
            .iter()
            .any(|reason| reason.contains("b46#0") && reason.contains("not live there")),
        "the stale carry names a retired generation: {findings:?}"
    );
}

#[test]
fn terminal_transfer_uses_the_live_adopted_generation() {
    use crate::model::{OwnerId, OwnershipEvent};

    let provisional = OwnerId {
        binding: BindingId(u32::MAX - 64),
        generation: 0,
    };
    let named = OwnerId {
        binding: BindingId(47),
        generation: 0,
    };
    let mut blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: provisional,
                place: Place::Local(0),
                ty: ResolvedTy::String,
            }),
            Instr::Move {
                dest: Place::Local(3),
                src: Place::Local(0),
            },
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: provisional,
                from: Place::Local(0),
                to: Some(Place::Local(3)),
                to_owner: Some(named),
                to_ty: Some(ResolvedTy::String),
            }),
            Instr::Move {
                dest: Place::Local(5),
                src: Place::Local(3),
            },
            Instr::OwnershipEvent(OwnershipEvent::Relocate {
                owner: named,
                from: Place::Local(3),
                to: Place::Local(5),
            }),
            // This models a carrier-neutralisation event emitted before the
            // provisional-to-named adoption was sealed.
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: provisional,
                from: Place::Local(5),
                to: Some(Place::Local(5)),
                to_owner: None,
                to_ty: None,
            }),
        ],
        terminator: Terminator::Return,
    }];

    super::canonicalize_terminal_transfer_owner_ids(&mut blocks);

    assert!(matches!(
        blocks[0].instructions.last(),
        Some(Instr::OwnershipEvent(OwnershipEvent::Transfer { owner, .. }))
            if *owner == named
    ));
    let exact_states = exact_owner_states(&blocks);
    let exits = &exact_states.1;
    assert!(
        exits.get(&ENTRY_BLOCK_ID).is_some_and(HashMap::is_empty),
        "the terminal carrier consume must end the adopted generation"
    );
}

#[test]
fn terminal_transfer_preserves_an_already_correct_named_generation() {
    use crate::model::{OwnerId, OwnershipEvent};

    let named = OwnerId {
        binding: BindingId(48),
        generation: 0,
    };
    let place = Place::Local(5);
    let mut blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: named,
                place,
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: named,
                from: place,
                to: None,
                to_owner: None,
                to_ty: None,
            }),
        ],
        terminator: Terminator::Return,
    }];

    super::canonicalize_terminal_transfer_owner_ids(&mut blocks);

    assert!(matches!(
        blocks[0].instructions.last(),
        Some(Instr::OwnershipEvent(OwnershipEvent::Transfer { owner, .. }))
            if *owner == named
    ));
    let exact_states = exact_owner_states(&blocks);
    let exits = &exact_states.1;
    assert!(exits.get(&ENTRY_BLOCK_ID).is_some_and(HashMap::is_empty));
}

#[test]
fn one_neutralize_keeps_only_the_relocated_terminal_transfer() {
    use crate::model::{NeutralizeAuthority, OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(49),
        generation: 0,
    };
    let source = Place::Local(2);
    let relocated = Place::Local(16);
    let block = BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::Move {
                dest: relocated,
                src: source,
            },
            Instr::NeutralizePayloadSlot {
                place: source,
                transferee: Some(relocated),
                authority: NeutralizeAuthority::SendTransferLastUse,
            },
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: relocated,
                to: Some(relocated),
                to_owner: None,
                to_ty: None,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: source,
                to: Some(relocated),
                to_owner: None,
                to_ty: None,
            }),
        ],
        terminator: Terminator::Return,
    };

    assert_eq!(
        super::duplicate_terminal_neutralize_transfer(&block, 3),
        Some(3),
        "the source-slot publication duplicates the relocated owner consume"
    );
}

#[test]
fn adopted_successor_is_not_terminally_consumed_by_the_same_neutralize() {
    use crate::model::{NeutralizeAuthority, OwnerId, OwnershipEvent};

    let provisional = OwnerId {
        binding: BindingId(u32::MAX - 65),
        generation: 0,
    };
    let named = OwnerId {
        binding: BindingId(55),
        generation: 0,
    };
    let source = Place::Local(1);
    let destination = Place::Local(8);
    let block = BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: provisional,
                from: source,
                to: Some(destination),
                to_owner: Some(named),
                to_ty: Some(ResolvedTy::String),
            }),
            Instr::NeutralizePayloadSlot {
                place: source,
                transferee: Some(destination),
                authority: NeutralizeAuthority::WholeCarrierConsume,
            },
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: named,
                from: destination,
                to: Some(destination),
                to_owner: None,
                to_ty: None,
            }),
        ],
        terminator: Terminator::Return,
    };

    assert!(super::terminal_transfer_duplicates_adopted_successor(
        &block, 2
    ));
}

#[test]
fn adopted_successor_replaces_same_point_legacy_relocation_only() {
    use crate::model::{NeutralizeAuthority, OwnerId, OwnershipEvent, OwnershipGuardKind};

    let predecessor = OwnerId {
        binding: BindingId(56),
        generation: 0,
    };
    let successor = OwnerId {
        binding: BindingId(57),
        generation: 0,
    };
    let source = Place::Local(2);
    let destination = Place::Local(8);
    let physical = vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: predecessor,
            place: source,
            ty: ResolvedTy::String,
        }),
        Instr::Move {
            dest: destination,
            src: source,
        },
        Instr::NeutralizePayloadSlot {
            place: source,
            transferee: Some(destination),
            authority: NeutralizeAuthority::DivergentSelectionTransfer,
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: predecessor,
            from: source,
            to: Some(destination),
            to_owner: Some(successor),
            to_ty: Some(ResolvedTy::String),
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: successor,
            recipe: checked_test_string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner: successor,
            flag: Place::Local(9),
            kind: OwnershipGuardKind::Collection,
        }),
        Instr::NeutralizePayloadSlot {
            place: source,
            transferee: Some(destination),
            authority: NeutralizeAuthority::WholeCarrierConsume,
        },
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: predecessor,
            from: source,
            to: destination,
        }),
    ];
    let mut blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: physical.clone(),
        terminator: Terminator::Return,
    }];

    assert!(super::relocation_duplicates_prior_adoption(&blocks[0], 7));
    assert_eq!(
        super::duplicate_neutralize_before_relocation(&blocks[0], 7),
        Some(6)
    );
    super::deduplicate_ownership_spines(&mut blocks, &mut Builder::default());
    assert_eq!(
        blocks[0]
            .instructions
            .iter()
            .filter(|instruction| matches!(instruction, Instr::NeutralizePayloadSlot { .. }))
            .count(),
        1
    );
    assert!(!blocks[0].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, .. }) if *owner == predecessor
    )));
    let exact_states = exact_owner_states(&blocks);
    let exits = &exact_states.1;
    assert_eq!(exits[&ENTRY_BLOCK_ID].get(&successor), Some(&destination));

    let mut separated = BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: physical,
        terminator: Terminator::Return,
    };
    separated.instructions.insert(
        6,
        Instr::ConstI64 {
            dest: Place::Local(10),
            value: 1,
        },
    );
    assert!(
        !super::relocation_duplicates_prior_adoption(&separated, 8),
        "an executable instruction separates ownership program points"
    );
}

#[cfg(test)]
fn nested_aggregate_rewrap_fixture(
    relocation_generation: u32,
) -> (Vec<BasicBlock>, crate::model::OwnerId) {
    use crate::model::{OwnerId, OwnershipEvent};

    let live = OwnerId {
        binding: BindingId(58),
        generation: 7,
    };
    let stale = OwnerId {
        binding: live.binding,
        generation: 6,
    };
    let relocation = OwnerId {
        binding: live.binding,
        generation: relocation_generation,
    };
    let aggregate_owner = OwnerId {
        binding: BindingId(u32::MAX - 96),
        generation: 0,
    };
    let field = Place::MachineVariant {
        local: 12,
        variant_idx: 0,
        field_idx: 0,
    };
    let aggregate = Place::Local(12);
    let blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: live,
                place: Place::Local(5),
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Relocate {
                owner: live,
                from: Place::Local(5),
                to: field,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: live,
                from: field,
                to: Some(field),
                to_owner: None,
                to_ty: None,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Relocate {
                owner: relocation,
                from: Place::Local(5),
                to: field,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: stale,
                from: field,
                to: Some(aggregate),
                to_owner: None,
                to_ty: None,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: aggregate_owner,
                place: aggregate,
                ty: ResolvedTy::String,
            }),
        ],
        terminator: Terminator::Return,
    }];
    (blocks, relocation)
}

#[test]
fn nested_aggregate_rewrap_keeps_one_terminal_child_handoff() {
    use crate::model::OwnershipEvent;

    let (mut blocks, _) = nested_aggregate_rewrap_fixture(0);

    assert!(super::historical_relocation_duplicates_exact_handoff(
        &blocks[0],
        3,
        &HashMap::new(),
    ));
    assert!(super::terminal_transfer_rewraps_ended_aggregate_member(
        &blocks[0],
        4,
        &HashMap::new(),
    ));
    super::deduplicate_ownership_spines(&mut blocks, &mut Builder::default());
    assert_eq!(
        blocks[0]
            .instructions
            .iter()
            .filter(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Transfer { .. })
            ))
            .count(),
        1
    );
    assert!(validate_ownership_events(&checked_with_ownership_events(
        blocks.remove(0).instructions
    ))
    .is_empty());
}

#[test]
fn future_generation_aggregate_relocation_remains_a_hard_error() {
    use crate::model::OwnershipEvent;

    let (mut blocks, future) = nested_aggregate_rewrap_fixture(8);
    assert!(
        !super::historical_relocation_duplicates_exact_handoff(&blocks[0], 3, &HashMap::new(),),
        "a future generation is not historical residue"
    );
    super::deduplicate_ownership_spines(&mut blocks, &mut Builder::default());
    assert!(blocks[0].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, .. }) if *owner == future
    )));
    let future_findings = validate_ownership_events(&checked_with_ownership_events(
        blocks.remove(0).instructions,
    ));
    assert!(future_findings.iter().any(|finding| matches!(
        finding,
        MirCheck::DischargeAuthorityDrift { reason, .. }
            if reason.contains(&format!("owner {future} is relocated after its generation ended"))
    )), "a fabricated future-generation relocation must remain a hard error; got {future_findings:?}");
}

#[test]
fn co_live_older_generation_relocation_is_not_deduplicated() {
    use crate::model::OwnershipEvent;

    let (mut blocks, older) = nested_aggregate_rewrap_fixture(6);
    blocks[0].instructions.insert(
        1,
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: older,
            place: Place::Local(5),
            ty: ResolvedTy::String,
        }),
    );
    assert!(
        !super::historical_relocation_duplicates_exact_handoff(&blocks[0], 4, &HashMap::new(),),
        "an older generation that is still live is not historical residue"
    );
    super::deduplicate_ownership_spines(&mut blocks, &mut Builder::default());
    assert!(blocks[0].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, .. }) if *owner == older
    )));
    let findings = validate_ownership_events(&checked_with_ownership_events(
        blocks.remove(0).instructions,
    ));
    assert!(
        findings.iter().any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { reason, .. }
                if reason.contains("another generation of that binding is already live")
        )),
        "co-live generations must remain a hard error; got {findings:?}"
    );
}

#[test]
fn mint_rejects_prior_generation_live_on_only_one_join_predecessor() {
    use crate::model::{OwnerId, OwnershipEvent};

    let prior = OwnerId {
        binding: BindingId(257),
        generation: 0,
    };
    let replacement = OwnerId {
        binding: prior.binding,
        generation: 1,
    };
    let place = Place::Local(7);
    let mut checked = checked_with_ownership_events(vec![]);
    checked.blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: prior,
                place,
                ty: ResolvedTy::String,
            })],
            terminator: Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Release {
                owner: prior,
                place,
            })],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: replacement,
                place,
                ty: ResolvedTy::String,
            })],
            terminator: Terminator::Return,
        },
    ];

    let findings = validate_ownership_events(&checked);
    assert!(
        findings.iter().any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { block: 3, reason, .. }
                if reason.contains("another generation of that binding is already live")
        )),
        "a generation live on only one predecessor must still make the join mint invalid; got {findings:?}"
    );
}

#[test]
fn unrelated_stale_terminal_transfer_is_not_hidden_as_an_aggregate_rewrap() {
    use crate::model::{OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(59),
        generation: 0,
    };
    let field = Place::MachineVariant {
        local: 13,
        variant_idx: 0,
        field_idx: 0,
    };
    let block = BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner,
                place: field,
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: field,
                to: Some(field),
                to_owner: None,
                to_ty: None,
            }),
            Instr::ConstI64 {
                dest: Place::Local(99),
                value: 0,
            },
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: field,
                to: Some(Place::Local(13)),
                to_owner: None,
                to_ty: None,
            }),
        ],
        terminator: Terminator::Return,
    };

    assert!(!super::terminal_transfer_rewraps_ended_aggregate_member(
        &block,
        3,
        &HashMap::new(),
    ));
    assert_eq!(
        validate_ownership_events(&checked_with_ownership_events(block.instructions)).len(),
        1,
        "a separated reused-generation transfer must remain a hard error"
    );
}

#[test]
fn terminal_consume_occurs_after_the_physical_carrier_move() {
    use crate::model::{NeutralizeAuthority, OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(57),
        generation: 0,
    };
    let stale = OwnerId {
        binding: BindingId(u32::MAX - 66),
        generation: 0,
    };
    let source = Place::Local(4);
    let destination = Place::Local(17);
    let block = BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: source,
                to: None,
                to_owner: None,
                to_ty: None,
            }),
            Instr::Move {
                dest: destination,
                src: source,
            },
            Instr::NeutralizePayloadSlot {
                place: source,
                transferee: Some(destination),
                authority: NeutralizeAuthority::SendTransferLastUse,
            },
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: stale,
                from: destination,
                to: Some(destination),
                to_owner: None,
                to_ty: None,
            }),
        ],
        terminator: Terminator::Return,
    };

    assert!(super::terminal_transfer_precedes_its_physical_carrier_move(
        &block, 0
    ));
}

#[test]
fn pre_call_relocation_suppresses_same_point_terminal_consume() {
    use crate::model::{OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(60),
        generation: 2,
    };
    let source = Place::Local(1);
    let carrier = Place::Local(22);
    let mut blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner,
                place: source,
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Relocate {
                owner,
                from: source,
                to: carrier,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: source,
                to: Some(carrier),
                to_owner: None,
                to_ty: None,
            }),
        ],
        terminator: Terminator::Return,
    }];

    assert!(super::terminal_transfer_duplicates_prior_relocation(
        &blocks[0], 2
    ));
    super::deduplicate_ownership_spines(&mut blocks, &mut Builder::default());
    assert!(matches!(
        blocks[0].instructions.last(),
        Some(Instr::OwnershipEvent(OwnershipEvent::Relocate { .. }))
    ));
    let exact_states = exact_owner_states(&blocks);
    let exits = &exact_states.1;
    assert_eq!(exits[&ENTRY_BLOCK_ID].get(&owner), Some(&carrier));
}

#[test]
fn dyn_coercion_has_one_post_coercion_terminal_authority() {
    use crate::model::{NeutralizeAuthority, OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(58),
        generation: 0,
    };
    let source = Place::Local(4);
    let destination = Place::Local(18);
    let block = BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: source,
                to: None,
                to_owner: None,
                to_ty: None,
            }),
            Instr::CoerceToDynTrait {
                value: source,
                dest: destination,
                trait_name: "Display".to_string(),
                concrete_type: ResolvedTy::String,
                method_table: vec![],
                vtable_entries: vec![],
            },
            Instr::NeutralizePayloadSlot {
                place: source,
                transferee: Some(destination),
                authority: NeutralizeAuthority::WholeCarrierConsume,
            },
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: source,
                to: Some(destination),
                to_owner: None,
                to_ty: None,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: source,
                to: None,
                to_owner: None,
                to_ty: None,
            }),
        ],
        terminator: Terminator::Return,
    };

    assert!(super::terminal_transfer_precedes_dyn_coercion(&block, 0));
    assert!(super::duplicate_dyn_coercion_terminal_transfer(&block, 4));

    let mut separated = block;
    separated.instructions.insert(
        4,
        Instr::ConstI64 {
            dest: Place::Local(19),
            value: 1,
        },
    );
    assert!(
        !super::duplicate_dyn_coercion_terminal_transfer(&separated, 5),
        "a distinct executable program point must remain validator-visible"
    );
}

#[test]
fn occupied_place_mint_is_an_explicit_owner_adoption() {
    use crate::model::{OwnerId, OwnershipEvent};

    let predecessor = OwnerId {
        binding: BindingId(59),
        generation: 0,
    };
    let provisional = OwnerId {
        binding: BindingId(u32::MAX - 67),
        generation: 0,
    };
    let place = Place::Local(21);
    let mut blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: predecessor,
                place,
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: provisional,
                place,
                ty: ResolvedTy::String,
            }),
            Instr::ConstI64 {
                dest: Place::Local(22),
                value: 0,
            },
        ],
        terminator: Terminator::Return,
    }];

    super::canonicalize_preminted_move_adoptions(&mut blocks);

    assert!(matches!(
        blocks[0].instructions.get(1),
        Some(Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner,
            from,
            to: Some(destination),
            to_owner: Some(successor),
            ..
        })) if *owner == predecessor
            && *from == place
            && *destination == place
            && *successor == provisional
    ));
    let exact_states = exact_owner_states(&blocks);
    let exits = &exact_states.1;
    assert_eq!(
        exits[&ENTRY_BLOCK_ID],
        HashMap::from([(provisional, place)]),
        "one physical carrier must have one exact owner generation"
    );
}

#[test]
fn separated_terminal_transfers_remain_validator_visible() {
    use crate::model::{NeutralizeAuthority, OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(50),
        generation: 0,
    };
    let source = Place::Local(2);
    let relocated = Place::Local(16);
    let block = BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::NeutralizePayloadSlot {
                place: source,
                transferee: Some(relocated),
                authority: NeutralizeAuthority::SendTransferLastUse,
            },
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: relocated,
                to: Some(relocated),
                to_owner: None,
                to_ty: None,
            }),
            Instr::ConstI64 {
                dest: Place::Local(20),
                value: 1,
            },
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: source,
                to: Some(relocated),
                to_owner: None,
                to_ty: None,
            }),
        ],
        terminator: Terminator::Return,
    };

    assert_eq!(
        super::duplicate_terminal_neutralize_transfer(&block, 3),
        None,
        "an executable program point prevents ownership-spine deduplication"
    );
}

#[test]
fn scope_exit_owner_set_is_the_adjacent_cleanup_authority() {
    use crate::model::{OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(56),
        generation: 0,
    };
    let cleanup = |owners| BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::Drop {
                place: Place::Local(9),
                ty: ResolvedTy::String,
                drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
            },
            Instr::OwnershipEvent(OwnershipEvent::Release {
                owner,
                place: Place::Local(9),
            }),
            Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
                scopes: vec![hew_hir::ScopeId(3)],
                owners,
                carry_places: vec![],
                carried: vec![],
            }),
        ],
        terminator: Terminator::Return,
    };

    let unclaimed = cleanup(vec![]);
    assert_eq!(
        super::adjacent_scope_release_owners(
            &unclaimed,
            2,
            &HashSet::from([hew_hir::ScopeId(3)]),
            &HashMap::from([(owner.binding, hew_hir::ScopeId(3))]),
            &HashSet::new(),
        ),
        vec![owner],
        "a physical release at the same lexical program point must be adopted by ScopeExit"
    );
    assert_eq!(
        super::unowned_scope_exit_cleanup_pair(&unclaimed, 2),
        Some((0, 1))
    );
    assert_eq!(
        super::unowned_scope_exit_cleanup_pair(&cleanup(vec![owner]), 2),
        None,
        "a cleanup explicitly claimed by ScopeExit must remain"
    );
}

#[cfg(test)]
fn conditional_scope_exit_fixture(
    guard_kind: Option<crate::model::OwnershipGuardKind>,
    recipe: crate::model::OwnerDropRecipe,
) -> (Vec<BasicBlock>, super::Builder) {
    use crate::model::{OwnerId, OwnershipEvent};

    let binding = BindingId(63);
    let owner = OwnerId {
        binding,
        generation: 0,
    };
    let replacement = OwnerId {
        binding,
        generation: 1,
    };
    let place = Place::Local(1);
    let flag = Place::Local(2);
    let ty = recipe.ty.clone();
    let mut definition = vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner,
            place,
            ty: ty.clone(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner,
            recipe: recipe.clone(),
        }),
    ];
    if let Some(kind) = guard_kind {
        definition.push(Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner,
            flag,
            kind,
        }));
    }
    let blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: definition,
            terminator: Terminator::Branch {
                cond: Place::Local(3),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: flag,
                    value: 1,
                },
                Instr::OwnershipEvent(OwnershipEvent::Release { owner, place }),
            ],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
                scopes: vec![hew_hir::ScopeId(7)],
                owners: vec![],
                carry_places: vec![],
                carried: vec![],
            })],
            terminator: Terminator::Goto { target: 4 },
        },
        BasicBlock {
            id: 4,
            statements: vec![],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner: replacement,
                    place,
                    ty,
                }),
                Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                    owner: replacement,
                    recipe,
                }),
            ],
            terminator: Terminator::Return,
        },
    ];
    let mut builder = super::Builder {
        locals: vec![ResolvedTy::I64; 8],
        next_block_id: 5,
        ..super::Builder::default()
    };
    builder.type_classes.insert(
        "Buf".to_owned(),
        (hew_hir::ResourceMarker::Resource, Some("close".to_owned())),
    );
    builder.binding_scope.insert(binding, hew_hir::ScopeId(7));
    (blocks, builder)
}

#[test]
fn conditional_scope_exit_materializes_guarded_physical_and_logical_release() {
    use crate::model::{OwnerDropRecipe, OwnershipEvent, OwnershipGuardKind};

    let (mut blocks, mut builder) = conditional_scope_exit_fixture(
        Some(OwnershipGuardKind::AffineRelease),
        OwnerDropRecipe {
            ty: ResolvedTy::named_user("Buf", vec![]),
            drop_fn: None,
            kind: DropKind::RecordInPlace,
            declaration_order: 0,
        },
    );
    super::materialize_conditional_scope_exit_releases(&mut blocks, &mut builder);
    super::materialize_explicit_scope_exits(&mut blocks, &mut builder);

    let owner = crate::model::OwnerId {
        binding: BindingId(63),
        generation: 0,
    };
    let scope_block = blocks.iter().find(|block| block.id == 3).unwrap();
    assert!(matches!(
        scope_block.instructions.as_slice(),
        [
            Instr::OwnershipEvent(OwnershipEvent::GuardedRelease {
                owner: released,
                place: Place::Local(1),
                flag: Place::Local(2),
            }),
            Instr::OwnershipEvent(OwnershipEvent::ScopeExit { owners, .. }),
        ] if *released == owner && owners == &vec![owner]
    ));
    assert!(blocks.iter().any(|block| {
        matches!(
            block.instructions.as_slice(),
            [
                Instr::Drop {
                    place: Place::Local(1),
                    drop_fn: Some(crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::Record,
                    )),
                    ..
                },
                Instr::ConstI64 {
                    dest: Place::Local(2),
                    value: 1,
                },
            ]
        )
    }));
    let maybe_states = maybe_owner_states(&blocks);
    let maybe_entries = &maybe_states.0;
    assert!(
        !maybe_entries
            .get(&4)
            .is_some_and(|state| state.iter().any(|(candidate, _)| *candidate == owner)),
        "the explicit GuardedRelease must retire the conditional generation before reinitialization"
    );
}

#[test]
fn conditional_scope_exit_preserves_scalar_resource_close_recipe() {
    let (mut blocks, mut builder) = conditional_scope_exit_fixture(
        Some(crate::model::OwnershipGuardKind::AffineRelease),
        crate::model::OwnerDropRecipe {
            ty: ResolvedTy::named_user("Token", vec![]),
            drop_fn: Some(crate::model::DropFnSpec::UserClose(
                "Token::close".to_owned(),
            )),
            kind: DropKind::Resource,
            declaration_order: 0,
        },
    );
    builder.type_classes.insert(
        "Token".to_owned(),
        (hew_hir::ResourceMarker::Resource, Some("close".to_owned())),
    );
    super::materialize_conditional_scope_exit_releases(&mut blocks, &mut builder);

    assert!(blocks.iter().any(|block| {
        matches!(
            block.instructions.as_slice(),
            [
                Instr::Drop {
                    place: Place::Local(1),
                    drop_fn: Some(crate::model::DropFnSpec::UserClose(symbol)),
                    ..
                },
                Instr::ConstI64 {
                    dest: Place::Local(2),
                    value: 1,
                },
            ] if symbol == "Token::close"
        )
    }));
}

#[test]
fn conditional_scope_exit_without_published_guard_remains_fail_closed() {
    let (mut blocks, mut builder) = conditional_scope_exit_fixture(
        None,
        crate::model::OwnerDropRecipe {
            ty: ResolvedTy::named_user("Buf", vec![]),
            drop_fn: None,
            kind: DropKind::RecordInPlace,
            declaration_order: 0,
        },
    );
    let original_len = blocks.len();
    super::materialize_conditional_scope_exit_releases(&mut blocks, &mut builder);

    assert_eq!(blocks.len(), original_len);
    let old = crate::model::OwnerId {
        binding: BindingId(63),
        generation: 0,
    };
    let maybe_states = maybe_owner_states(&blocks);
    let maybe_entries = &maybe_states.0;
    assert!(
        maybe_entries
            .get(&4)
            .is_some_and(|state| state.iter().any(|(candidate, _)| *candidate == old)),
        "missing guard authority must not silently end the conditionally-live generation"
    );
}

#[test]
fn conditional_scope_exit_rejects_non_affine_guard_family() {
    let (mut blocks, mut builder) = conditional_scope_exit_fixture(
        Some(crate::model::OwnershipGuardKind::Overwrite),
        crate::model::OwnerDropRecipe {
            ty: ResolvedTy::named_user("Buf", vec![]),
            drop_fn: None,
            kind: DropKind::RecordInPlace,
            declaration_order: 0,
        },
    );
    let original_len = blocks.len();
    super::materialize_conditional_scope_exit_releases(&mut blocks, &mut builder);
    assert_eq!(
        blocks.len(),
        original_len,
        "non-affine flags cannot acquire affine scope-cleanup semantics"
    );
}

#[test]
fn conditional_scope_exit_rejects_kind_only_non_resource_recipe() {
    let (mut blocks, mut builder) = conditional_scope_exit_fixture(
        Some(crate::model::OwnershipGuardKind::AffineRelease),
        crate::model::OwnerDropRecipe {
            ty: ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64]),
            drop_fn: None,
            kind: DropKind::CowHeap {
                release: crate::ownership::CowHeapRelease::VecPlain,
            },
            declaration_order: 0,
        },
    );
    let original_len = blocks.len();
    super::materialize_conditional_scope_exit_releases(&mut blocks, &mut builder);
    assert_eq!(
        blocks.len(),
        original_len,
        "a kind-only Vec recipe cannot be logically released without a supported physical ritual"
    );
}

#[test]
fn missing_recipe_diagnostics_follow_source_binding_order() {
    use crate::model::{OwnerId, OwnershipEvent};

    let later_owner = OwnerId {
        binding: BindingId(81),
        generation: 0,
    };
    let earlier_owner = OwnerId {
        binding: BindingId(82),
        generation: 0,
    };
    let blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![
            MirStatement::Bind {
                binding: later_owner.binding,
                name: "later".to_owned(),
                site: SiteId(9),
                ty: ResolvedTy::String,
            },
            MirStatement::Bind {
                binding: earlier_owner.binding,
                name: "earlier".to_owned(),
                site: SiteId(3),
                ty: ResolvedTy::String,
            },
        ],
        // Deliberately mint in the opposite order from the source anchors.
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: later_owner,
                place: Place::Local(1),
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: earlier_owner,
                place: Place::Local(2),
                ty: ResolvedTy::String,
            }),
        ],
        terminator: Terminator::Return,
    }];
    let checked = CheckedMirFunction {
        name: "source_order".to_owned(),
        key: crate::model::MirCallableKey::for_test("source_order"),
        return_ty: ResolvedTy::Unit,
        blocks,
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: Some(Box::new(ElaboratedMirFunction {
            name: "source_order".to_owned(),
            key: crate::model::MirCallableKey::for_test("source_order"),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans: vec![(
                ExitPath::Return {
                    block: ENTRY_BLOCK_ID,
                },
                DropPlan::default(),
            )],
            coroutine: None,
            lambda_captures: vec![],
        })),
    };

    // Neither owner publishes a recipe: the definition-site findings must be
    // ordered by the binding's source site (3 before 9), not by mint order.
    let reasons = validate_ownership_events(&checked)
        .into_iter()
        .filter_map(|finding| match finding {
            MirCheck::DischargeAuthorityDrift { reason, .. }
                if reason.contains("no definition-site destructor recipe") =>
            {
                Some(reason)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        reasons,
        [
            format!("owner {earlier_owner} has no definition-site destructor recipe"),
            format!("owner {later_owner} has no definition-site destructor recipe"),
        ]
    );
}

#[test]
fn final_elaboration_preserves_moved_record_field_and_sibling_cleanup() {
    let source = r"
type Pair {
    moved: Vec<i64>;
    sibling: Vec<i64>;
}

fn main() {
    let pair = Pair {
        moved: [1],
        sibling: [2],
    };
    let moved = pair.moved;
}
";
    let module = crate::return_provenance::tests::lower_source(source);
    let pipeline = crate::lower_hir_module(&module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "fixture must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );

    let checked = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("checked main");
    let sibling_drops = checked
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter(|instruction| {
            matches!(
                instruction,
                Instr::RecordFieldDrop {
                    field_offset: FieldOffset(1),
                    ty: ResolvedTy::Named {
                        builtin: Some(hew_types::BuiltinType::Vec),
                        ..
                    },
                    ..
                }
            )
        })
        .count();
    assert_eq!(
        sibling_drops, 1,
        "the remaining owned sibling must have one field-addressed release"
    );

    let elaborated = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("elaborated main");
    let terminal_drops = elaborated
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .collect::<Vec<_>>();
    assert_eq!(
        terminal_drops
            .iter()
            .filter(|drop| {
                matches!(
                    drop.ty,
                    ResolvedTy::Named {
                        builtin: Some(hew_types::BuiltinType::Vec),
                        ..
                    }
                )
            })
            .count(),
        1,
        "the extracted Vec owner must have exactly one terminal destructor"
    );
    assert!(
        terminal_drops
            .iter()
            .all(|drop| drop.kind != DropKind::RecordInPlace),
        "final recipe reconstruction must not revive the transferred parent record"
    );
}

#[test]
fn exact_overwrite_releases_old_generation_before_store() {
    use crate::model::{NeutralizeAuthority, OwnerDropRecipe, OwnerId, OwnershipEvent};

    let binding = BindingId(61);
    let old = OwnerId {
        binding,
        generation: 0,
    };
    let source = OwnerId {
        binding: BindingId(u32::MAX - 68),
        generation: 0,
    };
    let replacement = OwnerId {
        binding,
        generation: 1,
    };
    let destination = Place::Local(24);
    let source_place = Place::Local(25);
    let recipe = OwnerDropRecipe {
        ty: ResolvedTy::String,
        drop_fn: None,
        kind: crate::model::DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::String,
        },
        declaration_order: 0,
    };
    let mut blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: old,
                place: destination,
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: source,
                place: source_place,
                ty: ResolvedTy::String,
            }),
            Instr::Move {
                dest: destination,
                src: source_place,
            },
            Instr::NeutralizePayloadSlot {
                place: source_place,
                transferee: Some(destination),
                authority: NeutralizeAuthority::WholeCarrierConsume,
            },
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: source,
                from: source_place,
                to: Some(destination),
                to_owner: Some(replacement),
                to_ty: Some(ResolvedTy::String),
            }),
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                owner: old,
                recipe: recipe.clone(),
            }),
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                owner: replacement,
                recipe,
            }),
        ],
        terminator: Terminator::Return,
    }];

    materialize_exact_overwrite_releases(&mut blocks, None);

    assert!(matches!(
        blocks[0].instructions.get(2),
        Some(Instr::Drop {
            place,
            drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
            ..
        }) if *place == destination
    ));
    assert!(matches!(
        blocks[0].instructions.get(3),
        Some(Instr::OwnershipEvent(OwnershipEvent::Release { owner, place }))
            if *owner == old && *place == destination
    ));
    let exact_states = exact_owner_states(&blocks);
    let exits = &exact_states.1;
    assert_eq!(
        exits[&ENTRY_BLOCK_ID]
            .iter()
            .filter(|(owner, place)| owner.binding == binding && **place == destination)
            .count(),
        1
    );
    assert_eq!(exits[&ENTRY_BLOCK_ID].get(&replacement), Some(&destination));
}

#[test]
fn nested_join_phi_is_one_exact_generation_at_outer_loop_header() {
    use crate::model::{OwnerId, OwnershipEvent};

    let binding = BindingId(49);
    let owner = |generation| OwnerId {
        binding,
        generation,
    };
    let place = Place::Local(2);
    let arm = |id, generation| BasicBlock {
        id,
        statements: vec![],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: owner(0),
                from: place,
                to: None,
                to_owner: None,
                to_ty: None,
            }),
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: owner(generation),
                place,
                ty: ResolvedTy::String,
            }),
        ],
        terminator: Terminator::Goto { target: 4 },
    };
    let mut blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: owner(0),
                place,
                ty: ResolvedTy::String,
            })],
            terminator: Terminator::Goto { target: 1 },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Branch {
                cond: Place::Local(0),
                then_target: 2,
                else_target: 3,
            },
        },
        arm(2, 1),
        arm(3, 2),
        BasicBlock {
            id: 4,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 5 },
        },
        BasicBlock {
            id: 5,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 1 },
        },
    ];

    super::materialize_exact_owner_join_transfers(&mut blocks, &mut Builder::default());

    let exact_states = exact_owner_states(&blocks);
    let entries = &exact_states.0;
    let exits = &exact_states.1;
    let mut header = entries
        .get(&1)
        .expect("outer loop header is reachable")
        .clone();
    let header_block = blocks
        .iter()
        .find(|block| block.id == 1)
        .expect("outer loop header exists");
    // `Join` is an explicit ownership-SSA block parameter. The fixed-point
    // entry map is the predecessor state immediately before that parameter;
    // replay the header operations to inspect its canonical exact identity.
    apply_exact_owner_ops(&header_block.instructions, &mut header);
    let exact = header
        .iter()
        .filter(|(candidate, candidate_place)| {
            candidate.binding == binding && **candidate_place == place
        })
        .collect::<Vec<_>>();
    assert_eq!(exact.len(), 1);
    assert!(
        exact[0].0.generation >= 3,
        "the outer loop must receive one generation after coalescing the initial and two branch identities: {exact:?}; header ops: {:?}",
        header_block.instructions
    );
    assert!(exits.values().all(|state| {
        state
            .keys()
            .filter(|candidate| candidate.binding == binding)
            .count()
            <= 1
    }));
}

#[test]
fn cyclic_conditional_reassignment_uses_must_owned_join_parameter() {
    use crate::model::{OwnerId, OwnershipEvent};

    let binding = BindingId(50);
    let initial = OwnerId {
        binding,
        generation: 0,
    };
    let reassigned = OwnerId {
        binding,
        generation: 1,
    };
    let place = Place::Local(2);
    let mut blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: initial,
                place,
                ty: ResolvedTy::String,
            })],
            terminator: Terminator::Goto { target: 1 },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Branch {
                cond: Place::Local(0),
                then_target: 2,
                else_target: 3,
            },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Release {
                    owner: initial,
                    place,
                }),
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner: reassigned,
                    place,
                    ty: ResolvedTy::String,
                }),
            ],
            terminator: Terminator::Goto { target: 4 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 4 },
        },
        BasicBlock {
            id: 4,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 1 },
        },
    ];

    super::materialize_exact_owner_join_transfers(&mut blocks, &mut Builder::default());

    assert!(blocks
        .iter()
        .find(|block| block.id == 1)
        .is_some_and(|block| block.instructions.iter().any(|instruction| {
            matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Join {
                    replacement,
                    place: joined_place,
                    ..
                }) if replacement.binding == binding && *joined_place == place
            )
        })));
    let exact_states = exact_owner_states(&blocks);
    let exits = &exact_states.1;
    assert!(exits.get(&1).is_some_and(|state| {
        state
            .iter()
            .any(|(owner, owner_place)| owner.binding == binding && *owner_place == place)
    }));
}

#[test]
fn join_canonicalizes_common_and_edge_local_generations() {
    use crate::model::{OwnerId, OwnershipEvent};

    let binding = BindingId(60);
    let common = OwnerId {
        binding,
        generation: 0,
    };
    let edge_local = OwnerId {
        binding,
        generation: 1,
    };
    let place = Place::Local(23);
    let mut blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: common,
                place,
                ty: ResolvedTy::String,
            })],
            terminator: Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Reset {
                previous: common,
                replacement: edge_local,
                place,
                ty: ResolvedTy::String,
            })],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];

    super::materialize_exact_owner_join_transfers(&mut blocks, &mut Builder::default());

    let target = blocks.iter().find(|block| block.id == 3).unwrap();
    assert!(target.instructions.iter().any(|instruction| {
        matches!(
            instruction,
            Instr::OwnershipEvent(OwnershipEvent::Join { incoming, .. })
                if incoming.contains(&common) && incoming.contains(&edge_local)
        )
    }));
    let exact_states = exact_owner_states(&blocks);
    let entries = &exact_states.0;
    let mut at_target = entries[&3].clone();
    apply_exact_owner_ops(&target.instructions, &mut at_target);
    assert_eq!(
        at_target
            .keys()
            .filter(|owner| owner.binding == binding)
            .count(),
        1,
        "the common identity and edge-local stale identity must become one SSA owner"
    );
}

/// Owning-block id for an `ExitPath`. Every variant carries a `block`
/// field — surfacing it as a single function keeps `validate_drop_plan`
/// uniform across exit kinds.
#[must_use]
pub(super) fn exit_block_id(exit: &ExitPath) -> u32 {
    match *exit {
        ExitPath::Return { block }
        | ExitPath::Goto { block, .. }
        | ExitPath::Branch { block, .. }
        | ExitPath::Call { block, .. }
        | ExitPath::Unwind { block, .. }
        | ExitPath::Panic { block }
        | ExitPath::Cancel { block }
        | ExitPath::Yield { block, .. }
        | ExitPath::Send { block, .. }
        | ExitPath::Ask { block, .. }
        | ExitPath::Select { block, .. }
        | ExitPath::Join { block, .. }
        | ExitPath::Suspend { block, .. } => block,
    }
}

/// Human-readable label for an `ExitPath` discriminator — surfaced in
/// `DropPlanUndetermined` diagnostics so the rejected exit is named.
#[must_use]
pub(super) fn exit_kind_label(exit: &ExitPath) -> &'static str {
    match exit {
        ExitPath::Return { .. } => "Return",
        ExitPath::Goto { .. } => "Goto",
        ExitPath::Branch { .. } => "Branch",
        ExitPath::Call { .. } => "Call",
        ExitPath::Unwind { .. } => "Unwind",
        ExitPath::Panic { .. } => "Panic",
        ExitPath::Cancel { .. } => "Cancel",
        ExitPath::Yield { .. } => "Yield",
        ExitPath::Send { .. } => "Send",
        ExitPath::Ask { .. } => "Ask",
        ExitPath::Select { .. } => "Select",
        ExitPath::Join { .. } => "Join",
        ExitPath::Suspend { .. } => "Suspend",
    }
}

/// Name an exit in the user-facing register (ladder §3.6): which kind of
/// path out of the function leaks, never the internal edge identity. The
/// exact block and owner generations stay in the engineer channel — the
/// `HEW_DEBUG_CHECKED_FUNCTION` dump prints the whole event stream and owner
/// state — so a user diagnostic never carries a `Place` or `OwnerId`.
#[must_use]
pub(super) fn exit_path_user_label(exit: &ExitPath) -> String {
    match exit {
        ExitPath::Return { .. } => "the return path".to_owned(),
        ExitPath::Goto { .. } | ExitPath::Branch { .. } => "a branch out of this scope".to_owned(),
        ExitPath::Call { callee, .. } => format!("the call to `{callee}`"),
        ExitPath::Unwind { callee, .. } => format!("the unwind path out of `{callee}`"),
        ExitPath::Panic { .. } => "the panic path".to_owned(),
        ExitPath::Cancel { .. } => "the cancellation path".to_owned(),
        ExitPath::Yield { .. } => "a yield".to_owned(),
        ExitPath::Send { actor, .. } => format!("the send to `{actor}`"),
        ExitPath::Ask { .. } => "an ask".to_owned(),
        ExitPath::Select { .. } => "a `select` arm".to_owned(),
        ExitPath::Join { .. } => "a `join` branch".to_owned(),
        ExitPath::Suspend { .. } => "a suspension point".to_owned(),
    }
}

/// Structural validation of an elaborated drop plan. Walks every
/// `(ExitPath, DropPlan)` entry and every `ElabBlock.drops` cleanup
/// list, verifying that each drop's `kind` matches what the drop's
/// `place` would select via `drop_kind_for`, that the per-block
/// consume-on-split invariant holds, and that the lambda-actor
/// capture side-table honours the weak-ref discipline. A mismatch
/// indicates the elaborator's drop-plan construction lost coherence —
/// surface as `MirCheck::DropPlanUndetermined` so the program is
/// rejected before codegen observes a partial / inconsistent plan.
///
/// This is the M2 substrate's fail-closed boundary: a `Place::
/// DuplexHandle` paired with `DropKind::Resource` would otherwise
/// route through the generic `close` method dispatch instead of the
/// runtime's close-both-directions protocol — silently dropping the
/// recv-direction queue. Same idea for `LambdaActorHandle` paired
/// with `DropKind::DuplexClose` (would skip the actor's stop-
/// protocol).
///
/// The walk covers EVERY exit-path discriminator, not just `Return`:
///   - `Return` is the canonical Hew exit; carries the replay-derived
///     drops of the owners live at that block's exit.
///   - `Panic` and `Cancel` exits transfer to a cleanup block whose
///     `ElabBlock.drops` carry the same replay-derived drops; both the
///     `DropPlan` and the destination cleanup block's `drops` are
///     validated.
///   - `Yield`, `Send`, `Select` exits carry empty `DropPlan`s today
///     (per-arm cleanup lives in codegen for `Select`; coroutine and
///     actor-send surfaces have no construction site on the integer
///     spine) but the walk treats them uniformly — when a future
///     surface populates a non-empty plan, it is checked the same
///     way without retrofitting.
///   - `Goto`, `Branch`, `Call` carry empty `DropPlan`s (intra-CFG
///     edges that don't fire drops) but are walked for forward
///     compatibility.
///
/// `ElabBlock.drops` are walked symmetrically so a malformed cleanup
/// block (e.g. a panic-cleanup block with an over-broad close-both-
/// dirs drop on a half-handle binding) is rejected at the same
/// boundary.
///
/// LESSONS: boundary-fail-closed, cleanup-all-exits.
#[must_use]
pub(super) fn validate_drop_plan(elab: &ElaboratedMirFunction) -> Vec<MirCheck> {
    let _timing = crate::timing::stage("validate_drop_plan");
    let mut findings = Vec::new();
    for (exit, plan) in &elab.drop_plans {
        let block = exit_block_id(exit);
        let kind_label = exit_kind_label(exit);
        for drop in &plan.drops {
            let expected = expected_drop_kind_for_validation(drop);
            if drop.kind != expected {
                findings.push(MirCheck::DropPlanUndetermined {
                    block,
                    reason: format!(
                        "drop on place {} has kind {:?}, but the place \
                         variant selects {:?}; elaborator must use the \
                         Place-driven kind (exit path: {kind_label})",
                        drop.place, drop.kind, expected,
                    ),
                });
            }
        }
        check_duplex_split_state(block, &plan.drops, &mut findings);
    }
    // Cleanup block drops are the panic / cancel landing pad. Validate
    // the same invariants against ElabBlock.drops so a malformed
    // cleanup block surfaces at the same boundary as a malformed
    // DropPlan.
    for block in &elab.blocks {
        for drop in &block.drops {
            let expected = expected_drop_kind_for_validation(drop);
            if drop.kind != expected {
                findings.push(MirCheck::DropPlanUndetermined {
                    block: block.id,
                    reason: format!(
                        "cleanup drop on place {} has kind {:?}, but the \
                         place variant selects {:?}; elaborator must use the \
                         Place-driven kind",
                        drop.place, drop.kind, expected,
                    ),
                });
            }
        }
        check_duplex_split_state(block.id, &block.drops, &mut findings);
    }
    validate_lambda_captures(&elab.lambda_captures, &mut findings);
    findings
}

/// Prove that every potentially-unwinding MIR call has exactly one normal
/// continuation plan and one exceptional cleanup plan. LLVM codegen consumes
/// these as the two successors of `invoke`; accepting a missing or duplicated
/// sibling would make cleanup depend on backend guesswork.
#[must_use]
pub(super) fn validate_unwind_cleanup_coverage(
    elab: &ElaboratedMirFunction,
    raw: &RawMirFunction,
) -> Vec<MirCheck> {
    let _timing = crate::timing::stage("validate_unwind_cleanup_coverage");
    validate_unwind_cleanup_coverage_over(elab, &raw.blocks)
}

#[allow(
    clippy::too_many_lines,
    reason = "coverage validates runtime calls, generator resumes, and ordinary calls under one exact sibling-plan rule"
)]
fn validate_unwind_cleanup_coverage_over(
    elab: &ElaboratedMirFunction,
    blocks: &[BasicBlock],
) -> Vec<MirCheck> {
    let mut findings = Vec::new();
    let mut expected: HashSet<(u32, String)> = HashSet::new();

    for block in blocks {
        let generator_resume_count = block
            .instructions
            .iter()
            .filter(|instruction| matches!(instruction, Instr::GeneratorNext { .. }))
            .count();
        if generator_resume_count == 0 {
            continue;
        }
        expected.insert((block.id, "hew_cont_resume".to_owned()));
        let unwind_count = elab
            .drop_plans
            .iter()
            .filter(|(exit, _)| {
                matches!(
                    exit,
                    ExitPath::Unwind { block: exit_block, callee }
                        if *exit_block == block.id && callee == "hew_cont_resume"
                )
            })
            .count();
        if generator_resume_count != 1 || unwind_count != 1 {
            findings.push(MirCheck::ObligationBalanceUnverified {
                function: elab.name.clone(),
                reason: format!(
                    "generator resume bb{} must carry exactly one GeneratorNext and one unwind \
                     cleanup plan; found {generator_resume_count} resume operations and \
                     {unwind_count} unwind plans",
                    block.id,
                ),
            });
        }
        if block.instructions.iter().any(|instruction| {
            matches!(
                instruction,
                Instr::GeneratorNext {
                    ctx_owner: None,
                    ..
                }
            )
        }) {
            findings.push(MirCheck::ObligationBalanceUnverified {
                function: elab.name.clone(),
                reason: format!(
                    "generator resume bb{} has no explicit context OwnerId; lowering must carry \
                     the live owner generation into Checked MIR",
                    block.id,
                ),
            });
        }
    }

    for block in blocks {
        let Terminator::Call {
            callee,
            authority,
            next,
            ..
        } = &block.terminator
        else {
            continue;
        };
        expected.insert((block.id, callee.clone()));

        let normal: Vec<_> = elab
            .drop_plans
            .iter()
            .filter(|(exit, _)| {
                matches!(
                    exit,
                    ExitPath::Call {
                        block: exit_block,
                        callee: exit_callee,
                        next: exit_next,
                    } if *exit_block == block.id && exit_callee == callee && exit_next == next
                )
            })
            .collect();
        let unwind: Vec<_> = elab
            .drop_plans
            .iter()
            .filter(|(exit, _)| {
                matches!(
                    exit,
                    ExitPath::Unwind {
                        block: exit_block,
                        callee: exit_callee,
                    } if *exit_block == block.id && exit_callee == callee
                )
            })
            .collect();

        let expected_normal = usize::from(!authority.is_no_return());
        if normal.len() != expected_normal || unwind.len() != 1 {
            findings.push(MirCheck::ObligationBalanceUnverified {
                function: elab.name.clone(),
                reason: format!(
                    "call bb{} -> `{callee}` must have exactly {expected_normal} normal plan(s) \
                     and one unwind cleanup plan; found {} normal and {} unwind plans",
                    block.id,
                    normal.len(),
                    unwind.len(),
                ),
            });
            continue;
        }
        if normal
            .first()
            .is_some_and(|(_, plan)| !plan.drops.is_empty())
        {
            findings.push(MirCheck::ObligationBalanceUnverified {
                function: elab.name.clone(),
                reason: format!(
                    "normal continuation for call bb{} -> `{callee}` destroys owners before \
                     entering bb{next}; normal call edges must carry ownership forward",
                    block.id,
                ),
            });
        }

        let mut places = HashSet::new();
        for drop in &unwind[0].1.drops {
            if !places.insert(drop.place) {
                findings.push(MirCheck::ObligationBalanceUnverified {
                    function: elab.name.clone(),
                    reason: format!(
                        "unwind cleanup for call bb{} -> `{callee}` destroys place {} more \
                         than once",
                        block.id, drop.place,
                    ),
                });
            }
        }
    }

    for block in blocks {
        for call_site in block
            .instructions
            .iter()
            .filter_map(|instruction| match instruction {
                Instr::CallClosure { call_site, .. } => Some(*call_site),
                _ => None,
            })
        {
            let closure_key = crate::model::indirect_closure_callee(call_site);
            expected.insert((block.id, closure_key.clone()));
            let unwind: Vec<_> = elab
                .drop_plans
                .iter()
                .filter(|(exit, _)| {
                    matches!(
                        exit,
                        ExitPath::Unwind { block: exit_block, callee }
                            if *exit_block == block.id
                                && callee == &closure_key
                    )
                })
                .collect();
            if unwind.len() != 1 {
                findings.push(MirCheck::ObligationBalanceUnverified {
                function: elab.name.clone(),
                reason: format!(
                    "indirect closure call bb{} at {call_site} must have exactly one unwind cleanup plan; found {}",
                    block.id,
                    unwind.len()
                ),
            });
                continue;
            }
            let mut places = HashSet::new();
            for drop in &unwind[0].1.drops {
                if !places.insert(drop.place) {
                    findings.push(MirCheck::ObligationBalanceUnverified {
                    function: elab.name.clone(),
                    reason: format!(
                        "indirect closure unwind cleanup for bb{} at {call_site} destroys place {} more than once",
                        block.id, drop.place,
                    ),
                });
                }
            }
        }
    }

    for (exit, _) in &elab.drop_plans {
        let ExitPath::Unwind { block, callee } = exit else {
            continue;
        };
        if !expected.contains(&(*block, callee.clone())) {
            findings.push(MirCheck::ObligationBalanceUnverified {
                function: elab.name.clone(),
                reason: format!(
                    "orphan unwind cleanup for bb{block} -> `{callee}` has no matching MIR call"
                ),
            });
        }
    }

    findings
}

/// Write (def) slots of a terminator that mint a tracked local: a call's
/// result dest, ask reply slots, generator/lambda-actor handle dests,
/// select-arm bindings, a join's result tuple.
pub(super) fn terminator_mint_places(term: &Terminator) -> Vec<Place> {
    match term {
        Terminator::Call { dest, .. } => dest.iter().copied().collect(),
        Terminator::Ask {
            result_dest,
            reply_dest,
            error_dest,
            ..
        }
        | Terminator::RemoteAsk {
            result_dest,
            reply_dest,
            error_dest,
            ..
        } => vec![*result_dest, *reply_dest, *error_dest],
        Terminator::MakeGenerator { dest, .. } | Terminator::MakeLambdaActor { dest, .. } => {
            vec![*dest]
        }
        Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => {
            arms.iter().filter_map(|arm| arm.binding).collect()
        }
        Terminator::Join { result, .. } => vec![*result],
        // No write slots. A bare Suspend's result writes occur only when the
        // continuation resumes, so `suspend_resume_mint_places` applies them
        // on that edge rather than to the pre-suspend frame state.
        _ => Vec::new(),
    }
}

/// The mint set comes from explicit ownership operations in MIR, never from
/// the mutable lowering ledger. Parameter slots are removed by the caller;
/// alias-only registrations emit no mint, while a provisional owner demoted to
/// an alias carries a program-point `Release` event that ends its generation.
/// What the source calls each binding, and where it was defined.
///
/// The FIRST `Bind` for a binding wins: a `var` is re-bound at every
/// assignment, and an obligation diagnostic anchors at the value's definition
/// site, not at whichever assignment happened to come last in block order.
/// A place that may be absent, rendered without the derived `Debug` shape.
fn describe_optional_place(place: Option<&Place>) -> String {
    place.map_or_else(|| "none".to_owned(), ToString::to_string)
}

fn binding_definition_metadata(blocks: &[BasicBlock]) -> HashMap<BindingId, (String, SiteId)> {
    let mut metadata: HashMap<BindingId, (String, SiteId)> = HashMap::new();
    for statement in blocks.iter().flat_map(|block| &block.statements) {
        if let MirStatement::Bind {
            binding,
            name,
            site,
            ..
        } = statement
        {
            metadata
                .entry(*binding)
                .or_insert_with(|| (name.clone(), *site));
        }
    }
    metadata
}

fn tracked_obligation_locals_with_sites(
    builder: &Builder,
    blocks: &[BasicBlock],
) -> (BTreeMap<u32, String>, BTreeMap<u32, SiteId>) {
    let mut tracked: BTreeMap<u32, String> = BTreeMap::new();
    let mut mint_sites: BTreeMap<u32, SiteId> = BTreeMap::new();
    let binding_metadata = binding_definition_metadata(blocks);
    for (owner, place, ty) in blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint { owner, place, ty }) => {
                Some((*owner, *place, ty.clone()))
            }
            Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Reset {
                    replacement,
                    place,
                    ty,
                    ..
                }
                | crate::model::OwnershipEvent::Rearm {
                    replacement,
                    place,
                    ty,
                    ..
                },
            ) => Some((*replacement, *place, ty.clone())),
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                to_owner: Some(owner),
                to: Some(place),
                to_ty: Some(ty),
                ..
            }) => Some((*owner, *place, ty.clone())),
            _ => None,
        })
    {
        // Re-apply the heap-ownership authority: the seed test admits every
        // non-BitCopy value class, which includes heap-free direct enums
        // (`Result<i64, AskError>`, `Colour`) that carry NO release
        // obligation. Track a binding only when its type transitively owns
        // heap (`ty_owns_heap` — the single structural authority) or its
        // class carries a non-heap drop ritual (`@resource` close /
        // `@linear` consume — the all-bitcopy resource record case).
        let class = ValueClass::of_ty(&ty, &builder.type_classes);
        let owns_heap = crate::model::ty_owns_heap_mir(
            &ty,
            &builder.record_field_orders,
            &builder.enum_layouts,
        );
        if !owns_heap && !matches!(class, ValueClass::AffineResource | ValueClass::Linear) {
            continue;
        }
        let Some(local) = base_local(place) else {
            continue;
        };
        let binding = binding_metadata.get(&owner.binding);
        let binding_site = binding.map(|(_, site)| *site);
        let diagnostic = builder.call_scrutinee_diagnostics.get(&local);
        let name = diagnostic.map_or_else(
            || {
                binding.map_or_else(
                    || format!("__hew_owner_{}", owner.binding.0),
                    |(name, _)| name.clone(),
                )
            },
            |(_, label)| label.clone(),
        );
        let site = diagnostic.map(|(site, _)| *site).or(binding_site);
        tracked.entry(local).or_insert(name);
        if let Some(site) = site {
            mint_sites.entry(local).or_insert(site);
        }
    }
    (tracked, mint_sites)
}

fn tracked_obligation_locals(builder: &Builder, blocks: &[BasicBlock]) -> BTreeMap<u32, String> {
    tracked_obligation_locals_with_sites(builder, blocks).0
}

// ============================================================================
// A — discharge-authority carriage (D159/U229)
// ============================================================================

/// Fail-closed discharge-authority backstop: every `NeutralizePayloadSlot`
/// whose [`crate::model::NeutralizeAuthority`] structurally owns a destination
/// (`SendTransferLastUse` / `WholeCarrierConsume`) MUST carry a `transferee`. A
/// `None` on such an authority is a fact-erased site that slipped a defaulted
/// authority past the `set_owned_local_consumed`/emit chokepoints — reject
/// before codegen (boundary-fail-closed, L49). The passing corpus has none; the
/// check exists so a future emit site that drops the fact fails closed rather
/// than silently reintroducing the erasure A removes.
pub(super) fn validate_discharge_authority(
    elab: &ElaboratedMirFunction,
    raw: &RawMirFunction,
) -> Vec<MirCheck> {
    let _timing = crate::timing::stage("validate_discharge_authority");
    validate_discharge_authority_over(&elab.name, &raw.blocks)
}

/// Testable core of [`validate_discharge_authority`] — hand-constructed blocks,
/// no `RawMirFunction`.
fn validate_discharge_authority_over(function: &str, blocks: &[BasicBlock]) -> Vec<MirCheck> {
    let mut findings = Vec::new();
    for block in blocks {
        for instr in &block.instructions {
            let Instr::NeutralizePayloadSlot {
                authority,
                transferee,
                ..
            } = instr
            else {
                continue;
            };
            if authority.requires_transferee() && transferee.is_none() {
                findings.push(MirCheck::DischargeAuthorityMissing {
                    function: function.to_string(),
                    block: block.id,
                    authority: *authority,
                    reason: format!(
                        "NeutralizePayloadSlot carries authority {authority:?}, which moves \
                         ownership into a destination local, but no transferee was recorded — \
                         the discharge fact was erased at the emit site"
                    ),
                });
            }
        }
    }
    findings
}

fn expected_drop_kind_for_validation(drop: &ElabDrop) -> DropKind {
    match drop.kind {
        // owned-aggregate record drops are keyed by both kind and `ElabDrop::ty`:
        // the place remains an ordinary stack `Local`, while the synthesized
        // helper identity is the user-record type. `user_record_layout_key`
        // accepts BOTH a bare-name monomorphic record and a generic
        // INSTANTIATION (`Pair<i64, string>`, whose `ty` carries args), keying
        // the latter on its `hew_hir::mangle`d name — the same key
        // `is_owned_aggregate_record_ty` (the admit authority) and the codegen
        // `record_inplace_drop_name` resolve, so the dedicated kind is accepted
        // exactly when the elaborator was authorised to emit it. A `Named`
        // resolved-but-bare type (`Place::Local`) is the only shape that earns
        // the dedicated kind; everything else re-derives via the Place-driven
        // dispatcher so a non-record place cannot silently carry a
        // `RecordInPlace` kind.
        DropKind::RecordInPlace => {
            if matches!(drop.place, Place::Local(_)) && user_record_layout_key(&drop.ty).is_some() {
                DropKind::RecordInPlace
            } else {
                drop_kind_for(drop.place, &drop.ty, None)
            }
        }
        // W5.020 — heap-owning enum composite drops are keyed by both kind and
        // `ElabDrop::ty` (the enum identity selects the synthesized in-place
        // helper), while the place is an ordinary stack `Local`. Accept the
        // dedicated kind on local enum-composite storage; any other shape
        // re-derives via the Place-driven dispatcher and so cannot silently
        // carry an `EnumInPlace` kind on a non-enum place.
        DropKind::EnumInPlace => {
            if matches!(drop.place, Place::Local(_)) && matches!(&drop.ty, ResolvedTy::Named { .. })
            {
                DropKind::EnumInPlace
            } else {
                drop_kind_for(drop.place, &drop.ty, None)
            }
        }
        // W5.016 — owned-element `Vec<T>` scope-exit release. The place is an
        // ordinary stack `Local` holding the Vec handle; the owned-ness
        // decision lives in the function-scoped owned-element key set, not in
        // the type-only `drop_kind_for` dispatcher (which would re-derive
        // `Resource` for a `Vec` Local). Accept the dedicated
        // `CowHeap { hew_vec_free_owned }` kind on a local Vec; any other shape
        // re-derives via the dispatcher so a non-Vec place cannot silently
        // carry the owned-Vec release symbol.
        DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::VecOwnedElement,
        } if matches!(drop.place, Place::Local(_)) && ty_is_vec(&drop.ty) => DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::VecOwnedElement,
        },
        // Closure-pair `Vec<fn(...)>` scope-exit release: same dedicated-kind
        // acceptance shape as the owned-Vec arm — a local Vec whose element
        // is a closure pair may carry the closure-pair release symbol; any
        // other shape re-derives via the Place-driven dispatcher.
        DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::VecClosurePairs,
        } if matches!(drop.place, Place::Local(_)) && ty_is_closure_pair_vec(&drop.ty) => {
            DropKind::CowHeap {
                release: crate::ownership::CowHeapRelease::VecClosurePairs,
            }
        }
        // Plain `Vec<T>` scope-exit release: same dedicated-kind acceptance
        // shape as the owned-Vec / closure-pair arms — a local Vec may carry
        // the plain `hew_vec_free` release symbol (the admit authority is the
        // function-scoped `plain_vec_drop_allowed` derivation, not the
        // type-only `drop_kind_for` dispatcher); any other shape re-derives
        // via the Place-driven dispatcher so a non-Vec place cannot silently
        // carry the plain-Vec release symbol. Codegen re-validates the
        // (type, symbol) congruence again before emitting the call.
        DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::VecPlain,
        } if matches!(drop.place, Place::Local(_)) && ty_is_vec(&drop.ty) => DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::VecPlain,
        },
        // First-class VecIter abandonment drops address field 0 of a local
        // cursor record. The release refinement is carried explicitly and
        // codegen re-derives it against the concrete element/layout before
        // emitting the field release.
        DropKind::VecIterCursor { release }
            if matches!(drop.place, Place::Local(_))
                && matches!(
                    &drop.ty,
                    ResolvedTy::Named {
                        args,
                        builtin: Some(hew_types::BuiltinType::VecIter),
                        ..
                    } if args.len() == 1
                ) =>
        {
            DropKind::VecIterCursor { release }
        }
        // W5.021 — heap-owning tuple drops are keyed by both kind and
        // `ElabDrop::ty` (the structural tuple shape selects the synthesized
        // in-place helper), while the place is an ordinary stack `Local`
        // holding the tuple struct. Accept the dedicated kind on a local tuple
        // place; any other shape re-derives via the Place-driven dispatcher and
        // so cannot silently carry a `TupleInPlace` kind on a non-tuple place.
        DropKind::TupleInPlace => {
            if matches!(drop.place, Place::Local(_)) && matches!(&drop.ty, ResolvedTy::Tuple(_)) {
                DropKind::TupleInPlace
            } else {
                drop_kind_for(drop.place, &drop.ty, None)
            }
        }
        // Per-yield structural tuple/array payloads use the recursive inline
        // walker rather than a registered tuple thunk. Their place is still a
        // stack `Local`, so accept the dedicated kind on exactly those
        // structural aggregate types; every other pairing falls back to the
        // Place-driven dispatcher and fails closed on disagreement.
        DropKind::AggregateRecursive => {
            if matches!(drop.place, Place::Local(_))
                && matches!(&drop.ty, ResolvedTy::Tuple(_) | ResolvedTy::Array(_, _))
            {
                DropKind::AggregateRecursive
            } else {
                drop_kind_for(drop.place, &drop.ty, None)
            }
        }
        // Escaping-closure pair drops are keyed by both kind and
        // `ElabDrop::ty` (the fn surface type confirms the two-pointer pair
        // ABI), while the place is an ordinary stack `Local` holding the
        // pair. Accept the dedicated kind on a local fn-typed place; any
        // other shape re-derives via the Place-driven dispatcher so a
        // non-pair place cannot silently carry a `ClosurePair` kind.
        DropKind::ClosurePair => {
            if matches!(drop.place, Place::Local(_)) && ty_is_closure_pair(&drop.ty) {
                DropKind::ClosurePair
            } else {
                drop_kind_for(drop.place, &drop.ty, None)
            }
        }
        // Indirect-enum heap-node drops are keyed by both kind and
        // `ElabDrop::ty` (the enum identity selects the node size/align and the
        // recursive child-free walk), while the place is an ordinary stack
        // `Local` holding the heap pointer. `drop_kind_for` is layout-blind (it
        // has no `enum_layouts` to tell an indirect enum from an inline one), so
        // the indirect-enum kind is validated here by shape rather than
        // re-derived through the dispatcher: accept the dedicated kind on a local
        // `Named` place; any other shape re-derives via the dispatcher so a
        // non-indirect-enum place cannot silently carry an `IndirectEnum` kind.
        DropKind::IndirectEnum => {
            if matches!(drop.place, Place::Local(_)) && matches!(&drop.ty, ResolvedTy::Named { .. })
            {
                DropKind::IndirectEnum
            } else {
                drop_kind_for(drop.place, &drop.ty, None)
            }
        }
        // Extract the storage discriminator from the elaborated drop kind
        // itself so the dispatcher can re-derive the same
        // `DropKind::TraitObject { storage }` for the expected-vs-actual
        // comparison. Non-dyn drops pass `None` (the dispatcher ignores it).
        DropKind::TraitObject { storage } => drop_kind_for(drop.place, &drop.ty, Some(storage)),
        _ => drop_kind_for(drop.place, &drop.ty, None),
    }
}
/// Structural legality rules for [`Instr::FieldDropInPlace`] — the pairing
/// verifier for the field-addressed in-place drop op. Three rules, each a
/// hard reject (`MirCheck::DropPlanUndetermined` upgrades to a diagnostic and
/// the CLI refuses the program), never a silent skip:
///
///   1. **Type admissibility.** The op's `ty` must be `string` (rerouted off
///      the retain-cancelling load+`Drop` pair) or a shape the shared
///      classifier admits (`field_drop_in_place_admissible` — the same
///      predicate MIR admission consults, so admission and verification
///      cannot drift).
///   2. **Base shape.** `base` must be a `Place::Local` whose registered type
///      matches the field address: a user record local for
///      `FieldAddr::Record(_)`, a tuple local for `FieldAddr::Tuple(_)`.
///   3. **Inline-composite pairing.** For an inline-composite `ty` — an
///      admitted aggregate that is NOT an indirect enum (record / tuple /
///      inline enum / fixed array) — the in-place helpers null-store NOTHING,
///      so idempotence rests entirely on exactly-once parent suppression: the
///      base local must not receive a composite in-place drop
///      (`RecordInPlace` / `EnumInPlace` / `TupleInPlace` /
///      `AggregateRecursive` / `IndirectEnum`) in any exit `DropPlan` or
///      cleanup block, because that drop would re-walk the freed field's
///      leaves (double-free). Pointer (`string`) and indirect-enum shapes
///      carry a null-store postcondition into codegen instead and tolerate a
///      structurally reachable second walk.
///
/// LESSONS: drop-allowset-from-value-flow (a no-temp field-addressed drop op
/// carries a direct prover-exclusion rule; this verifier is its enforcement
/// pairing), boundary-fail-closed.
pub(super) fn validate_field_drop_in_place(
    blocks: &[BasicBlock],
    elab: &ElaboratedMirFunction,
    locals: &[ResolvedTy],
    enum_layouts: &[crate::model::EnumLayout],
    admissible: &dyn Fn(&ResolvedTy) -> bool,
) -> Vec<MirCheck> {
    let mut findings = Vec::new();
    // Base locals of every composite in-place drop the elaborated plan still
    // fires — the set rule 3 requires the op's base to be absent from.
    let composite_dropped_locals: HashSet<u32> = elab
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .chain(elab.blocks.iter().flat_map(|block| block.drops.iter()))
        .filter(|drop| {
            matches!(
                drop.kind,
                DropKind::RecordInPlace
                    | DropKind::EnumInPlace
                    | DropKind::TupleInPlace
                    | DropKind::AggregateRecursive
                    | DropKind::IndirectEnum
            )
        })
        .filter_map(|drop| base_local(drop.place))
        .collect();
    for block in blocks {
        for instr in &block.instructions {
            let Instr::FieldDropInPlace { base, field, ty } = instr else {
                continue;
            };
            // Rule 1 — type admissibility.
            if !matches!(ty, ResolvedTy::String) && !admissible(ty) {
                findings.push(MirCheck::DropPlanUndetermined {
                    block: block.id,
                    reason: format!(
                        "FieldDropInPlace field type {} is neither `string` nor \
                         a shape the field-drop classifier admits; the codegen \
                         dispatcher has no in-place release for it",
                        ty.user_facing()
                    ),
                });
            }
            // Rule 2 — base shape must match the field address.
            let base_ty = match (base, base_local(*base)) {
                (Place::Local(_), Some(l)) => locals.get(l as usize),
                _ => None,
            };
            let base_ok = match (field, base_ty) {
                (crate::model::FieldAddr::Record(_), Some(bty)) => {
                    user_record_layout_key(bty).is_some()
                }
                (crate::model::FieldAddr::Tuple(_), Some(bty)) => {
                    matches!(bty, ResolvedTy::Tuple(_))
                }
                (_, None) => false,
            };
            if !base_ok {
                findings.push(MirCheck::DropPlanUndetermined {
                    block: block.id,
                    reason: format!(
                        "FieldDropInPlace base {base:?} at {field:?} is not a \
                         local of the matching aggregate shape (record local \
                         for a Record address, tuple local for a Tuple address)"
                    ),
                });
                continue;
            }
            // Rule 3 — inline-composite pairing: exactly-once parent
            // suppression is the op's whole idempotence story for shapes with
            // no null-store.
            let inline_composite = !matches!(ty, ResolvedTy::String)
                && !ty_is_indirect_enum(ty, enum_layouts)
                && admissible(ty);
            if inline_composite {
                if let Some(l) = base_local(*base) {
                    if composite_dropped_locals.contains(&l) {
                        findings.push(MirCheck::DropPlanUndetermined {
                            block: block.id,
                            reason: format!(
                                "FieldDropInPlace on local {l} releases an \
                                 inline-composite field ({}) while the base \
                                 still receives a composite in-place drop; the \
                                 composite walk would re-free the field's \
                                 leaves (no null-store exists on inline \
                                 composites) — the base's composite drop must \
                                 be suppressed",
                                ty.user_facing()
                            ),
                        });
                    }
                }
            }
        }
    }
    findings
}
/// Lambda-actor capture invariants. The capture side-table encodes the
/// runtime's self-binding weak-ref discipline (§5.9 ratification 2):
/// the recursive forward-bind case
///
/// ```hew
/// let fib = actor |n| { ... fib(n - 1) ... };
/// ```
///
/// captures the lambda's own let-binding name as a `Weak` reference so
/// the body does NOT keep the actor alive past external refcount zero.
///
/// Two structural invariants:
///
/// 1. A `Weak` capture must attach to a `LambdaActorHandle`. Attaching
///    a `Weak` capture to any other `Place` (a `DuplexHandle`, a plain
///    `Local`, etc.) would silently relax the refcount discipline on a
///    non-actor resource.
/// 2. At most ONE `Weak` capture per `LambdaActorHandle`. The self-
///    binding-name discipline is a single-name discipline — every
///    lambda has exactly one let-binding name, so a second `Weak`
///    capture on the same actor handle is a lowering bug.
///
/// (a) is the existing "Weak must attach to `LambdaActorHandle`" check.
/// (b) is the new "exactly one Weak per `LambdaActorHandle`" check. The
/// non-recursive lambda case (`let f = actor |n| { n + 1 }`) has zero
/// `Weak` captures and is silently accepted — the discipline only
/// applies when the body references its own binding name.
///
/// LESSONS: boundary-fail-closed, raii-null-after-move (the weak-ref
/// is the actor's null-after-move equivalent for the self-binding
/// reference).
fn validate_lambda_captures(captures: &[LambdaCapture], findings: &mut Vec<MirCheck>) {
    for capture in captures {
        if matches!(capture.capture_kind, crate::model::CaptureKind::Weak)
            && !matches!(capture.actor_handle, Place::LambdaActorHandle(_))
        {
            findings.push(MirCheck::DropPlanUndetermined {
                block: 0,
                reason: format!(
                    "weak capture of `{}` attached to non-lambda-actor handle \
                     {:?}; weak captures are exclusive to LambdaActorHandle \
                     places (§5.9 ratification 2)",
                    capture.name, capture.actor_handle,
                ),
            });
        }
    }

    // Tally Weak captures per LambdaActorHandle. The self-binding-name
    // discipline is a single-name discipline — every lambda has
    // exactly one let-binding name, so multiple Weak captures on the
    // same actor handle indicate a lowering bug.
    let mut weak_per_actor: BTreeMap<u32, Vec<&str>> = BTreeMap::new();
    for capture in captures {
        if !matches!(capture.capture_kind, crate::model::CaptureKind::Weak) {
            continue;
        }
        let Place::LambdaActorHandle(n) = capture.actor_handle else {
            continue; // already rejected above
        };
        weak_per_actor
            .entry(n)
            .or_default()
            .push(capture.name.as_str());
    }
    for (handle_id, names) in weak_per_actor {
        if names.len() > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block: 0,
                reason: format!(
                    "LambdaActorHandle({handle_id}) has {} weak captures ({}); \
                     the self-binding-name weak-ref discipline is exactly one \
                     per actor (§5.9 ratification 2)",
                    names.len(),
                    names.join(", "),
                ),
            });
        }
    }
}
/// Runtime ABIs whose owned-handle-leaf arguments are ALSO borrowed (not just
/// the non-handle-leaf args of [`is_borrowing_call_abi`]). The layout-witness
/// stream recv / `try_recv` runtime entries — `hew_stream_next_layout` /
/// `hew_stream_try_next_layout` for `Stream<T>::recv` / `try_recv` — READ
/// one item from the stream and decode it into the consumer's `Option<T>`
/// slot; the stream handle itself is borrowed for the call and continues to
/// live in the caller's slot afterwards (the caller's source drop is still
/// the SOLE free of the stream's runtime context). The channel layout entries
/// use the same endpoint-borrow contract: receive / try-receive move only the
/// decoded payload into the caller's out slot, while send deep-copies only its
/// payload into the queue. Sender/receiver close remains owned by the caller.
/// Their suspending siblings
/// ([`Terminator::SuspendingStreamNext`]) are already exempt because they are
/// not [`Terminator::Call`]s; listing the blocking / non-suspending entries
/// here keeps the same borrow-not-consume semantics for the
/// `let (sink, input) = stream.pipe(N); input.try_recv()` shape that goes
/// through [`Terminator::Call`].
///
/// Kept narrow: each entry's Rust impl takes a live endpoint pointer and
/// leaves the endpoint's ownership in the caller's slot; adding a callee that
/// actually consumes the handle here would silently disable the double-free
/// gate for its caller.
#[cfg(test)]
pub(super) fn is_handle_borrowing_call_abi(
    builtin: Option<hew_types::runtime_call::RuntimeCallFamily>,
) -> bool {
    use hew_types::runtime_call::RuntimeCallFamily as F;
    matches!(
        builtin,
        Some(
            F::StreamNextLayout
                | F::StreamTryNextLayout
                | F::ChannelRecvLayout
                | F::ChannelTryRecvLayout
                | F::ChannelSendLayout
        )
    )
}

#[cfg(test)]
mod handle_borrowing_call_abi_tests;
/// True when `ty` is a NON-OWNING owned-handle leaf: an actor pid
/// (`Pid`/`LocalPid`/`RemotePid`, `handle_family() == ActorPid`) with NO
/// `close`/release ABI (`close_method().is_none()`). Its drop frees nothing —
/// the actor lifecycle is owned by the runtime scheduler and the pid is a
/// by-value reference snapshot — so passing it by value to a call can never
/// alias a second free. The `close_method().is_none()` guard makes the
/// no-release safety property executable: a future pid-like builtin that gains
/// a release ABI would no longer be exempted. Distinguished from the OWNING
/// handle leaves (Generator/Stream/Sink/Duplex/.../CancellationToken), each of
/// which owns a runtime context released by its handle drop and so DOES
/// double-free when aliased into a container, an aggregate, or a storing call.
pub(super) fn ty_is_nonowning_handle_leaf(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Named { builtin: Some(b), .. }
            if matches!(
                b.handle_family(),
                Some(hew_types::builtin_type::BuiltinHandleFamily::ActorPid)
            ) && b.close_method().is_none()
    )
}

/// Narrow collection-element aliases to values that actually carry a release
/// obligation.
///
/// `LocalPid`/`RemotePid` are tracked as affine resources for move checking,
/// but their actor-pid handles are non-owning snapshots with no close ABI and
/// no drop glue. Borrowing one from `Vec<LocalPid<_>>` is therefore safe both
/// as an actor-send receiver and as a bit-copy insertion into another vector:
/// neither operation can create a second release authority. Every owning
/// handle remains gated, and a missing local type fails closed by retaining the
/// alias in the validator set.
#[must_use]
fn close_obligated_borrow_alias_locals(
    aliases: &HashSet<u32>,
    local_tys: &[ResolvedTy],
) -> HashSet<u32> {
    aliases
        .iter()
        .copied()
        .filter(|local| {
            local_tys
                .get(*local as usize)
                .is_none_or(|ty| !ty_is_nonowning_handle_leaf(ty))
        })
        .collect()
}

#[cfg(test)]
mod close_obligated_borrow_alias_tests {
    use super::*;

    #[test]
    fn nonowning_actor_pid_alias_is_not_a_close_obligation() {
        let local_tys = vec![ResolvedTy::Named {
            name: "LocalPid".to_string(),
            args: vec![ResolvedTy::I64],
            builtin: Some(BuiltinType::LocalPid),
            is_opaque: false,
        }];

        assert!(close_obligated_borrow_alias_locals(&HashSet::from([0]), &local_tys,).is_empty());
    }

    #[test]
    fn owning_or_untyped_alias_stays_fail_closed() {
        let local_tys = vec![ResolvedTy::Named {
            name: "Stream".to_string(),
            args: vec![ResolvedTy::I64],
            builtin: Some(BuiltinType::Stream),
            is_opaque: false,
        }];

        assert_eq!(
            close_obligated_borrow_alias_locals(&HashSet::from([0, 1]), &local_tys),
            HashSet::from([0, 1]),
        );
    }
}
/// True when `ty` is an owned-HANDLE LEAF the W3.053 gate guards: a
/// `Generator`/`AsyncGenerator` context, a `CancellationToken`, or a
/// `Resource`-marker builtin handle (Stream/Sink/Duplex/SendHalf/RecvHalf/
/// `LambdaActorHandle`). Each owns a single runtime context released only by its
/// handle drop, so aliasing it into an aggregate / container / storing call
/// creates the two-free hazard this gate guards.
///
/// Deliberately EXCLUDES the NON-OWNING actor-pid leaves
/// (`Pid`/`LocalPid`/`ChildRef`) and the inline `RemotePid` identity aggregate. None has
/// drop glue: local pid handles do not own actor lifetime, while `RemotePid` is
/// `BitCopy`. They can NEVER alias a second free in ANY context (call-arg,
/// actor-state field, tuple, return, re-aggregation). Gating them over-refuses
/// the stored-pid idiom (`spawn Conn(fetcher: f)`); excluding it here un-gates
/// the pid in every context and subsumes the per-call-arg borrow carve in
/// `terminator_escape_places`. The `close_method().is_none()` guard inside
/// `ty_is_nonowning_handle_leaf` keeps this executable: a future pid-like
/// builtin that gains a release ABI would fall through and stay gated.
///
/// Also EXCLUDES the copy-on-write value leaves (`String`/`Bytes`) and the
/// collection leaves (`Vec`/`HashMap`/`HashSet`, which are `Named` without a
/// handle builtin marker): their exactly-once is proven by `derive_cow_sole_owner`
/// / `owned_vec_drop_allowed` (refcount / sole-owner), and a string or vec
/// aliased into a tuple is a correct, common pattern those analyses admit — the
/// gate must not over-refuse it.
pub(super) fn ty_is_owned_handle_leaf(ty: &ResolvedTy) -> bool {
    // A non-owning actor-pid leaf has no drop glue (no close ABI; its drop is a
    // codegen no-op) and can never double-free, so it is never an origin the
    // gate tracks. Exclude it before the `Resource`-marker test below would
    // otherwise capture it.
    if ty_is_nonowning_handle_leaf(ty) {
        return false;
    }
    match ty {
        ResolvedTy::CancellationToken => true,
        ResolvedTy::Named {
            builtin: Some(b), ..
        } => {
            matches!(
                b,
                hew_types::BuiltinType::Generator | hew_types::BuiltinType::AsyncGenerator
            ) || matches!(
                b.marker(),
                hew_types::builtin_type::BuiltinTypeMarker::Resource
            )
        }
        _ => false,
    }
}
/// Render an owned-handle type for the W3.053 fail-closed diagnostic in
/// user-facing form (`Generator<i64, ()>`, `Stream<i64>`, `CancellationToken`)
/// rather than the `{:?}` `ResolvedTy` debug shape. Falls back to the type's
/// builtin / named identity for the handle kinds the gate covers.
pub(super) fn render_owned_handle_ty(ty: &ResolvedTy) -> String {
    match ty {
        ResolvedTy::Named {
            args,
            builtin:
                Some(builtin @ (hew_types::BuiltinType::Sender | hew_types::BuiltinType::Receiver)),
            ..
        } => {
            let family = match builtin {
                hew_types::BuiltinType::Sender => "channel.Sender",
                hew_types::BuiltinType::Receiver => "channel.Receiver",
                _ => unreachable!("pattern admits only channel endpoint builtins"),
            };
            if args.is_empty() {
                family.to_string()
            } else {
                let rendered: Vec<String> = args.iter().map(render_owned_handle_ty).collect();
                format!("{family}<{}>", rendered.join(", "))
            }
        }
        ResolvedTy::Named { name, args, .. } if args.is_empty() => name.clone(),
        ResolvedTy::Named { name, args, .. } => {
            let rendered: Vec<String> = args.iter().map(render_owned_handle_ty).collect();
            format!("{name}<{}>", rendered.join(", "))
        }
        ResolvedTy::CancellationToken => "CancellationToken".to_string(),
        ResolvedTy::Unit => "()".to_string(),
        ResolvedTy::I64 => "i64".to_string(),
        ResolvedTy::Tuple(elems) => {
            let rendered: Vec<String> = elems.iter().map(render_owned_handle_ty).collect();
            format!("({})", rendered.join(", "))
        }
        other => format!("{other:?}"),
    }
}
/// True when `ty` is a tuple type carrying at least one heap-owning element
/// (pointer handle or heap leaf). The single `ty_owns_heap` authority decides
/// heap-ownership so MIR and codegen agree; a tuple whose elements are all
/// `BitCopy` is excluded (no scope-exit drop needed). Record-aware: a tuple
/// element of nested-record type whose field owns heap (`(Boxed, i64)` where
/// `Boxed { payload: Vec<i64> }`) is recognised so its member-drop fires the
/// inner buffer's release (DIV-1; a record-blind walker leaked it). The same
/// shared structural authority also walks `Option` / user-enum payloads, so a
/// tuple such as `(Option<string>, i64)` or `(Wrap, i64)` is intentionally
/// admitted here; when that tuple is carried as a record field, the soundness
/// hinge is the record owner's replay (the field escape ends or neutralizes
/// its claim), not a tuple-specific shape whitelist.
pub(super) fn ty_is_heap_owning_tuple(
    ty: &ResolvedTy,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> bool {
    matches!(ty, ResolvedTy::Tuple(_))
        && crate::model::ty_carries_drop_obligation_mir(
            ty,
            record_field_orders,
            enum_layouts,
            lifecycle_registry,
        )
}
/// True when `ty` is a tagged-union enum composite (`Result`/`Option`/user
/// `enum`) whose active variant can own a heap allocation. Borrowed views,
/// `dyn Trait`, and non-enum aggregates are excluded — only an inline
/// tagged-union struct earns the in-place tag-aware drop. The heap-owning
/// decision delegates to the single record-AWARE `ty_owns_heap_mir` authority
/// so MIR and codegen agree even when a variant payload is a user record that
/// owns heap through a non-type-parameter field (`enum Wrap { A(Boxed) }`,
/// `Boxed { payload: Vec<i64> }`) — the record-blind `ty_contains_heap_owning`
/// would classify that variant non-owning and leak its payload.
pub(super) fn ty_is_heap_owning_enum_composite(
    ty: &ResolvedTy,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> bool {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    // Must resolve to a registered tagged-union enum layout (not a record,
    // not an opaque/builtin handle). Indirect (heap-boxed) enums route their
    // payload drop through the boxed-storage release path, not this in-place
    // helper, so they are excluded here.
    let layout = crate::model::find_enum_layout(name, args, enum_layouts);
    let Some(layout) = layout else {
        return false;
    };
    if layout.is_indirect {
        return false;
    }
    // Admission is the drop-OBLIGATION axis, not bare heap ownership: an
    // `Option<Tok>` / `enum Held { One(Tok) }` whose payload is a scalar-field
    // `#[resource]` owns no heap yet must run `close` exactly once — the
    // heap-only axis admitted it only when a heap sibling arm happened to
    // exist (`Result<Tok, string>` closed, `Result<Tok, i64>` leaked).
    crate::model::ty_carries_drop_obligation_mir(
        ty,
        record_field_orders,
        enum_layouts,
        lifecycle_registry,
    )
}
/// Resolve the `DropKind` for an `ElabDrop` given the addressable
/// `Place` and the binding's `ResolvedTy`.
///
/// The M2 substrate's drop kinds are selected by the `Place` variant
/// rather than the `ResolvedTy` alone — a binding whose type is
/// `Duplex<S, R>` may be addressed by either a `DuplexHandle`
/// (close-both-dirs) or a `SendHalf` / `RecvHalf` (close-one-dir
/// alias), and the kind must follow the Place. Lambda-actor handles
/// share the underlying `Duplex<Msg, Reply>` type but use
/// `LambdaActorHandle` Place addressing so they select
/// `LambdaActorRelease` — the stop-on-last-handle-drop protocol with
/// weak-ref body capture (§5.9 ratification 2).
///
/// `Place::Local` / `Place::ReturnSlot` fall through to
/// `DropKind::Resource` — the generic `@resource` close path. After
/// W3.030 Stage 2 this kind is interpreted by codegen's typed
/// `DropDispatch::{RuntimeSymbol, UserFn}` dispatcher: runtime
/// substrate names (`Duplex::close`, etc.) route to the wired C-ABI
/// symbol; mangled `<T>::close` symbols route to the user inherent-impl
/// function declared by the `#[resource]` type. A third path is
/// rejected at module-build time by the codegen drop-plan verifier.
///
/// LESSONS: cleanup-all-exits, raii-null-after-move,
/// boundary-fail-closed (kind is selected by Place; mismatching
/// Place + `DropKind` is structurally impossible because this function
/// is the single source of truth).
/// Test-only re-export: forwards to the private `drop_kind_for`.
/// Lives in this module so the function-private invariants stay
/// non-public while still being exercisable from the integration
/// test that pins the `dyn Trait` → `DropKind::TraitObject` contract.
///
/// `dyn_storage` is consulted only when `(place, ty)` selects the
/// `DropKind::TraitObject` arm; for every other arm it is ignored.
/// Passing `None` for a `(Local, ResolvedTy::TraitObject)` pair is
/// the fail-closed boundary (`drop_kind_for` panics) — the MIR
/// builder side-table population is mandatory before drop
/// elaboration reaches this dispatcher.
#[doc(hidden)]
#[must_use]
pub fn drop_kind_for_test_only(
    place: Place,
    ty: &ResolvedTy,
    dyn_storage: Option<TraitObjectStorage>,
) -> DropKind {
    drop_kind_for(place, ty, dyn_storage)
}
#[must_use]
pub(super) fn drop_kind_for(
    place: Place,
    ty: &ResolvedTy,
    dyn_storage: Option<TraitObjectStorage>,
) -> DropKind {
    match place {
        Place::DuplexHandle(_) => DropKind::DuplexClose,
        Place::LambdaActorHandle(_) => DropKind::LambdaActorRelease,
        Place::SendHalf(_) => DropKind::DuplexHalfClose(crate::model::Direction::Send),
        Place::RecvHalf(_) => DropKind::DuplexHalfClose(crate::model::Direction::Recv),
        Place::Local(_) | Place::ReturnSlot
            if matches!(
                ty,
                ResolvedTy::Named {
                    builtin: Some(BuiltinType::Rc),
                    ..
                }
            ) =>
        {
            DropKind::RcRelease
        }
        Place::Local(_) | Place::ReturnSlot
            if matches!(
                ty,
                ResolvedTy::Named {
                    builtin: Some(BuiltinType::Weak),
                    ..
                }
            ) =>
        {
            DropKind::WeakRelease
        }
        // `dyn Trait` locals carry their drop ritual in the vtable's slot 0
        // (`drop_in_place`); codegen emits the GEP-to-slot-0 dispatch plus
        // a storage-discriminated release ritual after `drop_in_place`
        // returns (FrameOwned → no-op; HeapBoxed → `hew_dyn_box_free`).
        // Discriminated by `ResolvedTy::TraitObject` rather than a Place
        // variant because trait objects share `Place::Local` storage with
        // every other by-value owned binding.
        //
        // Storage is sourced from the MIR builder's `dyn_trait_storage`
        // side table (populated at the binding's introducing `let`
        // statement, W3.031 Stage 1). Reaching this arm with no storage
        // hint is a structural fail-closed event — the owner's recipe is
        // never minted without a storage discriminator, and
        // `validate_drop_plan` extracts the storage from the elaborated
        // drop kind before re-running this dispatcher.
        Place::Local(_) | Place::ReturnSlot if matches!(ty, ResolvedTy::TraitObject { .. }) => {
            let storage = dyn_storage.expect(
                "drop_kind_for invariant: ResolvedTy::TraitObject reached the dispatcher \
                 without a TraitObjectStorage hint; the MIR builder must populate \
                 `dyn_trait_storage` for every owned `dyn Trait` binding before \
                 drop elaboration runs (W3.031 Stage 1)",
            );
            DropKind::TraitObject { storage }
        }
        // W5-011 P3 — a `string` owned local shares `Place::Local` /
        // `Place::ReturnSlot` storage with every other by-value binding, so
        // (like the `dyn Trait` arm above) it is discriminated by
        // `ResolvedTy::String` rather than a Place variant. Its function-scope
        // release is the C-ABI `hew_string_drop` (refcount decrement, free at
        // zero). This arm is the single source of truth the drop-plan
        // validator re-derives against, so the owner's `DropRecipe` must carry
        // the identical kind (see `cow_value_leaf_drop_symbol`).
        Place::Local(_) | Place::ReturnSlot if matches!(ty, ResolvedTy::String) => {
            DropKind::CowHeap {
                release: crate::ownership::CowHeapRelease::String,
            }
        }
        // A `bytes` owned local is a by-value `BytesTriple { ptr, i32, i32 }`
        // in its alloca (NOT a single owned pointer), so its release is the
        // triple-field-0 shape: GEP field 0, load the data pointer,
        // `hew_bytes_drop(data_ptr)` (refcount decrement, free at zero),
        // null-store the field. Codegen's `emit_one_elab_drop` intercepts the
        // `(ty == Bytes, drop_fn == "hew_bytes_drop")` pair BEFORE the generic
        // single-`ptr`-load CowHeap path and routes it through
        // `emit_bytes_inplace_drop` — the same emitter the inline
        // `Instr::Drop` bytes path uses, so the two cannot drift on which
        // field of the triple owns the heap allocation. This arm is the single
        // source of truth the drop-plan validator re-derives against, so
        // the owner's `DropRecipe` must carry the identical kind.
        Place::Local(_) | Place::ReturnSlot if matches!(ty, ResolvedTy::Bytes) => {
            DropKind::CowHeap {
                release: crate::ownership::CowHeapRelease::Bytes,
            }
        }
        // A `Generator<Y, R>` / `AsyncGenerator<Y>` owned local holds the heap
        // companion `{ ptr handle, ptr env, ptr env_drop_thunk,
        // ptr out_drop_thunk, i8 started, i8 pending, Y out }` (shared
        // `Place::Local` storage, discriminated by
        // the builtin type). Its sole release is `hew_gen_coro_destroy`, which
        // destroys the coro frame (running its `cleanup` outline over every value
        // the body still owns), typed-drops an un-consumed pending out-value via
        // the planted thunk, then frees the companion. CowHeap is the
        // self-describing load-pointer / call-symbol / null-store release the
        // codegen drop arm uses — null-after-free guards a double destroy
        // (raii-null-after-move), and the runtime null-guards as defence.
        Place::Local(_) | Place::ReturnSlot
            if matches!(
                ty,
                ResolvedTy::Named {
                    builtin: Some(BuiltinType::Generator | BuiltinType::AsyncGenerator),
                    ..
                }
            ) =>
        {
            DropKind::CowHeap {
                release: crate::ownership::CowHeapRelease::Generator,
            }
        }
        // A local `HashMap<K, V>` / `HashSet<E>` owned binding holds a single
        // `*mut HewLayoutHashMap` / `*mut HewLayoutHashSet` handle (shared
        // `Place::Local` storage, discriminated by the builtin type — like the
        // `string` and `Generator` arms above). Its sole release is the
        // layout-keyed `hew_hashmap_free_layout` / `hew_hashset_free_layout`,
        // which walks the live entries through the embedded layout descriptor
        // (per-record key/value drop) then frees the backing storage. CowHeap is
        // the self-describing load-pointer / call-symbol / null-store release the
        // codegen drop arm already wires for both symbols (`cow_heap_release_
        // symbol` / `is_known_cow_heap_drop_symbol` / `emit_cow_heap_drop`);
        // null-after-free guards a double free (raii-null-after-move). This arm
        // is the single source of truth the drop-plan validator re-derives
        // against, so the owner's `DropRecipe` must carry the identical kind. Dispatch
        // on the `builtin` discriminant (NOT the name string) so a user
        // `type HashMap { ... }` is never mistaken for the runtime handle.
        //
        // The release is an UNCONDITIONAL dealloc (the handle carries no
        // refcount); it is sound because the current M-COW spine is move-only —
        // exactly one live binding owns each handle, enforced by the move-checker
        // consuming the source on every share (see the M-COW spine invariant
        // on `frame_owned_heap_locals` in `lower/mod.rs`). When
        // retain-on-share lands this free must become refcount-aware in lockstep.
        Place::Local(_) | Place::ReturnSlot
            if matches!(
                ty,
                ResolvedTy::Named {
                    builtin: Some(BuiltinType::HashMap),
                    ..
                }
            ) =>
        {
            DropKind::CowHeap {
                release: crate::ownership::CowHeapRelease::HashMap,
            }
        }
        Place::Local(_) | Place::ReturnSlot
            if matches!(
                ty,
                ResolvedTy::Named {
                    builtin: Some(BuiltinType::HashSet),
                    ..
                }
            ) =>
        {
            DropKind::CowHeap {
                release: crate::ownership::CowHeapRelease::HashSet,
            }
        }
        // Machine tag and variant fields are sub-structure of a machine value,
        // not independent resources. Machine values are `BitCopy` by value
        // class overall; tag-dominant transition-out drops are a later machine
        // drop-elaboration slice and are not emitted by the Slice 4a step shell.
        //
        // THIS function is only reached through an owner's `DropRecipe`,
        // which is minted from `owned_locals` at binding granularity. A
        // machine binding in `owned_locals` should never be found here:
        // machine `self` is a
        // synthetic parameter, not a user-declared `let` binding, and is
        // therefore never inserted into `owned_locals`. Reaching this arm
        // means a future surface has incorrectly added a machine sub-place to
        // `owned_locals`. Fail-closed as `DropKind::Resource` so codegen
        // surfaces a diagnostic rather than silently no-op'ing.
        // WHY not `unreachable!`: a diagnostic is more actionable than a panic
        // at MIR-dump time; codegen's fail-closed arm on `DropKind::Resource`
        // for unrecognised locals is the backstop.
        Place::ActorHandle(_)
        | Place::Local(_)
        | Place::ReturnSlot
        | Place::MachineTag(_)
        | Place::MachineVariant { .. }
        | Place::EnumTag(_)
        | Place::EnumVariant { .. } => DropKind::Resource,
    }
}

/// Materialize the ordinary typed drop ritual used by a crash-only
/// representation owner. The descriptor is deliberately kept out of lexical
/// drop plans: success transfers or retires the owner without releasing the
/// destination storage, while the dynamic crash registry may invoke this
/// ritual if execution is abandoned before that handoff.
///
/// Keeping construction here makes the drop-plan dispatcher the only
/// authority that maps the typed cleanup kind to an [`ElabDrop`]; codegen never
/// infers the ritual from a raw local type or runtime symbol name.
///
/// # Errors
///
/// Returns an error when the typed cleanup kind is incompatible with the
/// owner's resolved storage shape.
pub fn crash_only_cleanup_drop(
    place: Place,
    ty: &ResolvedTy,
    cleanup: ParamCrashCleanupKind,
) -> Result<ElabDrop, String> {
    match cleanup {
        ParamCrashCleanupKind::Bytes if matches!(ty, ResolvedTy::Bytes) => {
            let descriptor = ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: drop_kind_for(place, ty, None),
                guard: None,
            };
            debug_assert_eq!(
                descriptor.kind,
                DropKind::CowHeap {
                    release: crate::ownership::CowHeapRelease::Bytes,
                }
            );
            Ok(descriptor)
        }
        ParamCrashCleanupKind::Bytes => Err(format!(
            "bytes crash-cleanup owner at {place:?} carried incompatible type `{ty}`"
        )),
    }
}
/// RAII-1 opaque-resource close registry: `(opaque_type_name, "<Type>::<close>")`
/// for every single-slot `#[resource] #[opaque]` handle whose close is a USER
/// method.
///
/// Parallel to the typed resource-record lifecycle registry (which keys off
/// `record_layouts` for `#[resource]` RECORDS): a single-handle opaque
/// `#[resource]` has no record layout, so it is excluded from
/// resource-record lifecycles — that exclusion is exactly the W3.029 leak this
/// registry closes. The classifier (`classify_named`) consults it to route such
/// a handle to [`StateFieldCloneKind::Resource`] instead of the no-op-drop
/// `OpaqueHandle`, and codegen reads the carried symbol to call `close(self)`
/// on the owning aggregate's drop spine.
///
/// Built identically at the MIR admission gate (the lowering `Builder`) and at
/// `IrPipeline` construction (for codegen) from the same `opaque_handle_names`
/// and `type_classes`, so MIR and codegen classify a resource-bearing record
/// the same way (no drift). The `<short>::<method>` symbol matches
/// `declare_function`'s flattened `<Self>::<method>` mangling and the spelling
/// lifecycle registry / `resource_drop_fn` use.
///
/// This registry contains declarations, not resolved use-site types. A user
/// resource is therefore always registered even when its short name and close
/// method collide with a builtin runtime descriptor (`Receiver::close`,
/// `MonitorRef::close`, ...). The resolved type's builtin discriminator is the
/// sole authority that selects runtime-vs-user teardown later.
#[cfg(test)]
pub(super) fn resource_opaque_close_registry(
    type_classes: &hew_hir::TypeClassTable,
) -> Vec<(String, String)> {
    type_classes
        .opaque_resource_lifecycles()
        .map(|lifecycle| {
            (
                lifecycle.resource_declaration.full_path().to_string(),
                lifecycle.close_symbol.clone(),
            )
        })
        .collect()
}

pub(super) fn resource_drop_fn(
    ty: &ResolvedTy,
    type_classes: &hew_hir::TypeClassTable,
) -> Option<crate::model::DropFnSpec> {
    use hew_types::runtime_call::RuntimeDropDescriptor;
    match ty {
        ResolvedTy::CancellationToken => Some(crate::model::DropFnSpec::Runtime(
            RuntimeDropDescriptor::CancellationTokenRelease,
        )),
        ResolvedTy::Named {
            builtin: Some(builtin),
            ..
        } => RuntimeDropDescriptor::for_builtin(*builtin).map(crate::model::DropFnSpec::Runtime),
        ResolvedTy::Named {
            name,
            builtin: None,
            is_opaque,
            ..
        } => {
            if let Some(lifecycle) = type_classes.opaque_resource_lifecycle_for_type_name(name) {
                return Some(crate::model::DropFnSpec::UserClose(
                    lifecycle.close_symbol.clone(),
                ));
            }
            if *is_opaque {
                return None;
            }
            let class_entry = type_classes.get_key_value(name);
            class_entry.and_then(|(class_name, (_, close))| {
                close.as_ref().map(|method| {
                    crate::model::DropFnSpec::UserClose(format!("{class_name}::{method}"))
                })
            })
        }
        // Task<T> and all other types have no user-visible close method.
        _ => None,
    }
}
#[cfg(test)]
mod typed_resource_close_authority;
/// Place-aware override of the type-derived `drop_fn`.
///
/// `Place::LambdaActorHandle(N)` carries a `ResolvedTy::Named { name: "Duplex" }`
/// (the surface-visible type of an `actor |..| {..}` expression), but its
/// runtime release ritual is `hew_lambda_actor_release`, NOT `hew_duplex_close`.
/// The type-derived `resource_drop_fn` returns `"Duplex::close"` — which would
/// route through the wrong symbol at codegen — so override here when the Place
/// variant says "this is a lambda-actor handle, not a plain duplex".
///
/// `SendHalf`/`RecvHalf` override their `drop_fn` the same way: both map to
/// `hew_duplex_close_half` (with a direction discriminant the codegen call
/// site materialises from the Place variant), not the type-derived
/// `Duplex::close` / `Stream::close` / etc.
///
/// LESSONS: end-to-end-before-layer-thickening, lifecycle-symmetry.
fn place_aware_drop_fn(
    place: Place,
    ty_derived: Option<crate::model::DropFnSpec>,
) -> Option<crate::model::DropFnSpec> {
    use hew_types::runtime_call::RuntimeDropDescriptor;
    match place {
        Place::LambdaActorHandle(_) => Some(crate::model::DropFnSpec::Runtime(
            RuntimeDropDescriptor::LambdaActorHandleClose,
        )),
        Place::SendHalf(_) => Some(crate::model::DropFnSpec::Runtime(
            RuntimeDropDescriptor::SendHalfClose,
        )),
        Place::RecvHalf(_) => Some(crate::model::DropFnSpec::Runtime(
            RuntimeDropDescriptor::RecvHalfClose,
        )),
        _ => ty_derived,
    }
}
/// True when an owned `AffineResource` binding needs a path-sensitive
/// runtime drop-flag (#1933 / #1941).
///
/// The flag is needed ONLY when the binding's scope-exit release is a
/// non-idempotent user `#[resource]` close: a `DropKind::Resource` (the
/// generic close path, selected for a `Place::Local` user-resource value)
/// whose ritual resolves to a `DropFnSpec::UserClose` (an open-set
/// generated symbol, NOT a closed-set runtime descriptor). The M2 handle
/// classes — Duplex (`DropKind::DuplexClose`), half-handles
/// (`DropKind::DuplexHalfClose`), lambda-actor (`DropKind::LambdaActorRelease`),
/// and the `Runtime`-descriptor closes (`CancellationToken`, the builtin
/// stream/sink handles) — are refcounted or null-after-free at runtime, so
/// a double-close on a `MaybeConsumed` join is already a no-op for them and
/// no flag is allocated.
///
/// This is the single predicate keyed by all three flag sites (allocation
/// at the binding's introduction, the `Consume` set + `mark_binding_moved`
/// skip, and the recipe's `Guard` event attachment), so they cannot
/// drift on which bindings are flag-gated.
pub(super) fn affine_release_needs_drop_flag(
    place: Place,
    ty: &ResolvedTy,
    type_classes: &hew_hir::TypeClassTable,
) -> bool {
    if matches!(place, Place::Local(_) | Place::ReturnSlot)
        && matches!(
            ty,
            ResolvedTy::Named {
                builtin: Some(BuiltinType::Rc | BuiltinType::Weak),
                ..
            }
        )
    {
        return true;
    }
    // Check the close-ritual classification FIRST: `resource_drop_fn` /
    // `place_aware_drop_fn` never panic and return `UserClose` ONLY for a
    // user `#[resource]` Named type (an open-set generated symbol). Gating
    // on it here keeps `drop_kind_for` — which `expect`s a
    // `TraitObjectStorage` hint for a `ResolvedTy::TraitObject` and would
    // panic with the `None` we pass — off every dyn-trait / non-resource
    // binding. A `UserClose` ritual implies a `Place::Local` non-dyn value,
    // so the subsequent `drop_kind_for` call is panic-free and resolves to
    // `DropKind::Resource`.
    if !matches!(
        place_aware_drop_fn(place, resource_drop_fn(ty, type_classes)),
        Some(crate::model::DropFnSpec::UserClose(_))
    ) {
        return false;
    }
    matches!(drop_kind_for(place, ty, None), DropKind::Resource)
}
/// contributes; `Linear` is the move-checker's responsibility (`MustConsume`),
/// and other classes have no implicit drop.
///
/// The `binding_locals` map is consulted to resolve each owned-local's
/// real backend `Place`. A binding without an entry (function parameters
/// and other surfaces that don't populate `binding_locals`) does not
/// appear in `owned_locals` either today, so the `ReturnSlot` fallback
/// arm is structurally unreachable; it survives only as a fail-soft for
/// future surfaces that may extend `owned_locals` ahead of `binding_locals`.
/// Walk the RHS of a `let` (or the inner `value` of a `CoerceToDynTrait`)
/// to find the source `BindingId` whose ownership is being transferred
/// into the new local. Returns `None` for "fresh" value-producing shapes
/// (record constructors, call results, literals, etc.) that do not
/// reference an existing binding.
///
/// Used by the dyn-trait drop-suppression mechanism in two places:
///
/// 1. At `Instr::CoerceToDynTrait` producer sites (`lower_value` arm for
///    `HirExprKind::CoerceToDynTrait`) — when a concrete binding flows
///    into the fat-pointer constructor, its independent scope-exit drop
///    must be suppressed; the dyn binding's vtable slot-0 `drop_in_place`
///    is now the sole owner of the concrete's close ritual.
///
/// 2. At the `HirStmtKind::Let` arm for transitive dyn-to-dyn rebinds
///    (`let d2 = d1;`) — the source dyn binding's scope-exit drop must
///    be suppressed so the vtable ritual runs exactly once at the final
///    binding's scope exit, not once per intermediate rebind.
///
/// Transparent wrappers walked:
/// - `HirExprKind::Block` with a `tail` expression — recurse on the tail.
///
/// Every other expression shape (including `CoerceToDynTrait`, `Call*`,
/// `RecordCtor`, literals, arithmetic, etc.) materialises a fresh value
/// in a newly-allocated local that is not registered in `owned_locals`,
/// so returning `None` is the correct "nothing to suppress" answer.
pub(super) fn dyn_rebind_source_binding(value: &HirExpr) -> Option<BindingId> {
    match &value.kind {
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => Some(*id),
        HirExprKind::Block(block) => block.tail.as_deref().and_then(dyn_rebind_source_binding),
        _ => None,
    }
}
/// W3.031 Stage 1: classify the `TraitObjectStorage` discriminator for a
/// `let`-binding whose resolved type is `ResolvedTy::TraitObject`, based
/// on the shape of the RHS expression.
///
/// Returns `Ok(storage)` for the two recognised shapes, or `Err(reason)`
/// naming the unrecognised RHS form. The caller emits a
/// `TraitObjectStorageUndetermined` diagnostic on `Err`; the binding is
/// not added to `owned_locals`, so no drop is elaborated for it and the
/// pipeline aborts at the MIR boundary.
///
/// Recognised shapes:
/// - `HirExprKind::CoerceToDynTrait` → `HeapBoxed`. Codegen transfers the
///   concrete value into a `hew_dyn_box_alloc` buffer, so the fat pointer
///   remains valid across helper returns, suspension, and crash snapshots; its
///   drop runs slot 0 and releases that buffer.
/// - `HirExprKind::Call` / `CallTraitMethodStatic` / `CallDynMethod`
///   whose return type is `dyn Trait` → `HeapBoxed`. Returning a fat
///   pointer across a call boundary is only well-defined via the
///   `hew_dyn_box_alloc` heap-box ABI (W3.031 Stage 0); the receiver's
///   drop must run `drop_in_place` + `hew_dyn_box_free`.
/// - `HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. }`
///   where the referenced binding already carries a storage entry →
///   propagate the existing storage. This covers `let d2 = d1;` chains
///   without re-classifying the original RHS.
/// - `HirExprKind::Block` with a tail expression → recurse on the tail.
///
/// Every other shape returns `Err`. Future stages may extend the
/// classifier; until then the fail-closed boundary keeps drop
/// elaboration honest.
pub(super) fn classify_dyn_trait_storage(
    value: &HirExpr,
    dyn_trait_storage: &HashMap<BindingId, TraitObjectStorage>,
) -> Result<TraitObjectStorage, String> {
    match &value.kind {
        HirExprKind::CoerceToDynTrait { .. }
        | HirExprKind::Call { .. }
        | HirExprKind::CallTraitMethodStatic { .. }
        | HirExprKind::ResolvedImplCall { .. }
        | HirExprKind::CallDynMethod { .. } => Ok(TraitObjectStorage::HeapBoxed),
        HirExprKind::BindingRef { resolved, .. } => {
            if let ResolvedRef::Binding(id) = resolved {
                if let Some(storage) = dyn_trait_storage.get(id).copied() {
                    return Ok(storage);
                }
            }
            Err(format!(
                "HirExprKind::BindingRef to dyn Trait binding without a prior \
                 dyn_trait_storage entry (resolved: {resolved:?})"
            ))
        }
        HirExprKind::Block(block) => block.tail.as_deref().map_or_else(
            || {
                Err(
                    "HirExprKind::Block with no tail expression cannot produce a \
                     dyn Trait value"
                        .to_string(),
                )
            },
            |tail| classify_dyn_trait_storage(tail, dyn_trait_storage),
        ),
        other => Err(format!(
            "unrecognised RHS shape for dyn Trait binding: {:?}",
            std::mem::discriminant(other)
        )),
    }
}
/// Classify a fn-typed `let` RHS for closure-pair ownership. `literal_heap`
/// is the `pending_closure_literal_heap` verdict captured after lowering the
/// RHS — for `Block` shapes it describes the tail literal (the last closure
/// literal lowered in the RHS). Mirrors `classify_dyn_trait_storage`'s
/// shape dispatch; unrecognised shapes return `NotOwned` (fail-closed:
/// the binding leaks rather than risking a stack-env free).
pub(super) fn classify_closure_pair_rhs(
    value: &HirExpr,
    literal_heap: bool,
    owned: &HashSet<BindingId>,
) -> ClosurePairRhs {
    match &value.kind {
        HirExprKind::Closure { .. } => {
            if literal_heap {
                ClosurePairRhs::Owned
            } else {
                ClosurePairRhs::NotOwned
            }
        }
        // Vec element reads are BORROWS: the vec slot keeps ownership of the
        // element's pair box and env. Admitting a `fns.get(i)` result would
        // double-free against the descriptor-driven Vec release. `pop` transfers
        // ownership out of the vec (the marshalling frees the element box and
        // the popped pair keeps the env), so it stays on the Owned path.
        HirExprKind::ResolvedImplCall { target_symbol, .. }
            if target_symbol == "hew_vec_get_ptr" =>
        {
            ClosurePairRhs::NotOwned
        }
        HirExprKind::Call { .. }
        | HirExprKind::ResolvedImplCall { .. }
        | HirExprKind::CallTraitMethodStatic { .. }
        | HirExprKind::CallDynMethod { .. } => ClosurePairRhs::Owned,
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } if owned.contains(id) => ClosurePairRhs::TransferFrom(*id),
        HirExprKind::Block(block) => block
            .tail
            .as_deref()
            .map_or(ClosurePairRhs::NotOwned, |tail| {
                classify_closure_pair_rhs(tail, literal_heap, owned)
            }),
        _ => ClosurePairRhs::NotOwned,
    }
}
/// True when `ty` is the two-pointer closure-pair value (`fn(...) -> T`
/// surface type). The ABI-shape confirmation for `DropKind::ClosurePair`;
/// the ownership decision is the `closure_pair_owned` admission at the
/// binding's `let` (`classify_closure_pair_rhs`).
pub(super) fn ty_is_closure_pair(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. })
}

/// True when `name` is an `indirect enum` registered in `enum_layouts` — the
/// MIR-side mirror of the codegen `is_indirect_enum` authority. An indirect
/// enum is heap-boxed: every binding of the type holds a `ptr` to a tagged-union
/// node, so its scope-exit release is a `hew_dealloc` of that node (recursing
/// into owned child nodes), not an inline composite drop.
#[must_use]
fn name_is_indirect_enum(name: &str, enum_layouts: &[crate::model::EnumLayout]) -> bool {
    crate::model::find_enum_layout(name, &[], enum_layouts).is_some_and(|layout| layout.is_indirect)
}
/// True when `ty` is an `indirect enum` type.
#[must_use]
pub(super) fn ty_is_indirect_enum(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
) -> bool {
    matches!(ty, ResolvedTy::Named { name, .. } if name_is_indirect_enum(name, enum_layouts))
}
/// A human description of a `Vec` element with no wired per-element release, for
/// the compile diagnostic emitted by
/// [`Builder::unsupported_vec_element_diagnostics`]. Names indirectness
/// explicitly (an indirect enum is the common case) so the message is
/// actionable.
pub(super) fn describe_vec_element(
    elem: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
) -> String {
    match elem {
        ResolvedTy::Bytes => "`bytes` (a fat `{ ptr, len, cap }` triple)".to_string(),
        ResolvedTy::Named { name, .. } if ty_is_indirect_enum(elem, enum_layouts) => {
            format!("the indirect enum `{}`", short_name(name))
        }
        ResolvedTy::Named { name, .. } => format!("`{}`", short_name(name)),
        other => format!("`{other:?}`"),
    }
}
/// W5-011 P3 (Slice 2). Map a `CowValue` leaf type to its C-ABI runtime
/// release symbol for function-scope drop elaboration, or `None` for types
/// outside this scalar-leaf authority.
///
/// This picker is intentionally restricted to the single `string` leaf.
/// Aggregate/container shapes are not scalar leaves: their scope-exit releases
/// are admitted by the `binding_ty_is_*_vec`, structural composite, and local
/// collection authorities. Keeping them `None` here is delegation, not an
/// assertion that those values receive no release.
///
/// `Bytes` is deliberately absent TOO, but for a different reason: its
/// retain/co-owner derivation is its own authority
/// (`derive_local_bytes_drop_allowed`) with its own `drop_kind_for` arm —
/// keeping it out of this table keeps exactly ONE prover in charge of bytes
/// co-owner minting. Adding a `Bytes` arm here would make bytes
/// bindings candidates of `derive_cow_sole_owner` as well, creating a second,
/// union-admitting authority for the same drop (LESSONS:
/// boundary-fail-closed — one admission authority per drop class).
pub(super) fn cow_value_leaf_drop_symbol(ty: &ResolvedTy) -> Option<&'static str> {
    match ty {
        ResolvedTy::String => Some("hew_string_drop"),
        // Exhaustive (no `_ => None` fall-through): a new `ResolvedTy` variant is
        // a compile error here, never a silent "no scope-exit drop" miss for a
        // heap-owning leaf. Every other leaf is `None` for a documented reason —
        // it owns no heap, or its scope-exit drop is a DIFFERENT authority's job,
        // never an unenumerated guess:
        //   - `Bytes`: a fat `{ ptr, len, cap }` triple with its own dedicated
        //     retain authority (`derive_local_bytes_drop_allowed`) and
        //     `drop_kind_for` arm — kept out so exactly ONE prover owns bytes
        //     co-owner minting (a second union-admitting authority would risk
        //     a double-free; LESSONS: boundary-fail-closed).
        //   - `Named` containers/handles (`Vec`/`HashMap`/`HashSet`/`Generator`/
        //     records/enums): NOT scalar leaves — their drop is the
        //     `binding_ty_is_*_vec` / collection-handle release buckets of
        //     `drop_kind_for`, NOT this per-leaf symbol picker.
        //   - scalars / `Unit` / `Never` / views (`Pointer`/`Borrow`/`Slice`/
        //     `Array`) / `Function` / `Closure` / `TraitObject` / `Task` /
        //     `CancellationToken` / `TypeParam`: own no scalar heap leaf to drop
        //     here (closure pairs, tasks, and tokens have their own release
        //     paths).
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize
        | ResolvedTy::F32
        | ResolvedTy::F64
        | ResolvedTy::Bool
        | ResolvedTy::Char
        | ResolvedTy::Duration
        | ResolvedTy::Bytes
        | ResolvedTy::CancellationToken
        | ResolvedTy::Unit
        | ResolvedTy::Never
        | ResolvedTy::Tuple(_)
        | ResolvedTy::Array(_, _)
        | ResolvedTy::Slice(_)
        | ResolvedTy::Named { .. }
        | ResolvedTy::Function { .. }
        | ResolvedTy::Closure { .. }
        | ResolvedTy::Pointer { .. }
        | ResolvedTy::Borrow { .. }
        | ResolvedTy::TraitObject { .. }
        | ResolvedTy::Task(_)
        | ResolvedTy::TypeParam { .. } => None,
    }
}
/// True when an overridden owned record field in a functional-update
/// expression must be destructively released via the `RecordFieldDrop` op
/// (raw load → release → null-store) rather than the `RecordFieldLoad` +
/// `Instr::Drop` pair.
///
/// The predicate selects every COW-heap field whose release can be driven
/// through the field slot ITSELF — `string`, `Vec<T>`, `HashMap`, `HashSet`,
/// the `Generator` / `AsyncGenerator` companion handle, and `bytes`.
///
/// The distinguishing property is not "the value is one pointer" but "the
/// post-drop null-store lands in the OWNING slot". `RecordFieldDrop` GEPs the
/// live record field and nulls it after releasing; the `RecordFieldLoad` +
/// `Instr::Drop` alternative first copies the field into a temp local, so its
/// null-store poisons the COPY and leaves the owner still pointing at freed
/// memory. Functional update emits this release ahead of an assignment whose
/// own overwrite-drop targets the same field, so a field routed to the copying
/// path is released TWICE — the second release on a live pointer. That is a
/// double free, not a leak.
///
/// `bytes` is included even though it is a fat `{ ptr, offset, len }` triple: only
/// the data pointer participates in ownership, `hew_bytes_drop` takes exactly
/// that pointer, and it is null-tolerant, so nulling the triple's pointer
/// sub-field in place gives the same idempotency the single-pointer fields get.
/// Codegen's `lower_record_field_drop` reaches the triple's field 0 for both the
/// release argument and the poison store.
///
/// Dispatch is on the `builtin` discriminant, never the `name` string, so a
/// user-defined `type Vec { ... }` (`builtin: None`) is never mis-routed to a
/// runtime release symbol (LESSONS: boundary-fail-closed, checker-authority).
pub(super) fn field_override_uses_record_field_drop(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::Named {
                builtin: Some(
                    hew_types::BuiltinType::Vec
                        | hew_types::BuiltinType::HashMap
                        | hew_types::BuiltinType::HashSet
                        | hew_types::BuiltinType::Generator
                        | hew_types::BuiltinType::AsyncGenerator,
                ),
                ..
            }
    )
}
/// True when `ty` is a builtin `Generator<Y, R>` / `AsyncGenerator<Y>` handle.
/// Dispatches on the `builtin` discriminant (not the name string) so a user
/// `type Generator { ... }` is never mistaken for the runtime handle.
pub(super) fn ty_is_generator_handle(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Named {
            builtin: Some(
                hew_types::BuiltinType::Generator | hew_types::BuiltinType::AsyncGenerator
            ),
            ..
        }
    )
}
/// The runtime close descriptor for a `Stream<T>` / `Receiver<T>` for-await
/// cursor: `StreamClose` (→ `hew_stream_close`) or `ReceiverClose`
/// (→ `hew_channel_receiver_close`). `None` for any other type. Selecting the
/// typed [`RuntimeDropDescriptor`](hew_types::runtime_call::RuntimeDropDescriptor)
/// (rather than a `Release` string) matches the `rt(StreamClose)` the
/// function-exit drop plan already emits for these cursors, so the inline
/// scope-close and the exit-plan close codegen identically.
pub(super) fn stream_handle_drop_descriptor(
    ty: &ResolvedTy,
) -> Option<hew_types::runtime_call::RuntimeDropDescriptor> {
    let ResolvedTy::Named {
        builtin: Some(builtin),
        ..
    } = ty
    else {
        return None;
    };
    matches!(
        builtin,
        hew_types::BuiltinType::Stream | hew_types::BuiltinType::Receiver
    )
    .then(|| hew_types::runtime_call::RuntimeDropDescriptor::for_builtin(*builtin))
    .flatten()
}
/// Whether `ty` is a `Stream<T>` / `Receiver<T>` for-await cursor handle — the
/// registration gate for `scope_stream_bindings`, mirroring
/// [`ty_is_generator_handle`]. Single source of truth with
/// [`stream_handle_drop_descriptor`].
pub(super) fn ty_is_stream_handle(ty: &ResolvedTy) -> bool {
    stream_handle_drop_descriptor(ty).is_some()
}
/// The `vec`-field source expression of a HIR `VecIter { vec, idx }` cursor
/// struct-init, or `None` for any other expression.
///
/// The HIR-level counterpart of [`vec_iter_record_init_vec_source`] (which reads
/// the already-lowered `Instr::RecordInit`): consulted before lowering, so it
/// can inspect the SOURCE's syntactic shape — a bare place (`for v in x`), a
/// field/tuple projection (`for v in x.v`), or an rvalue (`for v in make()`).
pub(super) fn vec_iter_init_vec_source_expr(value: &HirExpr) -> Option<&HirExpr> {
    let HirExprKind::StructInit {
        fields, base: None, ..
    } = &value.kind
    else {
        return None;
    };
    if !matches!(
        &value.ty,
        ResolvedTy::Named {
            args,
            builtin: Some(hew_types::BuiltinType::VecIter),
            ..
        } if args.len() == 1
    ) {
        return None;
    }
    let mut source = fields
        .iter()
        .find(|(field, _)| field == "vec")
        .map(|(_, src)| src)?;
    while let HirExprKind::SubsumedValue { source: inner, .. } = &source.kind {
        source = inner;
    }
    Some(source)
}
/// True when a `for x in …` cursor `let __hew_for_iter = <value>` makes the
/// cursor the SOLE owner of the `Vec` handle in its `vec` field, so the cursor
/// (not a still-live source binding) must free that handle when its scope
/// closes. #1949.
///
/// The cursor owns iff its initialiser is a fresh `StructInit VecIter { vec, idx }`
/// whose `vec`-field source is NOT a still-live captured place binding:
///   - **rvalue source** (`for x in make_vec()`): the `vec` field is a call /
///     index / other rvalue — no source binding survives, the cursor owns.
///   - **`HashSet` source** (`for x in s`): the `vec` field is the set's
///     `to_vec()` snapshot — a FRESH Vec the cursor solely owns (the set stays
///     live as a separate handle).
///   - **place source** (`for x in v`): the `vec` field is a bare `BindingRef`
///     to a still-live source binding shared via `CowShare` (`IntentKind::Capture`
///     — the only site that emits it). The source binding keeps its own
///     owner (the cursor ingress is a borrow, not an escape), so the cursor
///     BORROWS and must NOT drop. Returns false.
///
/// A non-`StructInit` value (e.g. `for x in it`, where the cursor is a
/// whole-value MOVE of an already-bound `VecIter` — that bound cursor owns and
/// is registered at ITS own `StructInit` let) returns false here: the move
/// consumes the source and the existing owner's registration already covers the
/// single free. Returning false in every ambiguous shape is fail-closed (leak,
/// never double-free), matching the sibling drop authorities.
///
/// NOTE: a field/tuple projection rooted at an actor state field
/// (`for v in x.v`) reaches this through the non-place `vec` source and returns
/// true (owns) — but the projected leaf is state-owned. The registration site
/// therefore ALSO gates on [`Builder::vec_iter_source_projects_actor_state_field`]
/// (#2540) so such a cursor borrows, letting the unconditional state drop free
/// the leaf exactly once.
pub(super) fn vec_iter_let_cursor_owns_handle(value: &HirExpr) -> bool {
    let Some(vec_src) = vec_iter_init_vec_source_expr(value) else {
        return false;
    };
    // A bare `BindingRef` with `Capture` intent is the CowShare place source
    // (`for x in v`) — the source binding owns; the cursor borrows. Every other
    // `vec`-field producer (rvalue call/index, `to_vec()` MethodCall, a moved /
    // consumed source) leaves the cursor the sole owner.
    let borrows_live_place = matches!(
        &vec_src.kind,
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(_),
            ..
        }
    ) && vec_src.intent == IntentKind::Capture;
    !borrows_live_place
}
/// True when `ty` is the builtin `Vec<T>`. The owned-element decision lives in
/// `Builder::binding_ty_is_owned_element_vec` (which consults the function's
/// owned-element key set); this is only the ABI-shape confirmation that the
/// `hew_vec_free_owned` handle is a Vec.
pub(super) fn ty_is_vec(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::Vec),
            ..
        }
    )
}
/// True when `ty` is a builtin `Vec<T>` whose element is a closure pair
/// (`fn(...) -> T` / closure surface type). Such a vec owns each element's
/// pair box and, transitively, the pair's env box; its scope-exit release is
/// descriptor-driven `hew_vec_free_owned` (element walk + buffer + handle).
///
/// THE projection of the [`VecElementRelease::ClosurePair`] bucket for
/// contexts without `Builder` state (the drop-plan validator and
/// `drop_kind_for` are free fns): `ty_is_closure_pair_vec(Vec<E>)` ≡
/// `classify_vec_element_release(E).is_closure_pair()`, pinned over the
/// element domain by `release_bucket_partition_is_total_over_vec_elements`.
/// `Builder`-side consumers (the release-symbol pickers) read the
/// classification itself.
pub(super) fn ty_is_closure_pair_vec(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::Vec),
            args,
            ..
        } if args.first().is_some_and(ty_is_closure_pair)
    )
}
pub(super) fn builtin_method_arg_is_move_ingress(family: hew_types::MethodTargetFamily) -> bool {
    matches!(
        family,
        hew_types::MethodTargetFamily::HashMap(hew_types::HashMapMethod::Insert)
            | hew_types::MethodTargetFamily::HashSet(hew_types::HashSetMethod::Insert)
    )
}
/// True when `ty` is a builtin `HashMap<K, V>` / `HashSet<E>` handle. Dispatches
/// on the `builtin` discriminant (NOT the name string) so a user
/// `type HashMap { ... }` is never mistaken for the runtime collection handle.
/// THE single ABI-shape authority for the collection-handle release bucket:
/// the confirmation that `hew_hashmap_free_layout` / `hew_hashset_free_layout`
/// is the correct release for the binding's single owned handle. Whether the
/// owner is still live at a given exit is the ownership replay's decision,
/// not this predicate's.
///
/// A projection of the typed ownership classification:
/// `ty_is_local_collection_handle(ty)` ≡ "the decision's drop class is the
/// `HashMap` / `HashSet` copy-on-write leaf", pinned over the heap-leaf
/// domain (plus the user-Named collision negative) by
/// `collection_handle_predicate_projects_from_heap_leaf`, whose
/// release-symbol tripwire also pins the two `*_free_layout` spellings to
/// `HeapLeaf::release_symbol`. A future builtin collection is classified
/// once, in `classify_named`; the pin fails if this predicate does not
/// follow.
pub(super) fn ty_is_local_collection_handle(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Named {
            builtin: Some(BuiltinType::HashMap | BuiltinType::HashSet),
            ..
        }
    )
}
/// True when `callee` is a `HashMap` / `HashSet` runtime operation that BORROWS
/// its receiver (arg[0]) — i.e. reads / mutates the handle in place without
/// freeing it. The receiver of such a call is a transient interior read, NOT an
/// ownership escape, so the escape scans that consult this list skip arg[0]
/// (they still scan arg[1..], which carry by-value keys / elements that
/// genuinely flow elsewhere).
///
/// This is an EXPLICIT allow-list, deliberately NOT a `hew_hashmap_` /
/// `hew_hashset_` prefix test (LESSONS: `boundary-fail-closed`). The consuming
/// release `hew_hashmap_free_layout` / `hew_hashset_free_layout` and the
/// constructors `*_new_with_layout` (which write a fresh handle, never read an
/// existing one as arg[0]) are intentionally absent: a future runtime op that
/// consumes its receiver must be classified here deliberately. An op left out of
/// this list is treated as a receiver ESCAPE, which over-excludes the binding
/// from the borrow exemption — a leak, never a double-free. Every entry below is
/// confirmed against the runtime signature to take the handle by shared/mutable
/// borrow (`*const` / `*mut`, never freed): `hew-runtime/src/hashmap.rs`,
/// `hew-runtime/src/hashset.rs`.
/// True when every source-Place reference to `binder` in `term` is the borrowed
/// receiver (arg[0]) of a collection-borrow call — i.e. reading `binder` here is
/// an interior borrow, not an ownership escape of the composite that owns it. A
/// borrowing STRING call (or `print`/`println`) is also drop-safe via
/// [`retained_string_terminator_drop_safe`]; this extends that exemption to the
/// collection-borrow family so a `Vec`/`HashMap` field/element binder read by
/// `.len()` / `.get(i)` / `m.contains_key(..)` keeps its composite's in-place
/// drop. Conservative: `false` (treat as escape) unless the read is provably the
/// borrowed receiver, mirroring the collection-LOCAL scan's arg[0] exemption.
pub(super) fn binder_read_is_borrow_safe_terminator(
    term: &Terminator,
    suspend_kind: Option<&SuspendKind>,
    binder: u32,
) -> bool {
    // The string-borrow / print exemption already covers its callees.
    if retained_string_terminator_drop_safe(term, suspend_kind, binder) {
        return true;
    }
    // A collection-borrow or bytes-receiver-borrow call borrows arg[0] in
    // place; only a read in the by-value tail (arg[1..]) genuinely escapes.
    // `binder` is borrow-safe iff it never appears past arg[0]. The bytes
    // contract is the SAME authority `derive_local_bytes_drop_allowed`
    // applies to a bytes LOCAL — a `bytes` enum-payload / field binder read
    // by `b.len()` / `b.is_empty()` (`hew_bytes_len` et al) is a transient
    // receiver borrow, not an ownership escape of the owning composite
    // (#2429: classifying it as an escape excluded every
    // `Result<bytes, _>` composite from its `EnumInPlace` drop and leaked
    // the payload on every loop iteration).
    if let Terminator::Call { callee, args, .. } = term {
        let contract = crate::runtime_symbols::callee_ownership_contract(callee);
        if contract.borrows_collection_binder_receiver() || contract.borrows_bytes_receiver() {
            return !args
                .iter()
                .skip(1)
                .any(|arg| place_refs_local(*arg, binder));
        }
        // `hew_bytes_append` borrows the receiver AND the unpacked source
        // triple — every operand is a read-only borrow, so a binder read
        // anywhere in the argument list stays owned by its composite.
        if contract.borrows_all_bytes_args() {
            return true;
        }
    }
    false
}
/// A `string` payload binder read as an ARGUMENT of a call that borrows its
/// string arguments is a transient borrow, not an ownership escape of the
/// composite that owns the buffer.
///
/// [`retained_string_terminator_drop_safe`] already exempts the runtime
/// `borrows_string_call_args` family plus the four print sinks, but it has no
/// view of the enclosing module so it cannot see the LARGER borrowing
/// population: every Hew-bodied function. `lower_params` ratifies that a
/// by-value `string` parameter is a caller-owned BORROW (it is registered in
/// `borrowed_string_param_locals` — `param_consume` is seeded only for
/// `is_user_resource_ty` params, and `snapshot_root_outside_carrier_protocol`
/// keeps `String` out of the owned-carrier protocol), so such a callee releases
/// nothing and the caller keeps the sole drop obligation.
///
/// Without this, `match mk() { .Some(s) => println(f"v={s}") }` classified `s` as
/// escaped — the f-string lowers to `string::fmt(s)`, a Hew-bodied stdlib
/// `impl Display for string` — and the whole `Option<string>` composite lost its
/// `EnumInPlace` drop, leaking the payload on every iteration.
///
/// Deliberately narrow, three conjunctive gates:
/// - the binder's registered type is exactly `ResolvedTy::String`. The shared
///   [`retained_string_terminator_drop_safe`] is also invoked with WHOLE-composite
///   locals, where a Hew-bodied callee may legitimately CONSUME the composite;
///   restricting to the leaf `string` type is what makes the `lower_params`
///   borrow rule applicable.
/// - [`string_call_borrows`] proves the callee borrows rather than consumes,
///   which vetoes every known runtime symbol lacking an explicit borrow row and
///   every symbol this module cannot see (`extern` FFI).
/// - every source-Place read of the binder in this terminator is an argument of
///   that call — a binder that also flows out as, say, a `Send` payload is not
///   covered here.
///
/// Conservative: `false` (treat as escape) whenever any gate is undecided. That
/// leaks, exactly as before, and can never double-release.
///
/// LESSONS: boundary-fail-closed (P0), cleanup-all-exits.
pub(super) fn string_binder_read_is_user_fn_borrow(
    term: &Terminator,
    suspend_kind: Option<&SuspendKind>,
    binder: u32,
    binder_ty: Option<&ResolvedTy>,
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
    extern_contracts: &crate::return_provenance::ExternContractTable,
) -> bool {
    if !matches!(binder_ty, Some(ResolvedTy::String)) {
        return false;
    }
    let Terminator::Call { callee, args, .. } = term else {
        return false;
    };
    if !string_call_borrows(
        callee,
        module_fn_names,
        module_generic_fn_names,
        extern_contracts,
    ) {
        return false;
    }
    // Every read of the binder here must be one of the borrowed arguments; a
    // read reaching the terminator through any other operand is unclassified.
    let reads_outside_args = terminator_source_places(term, suspend_kind)
        .into_iter()
        .filter(|place| place_refs_local(*place, binder))
        .any(|place| !args.contains(&place));
    !reads_outside_args && args.iter().any(|arg| place_refs_local(*arg, binder))
}
/// Instruction analogue of [`binder_read_is_borrow_safe_terminator`]. The `xs[i]`
/// bounds-check + getter path lowers `hew_vec_len` / `hew_vec_get_*` as
/// `Instr::CallRuntimeAbi` (not a terminator), so a field/element binder read
/// there must get the SAME arg[0] receiver-borrow exemption. Conservative:
/// `false` unless the only references to `binder` are the borrowed receiver.
pub(super) fn binder_read_is_borrow_safe_instr(instr: &Instr, binder: u32) -> bool {
    if matches!(
        instr,
        Instr::ValueSnapshotClone { src, .. } if place_refs_local(*src, binder)
    ) {
        return true;
    }
    if matches!(
        instr,
        Instr::IntCmp { lhs, rhs, .. }
            if place_refs_local(*lhs, binder) || place_refs_local(*rhs, binder)
    ) {
        return true;
    }
    if let Instr::CallRuntimeAbi(call) = instr {
        let contract = crate::runtime_symbols::callee_ownership_contract(call.symbol());
        if contract.borrows_string_call_args() {
            return true;
        }
        if contract.borrows_collection_binder_receiver() || contract.borrows_bytes_receiver() {
            return !call
                .args()
                .iter()
                .skip(1)
                .any(|arg| place_refs_local(*arg, binder));
        }
        // Bytes append: receiver and unpacked source triple are all borrows
        // (mirrors the terminator arm and the bytes-LOCAL scan's exemption).
        if contract.borrows_all_bytes_args() {
            return true;
        }
    }
    false
}
/// The block and instruction index of the indirect closure call whose unwind
/// edge `callee` names, when the block calls one inline.
fn closure_call_prefix<'a>(
    blocks: &'a [BasicBlock],
    block_id: u32,
    callee: &str,
) -> Option<(&'a BasicBlock, usize)> {
    blocks
        .iter()
        .find(|block| block.id == block_id)
        .and_then(|block| {
            block
                .instructions
                .iter()
                .position(|instruction| {
                    matches!(
                        instruction,
                        Instr::CallClosure { call_site, .. }
                            if crate::model::indirect_closure_callee(*call_site) == callee
                    )
                })
                .map(|position| (block, position))
        })
}

/// Reproduce the owner set visible at one exit program point. Terminator exits
/// observe the completed block; entry cancellation observes the pre-prologue
/// state; an indirect closure unwind observes the prefix immediately before
/// its site-keyed instruction.
fn exact_owner_state_for_exit(
    exit: &ExitPath,
    blocks: &[BasicBlock],
    entries: &HashMap<u32, ExactOwnerState>,
    exits: &HashMap<u32, ExactOwnerState>,
) -> ExactOwnerState {
    let block_id = exit_block_id(exit);
    if matches!(exit, ExitPath::Cancel { block } if *block == ENTRY_BLOCK_ID) {
        return entries.get(&block_id).cloned().unwrap_or_default();
    }
    if let ExitPath::Unwind { callee, .. } = exit {
        if let Some((block, position)) =
            blocks
                .iter()
                .find(|block| block.id == block_id)
                .and_then(|block| {
                    block
                        .instructions
                        .iter()
                        .position(|instruction| {
                            matches!(
                                instruction,
                                Instr::CallClosure { call_site, .. }
                                    if crate::model::indirect_closure_callee(*call_site) == *callee
                            )
                        })
                        .map(|position| (block, position))
                })
        {
            let mut state = entries.get(&block_id).cloned().unwrap_or_default();
            apply_exact_owner_ops(&block.instructions[..position], &mut state);
            return state;
        }
    }
    exits.get(&block_id).cloned().unwrap_or_default()
}

/// Reproduce the generations that can be live at an exit on at least one
/// incoming path. This mirrors [`exact_owner_state_for_exit`], including the
/// pre-prologue entry-cancel and instruction-prefix unwind program points.
fn maybe_owner_state_for_exit(
    exit: &ExitPath,
    blocks: &[BasicBlock],
    entries: &HashMap<u32, MaybeOwnerState>,
    exits: &HashMap<u32, MaybeOwnerState>,
) -> MaybeOwnerState {
    let block_id = exit_block_id(exit);
    if matches!(exit, ExitPath::Cancel { block } if *block == ENTRY_BLOCK_ID) {
        return entries.get(&block_id).cloned().unwrap_or_default();
    }
    if let ExitPath::Unwind { callee, .. } = exit {
        if let Some((block, position)) = closure_call_prefix(blocks, block_id, callee) {
            let mut state = entries.get(&block_id).cloned().unwrap_or_default();
            apply_maybe_owner_ops(&block.instructions[..position], &mut state);
            return state;
        }
    }
    exits.get(&block_id).cloned().unwrap_or_default()
}

/// Exact generations that must be destroyed when one exit is taken.
///
/// This is the single continuation rule shared by elaboration and final
/// validation. It consumes only Checked-MIR ownership operations: ordinary
/// normal successors use their exact entry state, and terminal/unwind/abandon
/// exits retain every live non-return owner. A `Goto` never discharges: every
/// generation live at the source is carried into the target by replay, and
/// `validate_ownership_events` requires the source-side `EdgeCarry` witness
/// for each one, so a lexical `break` cannot launder a body-local generation
/// across a join without its explicit release. Planning a drop for a
/// generation the target still sees as live would execute it twice.
fn exact_required_owners_for_exit(
    exit: &ExitPath,
    blocks: &[BasicBlock],
    entries: &HashMap<u32, ExactOwnerState>,
    exits: &HashMap<u32, ExactOwnerState>,
) -> ExactOwnerState {
    let live = exact_owner_state_for_exit(exit, blocks, entries, exits);
    live.into_iter()
        .filter(|(owner, place)| {
            if matches!(place, Place::ReturnSlot) {
                return false;
            }
            let continues = match exit {
                ExitPath::Call { next, .. }
                | ExitPath::Send { next, .. }
                | ExitPath::Ask { next, .. }
                | ExitPath::Select { next, .. }
                | ExitPath::Join { next, .. } => entries
                    .get(next)
                    .is_some_and(|state| state.get(owner) == Some(place)),
                ExitPath::Goto { .. } | ExitPath::Branch { .. } => true,
                ExitPath::Return { .. }
                | ExitPath::Unwind { .. }
                | ExitPath::Panic { .. }
                | ExitPath::Cancel { .. }
                | ExitPath::Yield { .. }
                | ExitPath::Suspend { .. } => false,
            };
            !continues
        })
        .collect()
}

type GuardedOwnerGroups = HashMap<BindingId, HashMap<Place, HashSet<crate::model::OwnerId>>>;

/// An exit that leaves the function body without a normal continuation:
/// every generation still live there must be discharged by the exit plan.
fn abandonment_exit(exit: &ExitPath) -> bool {
    matches!(
        exit,
        ExitPath::Return { .. }
            | ExitPath::Panic { .. }
            | ExitPath::Unwind { .. }
            | ExitPath::Cancel { .. }
            | ExitPath::Yield { .. }
            | ExitPath::Suspend { .. }
    )
}

fn guarded_owner_groups_for_exit(
    exit: &ExitPath,
    blocks: &[BasicBlock],
    maybe_entries: &HashMap<u32, MaybeOwnerState>,
    maybe_exits: &HashMap<u32, MaybeOwnerState>,
    guarded_owners: &HashSet<crate::model::OwnerId>,
) -> GuardedOwnerGroups {
    if !abandonment_exit(exit) {
        return HashMap::new();
    }

    let maybe_live = maybe_owner_state_for_exit(exit, blocks, maybe_entries, maybe_exits);
    let mut groups = GuardedOwnerGroups::new();
    for (owner, place) in maybe_live {
        if guarded_owners.contains(&owner) && !matches!(place, Place::ReturnSlot) {
            groups
                .entry(owner.binding)
                .or_default()
                .entry(place)
                .or_default()
                .insert(owner);
        }
    }
    groups
}

fn ambiguous_guarded_owners_for_exit(
    exit: &ExitPath,
    blocks: &[BasicBlock],
    maybe_entries: &HashMap<u32, MaybeOwnerState>,
    maybe_exits: &HashMap<u32, MaybeOwnerState>,
    guarded_owners: &HashSet<crate::model::OwnerId>,
) -> Vec<(BindingId, Vec<(crate::model::OwnerId, Place)>)> {
    let mut ambiguous =
        guarded_owner_groups_for_exit(exit, blocks, maybe_entries, maybe_exits, guarded_owners)
            .into_iter()
            .filter_map(|(binding, places)| {
                let mut candidates = places
                    .into_iter()
                    .flat_map(|(place, owners)| owners.into_iter().map(move |owner| (owner, place)))
                    .collect::<Vec<_>>();
                candidates.sort_by_key(|(owner, _)| *owner);
                (candidates.len() != 1).then_some((binding, candidates))
            })
            .collect::<Vec<_>>();
    ambiguous.sort_by_key(|(binding, _)| *binding);
    ambiguous
}

/// Extend must-live cleanup authority with a narrowly typed conditional owner.
///
/// Every [`crate::model::OwnershipEvent::Guard`] publishes the runtime
/// discriminator for one exact owner generation. At a terminal join, the
/// must-owner intersection intentionally loses an owner that is live on only
/// some incoming paths, but the may-owner union plus that definition-site
/// guard proves a guarded destructor is both necessary on the live path and
/// suppressed on the transferred path. No continuation exit receives this
/// authority, and an unguarded or ambiguously generated owner remains
/// excluded.
fn guarded_required_owners_for_exit(
    exit: &ExitPath,
    blocks: &[BasicBlock],
    exact_entries: &HashMap<u32, ExactOwnerState>,
    exact_exits: &HashMap<u32, ExactOwnerState>,
    maybe_entries: &HashMap<u32, MaybeOwnerState>,
    maybe_exits: &HashMap<u32, MaybeOwnerState>,
    guarded_owners: &HashSet<crate::model::OwnerId>,
) -> ExactOwnerState {
    let mut required = exact_required_owners_for_exit(exit, blocks, exact_entries, exact_exits);
    for (_, places) in
        guarded_owner_groups_for_exit(exit, blocks, maybe_entries, maybe_exits, guarded_owners)
    {
        if places.len() != 1 {
            continue;
        }
        let (place, owners) = places
            .into_iter()
            .next()
            .expect("one guarded place was proven above");
        if owners.len() != 1 {
            continue;
        }
        let owner = owners
            .into_iter()
            .next()
            .expect("one guarded owner generation was proven above");
        required.entry(owner).or_insert(place);
    }
    required
}

/// Enumerate every exit edge of the Raw-MIR CFG, together with the
/// compatibility `ElabBlock` projection.
///
/// This names program points only. A `BlockKind::Cleanup` block is emitted
/// for every `Trap` terminator and filled from the matching `Panic` plan by
/// `synchronize_cleanup_blocks`; the plans themselves are derived from
/// ownership replay by [`derive_drop_plans_from_replay`].
#[allow(
    clippy::too_many_lines,
    reason = "a flat match over every Terminator variant; the line count is the variant count"
)]
pub(super) fn enumerate_exits(
    blocks: &[BasicBlock],
    cancellation_blocks: &HashSet<u32>,
) -> (Vec<ElabBlock>, Vec<ExitPath>) {
    // Cleanup-block ids start past the highest normal block id so the
    // single-block-era invariants stay intact.
    let max_normal_id = blocks.iter().map(|b| b.id).max().unwrap_or(0);
    let mut elab_blocks: Vec<ElabBlock> = blocks
        .iter()
        .map(|b| ElabBlock {
            id: b.id,
            kind: BlockKind::Normal,
            drops: Vec::new(),
            successor: None,
        })
        .collect();
    let mut next_cleanup_id = max_normal_id.saturating_add(1);
    let mut exits: Vec<ExitPath> = Vec::new();

    for block in blocks {
        let block_id = block.id;
        let exit = match &block.terminator {
            // A semantic unreachable is not a language-visible exit. Do not
            // reinterpret it as `Panic`/`Trap`, and do not run ownership
            // cleanup for a path the compiler has proved impossible. The
            // normal ElabBlock was still constructed above so this stage
            // preserves the Raw-MIR CFG identity explicitly.
            Terminator::Unreachable => continue,
            Terminator::Return => ExitPath::Return { block: block_id },
            // Lexical releases are executable Drop+Release operations at a
            // first-class ScopeExit program point; every generation that
            // survives a Goto edge is named by EdgeCarry.
            Terminator::Goto { target } => ExitPath::Goto {
                block: block_id,
                target: *target,
            },
            Terminator::Branch {
                cond: _,
                then_target,
                else_target,
            } => ExitPath::Branch {
                block: block_id,
                then_target: *then_target,
                else_target: *else_target,
            },
            Terminator::Call { callee, next, .. } => ExitPath::Call {
                block: block_id,
                callee: callee.clone(),
                next: *next,
            },
            // Trap is terminal: its cleanup block has no successor.
            Terminator::Trap { .. } => {
                let cleanup_id = next_cleanup_id;
                next_cleanup_id = next_cleanup_id.saturating_add(1);
                elab_blocks.push(ElabBlock {
                    id: cleanup_id,
                    kind: BlockKind::Cleanup,
                    drops: Vec::new(),
                    successor: None,
                });
                ExitPath::Panic { block: block_id }
            }
            // Generator-body `yield`: codegen fires this plan only on the
            // yield's destroy-while-parked edge, never on resume.
            Terminator::Yield { value: _, next } => ExitPath::Yield {
                block: block_id,
                next: *next,
            },
            // Generator construction is a synchronous runtime call with a
            // single `next` continuation; the constructed value's own drop is
            // scheduled by its owner's recipe, not here.
            Terminator::MakeGenerator { next, .. } => ExitPath::Call {
                block: block_id,
                callee: "hew_cont_frame_alloc".to_string(),
                next: *next,
            },
            // Lambda-actor construction is structurally identical to
            // MakeGenerator.
            Terminator::MakeLambdaActor { next, .. } => ExitPath::Call {
                block: block_id,
                callee: "hew_lambda_actor_new".to_string(),
                next: *next,
            },
            // `actor` is a Place; the ExitPath::Send slot carries the callee
            // name and no construction surface fills it — empty placeholder.
            Terminator::Send { next, .. } => ExitPath::Send {
                block: block_id,
                actor: String::new(),
                next: *next,
            },
            Terminator::Ask { actor, next, .. } | Terminator::RemoteAsk { actor, next, .. } => {
                ExitPath::Ask {
                    block: block_id,
                    actor: *actor,
                    next: *next,
                }
            }
            // Per-arm select-loser cleanup lives at the codegen dispatch site
            // (it needs the resource and the runtime registration id, not a
            // `place + drop_fn` pair), so the function-wide plan for a Select
            // exit carries only ordinary owner cleanup. See
            // `hew-runtime::stream` for the StreamNext cancel ABI.
            Terminator::Select { arms: _, next } => ExitPath::Select {
                block: block_id,
                next: *next,
            },
            // `join { }` — the wait-ALL sibling of `Select`; per-branch
            // cancel-rest cleanup is codegen-owned exactly as for `Select`.
            Terminator::Join {
                branches: _,
                result: _,
                next,
            } => ExitPath::Join {
                block: block_id,
                next: *next,
            },
            // Stackless suspend points. The plan fires on the case-1 destroy
            // edge (a parked continuation destroyed without resuming), before
            // the frame free, and only there. `SuspendingScopeDeadline` and
            // `SuspendingSelect` share that abandon-edge posture; their
            // scoped-children / observer teardown stays codegen-owned.
            Terminator::Suspend {
                resume, cleanup, ..
            }
            | Terminator::SuspendingScopeDeadline {
                resume, cleanup, ..
            }
            | Terminator::SuspendingSelect {
                resume, cleanup, ..
            } => ExitPath::Suspend {
                block: block_id,
                resume: *resume,
                cleanup: *cleanup,
            },
        };
        if !matches!(
            &block.terminator,
            Terminator::Call { authority, .. } if authority.is_no_return()
        ) {
            exits.push(exit);
        }
        if let Terminator::Call { callee, .. } = &block.terminator {
            exits.push(ExitPath::Unwind {
                block: block_id,
                callee: callee.clone(),
            });
        }
        // An instruction-level may-unwind edge observes ownership immediately
        // before that instruction (`exact_owner_state_for_exit` replays the
        // operation prefix), so a temporary owner stays live on the
        // exceptional edge even when its normal-path Release follows later in
        // the same block.
        for call_site in block
            .instructions
            .iter()
            .filter_map(|instruction| match instruction {
                Instr::CallClosure { call_site, .. } => Some(*call_site),
                _ => None,
            })
        {
            exits.push(ExitPath::Unwind {
                block: block_id,
                callee: crate::model::indirect_closure_callee(call_site),
            });
        }
        if block
            .instructions
            .iter()
            .any(|instruction| matches!(instruction, Instr::GeneratorNext { .. }))
        {
            // `Builder::push_instr` seals a GeneratorNext as the last
            // operation of its block, so the block exit state is the
            // pre-resume owner state on the `hew_cont_resume` unwind edge.
            exits.push(ExitPath::Unwind {
                block: block_id,
                callee: "hew_cont_resume".to_string(),
            });
        }
        if cancellation_blocks.contains(&block_id) {
            // A `FunctionEntry` cooperate site (always block 0) fires in the
            // prologue, before this block's instructions run, so its plan
            // reads the block ENTRY state; a `LoopBackEdge` site fires after
            // the block body and reads the EXIT state. `exact_owner_state_for_exit`
            // encodes that distinction from the exit's block id alone.
            exits.push(ExitPath::Cancel { block: block_id });
        }
    }
    (elab_blocks, exits)
}

#[cfg(test)]
mod field_drop_in_place_verifier {
    //! Structural tests for `validate_field_drop_in_place` — the pairing
    //! verifier for the field-addressed in-place drop op. The inline-composite
    //! pairing rule is the load-bearing one: exactly-once parent suppression
    //! is the op's WHOLE idempotence story for shapes with no null-store, so
    //! an inline-composite `ty` whose base still receives a composite
    //! in-place drop must be a verify error, never a silent double-free.
    use super::*;
    use crate::model::FieldAddr;

    fn rec_ty() -> ResolvedTy {
        ResolvedTy::named_user("Rec", vec![])
    }

    fn inner_ty() -> ResolvedTy {
        ResolvedTy::named_user("Inner", vec![])
    }

    fn one_block(instructions: Vec<Instr>) -> Vec<BasicBlock> {
        vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions,
            terminator: Terminator::Return,
        }]
    }

    fn elab_with_drops(drops: Vec<ElabDrop>) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: "synthetic".to_string(),
            key: crate::model::MirCallableKey::for_test("synthetic"),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans: vec![(ExitPath::Return { block: 0 }, DropPlan { drops })],
            coroutine: None,
            lambda_captures: vec![],
        }
    }

    fn composite_drop_on(local: u32, ty: ResolvedTy) -> ElabDrop {
        ElabDrop {
            place: Place::Local(local),
            ty,
            drop_fn: None,
            kind: DropKind::RecordInPlace,
            guard: None,
        }
    }

    fn validate(
        instrs: Vec<Instr>,
        drops: Vec<ElabDrop>,
        locals: &[ResolvedTy],
        admissible: bool,
    ) -> Vec<MirCheck> {
        let admit = move |_: &ResolvedTy| admissible;
        validate_field_drop_in_place(
            &one_block(instrs),
            &elab_with_drops(drops),
            locals,
            &[],
            &admit,
        )
    }

    fn field_drop(base: u32, field: FieldAddr, ty: ResolvedTy) -> Instr {
        Instr::FieldDropInPlace {
            base: Place::Local(base),
            field,
            ty,
        }
    }

    /// The pairing rule fires: an inline-composite field release whose base
    /// STILL receives a composite in-place drop is a verify error (the
    /// composite walk would re-free the field's leaves).
    #[test]
    fn inline_composite_with_paired_composite_drop_is_rejected() {
        let findings = validate(
            vec![field_drop(0, FieldAddr::Record(FieldOffset(0)), inner_ty())],
            vec![composite_drop_on(0, rec_ty())],
            &[rec_ty()],
            true,
        );
        assert_eq!(
            findings.len(),
            1,
            "an unsuppressed composite drop paired with an inline-composite \
             FieldDropInPlace must be exactly one verify error; got {findings:?}"
        );
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("composite in-place drop"),
            "the finding must name the unsuppressed composite drop; got: {reason}"
        );
    }

    /// With the base's composite drop suppressed (absent from every plan),
    /// the same op verifies clean.
    #[test]
    fn inline_composite_with_suppressed_composite_drop_is_accepted() {
        let findings = validate(
            vec![field_drop(0, FieldAddr::Record(FieldOffset(0)), inner_ty())],
            vec![],
            &[rec_ty()],
            true,
        );
        assert!(
            findings.is_empty(),
            "suppressed base composite drop satisfies the pairing rule; got \
             {findings:?}"
        );
    }

    /// A composite drop on a DIFFERENT local does not violate the pairing —
    /// the rule keys on the op's base local, not on plan non-emptiness.
    #[test]
    fn composite_drop_on_other_local_is_not_a_pairing_violation() {
        let findings = validate(
            vec![field_drop(0, FieldAddr::Record(FieldOffset(0)), inner_ty())],
            vec![composite_drop_on(1, rec_ty())],
            &[rec_ty(), rec_ty()],
            true,
        );
        assert!(
            findings.is_empty(),
            "a composite drop on an unrelated local is not a pairing \
             violation; got {findings:?}"
        );
    }

    /// A `string`-typed release carries the null-store postcondition, so a
    /// structurally reachable composite re-walk observes null and
    /// short-circuits — no pairing obligation.
    #[test]
    fn string_field_release_tolerates_base_composite_drop() {
        let findings = validate(
            vec![field_drop(
                0,
                FieldAddr::Record(FieldOffset(0)),
                ResolvedTy::String,
            )],
            vec![composite_drop_on(0, rec_ty())],
            &[rec_ty()],
            false,
        );
        assert!(
            findings.is_empty(),
            "a string field release null-stores its slot; the base composite \
             drop may stay; got {findings:?}"
        );
    }

    /// A field type neither `string` nor classifier-admitted is a verify
    /// error — the codegen dispatcher has no in-place release for it.
    #[test]
    fn inadmissible_field_ty_is_rejected() {
        let findings = validate(
            vec![field_drop(
                0,
                FieldAddr::Record(FieldOffset(0)),
                ResolvedTy::Slice(Box::new(ResolvedTy::I64)),
            )],
            vec![],
            &[rec_ty()],
            false,
        );
        assert_eq!(
            findings.len(),
            1,
            "an inadmissible field type must be exactly one verify error; \
             got {findings:?}"
        );
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("classifier"),
            "the finding must name the classifier refusal; got: {reason}"
        );
    }

    /// A `Tuple` field address on a record-typed base is a verify error.
    #[test]
    fn tuple_address_on_non_tuple_base_is_rejected() {
        let findings = validate(
            vec![field_drop(0, FieldAddr::Tuple(0), ResolvedTy::String)],
            vec![],
            &[rec_ty()],
            false,
        );
        assert_eq!(
            findings.len(),
            1,
            "a Tuple address on a record local must be a verify error; got \
             {findings:?}"
        );
    }

    /// A `Record` field address on a tuple-typed base is a verify error.
    #[test]
    fn record_address_on_non_record_base_is_rejected() {
        let findings = validate(
            vec![field_drop(
                0,
                FieldAddr::Record(FieldOffset(0)),
                ResolvedTy::String,
            )],
            vec![],
            &[ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64])],
            false,
        );
        assert_eq!(
            findings.len(),
            1,
            "a Record address on a tuple local must be a verify error; got \
             {findings:?}"
        );
    }

    /// The matching addresses verify clean on their own shapes (record
    /// address on record base above; tuple address on tuple base here).
    #[test]
    fn tuple_address_on_tuple_base_is_accepted() {
        let findings = validate(
            vec![field_drop(0, FieldAddr::Tuple(0), ResolvedTy::String)],
            vec![],
            &[ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64])],
            false,
        );
        assert!(
            findings.is_empty(),
            "a Tuple address on a tuple local is the matching shape; got \
             {findings:?}"
        );
    }
}
#[cfg(test)]
mod drop_admission_type_shape_pins;
#[cfg(test)]
mod twin_gate_classifier {
    //! #2648 (S3) — direct unit tests of the #2523 projected-payload twin
    //! classifier (`classify_scrutinee_origin`). The precise-freshness arms
    //! (Group B aggregate constructors, Group C1 `Binary`, the Group A `Call`
    //! interim PARAM reject) are unreachable through full lowering in current
    //! code — a temporary aggregate scrutinee hits an upstream "non-BitCopy match
    //! destructure on temporary scrutinee" NYI, and every real collection getter
    //! lowers to the fresh-owner clone choke — so the classifier's reject/admit
    //! verdict is pinned here directly on synthetic HIR. Exact-value assertions
    //! (the precise origin variant), fail-closed by default.
    use super::*;
    use hew_hir::HirProducedValueProducer;

    fn expr(kind: HirExprKind, ty: ResolvedTy) -> HirExpr {
        HirExpr {
            node: HirNodeId(u32::MAX),
            site: SiteId(u32::MAX),
            ty,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Read,
            kind,
            span: 0..0,
        }
    }

    fn binding_ref(name: &str, id: u32, ty: ResolvedTy) -> HirExpr {
        expr(
            HirExprKind::BindingRef {
                name: name.to_string(),
                resolved: ResolvedRef::Binding(BindingId(id)),
            },
            ty,
        )
    }

    /// A `ParamOwnershipFacts` publishing one call produced-value fact at the
    /// shared synthetic call site (`SiteId(u32::MAX)`, see `expr` above).
    /// Stands in for the checker's HIR-level publication — the same fact
    /// `classify_call_arm_scrutinee_origin` reads to decide the move-out.
    fn call_result_facts(ownership: ProducedValueOwnership) -> ParamOwnershipFacts {
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            SiteId(u32::MAX),
            HirProducedValueFact {
                producer: HirProducedValueProducer::Call,
                ownership,
                relation: HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        facts
    }

    fn borrowed_call_result_facts() -> ParamOwnershipFacts {
        call_result_facts(ProducedValueOwnership::Borrowed)
    }

    fn is_alias_reject(o: &ProjectedPayloadOrigin) -> bool {
        matches!(
            o,
            ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::AliasesCallerStorage)
        )
    }

    fn is_ephemeral(o: &ProjectedPayloadOrigin) -> bool {
        matches!(o, ProjectedPayloadOrigin::EphemeralTemp)
    }

    #[test]
    fn string_binary_scrutinee_is_fresh() {
        let b = Builder::default();
        let bin = expr(
            HirExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(binding_ref("a", 0, ResolvedTy::String)),
                right: Box::new(binding_ref("b", 1, ResolvedTy::String)),
            },
            ResolvedTy::String,
        );
        assert!(
            is_ephemeral(&b.classify_scrutinee_origin(&bin)),
            "a string concat allocates fresh (hew_string_concat) — a fresh sole owner"
        );
    }

    #[test]
    fn heap_non_string_binary_scrutinee_rejects() {
        let b = Builder::default();
        let bin = expr(
            HirExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(binding_ref("a", 0, ResolvedTy::Bytes)),
                right: Box::new(binding_ref("b", 1, ResolvedTy::Bytes)),
            },
            ResolvedTy::Bytes,
        );
        assert!(
            is_alias_reject(&b.classify_scrutinee_origin(&bin)),
            "a heap non-string Binary is not proven fresh — fail closed"
        );
    }

    #[test]
    fn aggregate_over_a_heap_place_operand_rejects() {
        let b = Builder::default();
        // (h.b, 0) — the string field `h.b` is a re-readable heap place.
        let field = expr(
            HirExprKind::FieldAccess {
                object: Box::new(binding_ref("h", 0, ResolvedTy::String)),
                field: "b".to_string(),
            },
            ResolvedTy::String,
        );
        let tuple = expr(
            HirExprKind::TupleLiteral {
                elements: vec![field, binding_ref("n", 1, ResolvedTy::I64)],
            },
            ResolvedTy::Unit,
        );
        assert!(
            is_alias_reject(&b.classify_scrutinee_origin(&tuple)),
            "an aggregate embedding a live heap place operand must reject"
        );
    }

    #[test]
    fn aggregate_over_fresh_operands_is_fresh() {
        let b = Builder::default();
        // (m, n) — both scalar (the type short-circuit proves each operand ∅).
        let tuple = expr(
            HirExprKind::TupleLiteral {
                elements: vec![
                    binding_ref("m", 0, ResolvedTy::I64),
                    binding_ref("n", 1, ResolvedTy::I64),
                ],
            },
            ResolvedTy::Unit,
        );
        assert!(
            is_ephemeral(&b.classify_scrutinee_origin(&tuple)),
            "an aggregate whose every operand is fresh is a fresh sole owner"
        );
    }

    #[test]
    fn call_forwarding_a_param_summary_rejects() {
        // A module-fn callee whose HIR-checked produced-value fact is `Borrowed`
        // forwards a by-value heap parameter — the classify arm treats a
        // `Borrowed` call result as caller-visible-alias and rejects the move
        // (the checker publishes this fact for a PARAM-forwarding return; see
        // `resolve_user_call_facts` in hew-hir/src/verify.rs).
        let b = Builder {
            param_ownership: Rc::new(borrowed_call_result_facts()),
            ..Builder::default()
        };
        let callee = expr(
            HirExprKind::BindingRef {
                name: "passthru".to_string(),
                resolved: ResolvedRef::Item(hew_hir::ItemId(7)),
            },
            ResolvedTy::Unit,
        );
        // `passthru(h.b)` — the forwarded argument is a re-readable heap PLACE,
        // so the S2b arg-scan cannot rescue the `{PARAM}` summary.
        let place_arg = expr(
            HirExprKind::FieldAccess {
                object: Box::new(binding_ref("h", 0, ResolvedTy::String)),
                field: "b".to_string(),
            },
            ResolvedTy::String,
        );
        let call = expr(
            HirExprKind::Call {
                target: hew_types::CallTarget::IndirectFunctionValue,
                callee: Box::new(callee),
                args: vec![place_arg],
            },
            ResolvedTy::String,
        );
        assert!(
            is_alias_reject(&b.classify_scrutinee_origin(&call)),
            "a PARAM-forwarding module-fn call scrutinee over a place arg must reject"
        );
    }

    /// Pin every `ProducedValueOwnership` state the call arm can be handed
    /// against the verdict it must produce. A single-state test cannot fail on
    /// an arm merged into the wrong bucket, because `Owned`, `NoOwner` and
    /// `Unknown` all admit today: only the whole table distinguishes "the
    /// classifier read the checker's verdict" from "the classifier fell
    /// through to the legacy admit".
    #[test]
    fn call_result_ownership_selects_the_payload_transfer_verdict() {
        let cases: [(ProducedValueOwnership, bool); 5] = [
            // A fresh owned result is the sole owner of its temporary, so the
            // payload transfer can neutralize that temporary.
            (
                ProducedValueOwnership::owned(hew_types::ProducedValueAcquisition::Fresh),
                true,
            ),
            // A borrowed result may forward caller-visible storage.
            (ProducedValueOwnership::Borrowed, false),
            // A receiver-identity result IS the receiver's storage.
            (ProducedValueOwnership::ReceiverIdentity, false),
            // A foreign result has no proved caller-side owner to transfer.
            (ProducedValueOwnership::NoOwner, false),
            // An unresolved call has no ownership proof and fails closed.
            (ProducedValueOwnership::Unknown, false),
        ];
        for (ownership, admits) in cases {
            let b = Builder {
                param_ownership: Rc::new(call_result_facts(ownership)),
                ..Builder::default()
            };
            let callee = expr(
                HirExprKind::BindingRef {
                    name: "produce".to_string(),
                    resolved: ResolvedRef::Item(hew_hir::ItemId(7)),
                },
                ResolvedTy::Unit,
            );
            let literal_arg = expr(
                HirExprKind::Literal(hew_hir::HirLiteral::String("hello".to_string())),
                ResolvedTy::String,
            );
            let call = expr(
                HirExprKind::Call {
                    target: hew_types::CallTarget::IndirectFunctionValue,
                    callee: Box::new(callee),
                    args: vec![literal_arg],
                },
                ResolvedTy::String,
            );
            let origin = b.classify_scrutinee_origin(&call);
            assert_eq!(
                is_ephemeral(&origin),
                admits,
                "{ownership:?} call result took the wrong payload-transfer verdict: {origin:?}"
            );
            assert_eq!(
                is_alias_reject(&origin),
                !admits,
                "{ownership:?} call result took the wrong payload-transfer verdict: {origin:?}"
            );
        }
    }
}
#[cfg(test)]
mod returned_member_read_aliases {}
#[cfg(test)]
mod obligation_balance_validator;

#[cfg(test)]
mod replay_plan_tests;

#[cfg(all(test, not(target_arch = "wasm32")))]
mod replay_plan_proptests;
