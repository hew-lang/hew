#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    base_local, blocks_reachable_from, check_duplex_split_state,
    close_obligated_borrow_alias_violations, collection_borrow_getter_alias_locals,
    compute_collection_interior_alias_taint, compute_projection_alias_taint, dataflow,
    derive_borrowed_builtin_handle_projection_alias_bindings, derive_bytes_actor_transfer_blocks,
    derive_consumed_local_aggregate_member_bindings, derive_cow_fresh_borrowed_owner,
    derive_cow_sole_owner, derive_enum_composite_drop_allowed, derive_local_bytes_drop_allowed,
    derive_local_collection_drop_allowed, derive_owned_record_drop_allowed,
    derive_owned_tuple_handle_projection_bindings, derive_returned_aggregate_member_bindings,
    derive_returned_member_transfer_blocks, derive_spawn_consumed_handle_bindings,
    derive_tuple_composite_drop_allowed, instr_source_places, interior_alias_receiver_violations,
    outbound_record_layouts, place_is_interior_projection, place_refs_local,
    propagate_seeded_whole_value_alias_roots_excluding_moves, propagate_whole_value_alias_roots,
    retained_string_terminator_drop_safe, short_name, string_call_borrows,
    terminator_source_places, user_record_layout_key, vec_iter_record_init_vec_source, BTreeMap,
    BasicBlock, BindingId, BlockKind, Builder, BuiltinType, CheckedMirFunction,
    ClosureEnvFieldOwnership, ClosurePairRhs, DropKind, DropPlan, ElabBlock, ElabDrop,
    ElaboratedMirFunction, ExitPath, HashMap, HashSet, HirExpr, HirExprKind, Instr, IntentKind,
    LambdaCapture, MirCheck, MirDiagnostic, MirDiagnosticKind, MirStatement, ParamCrashCleanupKind,
    Place, RawMirFunction, ResolvedRef, ResolvedTy, SiteId, SuspendKind, Terminator,
    TraitObjectStorage, ValueClass, ENTRY_BLOCK_ID,
};
use crate::model::CooperateSite;
#[cfg(test)]
use hew_hir::ResourceMarker;

mod diagnostic_projection;
mod returned_member_release;
mod vec_iter_yield_abandonment;

pub(super) use diagnostic_projection::check_to_diagnostic;
use returned_member_release::{
    existing_releases_replaced_by_candidate, normal_goto_precedes_abandonment,
    returned_member_alias_read_blocks, returned_member_re_admission_path,
    select_returned_member_re_admissions, ReturnedMemberReAdmission, ReturnedMemberReAdmissionPath,
};
pub(super) use vec_iter_yield_abandonment::vec_iter_yield_abandonment_diagnostics;
use vec_iter_yield_abandonment::vec_iter_yield_body_region;

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
///   2. For every `Terminator::Return` exit, emit a `DropPlan` whose
///      `drops` are the live owned-local list in reverse declaration
///      order (LIFO). `If`-lowering (Slice 2) constructs
///      `Terminator::Branch` and `Terminator::Goto` in addition to
///      `Terminator::Return`; `enumerate_exits` handles all three.
///   3. For declared-but-not-constructed terminators (`Panic`, `Yield`,
///      `Send`, `Call`), the pass enumerates them with an empty drop
///      plan when reached — later cluster additions add the construction
///      surfaces that turn these into populated plans.
///   4. A `BlockKind::Cleanup` block is emitted ONLY when a
///      `Terminator::Panic` is constructed in the function's CFG
///      (currently no spine surface — declared scaffold). Same for
///      `ExitPath::Cancel` (scope-structural cancellation, also
///      declared scaffold in v0.5).
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
    clippy::match_same_arms,
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "derivation threads the sealed Checked-MIR inputs and each per-class drop-allow derivation (cow / enum \
              / owned-Vec / owned-record / tuple-composite / returned-aggregate \
              members) into one ordered pass; each is a distinct fail-closed \
              authority and splitting them scatters the ordering contract"
)]
pub(super) fn derive_elaboration(
    name: &str,
    return_ty: &ResolvedTy,
    blocks: &[BasicBlock],
    cooperate_sites: &[CooperateSite],
    builder: &Builder,
    flat_statements: &[MirStatement],
    dataflow_result: &dataflow::DataflowResult,
    precomputed_cow_drop_allowed: Option<&HashSet<BindingId>>,
    precomputed_local_bytes_drop_allowed: Option<&HashSet<BindingId>>,
) -> (ElaboratedMirFunction, Vec<MirDiagnostic>) {
    let mut elaboration_diagnostics = Vec::new();
    // Statements stream: retained for snapshot/compat continuity with
    // the pre-Cluster-3 elaborator. Every non-`BitCopy` owned local
    // gets a checker-stream `Drop` entry in reverse-declaration order;
    // the structural drop plan in `drop_plans` is the authoritative
    // per-`ExitPath` answer. The flat stream is the union of every
    // block's `statements` in construction order — Slice 1 maintains
    // pre-CFG snapshot continuity by feeding the same union here.
    let mut elaborated_statements: Vec<MirStatement> = flat_statements.to_vec();
    // Every local that can own on any path to an exit, materialised once for
    // the reverse-declaration drop stream and every per-class allow-set below.
    // A later move/return is path-local: excluding `ConsumedAt` definitions
    // globally leaves their earlier call-unwind intervals uncovered. Per-exit
    // ownership dataflow suppresses the drop after a successful transfer.
    let binding_names: HashMap<BindingId, String> = flat_statements
        .iter()
        .filter_map(|statement| match statement {
            MirStatement::Bind { binding, name, .. } => Some((*binding, name.clone())),
            _ => None,
        })
        .collect();
    // Ownership cleanup admission is driven by explicit generation-aware MIR
    // events. Lowering may stage facts while constructing Raw MIR, but once an
    // event is emitted no elaboration step reverse-maps places, guesses from
    // adjacent writes, or closes a whole-function Move ancestry.
    let ownership_transfers: Vec<&crate::model::OwnershipEvent> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(event) => Some(event),
            _ => None,
        })
        .collect();
    // Owner definitions and storage are reconstructed only from the explicit
    // program-point ownership operations. `binding_locals` remains available
    // below for ordinary expression/projection topology, but it is not allowed
    // to override where an OwnerId says its value resides.
    let mut owned_locals_snapshot = Vec::new();
    let mut ownership_binding_locals = HashMap::new();
    let mut latest_owner_by_binding = HashMap::new();
    let mut seen_owner_bindings = HashSet::new();
    let mut record_owner_definition =
        |owner: crate::model::OwnerId, place: Place, ty: ResolvedTy| {
            ownership_binding_locals.insert(owner.binding, place);
            latest_owner_by_binding
                .entry(owner.binding)
                .and_modify(|latest: &mut crate::model::OwnerId| {
                    if owner.generation > latest.generation {
                        *latest = owner;
                    }
                })
                .or_insert(owner);
            if seen_owner_bindings.insert(owner.binding) {
                let name = binding_names
                    .get(&owner.binding)
                    .cloned()
                    .unwrap_or_else(|| format!("__hew_owner_{}", owner.binding.0));
                owned_locals_snapshot.push((owner.binding, name, ty));
            }
        };
    for event in &ownership_transfers {
        match event {
            crate::model::OwnershipEvent::Mint { owner, place, ty } => {
                record_owner_definition(*owner, *place, ty.clone());
            }
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
            } => {
                record_owner_definition(*replacement, *place, ty.clone());
            }
            crate::model::OwnershipEvent::Transfer {
                to: Some(place),
                to_owner: Some(owner),
                to_ty: Some(ty),
                ..
            } => {
                record_owner_definition(*owner, *place, ty.clone());
            }
            crate::model::OwnershipEvent::Join {
                replacement,
                place,
                ty,
                ..
            } => {
                record_owner_definition(*replacement, *place, ty.clone());
            }
            crate::model::OwnershipEvent::DropRecipe { .. }
            | crate::model::OwnershipEvent::Transfer { .. }
            | crate::model::OwnershipEvent::Relocate { .. }
            | crate::model::OwnershipEvent::Release { .. }
            | crate::model::OwnershipEvent::GuardedRelease { .. }
            | crate::model::OwnershipEvent::DemoteToAlias { .. }
            | crate::model::OwnershipEvent::Guard { .. }
            | crate::model::OwnershipEvent::InteriorAlias { .. }
            | crate::model::OwnershipEvent::AliasRelocate { .. }
            | crate::model::OwnershipEvent::AliasEnd { .. }
            | crate::model::OwnershipEvent::EdgeCarry { .. }
            | crate::model::OwnershipEvent::ScopeExit { .. } => {}
        }
    }
    // A same-place transfer replaces a provisional publication identity with
    // its named owner without changing physical storage. Both OwnerIds remain
    // in the immutable history (the earlier one must still validate before the
    // handoff), but there is only one destructor slot. Select the replacement
    // identity from that explicit transfer operation before building the LIFO
    // template; otherwise both historical identities would schedule the same
    // Place on an unwind edge.
    let adopted_owner_by_place: HashMap<Place, BindingId> = ownership_transfers
        .iter()
        .filter_map(|event| match event {
            crate::model::OwnershipEvent::Transfer {
                from,
                to: Some(to),
                to_owner: Some(owner),
                ..
            } if from == to => Some((*to, owner.binding)),
            _ => None,
        })
        .collect();
    owned_locals_snapshot.retain(|(binding, _, _)| {
        let Some(place) = ownership_binding_locals.get(binding) else {
            return false;
        };
        adopted_owner_by_place
            .get(place)
            .is_none_or(|adopted| binding == adopted)
    });
    let mir_owner_guards: HashMap<crate::model::OwnerId, Place> = ownership_transfers
        .iter()
        .filter_map(|event| match event {
            crate::model::OwnershipEvent::Guard { owner, flag, .. } => Some((*owner, *flag)),
            _ => None,
        })
        .collect();
    let guard_flags = |kind: crate::model::OwnershipGuardKind| {
        ownership_transfers
            .iter()
            .filter_map(|event| match event {
                crate::model::OwnershipEvent::Guard {
                    owner,
                    flag,
                    kind: actual,
                } if *actual == kind => Some((owner.binding, *flag)),
                _ => None,
            })
            .collect::<HashMap<BindingId, Place>>()
    };
    let affine_release_guard_flags = guard_flags(crate::model::OwnershipGuardKind::AffineRelease);
    let overwrite_guard_flags = guard_flags(crate::model::OwnershipGuardKind::Overwrite);
    let collection_guard_flags = guard_flags(crate::model::OwnershipGuardKind::Collection);
    let actor_message_cow_guard_flags =
        guard_flags(crate::model::OwnershipGuardKind::ActorMessageCow);
    let conditional_record_guard_flags =
        guard_flags(crate::model::OwnershipGuardKind::ConditionalRecord);
    let vec_iter_guard_flags = guard_flags(crate::model::OwnershipGuardKind::VecIter);
    let guarded_by_latest = |binding: BindingId, flags: &HashMap<BindingId, Place>| {
        latest_owner_by_binding
            .get(&binding)
            .copied()
            .and_then(|owner| {
                flags
                    .get(&binding)
                    .copied()
                    .filter(|flag| mir_owner_guards.get(&owner) == Some(flag))
                    .map(|flag| crate::model::ElabDropGuard { owner, flag })
            })
    };
    let path_local_transfer_cleanup_bindings: HashSet<BindingId> = ownership_transfers
        .iter()
        .filter_map(|event| match event {
            crate::model::OwnershipEvent::Transfer { owner, .. } => Some(owner.binding),
            crate::model::OwnershipEvent::Relocate { .. } => None,
            crate::model::OwnershipEvent::Reset { replacement, .. }
            | crate::model::OwnershipEvent::Rearm { replacement, .. } => Some(replacement.binding),
            crate::model::OwnershipEvent::Join { replacement, .. } => Some(replacement.binding),
            crate::model::OwnershipEvent::Mint { .. }
            | crate::model::OwnershipEvent::DropRecipe { .. }
            | crate::model::OwnershipEvent::Release { .. }
            | crate::model::OwnershipEvent::GuardedRelease { .. }
            | crate::model::OwnershipEvent::DemoteToAlias { .. }
            | crate::model::OwnershipEvent::Guard { .. }
            | crate::model::OwnershipEvent::InteriorAlias { .. }
            | crate::model::OwnershipEvent::AliasRelocate { .. }
            | crate::model::OwnershipEvent::AliasEnd { .. }
            | crate::model::OwnershipEvent::EdgeCarry { .. }
            | crate::model::OwnershipEvent::ScopeExit { .. } => None,
        })
        .collect();
    let return_move_chain_cleanup_bindings: HashSet<BindingId> = ownership_transfers
        .iter()
        .filter_map(|event| match event {
            crate::model::OwnershipEvent::Transfer {
                owner,
                to: Some(Place::ReturnSlot),
                ..
            } => Some(owner.binding),
            _ => None,
        })
        .collect();
    let transfer_destination_cleanup_bindings: HashSet<BindingId> = ownership_transfers
        .iter()
        .filter_map(|event| match event {
            crate::model::OwnershipEvent::Transfer {
                to_owner: Some(owner),
                ..
            } => Some(owner.binding),
            _ => None,
        })
        .collect();
    let mir_consumed_project_binder_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::NeutralizePayloadSlot {
                place: Place::MachineVariant { .. } | Place::EnumVariant { .. },
                transferee: Some(transferee),
                ..
            } => base_local(*transferee),
            _ => None,
        })
        .collect();
    // Successful return transfers do not make an upstream owner globally
    // absent: an earlier unwind edge still owns it. Projection and pattern
    // consumption remain explicit exclusions until their own ownership events
    // mint the destination generation.
    let return_move_chain_resident_bindings = return_move_chain_cleanup_bindings.clone();
    // A lexical-scope close may disposition a binding as `ScopeReleased`
    // even when the scope's result immediately transfers that binding into the
    // function return slot.  The disposition describes the successful normal
    // edge; it says nothing about an earlier call's unwind edge.  Restore every
    // structurally identified transfer source to the function-wide cleanup
    // template and let the per-edge ownership state below decide whether it is
    // uninitialized, still live, or already transferred.  This is the MIR
    // analogue of LLVM `invoke`: the normal successor commits the handoff,
    // while the exceptional successor destroys the pre-call live set.
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

    // Function-wide LIFO drop sequence — one ElabDrop per
    // AffineResource owned local in reverse declaration order. The
    // per-Return-block exit live-set then narrows this sequence to
    // bindings still Live at that block's exit (drops fire only for
    // bindings whose state is Live at the exit; Consumed / Uninit
    // skip; MaybeConsumed is rejected upstream by the move-checker).
    //
    // String retain-on-share allow-set. By-value calls borrow; aggregate,
    // variant, return, and live-local co-owner mints carry explicit
    // `StringRetain` markers. Last-use handoffs keep one owner and emit no
    // retain. The marker sites and this drop allow-set come from the same MIR
    // derivation, so emission and admission cannot drift. For a flagged actor
    // message string, that same derivation admits the guarded drop only when
    // every owner-minting use is either preceded by the path-local transfer
    // flag or receives an explicit retain. A read-only aggregate ingress must
    // therefore retain; it cannot override the alias verdict merely because
    // some sibling path consumes the binding.
    //
    // Ambiguous consume facts narrow the allow-set further. A definite
    // `Consumed`/`Discharged` state is path-local and remains in the LIFO
    // template so an earlier unwind edge can destroy the owner; the per-edge
    // state filter suppresses it after the handoff. `MaybeConsumed` is removed
    // because the current CoW move checker cannot select a safe runtime arm.
    let mut cow_drop_allowed = if let Some(precomputed) = precomputed_cow_drop_allowed {
        precomputed.clone()
    } else {
        let fresh_owner_dest_locals = builder.fresh_owner_dest_locals();
        let mut derived = derive_cow_sole_owner(
            blocks,
            &builder.suspend_kinds,
            &owned_locals_snapshot,
            &ownership_binding_locals,
            &mir_consumed_project_binder_locals,
            &fresh_owner_dest_locals,
            &builder.locals,
            &builder.borrowed_string_param_locals,
            &builder.parameter_locals,
            &actor_message_cow_guard_flags,
            &builder.module_fn_names,
            &builder.module_generic_fn_names,
            &builder.call_scrutinee_provenance.extern_table,
            &builder
                .call_scrutinee_provenance
                .owned_string_return_carrier_symbols,
        )
        .allowed;
        derived.extend(derive_cow_fresh_borrowed_owner(
            blocks,
            &builder.suspend_kinds,
            &owned_locals_snapshot,
            &ownership_binding_locals,
            &builder.locals,
            &builder.module_fn_names,
            &builder.module_generic_fn_names,
            &builder.call_scrutinee_provenance.extern_table,
            &builder
                .call_scrutinee_provenance
                .owned_string_return_carrier_symbols,
        ));
        for states in dataflow_result.exit_states.values() {
            for (binding, state) in states {
                if matches!(
                    state,
                    dataflow::BindingState::Discharged(_)
                        | dataflow::BindingState::Consumed(_)
                        | dataflow::BindingState::MaybeConsumed(_)
                ) && !actor_message_cow_guard_flags.contains_key(binding)
                {
                    derived.remove(binding);
                }
            }
        }
        derived
    };
    // W5.020 — fail-closed sole-owner allow-set for heap-owning enum
    // composite bindings (`Result<T, string>` / `Option<string>` / user enums
    // with an owned-payload variant). A composite is admitted for the tag-aware
    // `DropKind::EnumInPlace` scope-exit drop only when its active payload is
    // proven not to escape; everything else leaks (as before W5.020) rather
    // than double-free. Empty when the builder carries no enum layouts (some
    // synthetic test pipelines), so those bodies keep the pre-W5.020 posture.
    let outbound_records = outbound_record_layouts(builder);
    let mut enum_composite_drop_allowed = derive_enum_composite_drop_allowed(
        blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &ownership_binding_locals,
        &builder.binding_scope,
        &builder.transient_local_scopes,
        &builder.scope_info,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        &builder.type_classes,
        &outbound_records,
        &builder.opaque_handle_names,
        &builder.lifecycle_registry,
        &builder.fresh_variant_payload_binder_locals,
        &builder.proven_borrow_call_args,
        &builder.module_fn_names,
        &builder.module_generic_fn_names,
        &builder.call_scrutinee_provenance.extern_table,
    );
    // A mutable enum consumed on one path and reassigned on another carries a
    // runtime ownership flag. Re-admit its fresh post-store generation: the
    // guarded scope-exit drop fires only where the reassignment reset the flag
    // to zero and is skipped where the prior value moved out.
    enum_composite_drop_allowed.extend(overwrite_guard_flags.keys().copied());
    let call_carrier_has_declared_release = |ty: &ResolvedTy| {
        super::composite_own::direct_payload_has_registered_resource_record(
            ty,
            &builder.enum_layouts,
            &builder.lifecycle_registry,
        )
    };
    let call_carrier_shell_drop_safe = |ty: &ResolvedTy| {
        super::composite_own::enum_payloads_are_shell_drop_safe(
            ty,
            &builder.enum_layouts,
            &builder.record_field_orders,
            &builder.type_classes,
            &outbound_records,
            &builder.opaque_handle_names,
            &builder.lifecycle_registry,
        )
    };
    // A typed call-result mint is the release authority for its exact carrier
    // generation. Re-admit that binding even when a selected payload move
    // makes the generic sole-owner scan conservative: projection consumes
    // null their source slot, so the tag-aware drop releases only the shell
    // and payload slots that remain owned on each exit.
    //
    // The exception is an unneutralized payload binder handed to a call whose
    // ownership contract does not prove a borrow (most importantly an
    // unaudited extern). That payload is still a view into the carrier, but the
    // callee may retain/take it; restoring the carrier drop would release the
    // callee-owned handle. Keep this veto exact so ordinary domestic carriers
    // such as rC, whose payload merely flows through local aggregate logic,
    // retain their required per-iteration release.
    let neutralized_payload_slots: HashSet<Place> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::NeutralizePayloadSlot { place, .. } => Some(*place),
            _ => None,
        })
        .collect();
    let call_carriers_with_opaque_payload_handoff: HashSet<u32> = builder
        .projected_payload_provenance
        .iter()
        .filter_map(|(binding, provenance)| {
            let carrier = base_local(provenance.source_place)?;
            if !builder
                .call_scrutinee_carrier_mint_locals
                .contains(&carrier)
                || neutralized_payload_slots.contains(&provenance.source_place)
            {
                return None;
            }
            let alias = builder
                .binding_locals
                .get(binding)
                .copied()
                .and_then(base_local)?;
            blocks
                .iter()
                .any(|block| {
                    matches!(
                        &block.terminator,
                        Terminator::Call { args, .. }
                            if args.iter().any(|arg| base_local(*arg) == Some(alias))
                                && !binder_read_is_borrow_safe_terminator(
                                    &block.terminator,
                                    builder.suspend_kinds.get(&block.id),
                                    alias,
                                )
                                && !string_binder_read_is_user_fn_borrow(
                                    &block.terminator,
                                    builder.suspend_kinds.get(&block.id),
                                    alias,
                                    builder.locals.get(alias as usize),
                                    &builder.module_fn_names,
                                    &builder.module_generic_fn_names,
                                    &builder.call_scrutinee_provenance.extern_table,
                                )
                    )
                })
                .then_some(carrier)
        })
        .collect();
    enum_composite_drop_allowed.extend(ownership_binding_locals.iter().filter_map(
        |(binding, place)| {
            let local = base_local(*place)?;
            let ty = builder.locals.get(local as usize)?;
            (builder.call_scrutinee_carrier_mint_locals.contains(&local)
                && !call_carriers_with_opaque_payload_handoff.contains(&local)
                && !call_carrier_has_declared_release(ty)
                && call_carrier_shell_drop_safe(ty))
            .then_some(*binding)
        },
    ));
    // A carrier with a projected payload transfer and an explicit tag-aware
    // shell drop uses the same null-after-transfer protocol on every exit.
    // Re-admit that exact binding for its ordinary scope and loop-edge plans:
    // transferred paths see a null slot, while paths that keep the payload
    // still perform the required release.
    let partially_transferred_carriers: HashSet<u32> = blocks
        .iter()
        .filter(|block| matches!(block.terminator, Terminator::Return))
        .flat_map(|block| {
            block.instructions.iter().filter_map(|instruction| {
                let local = match instruction {
                    Instr::NeutralizePayloadSlot {
                        place: Place::MachineVariant { local, .. },
                        ..
                    } => *local,
                    _ => return None,
                };
                block
                    .instructions
                    .iter()
                    .any(|candidate| {
                        matches!(
                            candidate,
                            Instr::Drop {
                                place: Place::Local(drop_local),
                                drop_fn: Some(crate::model::DropFnSpec::InPlace(
                                    crate::ownership::InPlaceReleaseKind::Enum,
                                )),
                                ..
                            } if *drop_local == local
                        )
                    })
                    .then_some(local)
            })
        })
        .filter(|local| builder.call_scrutinee_carrier_mint_locals.contains(local))
        .collect();
    enum_composite_drop_allowed.extend(ownership_binding_locals.iter().filter_map(
        |(binding, place)| {
            base_local(*place)
                .filter(|local| partially_transferred_carriers.contains(local))
                .map(|_| *binding)
        },
    ));

    // The generic sole-owner proof predates call-carrier minting and can admit
    // a nested affine payload whose helper family codegen cannot synthesize.
    // A minted carrier must pass the same structural shell-safety authority as
    // the explicit arm release above; otherwise keep it out of every enum drop
    // plan so the obligation validator reports the missing discharge.
    enum_composite_drop_allowed.retain(|binding| {
        let Some(local) = builder
            .binding_locals
            .get(binding)
            .and_then(|place| base_local(*place))
        else {
            return true;
        };
        !builder.call_scrutinee_carrier_mint_locals.contains(&local)
            || builder.locals.get(local as usize).is_some_and(|ty| {
                call_carrier_has_declared_release(ty) || call_carrier_shell_drop_safe(ty)
            })
    });

    // Machine-typed owned locals. A machine value is `ValueClass::Unknown`, so
    // before this derivation its binding fell through every drop class and the
    // resource held in its LAST state leaked with no diagnostic. Machines are
    // enums at the value-classification layer, so an admitted binding rides the
    // same tag-aware `DropKind::EnumInPlace` helper family; the derivation is
    // separate because the step round-trip (`m = step(m, e)`) reads as an
    // escape to the generic enum prover. Fail-closed: anything the machine
    // prover cannot clear keeps the pre-existing leak posture.
    let machine_composite_drop_allowed = super::machine_own::derive_machine_composite_drop_allowed(
        blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &ownership_binding_locals,
        &builder.machine_layout_names,
        &builder.enum_layouts,
        &builder.record_field_orders,
        &builder.type_classes,
        &outbound_records,
        &builder.opaque_handle_names,
        &builder.lifecycle_registry,
    );

    // W5.016 — owned-element `Vec<T>` scope-exit drop allow-set. An owned Vec
    // earns its `hew_vec_free_owned` release UNLESS the fail-closed escape-scan
    // proves it leaves this scope's sole ownership — most importantly a handle
    // moved into an actor's initial state (`spawn A(tasks: t)`, which lowers to
    // `RecordInit` → `SpawnActor`). That ingress sets the source's dataflow
    // state to `AliasedIntoAggregate`, NOT `Consumed`, so the `Consumed` /
    // `MaybeConsumed` filter alone left it Live at the exit and fired a second
    // `hew_vec_free_owned` against the handle the actor's `state_drop_fn` now
    // owns — the F-01 use-after-free → SIGSEGV. The escape-scan — the SAME
    // `derive_local_collection_drop_allowed` authority the HashMap/HashSet,
    // closure-pair Vec, and bytes arms use — is the primary gate: it removes the
    // spawn-moved / returned / aggregate-stored handle from the LIFO before the
    // per-exit `Live` filter in `enumerate_exits` ever sees it, while its
    // callee contract keeps a normal owned Vec whose only reads are `push` /
    // `get` / `len` admitted (those borrow arg[0] / deep-clone the element, they
    // do not escape the handle). The dataflow `Consumed` /
    // `MaybeConsumed` removal below is the same belt-and-suspenders net the
    // sibling arms keep for a handle moved out by a by-value consume; the
    // interior-alias retain and `dedup_whole_value_handoff` further down close
    // the `vec.get(i)` ingress-borrow and array-literal-desugar handoff cases.
    // Every direction only ever over-EXCLUDES (leak), never re-admits — a handle
    // the prover did not clear is never double-freed
    // (`drop-allowset-from-value-flow`, `boundary-fail-closed`,
    // `raii-null-after-move`, `cleanup-all-exits`).
    let mut owned_vec_drop_allowed = admit_with_flagged_fallback(
        &owned_locals_snapshot,
        &collection_guard_flags,
        |ty| builder.binding_ty_is_owned_element_vec(ty),
        |view| {
            derive_local_collection_drop_allowed(
                blocks,
                &builder.suspend_kinds,
                view,
                &ownership_binding_locals,
                &builder.proven_borrow_call_args,
                |ty| builder.binding_ty_is_owned_element_vec(ty),
            )
        },
    );
    // D65: a place-source VecIter record init borrows its source handle. The
    // generic dataflow sees the handle copied into the cursor aggregate and may
    // conservatively report the source binding as Consumed, but the cursor is
    // deliberately not an owner (`vec_iter_let_cursor_owns_handle`): recursive
    // release must remain on the source binding. Recover that carried fact from
    // the finalized RecordInit source place so increasing release depth and
    // cursor ownership classification move together.
    let vec_iter_borrowed_owned_sources: HashSet<BindingId> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(vec_iter_record_init_vec_source)
        .filter_map(base_local)
        .filter_map(|local| {
            ownership_binding_locals
                .iter()
                .find_map(|(binding, place)| {
                    (base_local(*place) == Some(local)).then_some(*binding)
                })
        })
        .filter(|binding| {
            owned_locals_snapshot.iter().any(|(candidate, _, ty)| {
                candidate == binding && builder.binding_ty_is_owned_element_vec(ty)
            })
        })
        .collect();
    // #2418 — a binding carrying a path-sensitive collection drop-flag is
    // exempt from the consume-exit removal: its scope-exit release is gated on
    // `flag == 0` at runtime (skipped on the moved path, fired on the
    // not-moved path), and the per-exit `drops_for_exit` state filter still
    // excludes exits reached only through the consume. Unflagged bindings keep
    // the removal (fail-closed).
    for states in dataflow_result.exit_states.values() {
        for (binding, state) in states {
            if matches!(
                state,
                dataflow::BindingState::Discharged(_)
                    | dataflow::BindingState::Consumed(_)
                    | dataflow::BindingState::MaybeConsumed(_)
            ) && !collection_guard_flags.contains_key(binding)
                && !vec_iter_borrowed_owned_sources.contains(binding)
            {
                owned_vec_drop_allowed.remove(binding);
            }
        }
    }
    // Interior-alias INGRESS exclusion (fail-closed). The dataflow consume
    // filter above removes an owned Vec MOVED out of its slot, but not one
    // whose slot received an interior pointer of a still-live parent in the
    // first place — an owned-element handle loaded via `vec.get(i)` /
    // `data.get(row)` (`hew_vec_get_owned` / `hew_vec_get_ptr`) or a record/
    // tuple field is a BORROW the parent still owns. `hew_vec_free_owned` on
    // such a handle double-frees (the same class as the csv `Table::get` plain
    // Vec UAF, one level up). Exclude every interior-alias-tainted binding;
    // over-exclusion only leaks (`boundary-fail-closed`, `cleanup-all-exits`).
    {
        let owned_vec_interior_alias = compute_collection_interior_alias_taint(blocks);
        owned_vec_drop_allowed.retain(|binding| {
            builder
                .binding_locals
                .get(binding)
                .and_then(|place| base_local(*place))
                .is_none_or(|local| !owned_vec_interior_alias.contains(&local))
        });
    }

    // Whole-value hand-off dedup (same fix the plain/closure-pair Vec sets
    // apply below). The array-literal desugar (`let v: Vec<T> = []`) binds the
    // fresh owned-element vec to a synthetic `__hew_array_N` let, then hands
    // the SAME handle to the user binding through a chain of whole-value
    // `Move`s. The dataflow does NOT mark the synthetic source consumed, so the
    // `Consumed`/`MaybeConsumed` filter above leaves an intermediate admitted —
    // two admitted bindings over one handle would fire two `hew_vec_free_owned`
    // releases (a double free). Collapse the chain so exactly the final owner
    // releases (`drop-allowset-from-value-flow`, `raii-null-after-move`).
    dedup_whole_value_handoff(
        blocks,
        &ownership_binding_locals,
        &mut owned_vec_drop_allowed,
        &collection_guard_flags,
    );

    // Local `HashMap` / `HashSet` handle scope-exit drop allow-set. A local
    // collection handle earns its `hew_hashmap_free_layout` /
    // `hew_hashset_free_layout` release UNLESS the fail-closed escape-scan proves
    // it leaves this scope's sole ownership — most importantly a handle moved
    // into an actor's initial state (`spawn A(f: m)`), whose `AliasedIntoAggregate`
    // dataflow state would otherwise keep it Live at the exit and double-free
    // against the actor's `state_drop_fn`. The escape-scan is the primary
    // authority (it removes escapers from the LIFO before the per-exit Live
    // filter runs); the dataflow `Consumed` / `MaybeConsumed` removal below is
    // the same belt-and-suspenders net the owned-Vec / cow arms use for a handle
    // moved out by a by-value consume. Both directions only ever over-EXCLUDE
    // (leak), never re-admit — a handle the prover did not clear is never
    // double-freed (`boundary-fail-closed`, `cleanup-all-exits`).
    let mut local_collection_drop_allowed = admit_with_flagged_fallback(
        &owned_locals_snapshot,
        &collection_guard_flags,
        ty_is_local_collection_handle,
        |view| {
            derive_local_collection_drop_allowed(
                blocks,
                &builder.suspend_kinds,
                view,
                &ownership_binding_locals,
                &builder.proven_borrow_call_args,
                ty_is_local_collection_handle,
            )
        },
    );
    // #2418 — flagged bindings are exempt from the consume-exit removal (the
    // runtime `flag == 0` gate discriminates the moved path); see the
    // owned-Vec loop above.
    for states in dataflow_result.exit_states.values() {
        for (binding, state) in states {
            if matches!(
                state,
                dataflow::BindingState::Discharged(_)
                    | dataflow::BindingState::Consumed(_)
                    | dataflow::BindingState::MaybeConsumed(_)
            ) && !collection_guard_flags.contains_key(binding)
            {
                local_collection_drop_allowed.remove(binding);
            }
        }
    }

    // Local `bytes` scope-exit drop allow-set. A bytes binding earns its
    // triple-field-0 `hew_bytes_drop` release UNLESS the fail-closed
    // escape-scan proves it leaves this scope's sole ownership — most
    // importantly a triple consumed by an actor `Send` (the mailbox `memcpy`
    // hand-off the probe trace pins: the actor's state / receive handler owns
    // the buffer from then on, and the synthesised `state_drop_fn` is its
    // release) or loaded out of a still-live aggregate (projection taint).
    // The escape-scan is the primary authority; the dataflow `Consumed` /
    // `MaybeConsumed` removal below is the same belt-and-suspenders net the
    // owned-Vec / collection arms use. Both directions only ever over-EXCLUDE
    // (leak), never re-admit — a binding the prover did not clear is never
    // double-freed (`boundary-fail-closed`, `cleanup-all-exits`).
    let mut local_bytes_drop_allowed =
        if let Some(precomputed) = precomputed_local_bytes_drop_allowed {
            precomputed.clone()
        } else {
            let mut derived = derive_local_bytes_drop_allowed(
                blocks,
                &builder.suspend_kinds,
                &owned_locals_snapshot,
                &ownership_binding_locals,
                &builder.locals,
                &builder.borrowed_bytes_param_locals,
            )
            .allowed;
            derived.extend(
                owned_locals_snapshot
                    .iter()
                    .filter(|(binding, _, ty)| {
                        matches!(ty, ResolvedTy::Bytes)
                            && actor_message_cow_guard_flags.contains_key(binding)
                    })
                    .map(|(binding, _, _)| *binding),
            );
            for states in dataflow_result.exit_states.values() {
                for (binding, state) in states {
                    if matches!(
                        state,
                        dataflow::BindingState::Discharged(_)
                            | dataflow::BindingState::Consumed(_)
                            | dataflow::BindingState::MaybeConsumed(_)
                    ) && !actor_message_cow_guard_flags.contains_key(binding)
                    {
                        derived.remove(binding);
                    }
                }
            }
            derived
        };

    // Closure-pair `Vec<fn(...)>` handle scope-exit drop allow-set. Rides the
    // same receiver-borrow escape model as the HashMap/HashSet derivation
    // (the handle is the owner; push/index/len reads borrow it), narrowed by
    // the same consume filter. An admitted handle releases every element's
    // pair box + env box exactly once via `hew_vec_free_owned`; an
    // excluded handle leaks (as every plain Vec local does today), never
    // double-frees.
    let mut closure_vec_drop_allowed = derive_local_collection_drop_allowed(
        blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &ownership_binding_locals,
        &builder.proven_borrow_call_args,
        ty_is_closure_pair_vec,
    );
    for states in dataflow_result.exit_states.values() {
        for (binding, state) in states {
            if matches!(
                state,
                dataflow::BindingState::Discharged(_)
                    | dataflow::BindingState::Consumed(_)
                    | dataflow::BindingState::MaybeConsumed(_)
            ) {
                closure_vec_drop_allowed.remove(binding);
            }
        }
    }
    // Plain `Vec<T>` handle scope-exit drop allow-set — a Vec local whose
    // element is a BitCopy scalar or `string` (`Vec<i64>`, `Vec<u8>`,
    // `Vec<bool>`, `Vec<f64>`, `Vec<string>`, …). Pre-fix these had no drop
    // class at all: a plain Vec is `ValueClass::CowValue` but
    // `cow_value_leaf_drop_symbol` only handles the leaf `string` case, so
    // every plain Vec local fell through to the no-op CowValue arm and LEAKED
    // its backing buffer (and, for `Vec<string>`, every element) on every
    // exit. Rides the same receiver-borrow escape model as the
    // HashMap/HashSet and closure-pair derivations (the handle is the owner;
    // push/index/len reads carry a Vec receiver-borrow contract), narrowed by
    // the same consume filter. The matching release is the plain
    // `hew_vec_free` — buffer + handle, with the runtime's own
    // `ElemKind::String` element walk for string vecs; BitCopy elements have
    // no element-release path, so the single unconditional free is sound
    // under the same no-retain-on-share invariant the collection handles
    // document above. An excluded handle leaks (as before this fix), never
    // double-frees (`boundary-fail-closed`, `cleanup-all-exits`).
    let mut plain_vec_drop_allowed = admit_with_flagged_fallback(
        &owned_locals_snapshot,
        &collection_guard_flags,
        |ty| builder.binding_ty_is_plain_vec(ty),
        |view| {
            derive_local_collection_drop_allowed(
                blocks,
                &builder.suspend_kinds,
                view,
                &ownership_binding_locals,
                &builder.proven_borrow_call_args,
                |ty| builder.binding_ty_is_plain_vec(ty),
            )
        },
    );
    // #2418 — flagged bindings are exempt from the consume-exit removal (the
    // runtime `flag == 0` gate discriminates the moved path); see the
    // owned-Vec loop above.
    for states in dataflow_result.exit_states.values() {
        for (binding, state) in states {
            if matches!(
                state,
                dataflow::BindingState::Discharged(_)
                    | dataflow::BindingState::Consumed(_)
                    | dataflow::BindingState::MaybeConsumed(_)
            ) && !collection_guard_flags.contains_key(binding)
            {
                plain_vec_drop_allowed.remove(binding);
            }
        }
    }

    // Whole-value hand-off dedup for both Vec-handle allow-sets (the
    // closure-pair set and the plain set; a handle never changes element
    // class across a `Move`, so the two sets cannot hand off to each other).
    dedup_whole_value_handoff(
        blocks,
        &ownership_binding_locals,
        &mut closure_vec_drop_allowed,
        &collection_guard_flags,
    );
    dedup_whole_value_handoff(
        blocks,
        &ownership_binding_locals,
        &mut plain_vec_drop_allowed,
        &collection_guard_flags,
    );

    // A by-value owned record (RC-4 bytes field / RC-6 string field / G12
    // Vec/HashMap/HashSet field) earns the tag-aware `DropKind::RecordInPlace`
    // (the recursive per-field `__hew_record_drop_inplace_<R>` thunk) ONLY when
    // the fail-closed escape-scan proves it still solely owns its heap fields at
    // scope exit. A whole-record return (`Move { dest: ReturnSlot }` at an
    // if-arm/function tail) does NOT emit a `MirStatement::Use { Consume }`, so
    // an `exit_states`-only gate (as the owned-Vec arm uses) would keep the
    // binding and double-free its fields on the return path. The escape-scan
    // (modelled on `derive_enum_composite_drop_allowed`: a record is an OWNER
    // and the question is whether it escapes) is the correct authority — it
    // excludes the return path and admits the field-read-only path.
    //
    // This is the audit #5 reconciliation: the owned-record drop is now gated by
    // a per-exit escape analysis instead of the path-insensitive global
    // `owned_locals` removal (`mark_binding_moved`), so a record consumed on one
    // arm but live on another is still dropped on the live arm. The legacy
    // `owned_string_record_bindings` membership is folded into the same gate (a
    // string record is a subset of the owned-aggregate records covered here).
    let alias_field_binders = builder.alias_owner_field_binders();
    let is_owned_record = |ty: &ResolvedTy| builder.is_owned_aggregate_record_ty(ty);
    let record_field_store_preserves_owner = |record, field_offset| {
        builder.record_field_store_preserves_record_owner(record, field_offset)
    };
    let mut owned_record_drop_allowed = derive_owned_record_drop_allowed(
        blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &ownership_binding_locals,
        &builder.locals,
        &is_owned_record,
        &record_field_store_preserves_owner,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
        &alias_field_binders,
        &builder.proven_borrow_call_args,
        &builder.vec_iter_projection_borrow_inits,
    );
    // A flagged fresh String/BitCopy record has an explicit runtime owner
    // discriminator. The ordinary escape scan excludes it because one path
    // reaches an owning sink; re-admit exactly the bindings whose construction
    // authority and consume hook installed the flag. The guarded drop fires
    // only where that sink did not execute.
    owned_record_drop_allowed.extend(conditional_record_guard_flags.keys().copied());
    // An active mixed-return payload is not an alias of a dropped enum shell:
    // its per-variant transfer proof deliberately withheld the shell owner and
    // assigned this binder the sole recursive record teardown. The usual record
    // projection prover cannot infer that from a raw `MachineVariant` move, so
    // thread the proof recorded at the match site directly.
    owned_record_drop_allowed.extend(owned_locals_snapshot.iter().filter_map(
        |(binding, _name, ty)| {
            (builder.fresh_variant_payload_bindings.contains(binding) && is_owned_record(ty))
                .then_some(*binding)
        },
    ));

    // W5.021 — fail-closed sole-owner allow-set for heap-owning **tuple**
    // bindings (the tuple/record-of-owned-handles drop spine). A by-value owned
    // tuple `(Sink, Stream)` / `(string, string)` earns the
    // `DropKind::TupleInPlace` per-element drop ONLY when the escape-scan proves
    // it still solely owns its members at scope exit. The canonical exclusion is
    // the `__tuple_N` destructure temp whose elements are loaded out into their
    // own owning bindings (DEFECT #3): the element binders own the handles, so
    // the temp must not drop. A returned tuple is excluded too (the ReturnSlot
    // owns it). Everything the prover does not clear leaks rather than
    // double-frees.
    let mut tuple_composite_drop_allowed = derive_tuple_composite_drop_allowed(
        blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &ownership_binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
        &alias_field_binders,
        &builder.proven_borrow_call_args,
    );

    // Re-admit a transferred source only to the one drop class selected by its
    // type. Extending every allow-set made a plain `Vec<i64>` match the earlier
    // owned-element arm and call the wrong destructor on exceptional paths.
    for (binding, _name, ty) in &owned_locals_snapshot {
        let is_explicit_transfer_generation = path_local_transfer_cleanup_bindings
            .contains(binding)
            || return_move_chain_cleanup_bindings.contains(binding)
            || transfer_destination_cleanup_bindings.contains(binding)
            || builder
                .typed_produced_value_owner_bindings
                .contains(binding)
            || builder.scrutinee_payload_owner_bindings.contains(binding);
        let is_owned_actor_message_ingress = builder.current_function_call_conv
            == crate::model::FunctionCallConv::ActorHandler
            && ownership_binding_locals
                .get(binding)
                .and_then(|place| base_local(*place))
                .is_some_and(|local| builder.parameter_locals.contains(&local));
        // The template is a type/layout catalogue, not a whole-function
        // liveness verdict. Existing leaf/collection cleanup keeps its
        // generation-aware unwind coverage; the exact Checked-MIR owner state
        // below decides whether that generation is live at each exit. Owned
        // records are different: their fail-closed sole-owner exclusion must
        // survive unless an explicit transfer event requires pre-handoff
        // cleanup for that exact generation, a typed produced-value verdict
        // published a fresh synthetic owner, or a provenance-checked
        // scrutinee-payload warrant minted the record owner after a projection
        // load made the structural scan conservative. An actor handler's
        // registered message parameter is also explicit ingress ownership;
        // admitting its recipe covers suspension before any later field
        // handoff, while exact owner replay suppresses it after a transfer.
        if matches!(ty, ResolvedTy::String) {
            cow_drop_allowed.insert(*binding);
        }
        if ty_is_heap_owning_enum_composite(
            ty,
            &builder.record_field_orders,
            &builder.enum_layouts,
            builder.type_classes.lifecycle_registry(),
        ) {
            enum_composite_drop_allowed.insert(*binding);
        }
        if builder.binding_ty_is_owned_element_vec(ty) {
            owned_vec_drop_allowed.insert(*binding);
        }
        if ty_is_local_collection_handle(ty) {
            local_collection_drop_allowed.insert(*binding);
        }
        if matches!(ty, ResolvedTy::Bytes) {
            local_bytes_drop_allowed.insert(*binding);
        }
        if ty_is_closure_pair_vec(ty) {
            closure_vec_drop_allowed.insert(*binding);
        }
        if builder.binding_ty_is_plain_vec(ty) {
            plain_vec_drop_allowed.insert(*binding);
        }
        if is_owned_record(ty)
            && (is_explicit_transfer_generation || is_owned_actor_message_ingress)
        {
            owned_record_drop_allowed.insert(*binding);
        }
        if ty_is_heap_owning_tuple(
            ty,
            &builder.record_field_orders,
            &builder.enum_layouts,
            builder.type_classes.lifecycle_registry(),
        ) {
            tuple_composite_drop_allowed.insert(*binding);
        }
    }

    // W5.021 (defect #1) — owned members the caller now owns via a returned
    // aggregate; excluded from every drop class below (see the function doc).
    let mut returned_aggregate_members = derive_returned_aggregate_member_bindings(
        blocks,
        &owned_locals_snapshot,
        &ownership_binding_locals,
    );
    returned_aggregate_members.retain(|binding| {
        !path_local_transfer_cleanup_bindings.contains(binding)
            && !return_move_chain_resident_bindings.contains(binding)
    });
    // Path-sensitive re-admission map for values handed to the caller through the
    // return flow. The blanket exclusion (an aggregate member the return handoff
    // removes, `semver::try_parse`; or a whole-value return that retracts its
    // binding to `ConsumedAt`, `base64::decode`) is correct on the return path but
    // leaks the value on a guard early-return that exits BEFORE the hand-off and
    // still owns it locally. Sourced from the returned-candidate view (scope-exit
    // OR consume-retracted owners) so BOTH shapes are covered; this locates where
    // each candidate enters the return flow so the elaborator can restore its
    // scope-exit drop on exactly the `Return` exits that transfer cannot reach.
    let returned_member_candidates = owned_locals_snapshot.clone();
    let returned_member_transfer_blocks = derive_returned_member_transfer_blocks(
        blocks,
        &returned_member_candidates,
        &ownership_binding_locals,
    );

    // W3.053 — owned-handle members moved into a LOCAL aggregate and then
    // extracted-and-consumed back out (for-in / `let` extraction) by a downstream
    // release-consumer; the consumer owns the single free, so the source binding
    // must not also drop. The local-aggregate analogue of
    // `returned_aggregate_members` (see the function doc).
    let mut consumed_local_aggregate_members = derive_consumed_local_aggregate_member_bindings(
        blocks,
        &owned_locals_snapshot,
        &ownership_binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
    );
    consumed_local_aggregate_members
        .retain(|binding| !path_local_transfer_cleanup_bindings.contains(binding));
    // CAP-08 — owned handle-leaf bindings moved into an actor initial-state
    // record consumed by `SpawnActor`. The actor's synthesised `state_drop_fn`
    // is the single free site (Stream→`hew_stream_close` / Sink→`hew_sink_close`),
    // so the source binding's own scope-exit drop is removed here. The W3.053
    // gate consumes the SAME derivation via `source_excluded` so its free-count
    // model matches the drop this removal actually elides.
    let mut spawn_consumed_handle_members = derive_spawn_consumed_handle_bindings(
        blocks,
        &owned_locals_snapshot,
        &ownership_binding_locals,
        &builder.locals,
    );
    spawn_consumed_handle_members
        .retain(|binding| !path_local_transfer_cleanup_bindings.contains(binding));

    // Indirect-enum heap-node sole-owner allow-set (spec §3.7.4). A constructed
    // indirect-enum node earns its recursive `hew_dealloc` release UNLESS the
    // fail-closed structural proof shows it is an alias (a destructure binder),
    // a child handed to a parent node, returned, or consumed. Because indirect
    // enums are borrow-on-use everywhere (`intent = Read`), the consume-dataflow
    // alone cannot prove sole ownership — this derivation supplies the
    // construction-site + parent-ingress proof; the `returned_aggregate_members`
    // skip and the `Consumed`/`MaybeConsumed` exit filter below complete it.
    let actor_message_ingress_locals: HashSet<u32> =
        if builder.current_function_call_conv == crate::model::FunctionCallConv::ActorHandler {
            builder
                .parameter_locals
                .iter()
                .copied()
                .filter(|local| {
                    builder
                        .locals
                        .get(*local as usize)
                        .is_some_and(|ty| ty_is_indirect_enum(ty, &builder.enum_layouts))
                })
                .collect()
        } else {
            HashSet::new()
        };
    let mut indirect_enum_drop_allowed = derive_indirect_enum_drop_allowed(
        blocks,
        &owned_locals_snapshot,
        &ownership_binding_locals,
        &builder.enum_layouts,
        &actor_message_ingress_locals,
    );
    for states in dataflow_result.exit_states.values() {
        for (binding, state) in states {
            if matches!(
                state,
                dataflow::BindingState::Discharged(_)
                    | dataflow::BindingState::Consumed(_)
                    | dataflow::BindingState::MaybeConsumed(_)
            ) {
                indirect_enum_drop_allowed.remove(binding);
            }
        }
    }
    // A neutralized aggregate-member source that is an `indirect enum` node must
    // release through the recursive `DropKind::IndirectEnum` free (null-tolerant
    // after the `NeutralizePayloadSlot` nulls its slot), NOT the whole-record
    // `RecordInPlace` arm — an indirect enum has no `__hew_record_drop_inplace_*`
    // helper, so the blanket `owned_record_drop_allowed` extension above would
    // fail closed at codegen. Admit these members here (the indirect arm in
    // `build_lifo_drops` runs before the record arm) so the child is freed on a
    // pre-construction early exit and no-ops on the moved-into-parent path.
    for (binding, _name, ty) in &owned_locals_snapshot {
        if (path_local_transfer_cleanup_bindings.contains(binding)
            || return_move_chain_cleanup_bindings.contains(binding))
            && ty_is_indirect_enum(ty, &builder.enum_layouts)
        {
            indirect_enum_drop_allowed.insert(*binding);
        }
    }

    // A parent overwrite or borrow-spine call forwarding can transfer a direct
    // string payload's release authority to its still-live binder. Re-admit
    // exactly those delayed owners into the leaf drop class and remove their
    // projection-alias suppression; their per-binder flag remains one on every
    // path where no transfer occurred, so the resulting scope-exit drop is
    // skipped there and cannot compete with the parent's recursive release.
    cow_drop_allowed.extend(builder.projected_payload_delayed_releases.iter().copied());
    let mut projection_alias_tainted = compute_projection_alias_taint(
        blocks,
        &mir_consumed_project_binder_locals,
        &builder.fresh_variant_payload_binder_locals,
        &builder.locals,
    );
    // A ledger-owned heap enum is a real carrier owner even when its payload is
    // inspected by one or more non-consuming matches. Projection binders remain
    // tainted aliases and are excluded; the carrier itself keeps the tag-aware
    // terminal drop. This replaces the old global "payload was projected"
    // escape inference, which leaked ordinary local enums after read-only
    // matches.
    let actor_state_load_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::ActorStateFieldLoad { dest, .. } => base_local(*dest),
            _ => None,
        })
        .collect();
    let actor_state_consumed_source_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::ActorStateFieldStore {
                src,
                handoff: crate::model::ActorStateStoreHandoff::ConsumeSource,
                ..
            } => base_local(*src),
            _ => None,
        })
        .collect();
    let consumed_enum_carrier_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::NeutralizePayloadSlot { place, .. } => payload_carrier_local(*place),
            _ => None,
        })
        .collect();
    enum_composite_drop_allowed.extend(owned_locals_snapshot.iter().filter_map(
        |(binding, _name, ty)| {
            let local = builder
                .binding_locals
                .get(binding)
                .and_then(|place| base_local(*place))?;
            (ty_is_heap_owning_enum_composite(
                ty,
                &builder.record_field_orders,
                &builder.enum_layouts,
                builder.type_classes.lifecycle_registry(),
            ) && !projection_alias_tainted.contains(&local)
                && !actor_state_load_locals.contains(&local)
                && !actor_state_consumed_source_locals.contains(&local)
                && !consumed_enum_carrier_locals.contains(&local))
            .then_some(*binding)
        },
    ));
    projection_alias_tainted.retain(|local| {
        !builder
            .projected_payload_delayed_releases
            .iter()
            .any(|binding| {
                builder
                    .binding_locals
                    .get(binding)
                    .and_then(|place| base_local(*place))
                    == Some(*local)
            })
    });
    let mut borrowed_builtin_handle_projection_aliases =
        derive_borrowed_builtin_handle_projection_alias_bindings(
            &ownership_binding_locals,
            &builder.locals,
            &projection_alias_tainted,
        );
    // A projected builtin handle can defer to its enum carrier only when that
    // carrier actually earned an `EnumInPlace` drop. If carrier admission was
    // withheld, suppressing the binder as an alias removes both release paths.
    // Keep alias suppression only for the exact carrier bindings present in the
    // enum allow-set; otherwise the binder remains the sole close authority.
    let payload_alias_carriers = collect_payload_alias_map(blocks);
    borrowed_builtin_handle_projection_aliases.retain(|binding| {
        let Some(alias_local) = builder
            .binding_locals
            .get(binding)
            .and_then(|place| base_local(*place))
        else {
            return false;
        };
        let Some(carrier_local) = payload_alias_carriers.get(&alias_local) else {
            return true;
        };
        owned_locals_snapshot.iter().any(|(carrier, _, _)| {
            enum_composite_drop_allowed.contains(carrier)
                && builder
                    .binding_locals
                    .get(carrier)
                    .and_then(|place| base_local(*place))
                    == Some(*carrier_local)
        })
    });
    let owned_tuple_handle_projections = derive_owned_tuple_handle_projection_bindings(
        blocks,
        &owned_locals_snapshot,
        &ownership_binding_locals,
        &builder.locals,
        &tuple_composite_drop_allowed,
    );
    borrowed_builtin_handle_projection_aliases
        .retain(|binding| !owned_tuple_handle_projections.contains(binding));
    // A final disposition proves absence only AFTER its explicit MIR
    // transfer/release. The LIFO template therefore retains every real owner;
    // the CFG state at each program point excludes generations that have not
    // yet been minted or have already ended. Interior aliases are the sole
    // non-owner class and were excluded by `owned_locals_exit_candidates`.
    let owned_locals_exit_candidates = owned_locals_snapshot.clone();
    // Every receiver-interior alias is ownerless: the collection remains the
    // sole destructor authority for ordinary heap values, nested collections,
    // and close-obligated handles alike.  The escape/use proof below is needed
    // only for the close-obligated subset, but that narrower proof must never
    // be confused with ownership admission.
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
    let lifo_drops = build_lifo_drops(
        &owned_locals_exit_candidates,
        &ownership_binding_locals,
        &builder.type_classes,
        &builder.dyn_trait_storage,
        &owned_record_drop_allowed,
        &cow_drop_allowed,
        &builder.record_field_orders,
        &builder.enum_layouts,
        &enum_composite_drop_allowed,
        &machine_composite_drop_allowed,
        &owned_vec_drop_allowed,
        &local_collection_drop_allowed,
        &local_bytes_drop_allowed,
        &tuple_composite_drop_allowed,
        &returned_aggregate_members,
        &consumed_local_aggregate_members,
        &spawn_consumed_handle_members,
        &closure_vec_drop_allowed,
        &plain_vec_drop_allowed,
        &indirect_enum_drop_allowed,
        &affine_release_guard_flags,
        &overwrite_guard_flags,
        &collection_guard_flags,
        &actor_message_cow_guard_flags,
        &conditional_record_guard_flags,
        &vec_iter_guard_flags,
        &latest_owner_by_binding,
        &projection_alias_tainted,
        &borrowed_builtin_handle_projection_aliases,
        &borrow_getter_aliases,
    );
    let (elab_blocks, mut drop_plans) = enumerate_exits(
        blocks,
        &lifo_drops,
        &dataflow_result.exit_states,
        &dataflow_result.entry_states,
        &ownership_binding_locals,
        &cooperate_sites
            .iter()
            .map(|site| site.bb_id)
            .collect::<HashSet<_>>(),
        &projection_alias_tainted,
        &mir_owner_guards,
    );

    // `GeneratorNext` borrows its generator on the normal edge, but a panic
    // from `hew_cont_resume` exits the whole caller before the loop's inline
    // body-end destroy can run. Re-admit that exact carried OwnerId only on the
    // instruction's unwind plan. This is program-point ownership, not a
    // function-global type/shape recovery: Builder sealed the instruction as
    // the block's last operation and attached the current owner generation.
    for block in blocks {
        let Some((ctx, owner)) =
            block
                .instructions
                .iter()
                .find_map(|instruction| match instruction {
                    Instr::GeneratorNext {
                        ctx,
                        ctx_owner: Some(owner),
                        ..
                    } => Some((*ctx, *owner)),
                    _ => None,
                })
        else {
            continue;
        };
        let Some((_, _, owner_ty)) = owned_locals_snapshot
            .iter()
            .find(|(binding, _, _)| *binding == owner.binding)
        else {
            elaboration_diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::DropPlanUndetermined {
                    block: block.id,
                    reason: format!(
                        "GeneratorNext carries missing owner {owner:?} for context {ctx:?}"
                    ),
                },
                note: "generator resume cleanup requires a live typed owner generation".to_string(),
            });
            continue;
        };
        let Some(owner_place) = ownership_binding_locals.get(&owner.binding).copied() else {
            elaboration_diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::DropPlanUndetermined {
                    block: block.id,
                    reason: format!(
                        "GeneratorNext owner {owner:?} has no canonical storage place"
                    ),
                },
                note: "generator resume cleanup must release the carried owner, not its borrowed context alias"
                    .to_string(),
            });
            continue;
        };
        let Some((_, plan)) = drop_plans.iter_mut().find(|(exit, _)| {
            matches!(
                exit,
                ExitPath::Unwind { block: exit_block, callee }
                    if *exit_block == block.id && callee == "hew_cont_resume"
            )
        }) else {
            continue;
        };
        if !plan.drops.iter().any(|drop| drop.place == owner_place) {
            plan.drops.push(ElabDrop {
                place: owner_place,
                ty: owner_ty.clone(),
                drop_fn: None,
                kind: drop_kind_for(owner_place, owner_ty, None),
                guard: None,
            });
        }
    }

    // A first-class VecIter cursor is an inline record whose field-0 Vec
    // snapshot is released on ordinary lexical/explicit exits by a
    // flag-gated RecordFieldDrop. Cancellation, panic, yield-destroy, and
    // suspend-destroy can abandon the frame without traversing that inline
    // cleanup. Re-admit the same typed field release on those alternate
    // terminal edges only while exact owner replay says the cursor binding is
    // live. A conditional cursor publishes a Guard event and carries it as the
    // ElabDrop guard; an unconditional named cursor needs no sidecar and must
    // still receive unwind cleanup.
    //
    // Function-entry cancellation observes the block ENTRY state because its
    // check runs before entry-block instructions. Every other abandonment
    // point observes EXIT state, matching enumerate_exits and codegen's
    // cooperate/suspend placement.
    let (vec_iter_owner_entries, vec_iter_owner_exits) = exact_owner_states(blocks);
    for (binding, _, cursor_ty) in owned_locals_snapshot.iter().rev() {
        let Some(&place) = ownership_binding_locals.get(binding) else {
            continue;
        };
        let Some(release) = builder.vec_iter_cursor_release_protocol(cursor_ty) else {
            continue;
        };
        for (exit, plan) in &mut drop_plans {
            let _block = match exit {
                ExitPath::Cancel { block }
                | ExitPath::Unwind { block, .. }
                | ExitPath::Panic { block }
                | ExitPath::Yield { block, .. }
                | ExitPath::Suspend { block, .. } => *block,
                ExitPath::Return { .. }
                | ExitPath::Goto { .. }
                | ExitPath::Branch { .. }
                | ExitPath::Call { .. }
                | ExitPath::Send { .. }
                | ExitPath::Ask { .. }
                | ExitPath::Select { .. }
                | ExitPath::Join { .. } => continue,
            };
            let exact_state = exact_owner_state_for_exit(
                exit,
                blocks,
                &vec_iter_owner_entries,
                &vec_iter_owner_exits,
            );
            let exact_owner = exact_state.iter().find_map(|(owner, owner_place)| {
                (owner.binding == *binding && *owner_place == place).then_some(*owner)
            });
            if exact_owner.is_none()
                || plan.drops.iter().any(|drop| {
                    drop.place == place && matches!(drop.kind, DropKind::VecIterCursor { .. })
                })
            {
                continue;
            }
            plan.drops.push(ElabDrop {
                place,
                ty: cursor_ty.clone(),
                drop_fn: None,
                kind: DropKind::VecIterCursor { release },
                guard: exact_owner.and_then(|owner| {
                    mir_owner_guards
                        .get(&owner)
                        .copied()
                        .map(|flag| crate::model::ElabDropGuard { owner, flag })
                }),
            });
        }
    }

    // A VecIter `Some(x)` binder is a fresh per-iteration owner. Normal body
    // completion and explicit break/continue/return edges release it inline,
    // but cancellation/panic/suspend abandonment can leave from the middle of
    // the body without traversing those instructions. Re-admit its typed drop
    // only on exits in the body-owned region. In particular, never put it in
    // the function-wide LIFO: that would also add stale scope-close/back-edge
    // drops after the inline release and would resurrect releases on the
    // corrected Option CFG's unreachable all-Uninit block.
    for exit_drop in &builder.vec_iter_yield_exit_drops {
        let region = vec_iter_yield_body_region(blocks, exit_drop);

        for (exit, plan) in &mut drop_plans {
            let block = match exit {
                ExitPath::Cancel { block }
                | ExitPath::Unwind { block, .. }
                | ExitPath::Panic { block }
                | ExitPath::Yield { block, .. }
                | ExitPath::Suspend { block, .. } => *block,
                ExitPath::Return { .. }
                | ExitPath::Goto { .. }
                | ExitPath::Branch { .. }
                | ExitPath::Call { .. }
                | ExitPath::Send { .. }
                | ExitPath::Ask { .. }
                | ExitPath::Select { .. }
                | ExitPath::Join { .. } => continue,
            };
            if !region.contains(&block) {
                continue;
            }
            let exact_state = exact_owner_state_for_exit(
                exit,
                blocks,
                &vec_iter_owner_entries,
                &vec_iter_owner_exits,
            );
            let exact_places = exact_state
                .iter()
                .filter_map(|(owner, place)| (owner.binding == exit_drop.binding).then_some(*place))
                .collect::<Vec<_>>();
            let [exact_place] = exact_places.as_slice() else {
                continue;
            };
            if plan.drops.iter().any(|drop| drop.place == *exact_place) {
                continue;
            }
            plan.drops.push(ElabDrop {
                place: *exact_place,
                ty: exit_drop.ty.clone(),
                drop_fn: None,
                kind: exit_drop.kind,
                guard: None,
            });
        }
    }

    super::for_await_drop_plan::admit_terminal_handoff_drops(
        blocks,
        builder,
        dataflow_result,
        &returned_member_candidates,
        &mut drop_plans,
    );

    // Path-sensitive re-admission of returned-aggregate member drops. A member
    // handed to the caller through the `ReturnSlot` is removed from EVERY drop
    // class (`build_lifo_drops` skips `returned_aggregate_members` before any
    // arm) because on the return path the caller owns it. But a guard
    // early-return that exits BEFORE the aggregate is constructed still owns the
    // member locally and must release it — the `semver::try_parse` guard-return
    // and `base64::decode` reject-path leaks. The same fact holds on an arm's
    // `Goto` to the match/if join: when a sibling member is handed to the
    // caller on the OTHER arm, the whole-function exclusion must not strand the
    // still-live sibling on this arm. Restore the scope-exit drop on exactly the
    // `Return` and arm-to-join `Goto` exits whose member transfer site provably
    // cannot occur on this execution, and only where the dataflow proves it
    // definitely `Live` (owned) at the source exit. This is purely additive: it
    // never removes an existing drop, and it emits one only where the value is
    // provably the still-live sole owner, so it can leak-fix without any
    // double-free. Scoped to leaf CoW values (`string` / `bytes`) and scalar
    // affine resources. The latter reconstruct the SAME typed close descriptor
    // and consume guard as `build_lifo_drops`: a divergent return such as
    // `if c { (s1, r1) } else { (s2, r2) }` transfers only one arm's
    // Sink/Stream pair, while the other pair remains locally owned and must be
    // closed on that arm-to-join edge. Field-bearing resource records stay
    // excluded here because their release is the recursive `RecordInPlace`
    // ritual rather than the scalar resource close; imprecision therefore
    // remains leak-safe instead of selecting an incomplete teardown.
    // LESSONS: cleanup-all-exits, raii-null-after-move, boundary-fail-closed,
    // drop-allowset-from-value-flow.
    if !returned_member_transfer_blocks.is_empty() {
        let owned_ty_by_binding: HashMap<BindingId, &ResolvedTy> = returned_member_candidates
            .iter()
            .map(|(binding, _name, ty)| (*binding, ty))
            .collect();
        // Cache transitive CFG reachability once. Besides the transfer/read
        // proofs below, this is the authority that prevents two re-admitted
        // plans for the same owner from firing in sequence.
        let block_reach: HashMap<u32, HashSet<u32>> = blocks
            .iter()
            .map(|block| (block.id, blocks_reachable_from(blocks, block.id)))
            .collect();
        // Blocks each member's hand-off can reach (inclusive of the transfer
        // block itself). A `Return` exit inside this set is at or downstream of
        // the hand-off, so the caller owns the value there — no re-admission.
        let mut member_transfer_reach: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for (binding, transfer_blocks) in &returned_member_transfer_blocks {
            if transfer_blocks.is_empty() {
                continue;
            }
            let mut reach: HashSet<u32> = HashSet::new();
            for &transfer_block in transfer_blocks {
                reach.insert(transfer_block);
                if let Some(from_transfer) = block_reach.get(&transfer_block) {
                    reach.extend(from_transfer);
                }
            }
            member_transfer_reach.insert(*binding, reach);
        }
        // The forward set above answers "has this member already been handed
        // off?". A Goto needs the complementary question too: "can this edge
        // still reach a hand-off later?". Build it by walking the CFG backwards
        // from each transfer. An unresolved/ambiguous route stays in this set
        // and therefore suppresses the re-admission (leak, never early free).
        let mut reverse_cfg: HashMap<u32, Vec<u32>> = HashMap::new();
        for block in blocks {
            for successor in block.successors() {
                reverse_cfg.entry(successor).or_default().push(block.id);
            }
        }
        let mut member_transfer_predecessors: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for (binding, transfer_blocks) in &returned_member_transfer_blocks {
            let mut predecessors = transfer_blocks.clone();
            let mut worklist: Vec<u32> = transfer_blocks.iter().copied().collect();
            while let Some(block) = worklist.pop() {
                for predecessor in reverse_cfg.get(&block).into_iter().flatten() {
                    if predecessors.insert(*predecessor) {
                        worklist.push(*predecessor);
                    }
                }
            }
            member_transfer_predecessors.insert(*binding, predecessors);
        }
        // A Goto is not itself terminal. Even after ruling out a later return
        // transfer, retain the blanket exclusion if the join can still READ the
        // member. This makes the arm cleanup a last-use release rather than an
        // eager free: a new lowering shape that keeps using a sibling after the
        // join fails closed by leaking it instead of freeing it early.
        let candidate_locals: HashMap<u32, BindingId> = returned_member_candidates
            .iter()
            .filter_map(|(binding, _name, _ty)| {
                builder
                    .binding_locals
                    .get(binding)
                    .and_then(|place| base_local(*place))
                    .map(|local| (local, *binding))
            })
            .collect();
        let member_read_blocks =
            returned_member_alias_read_blocks(blocks, &builder.suspend_kinds, &candidate_locals);
        let mut member_read_predecessors: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for (binding, read_blocks) in &member_read_blocks {
            let mut predecessors = read_blocks.clone();
            let mut worklist: Vec<u32> = read_blocks.iter().copied().collect();
            while let Some(block) = worklist.pop() {
                for predecessor in reverse_cfg.get(&block).into_iter().flatten() {
                    if predecessors.insert(*predecessor) {
                        worklist.push(*predecessor);
                    }
                }
            }
            member_read_predecessors.insert(*binding, predecessors);
        }
        for (binding, reach) in &member_transfer_reach {
            let Some(ty) = owned_ty_by_binding.get(binding).copied() else {
                continue;
            };
            // A borrowed builtin-handle projection never owns an independent
            // close, including on an early exit before a later return handoff.
            // Its parent aggregate remains the sole release authority.
            if borrowed_builtin_handle_projection_aliases.contains(binding) {
                continue;
            }
            let Some(&place) = ownership_binding_locals.get(binding) else {
                continue;
            };
            let re_admission_drop = if matches!(ty, ResolvedTy::String | ResolvedTy::Bytes) {
                ElabDrop {
                    place,
                    ty: ty.clone(),
                    drop_fn: None,
                    kind: drop_kind_for(place, ty, None),
                    guard: None,
                }
            } else if matches!(
                ValueClass::of_ty(ty, &builder.type_classes),
                ValueClass::AffineResource
            ) && !builder.is_owned_aggregate_record_ty(ty)
                && !(matches!(
                    ty,
                    ResolvedTy::Named {
                        builtin: Some(BuiltinType::Rc | BuiltinType::Weak),
                        ..
                    }
                ) && base_local(place)
                    .is_some_and(|local| projection_alias_tainted.contains(&local)))
            {
                ElabDrop {
                    place,
                    ty: ty.clone(),
                    drop_fn: place_aware_drop_fn(
                        place,
                        resource_drop_fn(ty, &builder.type_classes),
                    ),
                    kind: drop_kind_for(place, ty, None),
                    guard: guarded_by_latest(*binding, &affine_release_guard_flags),
                }
            } else {
                continue;
            };

            // First collect every locally-valid re-admission without mutating
            // the plans. Deciding one plan at a time is order-dependent: two
            // consecutive Gotos can both look locally valid and schedule the
            // same unconditional release, as in static_server::resolve_path.
            let mut candidates = Vec::new();
            for (plan_index, (exit, plan)) in drop_plans.iter().enumerate() {
                let (block, goto_target) = match exit {
                    ExitPath::Return { block }
                    | ExitPath::Panic { block }
                    | ExitPath::Cancel { block } => (*block, None),
                    ExitPath::Yield { block, .. } | ExitPath::Suspend { block, .. } => {
                        (*block, None)
                    }
                    ExitPath::Goto { block, target } => (*block, Some(*target)),
                    _ => continue,
                };
                // Abandonment exits run their plan after the block's own
                // instructions, except the function-entry cancellation branch
                // which leaves before that block executes. A normal exit at or
                // after the hand-off belongs to the caller; the entry cancel
                // has not reached a later transfer even when block 0 contains it.
                let is_entry_cancel =
                    matches!(exit, ExitPath::Cancel { .. }) && block == ENTRY_BLOCK_ID;
                if !is_entry_cancel && reach.contains(&block) {
                    continue;
                }
                if let Some(target) = goto_target {
                    // A normal continuation can still perform this member's
                    // transfer: retain the blanket exclusion until then.
                    if member_transfer_predecessors
                        .get(binding)
                        .is_some_and(|predecessors| predecessors.contains(&block))
                    {
                        continue;
                    }
                    if member_read_predecessors
                        .get(binding)
                        .is_some_and(|predecessors| predecessors.contains(&target))
                    {
                        continue;
                    }
                    // A release attached to a loop edge can fire more than
                    // once for one mint. Without a per-iteration owner flag,
                    // re-admission on such an edge is not exactly-once.
                    if target == block
                        || block_reach
                            .get(&target)
                            .is_some_and(|reachable| reachable.contains(&block))
                    {
                        continue;
                    }
                }
                // Only re-admit where the value is definitely still owned. A
                // `MaybeConsumed`/`Uninit`/`Consumed` state at this exit is
                // ambiguous or already discharged — skip (fail-closed).
                let state_maps = if is_entry_cancel {
                    &dataflow_result.entry_states
                } else {
                    &dataflow_result.exit_states
                };
                let Some(state_map) = state_maps.get(&block) else {
                    continue;
                };
                if !matches!(
                    state_map.get(binding).copied(),
                    Some(dataflow::BindingState::Live)
                ) {
                    continue;
                }
                if plan.drops.iter().any(|drop| drop.place == place) {
                    continue;
                }
                candidates.push(ReturnedMemberReAdmission {
                    plan_index,
                    block,
                    path: returned_member_re_admission_path(exit),
                });
            }

            // Existing plans participate in the same dominance/post-dominance
            // proof as new candidates. A plain reachability veto is too coarse:
            // in a diamond, a release on one arm reaches a common candidate,
            // but deleting the common candidate leaks the sibling arm. The
            // common candidate may replace that arm-local release exactly when
            // it postdominates the release's continuation. Removals stay
            // contingent on the replacement candidate surviving the
            // candidate-vs-candidate selection below.
            let existing_releases: Vec<ReturnedMemberReAdmission> = drop_plans
                .iter()
                .enumerate()
                .filter_map(|(index, (exit, plan))| {
                    if plan.drops.iter().any(|drop| drop.place == place) {
                        Some(ReturnedMemberReAdmission {
                            plan_index: index,
                            block: exit_block_id(exit),
                            path: returned_member_re_admission_path(exit),
                        })
                    } else {
                        None
                    }
                })
                .collect();
            let mut replacements: HashMap<usize, HashSet<usize>> = HashMap::new();
            let mut ambiguity = None;
            candidates.retain(|candidate| {
                let replaced = match existing_releases_replaced_by_candidate(
                    blocks,
                    &block_reach,
                    *candidate,
                    &existing_releases,
                ) {
                    Ok(Some(replaced)) => replaced,
                    Ok(None) => return false,
                    Err(found) => {
                        ambiguity = Some(found);
                        return false;
                    }
                };
                replacements.insert(candidate.plan_index, replaced);
                true
            });
            if let Some(ambiguity) = ambiguity {
                elaboration_diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::DropPlanUndetermined {
                        block: ambiguity.first.block,
                        reason: format!(
                            "returned-member cleanup candidates at bb{} and bb{} partially overlap",
                            ambiguity.first.block, ambiguity.second.block
                        ),
                    },
                    note: "returned-member cleanup has no single exactly-once owner on every \
                           normal path; refusing to emit a partial release plan"
                        .to_string(),
                });
                continue;
            }

            let mut selected = match select_returned_member_re_admissions(
                blocks,
                &block_reach,
                &candidates,
            ) {
                Ok(selected) => selected,
                Err(ambiguity) => {
                    elaboration_diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::DropPlanUndetermined {
                            block: ambiguity.first.block,
                            reason: format!(
                                "returned-member cleanup candidates at bb{} and bb{} partially overlap",
                                ambiguity.first.block, ambiguity.second.block
                            ),
                        },
                        note: "returned-member cleanup has no single exactly-once owner on every \
                               normal path; refusing to emit a partial release plan"
                            .to_string(),
                    });
                    continue;
                }
            };

            let mut replaced_existing: HashSet<usize> = selected
                .iter()
                .flat_map(|candidate| {
                    replacements
                        .get(&candidate.plan_index)
                        .into_iter()
                        .flatten()
                        .copied()
                })
                .collect();
            // Existing release plans are not candidates, so arbitration with
            // abandonment exits occurs only after their normal replacements are
            // known. A still-surviving normal Goto suppresses its later
            // abandonment plan; a selected normal Goto replaces only the later
            // existing abandonment it must execute before.
            selected.retain(|candidate| {
                !matches!(candidate.path, ReturnedMemberReAdmissionPath::Abandonment)
                    || !existing_releases.iter().any(|release| {
                        !replaced_existing.contains(&release.plan_index)
                            && normal_goto_precedes_abandonment(
                                blocks,
                                &block_reach,
                                *release,
                                *candidate,
                            )
                    })
            });
            for candidate in &selected {
                if !matches!(candidate.path, ReturnedMemberReAdmissionPath::Normal) {
                    continue;
                }
                for release in &existing_releases {
                    if normal_goto_precedes_abandonment(blocks, &block_reach, *candidate, *release)
                    {
                        replaced_existing.insert(release.plan_index);
                    }
                }
            }
            for plan_index in replaced_existing {
                drop_plans[plan_index]
                    .1
                    .drops
                    .retain(|drop| drop.place != place);
            }
            for candidate in selected {
                let plan = &mut drop_plans[candidate.plan_index].1;
                plan.drops.push(re_admission_drop.clone());
            }
        }
    }

    // W60.114 — path-sensitive re-admission of a handler-owned `bytes` local
    // excluded from `local_bytes_drop_allowed` by a forwarding actor `Send` /
    // `Ask` / `RemoteAsk`. `derive_local_bytes_drop_allowed`'s escape scan
    // treats any such read as an unconditional, WHOLE-FUNCTION exclusion (its
    // own doc: "excluded twice over") — sound for every exit the transfer can
    // reach, but wrong for one it cannot: a `CooperateKind::FunctionEntry`
    // cancellation branch fires in the prologue, strictly before the rest of
    // the handler body runs, so a receive handler that cooperates and THEN
    // forwards its `bytes` parameter leaked it on cancellation-before-transfer
    // (the exclusion suppressed the drop on every exit, including the one
    // reached before the mailbox hand-off ever executed).
    //
    // Mirrors the returned-aggregate-member re-admission immediately above,
    // but only at a REAL release boundary. `emit_elab_drops` fires a
    // `Call`/`Branch`/ordinary `Goto` plan while normal execution continues;
    // `BindingState::Live` means definitely initialised, NOT "past last use".
    // Re-admitting at such a checkpoint can therefore free `data` before a
    // later `data.len()` on a non-forwarding branch. The safe boundary set is:
    //
    // * terminal Return/Panic;
    // * alternate Cancel/Yield/Suspend abandon edges (never the resume edge);
    // * a forward Goto crossing the exact CFG frontier from outside the
    //   transfer's downstream region into it. This includes the F-04 not-live
    //   recover edge and a conditional non-transfer arm's final join edge,
    //   while excluding nested Gotos and loop back-edges before that frontier.
    //   Because transfer reach is forward-closed, one execution can cross the
    //   frontier at most once.
    //
    // A `Cancel` exit at the function-entry block reads the block's ENTRY
    // state (the cancel branch precedes that block's own `Bind` statements —
    // see `drops_for_entry_cancel`); every other exit reads its EXIT state.
    // Both are the SAME dataflow `enumerate_exits` already threads through, so
    // this pass adds no new liveness authority — only a wider reach over
    // where the existing one is consulted.
    let bytes_mailbox_transfer_blocks = derive_bytes_actor_transfer_blocks(
        blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &ownership_binding_locals,
    );
    if !bytes_mailbox_transfer_blocks.is_empty() {
        let mut transfer_reach: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for (binding, transfer_blocks) in &bytes_mailbox_transfer_blocks {
            let mut reach: HashSet<u32> = HashSet::new();
            for &transfer_block in transfer_blocks {
                reach.insert(transfer_block);
                reach.extend(blocks_reachable_from(blocks, transfer_block));
            }
            transfer_reach.insert(*binding, reach);
        }
        // Reverse CFG once, then walk backwards from each binding's transfer
        // sites. This is the exact "can normally reach the transfer itself"
        // set (not its downstream reach) in O(B+E) per binding.
        let mut reverse_cfg: HashMap<u32, Vec<u32>> = HashMap::new();
        for block in blocks {
            for successor in block.successors() {
                reverse_cfg.entry(successor).or_default().push(block.id);
            }
        }
        let bytes_predecessor_of_transfer = |transfer_blocks: &HashSet<u32>| -> HashSet<u32> {
            let mut predecessors = transfer_blocks.clone();
            let mut worklist: Vec<u32> = transfer_blocks.iter().copied().collect();
            while let Some(block) = worklist.pop() {
                for predecessor in reverse_cfg.get(&block).into_iter().flatten() {
                    if predecessors.insert(*predecessor) {
                        worklist.push(*predecessor);
                    }
                }
            }
            predecessors
        };
        let mut transfer_predecessors: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for (binding, transfer_blocks) in &bytes_mailbox_transfer_blocks {
            transfer_predecessors.insert(*binding, bytes_predecessor_of_transfer(transfer_blocks));
        }

        // Blocks that can reach a later READ of each candidate (through any
        // whole-value alias). A non-terminal Goto may release only when its
        // target is outside this set: that is the exact last-use condition
        // missing from a mere `BindingState::Live` check.
        let candidate_roots: HashMap<u32, BindingId> = owned_locals_snapshot
            .iter()
            .filter(|(_, _, ty)| matches!(ty, ResolvedTy::Bytes))
            .filter_map(|(binding, _, _)| {
                builder
                    .binding_locals
                    .get(binding)
                    .and_then(|place| base_local(*place))
                    .map(|local| (local, *binding))
            })
            .collect();
        let alias_roots =
            propagate_whole_value_alias_roots(blocks, candidate_roots.keys().copied());
        let mut read_blocks: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        let mut aggregate_owner_blocks: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for block in blocks {
            for instr in &block.instructions {
                let owning_sources: Vec<Place> = match instr {
                    Instr::RecordInit { fields, .. } => {
                        fields.iter().map(|(_, source)| *source).collect()
                    }
                    Instr::TupleConstruct { elements, .. } => elements.clone(),
                    Instr::ClosureEnvInit { fields, .. } => fields
                        .iter()
                        .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
                        .map(|field| field.src)
                        .collect(),
                    Instr::Move {
                        dest: Place::MachineVariant { .. } | Place::EnumVariant { .. },
                        src,
                    } => vec![*src],
                    _ => Vec::new(),
                };
                for source in owning_sources {
                    let Some(local) = base_local(source) else {
                        continue;
                    };
                    let Some(root) = alias_roots.get(&local) else {
                        continue;
                    };
                    let Some(binding) = candidate_roots.get(root) else {
                        continue;
                    };
                    aggregate_owner_blocks
                        .entry(*binding)
                        .or_default()
                        .insert(block.id);
                }
            }
            let sources = block
                .instructions
                .iter()
                .flat_map(instr_source_places)
                .chain(terminator_source_places(
                    &block.terminator,
                    builder.suspend_kinds.get(&block.id),
                ));
            for source in sources {
                let Some(local) = base_local(source) else {
                    continue;
                };
                let Some(root) = alias_roots.get(&local) else {
                    continue;
                };
                let Some(binding) = candidate_roots.get(root) else {
                    continue;
                };
                read_blocks.entry(*binding).or_default().insert(block.id);
            }
        }
        let mut read_predecessors: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for (binding, blocks) in &read_blocks {
            read_predecessors.insert(*binding, bytes_predecessor_of_transfer(blocks));
        }
        // A local aggregate construction is another owner sink. Once reached,
        // its Record/Tuple/Closure drop owns the member release; the mailbox
        // transfer is not the sole reason the source binding was excluded.
        // Track its forward reach separately so pre-construction cancellation
        // and disjoint non-aggregate paths can still recover their own release.
        let mut aggregate_owner_reach: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for (binding, owner_blocks) in &aggregate_owner_blocks {
            let mut reach = owner_blocks.clone();
            for owner_block in owner_blocks {
                reach.extend(blocks_reachable_from(blocks, *owner_block));
            }
            aggregate_owner_reach.insert(*binding, reach);
        }

        for (exit, plan) in &mut drop_plans {
            let block = exit_block_id(exit);
            let is_cancel = matches!(exit, ExitPath::Cancel { .. });
            let is_entry_cancel = block == ENTRY_BLOCK_ID && is_cancel;
            let is_release_boundary = match exit {
                ExitPath::Return { .. }
                | ExitPath::Panic { .. }
                | ExitPath::Unwind { .. }
                | ExitPath::Cancel { .. }
                | ExitPath::Yield { .. }
                | ExitPath::Suspend { .. }
                // A Goto's per-binding last-use decision is completed below.
                | ExitPath::Goto { .. } => true,
                ExitPath::Branch { .. }
                | ExitPath::Call { .. }
                | ExitPath::Send { .. }
                | ExitPath::Ask { .. }
                | ExitPath::Select { .. }
                | ExitPath::Join { .. } => false,
            };
            if !is_release_boundary {
                continue;
            }
            let state_map = if is_entry_cancel {
                dataflow_result.entry_states.get(&block)
            } else {
                dataflow_result.exit_states.get(&block)
            };
            let Some(state_map) = state_map else {
                continue;
            };
            for (binding, reach) in &transfer_reach {
                // An actor-message guard is explicit runtime authority for a
                // mixed live/transferred join. Re-admit it only at a terminal
                // shared exit where the binding dataflow is `MaybeConsumed`:
                // flag zero releases the live arm and flag one suppresses the
                // transferred arm. Intermediate Gotos stay empty, so a later
                // non-transfer read cannot observe a prematurely freed value.
                // An aggregate-owner arm is excluded because its recursive
                // drop, not this source slot, owns that arm's payload.
                if actor_message_cow_guard_flags.contains_key(binding) && !is_entry_cancel {
                    let guarded_terminal_join = matches!(
                        exit,
                        ExitPath::Return { .. }
                            | ExitPath::Panic { .. }
                            | ExitPath::Unwind { .. }
                            | ExitPath::Cancel { .. }
                            | ExitPath::Yield { .. }
                            | ExitPath::Suspend { .. }
                    ) && matches!(
                        state_map.get(binding).copied(),
                        Some(dataflow::BindingState::MaybeConsumed(_))
                    ) && !aggregate_owner_reach
                        .get(binding)
                        .is_some_and(|owner_reach| owner_reach.contains(&block));
                    if guarded_terminal_join {
                        if let (Some(&place), Some(guard)) = (
                            ownership_binding_locals.get(binding),
                            guarded_by_latest(*binding, &actor_message_cow_guard_flags),
                        ) {
                            if !plan.drops.iter().any(|drop| drop.place == place) {
                                plan.drops.push(ElabDrop {
                                    place,
                                    ty: ResolvedTy::Bytes,
                                    drop_fn: None,
                                    kind: drop_kind_for(place, &ResolvedTy::Bytes, None),
                                    guard: Some(guard),
                                });
                            }
                        }
                    }
                    continue;
                }
                // This exit is at or downstream of the transfer: the
                // receiving actor owns the buffer there — no re-admission.
                if reach.contains(&block) {
                    continue;
                }
                // An owning local aggregate already carries the same reference
                // on this path and releases it through its in-place drop. The
                // function-entry cancel branch runs before entry instructions,
                // so a construction later in block 0 cannot suppress that
                // pre-construction release.
                if !is_entry_cancel
                    && aggregate_owner_reach
                        .get(binding)
                        .is_some_and(|owner_reach| owner_reach.contains(&block))
                {
                    continue;
                }
                // A Goto releases only on the single frontier where a
                // non-transfer path joins the transfer's downstream region.
                // Nested scope Gotos and loop back-edges remain outside the
                // forward-closed region and therefore cannot accumulate drops.
                if let ExitPath::Goto { target, .. } = exit {
                    if !reach.contains(target) {
                        continue;
                    }
                    if read_predecessors
                        .get(binding)
                        .is_some_and(|predecessors| predecessors.contains(target))
                    {
                        continue;
                    }
                }
                // Every non-`Cancel` exit kind is a sequential checkpoint,
                // not an alternate outcome: if its own block can still
                // normally reach the transfer, this execution goes on to
                // transfer ownership later — never drop here first. A
                // `Cancel` branch diverts BEFORE its block's own terminator
                // runs, so this predecessor fact does not apply to it.
                if !is_cancel
                    && transfer_predecessors
                        .get(binding)
                        .is_some_and(|predecessors| predecessors.contains(&block))
                {
                    continue;
                }
                if !matches!(
                    state_map.get(binding).copied(),
                    Some(dataflow::BindingState::Live)
                ) {
                    continue;
                }
                let Some(&place) = ownership_binding_locals.get(binding) else {
                    continue;
                };
                if let Some(existing) = plan.drops.iter_mut().find(|drop| drop.place == place) {
                    if is_entry_cancel {
                        // The function-entry cancellation branch precedes the
                        // entry block's flag zero-initialisation. Dataflow
                        // proves the mailbox parameter itself Live there, so
                        // this is an unconditional release; reading the
                        // not-yet-initialised runtime flag would be undefined.
                        existing.guard = None;
                    }
                    continue;
                }
                plan.drops.push(ElabDrop {
                    place,
                    ty: ResolvedTy::Bytes,
                    drop_fn: None,
                    kind: drop_kind_for(place, &ResolvedTy::Bytes, None),
                    guard: None,
                });
            }
        }
    }

    // Append each suspend's escape-poisoned abandon-edge drops (an in-flight
    // `StreamSend` value or fresh string arguments to a suspending
    // `CallClosure`) to its `ExitPath::Suspend` plan. `drops_for_exit`'s
    // `BindingState` filter cannot see these values, so lowering records them by
    // suspend block id and folds them in here. Appended AFTER the generic drops
    // so they release in the same LIFO order codegen walks; codegen fires the
    // whole plan on the case-1 destroy edge only.
    for (exit, plan) in &mut drop_plans {
        if let ExitPath::Suspend { block, .. } = exit {
            if let Some(extra) = builder.suspend_abandon_extra_drops.get(block) {
                // The binding-scoped prover can now see some active generator
                // values that originally needed this abandon-only mirror.
                // Keep the mirror only when the ordinary exit plan has not
                // already established the exact same release authority.
                for drop in extra {
                    if !plan.drops.contains(drop) {
                        plan.drops.push(drop.clone());
                    }
                }
            }
        }
    }

    // Final generation qualification covers every plan producer, including
    // specialized cursor/generator and abandon-edge additions made after the
    // generic exit enumeration. The exact owner state is replayed solely from
    // Checked-MIR operations. A Guard event overrides only the matching owner
    // generation; a later Reset can neither steal nor erase an earlier exit's
    // guard.
    let (exact_entries, exact_exits) = exact_owner_states(blocks);
    let (maybe_entries, maybe_exits) = maybe_owner_states(blocks);
    let actor_message_guard_owners = ownership_transfers
        .iter()
        .filter_map(|event| match event {
            crate::model::OwnershipEvent::Guard {
                owner,
                kind: crate::model::OwnershipGuardKind::ActorMessageCow,
                ..
            } => Some(*owner),
            _ => None,
        })
        .collect::<HashSet<_>>();
    for (exit, plan) in &mut drop_plans {
        let state = exact_owner_state_for_exit(exit, blocks, &exact_entries, &exact_exits);
        let required = guarded_required_owners_for_exit(
            exit,
            blocks,
            &exact_entries,
            &exact_exits,
            &maybe_entries,
            &maybe_exits,
            &actor_message_guard_owners,
        );
        // Specialized lowering paths may propose abandonment or returned-
        // member cleanup, but the immutable OwnerId program is the final
        // admission authority. The sole ownerless exception is a typed
        // StreamSend's record/enum payload: the pump owns this escape-poisoned
        // local without minting a lexical OwnerId, and its producer records the
        // exact abandon-only twin of the inline resume-edge drop. Require all
        // three closed facts (Suspend exit, StreamSend payload identity, exact
        // composite extra-drop entry) so no unknown kind/place can bypass the
        // validator. In particular, an explicit EdgeCarry still proves a Goto
        // transports its generation; a retrospective re-admission must not
        // schedule its destructor on that same edge.
        plan.drops.retain(|drop| {
            let owner_authorized = required.iter().any(|(owner, place)| {
                *place == drop.place && drop.guard.is_none_or(|guard| guard.owner == *owner)
            });
            let stream_send_composite_authorized =
                stream_send_composite_abandon_drops(builder, exit).contains(drop);
            owner_authorized || stream_send_composite_authorized
        });
        for drop in &mut plan.drops {
            let binding = drop.guard.map(|guard| guard.owner.binding);
            let mut owners = state.iter().filter_map(|(owner, place)| {
                (*place == drop.place && binding.is_none_or(|binding| owner.binding == binding))
                    .then_some(*owner)
            });
            let Some(owner) = owners.next() else {
                continue;
            };
            if owners.next().is_some() {
                continue;
            }
            if let Some(flag) = mir_owner_guards.get(&owner).copied() {
                drop.guard = Some(crate::model::ElabDropGuard { owner, flag });
            } else if let Some(guard) = &mut drop.guard {
                guard.owner = owner;
            }
        }
    }

    (
        ElaboratedMirFunction {
            name: name.to_owned(),
            return_ty: return_ty.clone(),
            statements: elaborated_statements,
            decisions: builder.decisions.clone(),
            blocks: elab_blocks,
            drop_plans,
            coroutine: None,
            // Lambda-actor capture set, populated by the MIR producer at
            // each `HirExprKind::SpawnLambdaActor` site (see
            // `Builder::lower_spawn_lambda_actor`). The HIR resolver
            // forward-binds the lambda's own let-name before lowering the
            // body, so a body-internal reference to that name resolves to
            // a `BindingRef { resolved: Binding(let_id) }`; the resolver
            // classifies that capture as `HirCaptureKind::Weak` and every
            // other free-variable reference as `Strong`. The MIR producer
            // copies the list through with the source binding's MIR
            // `Place` attached. The structural fail-closed checker
            // `validate_lambda_captures` enforces the invariants (Weak
            // attaches to LambdaActorHandle; at most one Weak per actor
            // handle) on the emitted ledger.
            lambda_captures: builder.lambda_captures.clone(),
        },
        elaboration_diagnostics,
    )
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
            .unwrap_or_else(|| panic!("owner {owner:?} defines Vec without an element type"));
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
                crate::ownership::FailClosedReason::UnenumeratedShape,
            ) => crate::ownership::CowHeapRelease::VecPlain,
            crate::ownership::VecElementRelease::Unsupported(
                crate::ownership::FailClosedReason::NoReleaseProtocol,
            ) if builder.elem_is_owned_abi_releasable(element) => {
                crate::ownership::CowHeapRelease::VecOwnedElement
            }
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
                panic!("owner {owner:?} defines dyn Trait without a storage discriminator")
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

/// Return the closed set of ownerless aggregate drops authorised for one
/// generator suspend. A stream send borrows its payload, so the pump remains
/// responsible for the in-flight value: normal resumption runs the inline
/// in-place drop, while abandonment runs this mutually-exclusive plan twin.
///
/// This is physical-cleanup authority, not a lexical ownership ledger. Require
/// the exact typed producer tuple recorded by lowering — Suspend block,
/// `StreamSend` payload place, composite kind, and aggregate classification —
/// so an unknown place/kind or a `BitCopy` record remains fail-closed.
fn stream_send_composite_abandon_drops(builder: &Builder, exit: &ExitPath) -> Vec<ElabDrop> {
    let ExitPath::Suspend { block, .. } = exit else {
        return Vec::new();
    };
    let Some(SuspendKind::StreamSend { value, .. }) = builder.suspend_kinds.get(block) else {
        return Vec::new();
    };
    builder
        .suspend_abandon_extra_drops
        .get(block)
        .into_iter()
        .flatten()
        .filter(|drop| {
            drop.place == *value
                && drop.guard.is_none()
                && drop.drop_fn.is_none()
                && match drop.kind {
                    DropKind::RecordInPlace => builder.is_owned_aggregate_record_ty(&drop.ty),
                    DropKind::EnumInPlace => ty_is_heap_owning_enum_composite(
                        &drop.ty,
                        &builder.record_field_orders,
                        &builder.enum_layouts,
                        builder.type_classes.lifecycle_registry(),
                    ),
                    _ => false,
                }
        })
        .cloned()
        .collect()
}

/// Whether final owner-recipe replay may materialize a destructor on this exit.
///
/// A guarded owner can be absent from the exact-state intersection at a join:
/// one predecessor transferred it and set the guard while another still owns
/// it. The guard is explicit generation authority for that conditional drop.
/// An unguarded record still needs the provisional per-exit plan to have
/// admitted it, preserving the fail-closed moved-field parent exclusion.
fn owner_recipe_admitted_on_exit(
    plan: &DropPlan,
    builder: &Builder,
    owner: crate::model::OwnerId,
    place: Place,
    kind: DropKind,
    is_guarded: bool,
) -> bool {
    if kind != DropKind::RecordInPlace || is_guarded {
        return true;
    }
    let binding_place = builder.binding_locals.get(&owner.binding).copied();
    plan.drops
        .iter()
        .any(|drop| drop.place == place || Some(drop.place) == binding_place)
}

#[allow(
    clippy::too_many_lines,
    reason = "recipe replay, generation selection, guard attachment, ordering, and exit synchronization form one atomic reconstruction pass"
)]
fn rebuild_drop_plans_from_owner_recipes(
    blocks: &[BasicBlock],
    decisions: &[super::DecisionFact],
    builder: &Builder,
    elaboration: &mut ElaboratedMirFunction,
) {
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
            "owner {owner:?} publishes more than one destructor recipe"
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
    let guarded_owners = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Guard { owner, .. }) => Some(*owner),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let (entries, exits) = exact_owner_states(blocks);
    let (maybe_entries, maybe_exits) = maybe_owner_states(blocks);
    let entry_parameter_owners = entry_cancel_parameter_owners(blocks, decisions);
    for (exit, plan) in &mut elaboration.drop_plans {
        // Recipe replay refines cleanup already admitted on this exact exit
        // with the live generation, place, and guard; it must not invent
        // admission. This keeps a source covered on pre-transfer unwind while
        // preserving its terminal sole-owner exclusion after handoff.
        // Function-entry cancellation is the sole exception: parameter storage
        // is live before MIR's leading Mint and is admitted by typed boundary
        // facts below.
        let is_entry_cancel =
            matches!(exit, ExitPath::Cancel { block } if *block == ENTRY_BLOCK_ID);
        let required = if is_entry_cancel {
            entry_parameter_owners.clone()
        } else {
            guarded_required_owners_for_exit(
                exit,
                blocks,
                &entries,
                &exits,
                &maybe_entries,
                &maybe_exits,
                &guarded_owners,
            )
        };
        let mut synthesized = required
            .iter()
            .filter_map(|(owner, place)| {
                let recipe = recipes.get(owner)?;
                if !is_entry_cancel
                    && !owner_recipe_admitted_on_exit(
                        plan,
                        builder,
                        *owner,
                        *place,
                        recipe.kind,
                        guarded_owners.contains(owner),
                    )
                {
                    return None;
                }
                Some({
                    (
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
                    )
                })
            })
            .collect::<Vec<_>>();
        synthesized.sort_by(|left, right| right.0.cmp(&left.0).then_with(|| right.1.cmp(&left.1)));

        let mut drops = synthesized
            .into_iter()
            .map(|(_, _, drop)| drop)
            .collect::<Vec<_>>();
        for drop in stream_send_composite_abandon_drops(builder, exit) {
            if !drops.contains(&drop) {
                drops.push(drop);
            }
        }
        plan.drops = drops;
    }
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
pub(super) fn materialize_exact_overwrite_releases(blocks: &mut [BasicBlock]) {
    use crate::model::OwnershipEvent;

    let recipes = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe { owner, recipe }) => {
                Some((*owner, recipe.clone()))
            }
            _ => None,
        })
        .collect::<HashMap<_, _>>();
    let guarded = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Guard { owner, .. }) => Some(*owner),
            _ => None,
        })
        .collect::<HashSet<_>>();
    loop {
        let (entries, _) = exact_owner_states(blocks);
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
                    if let ([old_owner], Some(move_index)) = (old.as_slice(), move_index) {
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
    return_ty: ResolvedTy,
    blocks: Vec<BasicBlock>,
    _raw: &RawMirFunction,
    decisions: Vec<super::DecisionFact>,
    checks: Vec<MirCheck>,
    cooperate_sites: Vec<CooperateSite>,
    builder: &Builder,
    flat_statements: &[MirStatement],
    dataflow_result: &dataflow::DataflowResult,
    precomputed_cow_drop_allowed: Option<&HashSet<BindingId>>,
    precomputed_local_bytes_drop_allowed: Option<&HashSet<BindingId>>,
) -> (CheckedMirFunction, Vec<MirDiagnostic>) {
    let (mut ownership_elaboration, mut diagnostics) = derive_elaboration(
        &name,
        &return_ty,
        &blocks,
        &cooperate_sites,
        builder,
        flat_statements,
        dataflow_result,
        precomputed_cow_drop_allowed,
        precomputed_local_bytes_drop_allowed,
    );
    rebuild_drop_plans_from_owner_recipes(&blocks, &decisions, builder, &mut ownership_elaboration);
    synchronize_cleanup_blocks(&mut ownership_elaboration);
    let field_drop_admissible = |ty: &ResolvedTy| builder.field_drop_in_place_admissible(ty);
    for check in validate_field_drop_in_place(
        &blocks,
        &ownership_elaboration,
        &builder.locals,
        &builder.enum_layouts,
        &field_drop_admissible,
    ) {
        if let Some(diagnostic) = check_to_diagnostic(&check) {
            diagnostics.push(diagnostic);
        }
    }
    let checked = CheckedMirFunction {
        name,
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
#[allow(
    clippy::match_same_arms,
    clippy::too_many_lines,
    reason = "the generation-aware transfer function and its diagnostics form one validator invariant"
)]
pub(super) fn validate_ownership_events(checked: &CheckedMirFunction) -> Vec<MirCheck> {
    use crate::model::{OwnerId, OwnershipEvent};

    let (entries, exits) = exact_owner_states(&checked.blocks);
    let (maybe_entries, maybe_exits) = maybe_owner_states(&checked.blocks);
    let (must_binding_entries, _) = must_binding_owner_states(&checked.blocks);
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
                        reason: format!("owner {owner:?} has more than one definition site"),
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
                            "owner {owner:?} publishes conflicting cleanup guards: {prior_kind:?}@{prior_flag:?} and {kind:?}@{flag:?}"
                        ),
                    });
                }
            }
        }
    }
    if checked.ownership_elaboration.is_some() {
        for (owner, ty) in &definition_types {
            match recipes_by_owner.get(owner).map(Vec::as_slice) {
                Some([recipe]) if &recipe.ty == ty => {}
                Some([recipe]) => findings.push(MirCheck::DischargeAuthorityDrift {
                    function: checked.name.clone(),
                    block: ENTRY_BLOCK_ID,
                    name: "ownership-recipe".to_owned(),
                    reason: format!(
                        "owner {owner:?} is defined as {ty}, but its destructor recipe names {}",
                        recipe.ty
                    ),
                }),
                Some(recipes) => findings.push(MirCheck::DischargeAuthorityDrift {
                    function: checked.name.clone(),
                    block: ENTRY_BLOCK_ID,
                    name: "ownership-recipe".to_owned(),
                    reason: format!(
                        "owner {owner:?} must publish exactly one destructor recipe, found {}",
                        recipes.len()
                    ),
                }),
                None => findings.push(MirCheck::DischargeAuthorityDrift {
                    function: checked.name.clone(),
                    block: ENTRY_BLOCK_ID,
                    name: "ownership-recipe".to_owned(),
                    reason: format!("owner {owner:?} has no definition-site destructor recipe"),
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
                        "destructor recipe for {owner:?} has no matching owner definition"
                    ),
                });
            }
        }
    }
    findings.extend(relocated_binding_use_checks(
        checked,
        &entries,
        &definition_places,
    ));
    let guarded_owners = published_guards.keys().copied().collect::<HashSet<_>>();
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
                            "owner {owner:?} is minted while another generation of that binding is already live"
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
                        "owner {owner:?} is transferred after its generation ended"
                    )),
                    Some(actual) if actual != from => {
                        let paired_relocation = to == &Some(*actual)
                            && pending_relocations.get(owner) == Some(&(*from, *actual));
                        if paired_relocation {
                            None
                        } else {
                            Some(format!(
                                "owner {owner:?} is transferred from {from:?}, but Checked MIR records its current place as {actual:?}"
                            ))
                        }
                    }
                    Some(_) if to_owner.is_some() && to.is_none() => Some(format!(
                        "owner {owner:?} transfers to a new owner without a destination place"
                    )),
                    Some(_) if to_owner.is_some() != to_ty.is_some() => Some(format!(
                        "owner {owner:?} transfer successor identity/type authority is incomplete"
                    )),
                    Some(_) if to_owner.is_some_and(|next| live.contains_key(&next)) => {
                        Some(format!(
                            "owner {owner:?} transfers into an already-live owner generation {to_owner:?}"
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
                        "owner {owner:?} is relocated after its generation ended"
                    )),
                    Some(actual) if actual != from && actual != to => Some(format!(
                        "owner {owner:?} is relocated from {from:?}, but Checked MIR records its current place as {actual:?}"
                    )),
                    Some(_) => None,
                },
                OwnershipEvent::Release { owner, place } => match live.get(owner) {
                    None => Some(format!(
                        "owner {owner:?} is released after its generation ended"
                    )),
                    Some(actual) if actual != place => Some(format!(
                        "owner {owner:?} is released from {place:?}, but Checked MIR records its current place as {actual:?}"
                    )),
                    Some(_) => None,
                },
                OwnershipEvent::GuardedRelease { owner, place, flag } => {
                    if published_guards.get(owner).map(|(published, _)| published) != Some(flag) {
                        Some(format!(
                            "guarded release for {owner:?} names {flag:?}, but that owner publishes {:?}",
                            published_guards.get(owner).map(|(published, _)| published)
                        ))
                    } else if live.get(owner).is_some_and(|actual| actual != place) {
                        Some(format!(
                            "guarded release for {owner:?} names {place:?}, but Checked MIR records its current place as {:?}",
                            live.get(owner)
                        ))
                    } else {
                        None
                    }
                }
                OwnershipEvent::DemoteToAlias { owner, place } => match live.get(owner) {
                    None => Some(format!(
                        "owner {owner:?} is demoted after its generation ended"
                    )),
                    Some(actual) if actual != place => Some(format!(
                        "owner {owner:?} is demoted at {place:?}, but Checked MIR records its current place as {actual:?}"
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
                            "reset {previous:?} -> {replacement:?} is not the next generation"
                        ))
                    } else if !(live.contains_key(previous)
                        || (published_guards.contains_key(previous)
                            && maybe_entries
                                .get(&block.id)
                                .is_some_and(|state| state.contains(&(*previous, *place)))))
                    {
                        Some(format!(
                            "reset source {previous:?} is neither live on every incoming path nor conditionally live under its published guard"
                        ))
                    } else if live.get(previous).is_some_and(|actual| actual != place) {
                        Some(format!(
                            "reset source {previous:?} names {place:?}, but Checked MIR records its current place as {:?}",
                            live.get(previous)
                        ))
                    } else if live.contains_key(replacement) {
                        Some(format!("reset replacement {replacement:?} is already live"))
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
                            "rearm {previous:?} -> {replacement:?} is not the next generation"
                        ))
                    } else if maybe_same_binding.as_slice() != [(*previous, *place)] {
                        Some(format!(
                            "rearm source {previous:?} is not the sole possibly-live generation at {place:?}: {maybe_same_binding:?}"
                        ))
                    } else if !exact_same_binding.is_empty()
                        && exact_same_binding.as_slice() != [(*previous, *place)]
                    {
                        Some(format!(
                            "rearm source {previous:?} has ambiguous exact generations/places {exact_same_binding:?}"
                        ))
                    } else if definition_places.get(previous) != Some(place)
                        || definition_places.get(replacement) != Some(place)
                    {
                        Some(format!(
                            "rearm {previous:?} -> {replacement:?} does not preserve its definition place {place:?}"
                        ))
                    } else if definition_types.get(previous) != Some(ty)
                        || definition_types.get(replacement) != Some(ty)
                    {
                        Some(format!(
                            "rearm {previous:?} -> {replacement:?} does not preserve its definition type {ty}"
                        ))
                    } else if !published_guards.contains_key(previous)
                        || published_guards.get(previous) != published_guards.get(replacement)
                    {
                        Some(format!(
                            "rearm {previous:?} -> {replacement:?} does not preserve one exact cleanup guard"
                        ))
                    } else if recipes_by_owner.get(previous).map(Vec::as_slice)
                        != recipes_by_owner.get(replacement).map(Vec::as_slice)
                        || recipes_by_owner
                            .get(previous)
                            .is_none_or(|recipes| recipes.len() != 1)
                    {
                        Some(format!(
                            "rearm {previous:?} -> {replacement:?} does not preserve one exact destructor recipe"
                        ))
                    } else if maybe_live.contains(&(*replacement, *place))
                        || live.contains_key(replacement)
                    {
                        Some(format!(
                            "rearm replacement {replacement:?} is already live before its lineage event"
                        ))
                    } else {
                        None
                    }
                }
                OwnershipEvent::Guard { owner, flag, .. } => {
                    (!live.contains_key(owner)).then(|| {
                        format!("cleanup guard {flag:?} is attached after owner {owner:?} ended")
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
                    if incoming.is_empty() || !same_binding || !distinct {
                        Some(format!(
                            "ownership join {incoming:?} -> {replacement:?} is not a non-empty set of distinct same-binding predecessors and successor"
                        ))
                    } else if possible != declared {
                        Some(format!(
                            "ownership join {incoming:?} -> {replacement:?} does not enumerate its exact possible incoming owners at {place:?}: {possible:?}"
                        ))
                    } else if !exact_same_binding.is_subset(&declared) {
                        Some(format!(
                            "ownership join {incoming:?} -> {replacement:?} has ambiguous exact incoming owners {exact_same_binding:?}"
                        ))
                    } else if must_binding_entries
                        .get(&block.id)
                        .and_then(|state| state.get(&replacement.binding))
                        != Some(place)
                    {
                        Some(format!(
                            "ownership join {incoming:?} -> {replacement:?} has an ownerless or wrong-place incoming path"
                        ))
                    } else if live.contains_key(replacement)
                        || maybe_live.contains(&(*replacement, *place))
                    {
                        Some(format!(
                            "ownership join replacement {replacement:?} is already live before convergence"
                        ))
                    } else if !metadata_matches {
                        Some(format!(
                            "ownership join {incoming:?} -> {replacement:?} does not preserve one exact place/type/guard/recipe"
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
        let binding_metadata: HashMap<BindingId, (String, SiteId)> = checked
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .filter_map(|statement| match statement {
                MirStatement::Bind {
                    binding,
                    name,
                    site,
                    ..
                } => Some((*binding, (name.clone(), *site))),
                _ => None,
            })
            .collect();
        for (exit, plan) in &elaboration.drop_plans {
            let block_id = exit_block_id(exit);
            let Some(_block) = checked.blocks.iter().find(|block| block.id == block_id) else {
                continue;
            };
            let live = exact_owner_state_for_exit(exit, &checked.blocks, &entries, &exits);
            let required = if matches!(exit, ExitPath::Cancel { block } if *block == ENTRY_BLOCK_ID)
            {
                entry_parameter_owners.clone()
            } else {
                guarded_required_owners_for_exit(
                    exit,
                    &checked.blocks,
                    &entries,
                    &exits,
                    &maybe_entries,
                    &maybe_exits,
                    &guarded_owners,
                )
            };
            for (binding, candidates) in ambiguous_guarded_owners_for_exit(
                exit,
                &checked.blocks,
                &maybe_entries,
                &maybe_exits,
                &guarded_owners,
            ) {
                if required.keys().any(|owner| owner.binding == binding) {
                    continue;
                }
                let (name, site) = binding_metadata
                    .get(&binding)
                    .cloned()
                    .unwrap_or_else(|| (format!("{binding:?}"), SiteId(0)));
                let local_ty = candidates
                    .first()
                    .and_then(|(owner, _)| definition_types.get(owner))
                    .map_or_else(|| "<unknown>".to_owned(), ToString::to_string);
                findings.push(MirCheck::ObligationUnderReleased {
                    function: checked.name.clone(),
                    blocks: vec![block_id],
                    site,
                    name,
                    local_ty,
                    mint_provenance: crate::model::ObligationMintProvenance::Ordinary,
                    reason: format!(
                        "conditionally live guarded owner has ambiguous generations/places {candidates:?} on {exit:?}; no frozen cleanup can be admitted"
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
                                    "cleanup at {:?} is guarded by {:?}@{:?}, but live generation {owner:?} carries {:?}",
                                    drop.place,
                                    guard.owner,
                                    guard.flag,
                                    owner_guards.get(owner)
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
                                "cleanup ritual at {:?} does not equal the definition-site recipe for {owner:?}",
                                drop.place
                            ),
                        }),
                    [owner] => findings.push(MirCheck::ObligationOverReleased {
                        function: checked.name.clone(),
                        block: block_id,
                        name: binding_metadata
                            .get(&owner.binding)
                            .map_or_else(|| format!("{owner:?}"), |(name, _)| name.clone()),
                        reason: format!(
                            "frozen exit plan contains more than one cleanup for exact owner {owner:?} at {:?}",
                            drop.place,
                        ),
                    }),
                    [] => findings.push(MirCheck::ObligationOverReleased {
                        function: checked.name.clone(),
                        block: block_id,
                        name: "checked-ownership-plan".to_owned(),
                        reason: format!(
                            "frozen exit plan drops {:?} as {}, but Checked MIR has no exact live owner requiring that cleanup on {exit:?}",
                            drop.place, drop.ty,
                        ),
                    }),
                    _ => findings.push(MirCheck::DischargeAuthorityDrift {
                        function: checked.name.clone(),
                        block: block_id,
                        name: "checked-ownership-plan".to_owned(),
                        reason: format!(
                            "cleanup at {:?} ambiguously matches multiple exact owner generations {candidates:?}",
                            drop.place,
                        ),
                    }),
                }
            }
            for (owner, place) in required {
                if matched.contains(&owner) {
                    continue;
                }
                let ty = owner_types
                    .get(&owner)
                    .map_or_else(|| "<unknown>".to_owned(), ToString::to_string);
                let (name, site) = binding_metadata
                    .get(&owner.binding)
                    .cloned()
                    .unwrap_or_else(|| (format!("{owner:?}"), SiteId(0)));
                findings.push(MirCheck::ObligationUnderReleased {
                    function: checked.name.clone(),
                    blocks: vec![block_id],
                    site,
                    name,
                    local_ty: ty,
                    mint_provenance: crate::model::ObligationMintProvenance::Ordinary,
                    reason: format!(
                        "exact owner {owner:?} remains live at {place:?} on {exit:?}, but the frozen cleanup plan contains no matching destructor"
                    ),
                });
            }
        }
    }
    findings
}

#[cfg(test)]
fn checked_with_ownership_events(events: Vec<Instr>) -> CheckedMirFunction {
    CheckedMirFunction {
        name: "ownership_event_falsifier".to_owned(),
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
        return_ty: ResolvedTy::Unit,
        blocks: vec![BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions,
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: Some(Box::new(ElaboratedMirFunction {
            name: "recipe_invariant".to_owned(),
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
    let initial = OwnerId {
        binding,
        generation: 0,
    };
    let branch = OwnerId {
        binding,
        generation: 1,
    };
    let joined = OwnerId {
        binding,
        generation: 2,
    };
    let rearmed = OwnerId {
        binding,
        generation: 3,
    };
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
            instructions: vec![],
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
fn final_join_refresh_leaves_ambiguous_stale_event_for_validation() {
    use crate::model::{OwnerId, OwnershipEvent};

    let mut checked = checked_join_rearm_fixture();
    let extra = OwnerId {
        binding: BindingId(180),
        generation: 4,
    };
    checked.blocks[1]
        .instructions
        .push(Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: extra,
            place: Place::Local(15),
            ty: ResolvedTy::String,
        }));
    let declared_before = checked.blocks[3]
        .instructions
        .iter()
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Join { incoming, .. }) => Some(incoming.clone()),
            _ => None,
        })
        .expect("fixture contains a Join");

    super::canonicalize_join_incoming_owner_ids(&mut checked.blocks, &Builder::default());

    let declared_after = checked.blocks[3]
        .instructions
        .iter()
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Join { incoming, .. }) => Some(incoming),
            _ => None,
        })
        .expect("fixture contains a Join");
    assert_eq!(declared_after, &declared_before);
    assert!(validate_ownership_events(&checked)
        .iter()
        .any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { reason, .. }
                if reason.contains("does not enumerate its exact possible incoming owners")
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
    let (_, exits) = exact_owner_states(&checked.blocks);
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
        matches!(finding, MirCheck::ObligationOverReleased { reason, .. }
            if reason.contains("no exact live owner"))
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

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the two-exit fixture spells out both generations and their complete MIR operations"
)]
fn exit_plans_keep_each_live_generation_guard_across_reset() {
    use crate::model::{ElabDropGuard, OwnerId, OwnershipEvent};

    let binding = BindingId(41);
    let old = OwnerId {
        binding,
        generation: 0,
    };
    let replacement = OwnerId {
        binding,
        generation: 1,
    };
    let place = Place::Local(9);
    let old_flag = Place::Local(20);
    let replacement_flag = Place::Local(21);
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
                Instr::OwnershipEvent(OwnershipEvent::Guard {
                    owner: old,
                    flag: old_flag,
                    kind: crate::model::OwnershipGuardKind::ActorMessageCow,
                }),
            ],
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
            terminator: Terminator::Return,
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Reset {
                    previous: old,
                    replacement,
                    place,
                    ty: ResolvedTy::String,
                }),
                Instr::OwnershipEvent(OwnershipEvent::Guard {
                    owner: replacement,
                    flag: replacement_flag,
                    kind: crate::model::OwnershipGuardKind::ActorMessageCow,
                }),
            ],
            terminator: Terminator::Return,
        },
    ];
    let lifo = vec![ElabDrop {
        place,
        ty: ResolvedTy::String,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::String,
        },
        guard: None,
    }];
    let live = std::collections::BTreeMap::from([(binding, dataflow::BindingState::Live)]);
    let states = HashMap::from([(0, live.clone()), (1, live.clone()), (2, live)]);
    let binding_locals = HashMap::from([(binding, place)]);
    let owner_guards = HashMap::from([(old, old_flag), (replacement, replacement_flag)]);

    let (_, plans) = enumerate_exits(
        &blocks,
        &lifo,
        &states,
        &states,
        &binding_locals,
        &HashSet::new(),
        &HashSet::new(),
        &owner_guards,
    );
    let return_guard = |block| {
        plans
            .iter()
            .find_map(|(exit, plan)| {
                matches!(exit, ExitPath::Return { block: actual } if *actual == block)
                    .then(|| plan.drops.first().and_then(|drop| drop.guard))
                    .flatten()
            })
            .expect("return plan must retain its generation-qualified guard")
    };
    assert_eq!(
        return_guard(1),
        ElabDropGuard {
            owner: old,
            flag: old_flag,
        }
    );
    assert_eq!(
        return_guard(2),
        ElabDropGuard {
            owner: replacement,
            flag: replacement_flag,
        }
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the structural oracle pins the complete scope-exit owner program and derived edge plan"
)]
fn scope_close_is_an_explicit_checked_mir_release_not_a_goto_plan() {
    use crate::model::{OwnerId, OwnershipEvent};

    let binding = BindingId(42);
    let owner = OwnerId {
        binding,
        generation: 0,
    };
    let place = Place::Local(9);
    let blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner,
                    place,
                    ty: ResolvedTy::String,
                }),
                Instr::Drop {
                    place,
                    ty: ResolvedTy::String,
                    drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                },
                Instr::OwnershipEvent(OwnershipEvent::Release { owner, place }),
                Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
                    scopes: vec![ScopeId(7)],
                    owners: vec![owner],
                    carry_places: vec![],
                    carried: vec![],
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
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];
    let lifo = vec![ElabDrop {
        place,
        ty: ResolvedTy::String,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::String,
        },
        guard: None,
    }];
    // Deliberately stale: the former scope-edge selector read this map and
    // deferred the arm-local owner past the join, losing its only cleanup.
    let stale_live = std::collections::BTreeMap::from([(binding, dataflow::BindingState::Live)]);
    let states = HashMap::from([
        (ENTRY_BLOCK_ID, stale_live.clone()),
        (1, stale_live.clone()),
        (2, stale_live.clone()),
        (3, stale_live),
    ]);

    let (_, plans) = enumerate_exits(
        &blocks,
        &lifo,
        &states,
        &states,
        &HashMap::from([(binding, place)]),
        &HashSet::new(),
        &HashSet::new(),
        &HashMap::new(),
    );
    let drops_on = |needle: ExitPath| {
        plans
            .iter()
            .find(|(exit, _)| *exit == needle)
            .map_or(0, |(_, plan)| plan.drops.len())
    };
    assert_eq!(
        drops_on(ExitPath::Goto {
            block: 1,
            target: 3,
        }),
        0,
        "scope cleanup executes at ScopeExit; the Goto plan must not rediscover it"
    );
    assert_eq!(
        drops_on(ExitPath::Goto {
            block: 2,
            target: 3,
        }),
        0,
        "the sibling path never minted the owner"
    );
    assert_eq!(
        drops_on(ExitPath::Return { block: 3 }),
        0,
        "an owner absent at the join must not be rediscovered at return"
    );
    let (_, exits) = exact_owner_states(&blocks);
    assert!(
        exits
            .get(&1)
            .is_some_and(|state| !state.contains_key(&owner)),
        "the explicit Release must end the exact generation before the edge"
    );
}

#[test]
fn scope_close_defers_same_generation_that_survives_the_join() {
    use crate::model::{OwnerId, OwnershipEvent};

    let binding = BindingId(43);
    let owner = OwnerId {
        binding,
        generation: 0,
    };
    let place = Place::Local(9);
    let blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner,
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
            instructions: vec![],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];
    let lifo = vec![ElabDrop {
        place,
        ty: ResolvedTy::String,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::String,
        },
        guard: None,
    }];
    let live = std::collections::BTreeMap::from([(binding, dataflow::BindingState::Live)]);
    let states = HashMap::from([
        (ENTRY_BLOCK_ID, live.clone()),
        (1, live.clone()),
        (2, live.clone()),
        (3, live),
    ]);

    let (_, plans) = enumerate_exits(
        &blocks,
        &lifo,
        &states,
        &states,
        &HashMap::from([(binding, place)]),
        &HashSet::new(),
        &HashSet::new(),
        &HashMap::new(),
    );
    for block in [1, 2] {
        let (_, plan) = plans
            .iter()
            .find(|(exit, _)| *exit == ExitPath::Goto { block, target: 3 })
            .expect("each branch has a scope-edge plan");
        assert!(
            plan.drops.is_empty(),
            "the same generation survives branch {block} and must not close early"
        );
    }
    let (_, return_plan) = plans
        .iter()
        .find(|(exit, _)| *exit == ExitPath::Return { block: 3 })
        .expect("join return plan");
    assert_eq!(return_plan.drops.len(), 1);
}

#[test]
fn exact_goto_plan_requires_only_owners_not_named_by_edge_carry() {
    use crate::model::{OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(44),
        generation: 0,
    };
    let place = Place::Local(7);
    let blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner,
                    place,
                    ty: ResolvedTy::String,
                }),
                Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
                    owner,
                    place,
                    target: 1,
                }),
            ],
            terminator: Terminator::Goto { target: 1 },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];
    let (entries, exits) = exact_owner_states(&blocks);
    let required = exact_required_owners_for_exit(
        &ExitPath::Goto {
            block: ENTRY_BLOCK_ID,
            target: 1,
        },
        &blocks,
        &entries,
        &exits,
    );

    assert!(
        required.is_empty(),
        "the exact source-side EdgeCarry is the sole authority that preserves this generation"
    );
}

#[test]
fn exact_goto_plan_requires_live_owner_when_edge_carry_is_missing() {
    use crate::model::{OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(45),
        generation: 0,
    };
    let place = Place::Local(2);
    let blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner,
                place,
                ty: ResolvedTy::String,
            })],
            terminator: Terminator::Goto { target: 1 },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];
    let (entries, exits) = exact_owner_states(&blocks);
    let required = exact_required_owners_for_exit(
        &ExitPath::Goto {
            block: ENTRY_BLOCK_ID,
            target: 1,
        },
        &blocks,
        &entries,
        &exits,
    );

    assert_eq!(required, HashMap::from([(owner, place)]));
}

#[test]
fn exact_goto_plan_rejects_stale_generation_edge_carry() {
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
                Instr::OwnershipEvent(OwnershipEvent::Reset {
                    previous: old,
                    replacement,
                    place,
                    ty: ResolvedTy::String,
                }),
                Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
                    owner: old,
                    place,
                    target: 1,
                }),
            ],
            terminator: Terminator::Goto { target: 1 },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];
    let (entries, exits) = exact_owner_states(&blocks);
    let required = exact_required_owners_for_exit(
        &ExitPath::Goto {
            block: ENTRY_BLOCK_ID,
            target: 1,
        },
        &blocks,
        &entries,
        &exits,
    );

    assert_eq!(
        required,
        HashMap::from([(replacement, place)]),
        "a stale EdgeCarry must never preserve the replacement generation"
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the unit spells the complete guarded branch/join ownership program"
)]
fn guarded_terminal_join_admits_one_unique_conditional_generation() {
    use crate::model::{OwnerDropRecipe, OwnerId, OwnershipEvent, OwnershipGuardKind};

    let owner = OwnerId {
        binding: BindingId(146),
        generation: 0,
    };
    let place = Place::Local(3);
    let flag = Place::Local(4);
    let recipe = OwnerDropRecipe {
        ty: ResolvedTy::Bytes,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::Bytes,
        },
        declaration_order: 0,
    };
    let blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![MirStatement::Bind {
                binding: owner.binding,
                name: "data".to_owned(),
                site: SiteId(1),
                ty: ResolvedTy::Bytes,
            }],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner,
                    place,
                    ty: ResolvedTy::Bytes,
                }),
                Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                    owner,
                    recipe: recipe.clone(),
                }),
                Instr::OwnershipEvent(OwnershipEvent::Guard {
                    owner,
                    flag,
                    kind: OwnershipGuardKind::ConditionalRecord,
                }),
            ],
            terminator: Terminator::Branch {
                cond: Place::Local(5),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from: place,
                to: None,
                to_owner: None,
                to_ty: None,
            })],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
                owner,
                place,
                target: 3,
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
    let mut elaboration = ElaboratedMirFunction {
        name: "guarded_one_generation".to_owned(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(ExitPath::Return { block: 3 }, DropPlan::default())],
        coroutine: None,
        lambda_captures: vec![],
    };

    rebuild_drop_plans_from_owner_recipes(&blocks, &[], &Builder::default(), &mut elaboration);
    assert_eq!(
        elaboration.drop_plans[0].1.drops,
        vec![ElabDrop {
            place,
            ty: recipe.ty.clone(),
            drop_fn: recipe.drop_fn.clone(),
            kind: recipe.kind,
            guard: Some(crate::model::ElabDropGuard { owner, flag }),
        }]
    );
    let checked = CheckedMirFunction {
        name: elaboration.name.clone(),
        return_ty: ResolvedTy::Unit,
        blocks,
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: Some(Box::new(elaboration)),
    };
    assert!(
        validate_ownership_events(&checked).is_empty(),
        "one guarded generation is exact conditional cleanup authority"
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the unit spells the complete reset-generation branch/join ownership program"
)]
fn guarded_terminal_join_rejects_ambiguous_conditional_reset_generation() {
    use crate::model::{OwnerDropRecipe, OwnerId, OwnershipEvent, OwnershipGuardKind};

    let binding = BindingId(147);
    let old = OwnerId {
        binding,
        generation: 0,
    };
    let replacement = OwnerId {
        binding,
        generation: 1,
    };
    let place = Place::Local(6);
    let recipe = OwnerDropRecipe {
        ty: ResolvedTy::Bytes,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::Bytes,
        },
        declaration_order: 0,
    };
    let blocks = vec![
        BasicBlock {
            id: ENTRY_BLOCK_ID,
            statements: vec![MirStatement::Bind {
                binding,
                name: "data".to_owned(),
                site: SiteId(2),
                ty: ResolvedTy::Bytes,
            }],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner: old,
                    place,
                    ty: ResolvedTy::Bytes,
                }),
                Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                    owner: old,
                    recipe: recipe.clone(),
                }),
                Instr::OwnershipEvent(OwnershipEvent::Guard {
                    owner: old,
                    flag: Place::Local(7),
                    kind: OwnershipGuardKind::ConditionalRecord,
                }),
            ],
            terminator: Terminator::Branch {
                cond: Place::Local(8),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
                owner: old,
                place,
                target: 3,
            })],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Reset {
                    previous: old,
                    replacement,
                    place,
                    ty: ResolvedTy::Bytes,
                }),
                Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                    owner: replacement,
                    recipe,
                }),
                Instr::OwnershipEvent(OwnershipEvent::Guard {
                    owner: replacement,
                    flag: Place::Local(9),
                    kind: OwnershipGuardKind::ConditionalRecord,
                }),
                Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
                    owner: replacement,
                    place,
                    target: 3,
                }),
            ],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];
    let mut elaboration = ElaboratedMirFunction {
        name: "guarded_ambiguous_generation".to_owned(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(ExitPath::Return { block: 3 }, DropPlan::default())],
        coroutine: None,
        lambda_captures: vec![],
    };

    rebuild_drop_plans_from_owner_recipes(&blocks, &[], &Builder::default(), &mut elaboration);
    assert!(
        elaboration.drop_plans[0].1.drops.is_empty(),
        "two generations for one binding/place must not acquire cleanup authority"
    );
    let checked = CheckedMirFunction {
        name: elaboration.name.clone(),
        return_ty: ResolvedTy::Unit,
        blocks,
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: Some(Box::new(elaboration)),
    };
    assert!(validate_ownership_events(&checked).iter().any(|finding| {
        matches!(finding, MirCheck::ObligationUnderReleased { reason, .. }
            if reason.contains("ambiguous generations/places"))
    }));
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
    let (_, exits) = exact_owner_states(&blocks);
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
    let (_, exits) = exact_owner_states(&blocks);
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
    let (_, exits) = exact_owner_states(&blocks);
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
            if reason.contains(&format!("owner {future:?} is relocated after its generation ended"))
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
    let (_, exits) = exact_owner_states(&blocks);
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
    let (_, exits) = exact_owner_states(&blocks);
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
    let (maybe_entries, _) = maybe_owner_states(&blocks);
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
    let (maybe_entries, _) = maybe_owner_states(&blocks);
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
fn checked_owner_recipe_rebuilds_the_exact_exit_without_builder_state() {
    use crate::model::{OwnerDropRecipe, OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(53),
        generation: 0,
    };
    let place = Place::Local(7);
    let blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![MirStatement::Bind {
            binding: owner.binding,
            name: "s".to_owned(),
            site: SiteId(1),
            ty: ResolvedTy::String,
        }],
        instructions: vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner,
                place,
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                owner,
                recipe: OwnerDropRecipe {
                    ty: ResolvedTy::String,
                    drop_fn: None,
                    kind: drop_kind_for(place, &ResolvedTy::String, None),
                    declaration_order: 3,
                },
            }),
        ],
        terminator: Terminator::Return,
    }];
    let mut elaboration = ElaboratedMirFunction {
        name: "recipe_replay".to_owned(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![ElabBlock {
            id: ENTRY_BLOCK_ID,
            kind: BlockKind::Normal,
            drops: vec![],
            successor: None,
        }],
        drop_plans: vec![(
            ExitPath::Return {
                block: ENTRY_BLOCK_ID,
            },
            DropPlan {
                drops: vec![ElabDrop {
                    place,
                    ty: ResolvedTy::String,
                    drop_fn: None,
                    kind: drop_kind_for(place, &ResolvedTy::String, None),
                    guard: None,
                }],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    };

    rebuild_drop_plans_from_owner_recipes(&blocks, &[], &Builder::default(), &mut elaboration);
    assert_eq!(elaboration.drop_plans[0].1.drops.len(), 1);
    assert_eq!(elaboration.drop_plans[0].1.drops[0].place, place);
    let checked = CheckedMirFunction {
        name: "recipe_replay".to_owned(),
        return_ty: ResolvedTy::Unit,
        blocks,
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: Some(Box::new(elaboration)),
    };
    assert!(validate_ownership_events(&checked).is_empty());
}

#[test]
fn missing_checked_owner_recipe_cannot_materialize_a_destructor() {
    use crate::model::{OwnerId, OwnershipEvent};

    let owner = OwnerId {
        binding: BindingId(54),
        generation: 0,
    };
    let blocks = vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: vec![],
        instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner,
            place: Place::Local(8),
            ty: ResolvedTy::String,
        })],
        terminator: Terminator::Return,
    }];
    let mut elaboration = ElaboratedMirFunction {
        name: "missing_recipe".to_owned(),
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
    };

    rebuild_drop_plans_from_owner_recipes(&blocks, &[], &Builder::default(), &mut elaboration);
    assert!(elaboration.drop_plans[0].1.drops.is_empty());
    let checked = CheckedMirFunction {
        name: "missing_recipe".to_owned(),
        return_ty: ResolvedTy::Unit,
        blocks,
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: Some(Box::new(elaboration)),
    };
    assert!(validate_ownership_events(&checked)
        .iter()
        .any(|finding| matches!(finding, MirCheck::ObligationUnderReleased { .. })));
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

    materialize_exact_overwrite_releases(&mut blocks);

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
    let (_, exits) = exact_owner_states(&blocks);
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

    let (entries, exits) = exact_owner_states(&blocks);
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
    let (_, exits) = exact_owner_states(&blocks);
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
            instructions: vec![Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: edge_local,
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
    let (entries, _) = exact_owner_states(&blocks);
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
///   - `Return` is the canonical Hew exit; carries the function-wide
///     LIFO drops narrowed by per-block live-set.
///   - `Panic` and `Cancel` exits transfer to a cleanup block whose
///     `ElabBlock.drops` carry the same LIFO drops; both the
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
                        "drop on place {:?} has kind {:?}, but the place \
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
                        "cleanup drop on place {:?} has kind {:?}, but the \
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
                        "unwind cleanup for call bb{} -> `{callee}` destroys place {:?} more \
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
                        "indirect closure unwind cleanup for bb{} at {call_site} destroys place {:?} more than once",
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
// ============================================================================
// S1 — lite obligation-balance validator
// ============================================================================
//
// `validate_obligation_balance` is the SIBLING pass to `validate_drop_plan`:
// that pass owns KIND coherence (place ↔ DropKind), this pass owns BALANCE —
// for every heap-owning owned local (the MINT set, read off the type-directed
// registration ledger), every reachable `Return` exit path must carry one
// DISCHARGE per owner MINT. Ordinary definitions mint one owner; explicit
// retain instructions mint an additional co-owner. Fewer discharges than
// mints = under-release (leak); more = over-release (double-free).
//
// INDEPENDENCE INVARIANT: the discharge set is re-derived from the primitive
// `Instr` stream + the raw CFG + the per-exit `DropPlan`s (the elaborator's
// OUTPUT). It never consults the elaborator's `Disposition` ledger — the
// dispositions are the component under test (a bad `ConsumedAt` /
// terminal-drop cancellation writes the leak INTO that ledger, so a validator
// reading it back would certify the bug). The mint fact is type-directed and
// orthogonal to the balance bug, so reading it from the ledger is sound;
// `Disposition::AliasOf` (interior byte-copy aliases) is trusted for MINT-SET
// EXCLUSION only.
//
// MODEL (whole-local granularity, interval dataflow):
//   - state per tracked local = owner-mint and discharge-count intervals
//     (both saturating at 2) + a payload-neutralization flag, per CFG point;
//     a local absent from the state map is UNMINTED on every reaching path.
//   - explicit `BytesRetain` / `StringRetain` instructions increment the
//     source mint count. An adjacent retain + whole-local move instead gives
//     the destination an independent mint while preserving the source owner.
//   - DEFINITE discharges (`lo` and `hi`): a terminal/inline drop of the
//     local, a return-transfer (`Move`/`WitnessMove` into `ReturnSlot`), a
//     payload-slot neutralization after move-out, a `ValueSnapshotDrop`, a
//     send/ask transfer terminator, a spawn-consumed state record, a
//     generator/lambda-actor env consumed at construction, and the
//     `*_owned_move` runtime copy-ins.
//   - AMBIGUOUS discharges (`hi` only): shapes whose single-owner resolution
//     belongs to another authority (whole-local rebinds resolved by the alias
//     machinery, call arguments not proven borrow, aggregation/capture
//     operands resolved by the W3.053 prover family, guarded exit drops).
//     Ambiguity can only WIDEN the interval, so it can never manufacture a
//     definite verdict in either direction — a finding is reported only when
//     EVERY modelling of the path is unbalanced.
//   - verdict per `(Return exit, local)` after folding in the exit plan's
//     drops: `hi < mint_lo`, or an explicit-retain `mint_hi > hi`, →
//     under-release;
//     `max_definite > mint_hi` → over-release.
//
// WHAT THIS PASS DOES NOT DO (A278 / S1875): it counts DISCHARGES, not USES.
// A balanced value used after a transfer is the move-checker's concern; this
// pass never re-introduces move errors for function calls (the
// `forward_param` shape passes — by-value params are caller-retained CoW
// borrows and are excluded from the mint set entirely).
//
// SUSPEND EXITS: `ExitPath::Suspend` is folded into the verdict alongside
// `Return` (see `validate_obligation_balance`'s "suspend-abandon" edge) —
// ownership moved through the coro frame at a suspend point is accounted for,
// not excluded from this pass.
//
// LESSONS: lifecycle-symmetry (the invariant IS the row), checker-authority
// (with the independence twist above), boundary-fail-closed (findings reject
// before codegen), exhaustive-coverage (an unrecognised consuming instruction
// defaults to borrow-not-discharge, which biases toward loud under-release
// reports for triage — never toward an assumed-discharged silent suppress).

/// Per-path payload-neutralization state for one tracked local. A
/// `NeutralizePayloadSlot` on a carrier's variant projection transfers the
/// payload's heap to a new owner and nulls the slot, making every later drop
/// walk over that slot a null-tolerant no-op — so drops observed after a
/// neutralization must not count as discharges on that path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg(test)]
enum PayloadNeutralized {
    /// No neutralization on any reaching path.
    No,
    /// Neutralized on every reaching path: later drops of the local no-op.
    Yes,
    /// Neutralized on some-but-not-all reaching paths: later drops are
    /// ambiguous (fire on the un-neutralized paths only).
    Maybe,
}

#[cfg(test)]
impl PayloadNeutralized {
    fn meet(self, other: Self) -> Self {
        if self == other {
            self
        } else {
            PayloadNeutralized::Maybe
        }
    }
}

/// Discharge counts saturate here: 0, 1, and "2 or more" are the only
/// balance-relevant magnitudes.
#[cfg(test)]
const OBLIGATION_COUNT_SATURATION: u8 = 2;

/// Owner-mint and discharge-count intervals for one tracked local over the CFG
/// paths reaching a program point. `mint_lo` / `mint_hi` bound the owners;
/// `lo` / `hi` bound possible discharges; `max_definite` is the maximum number
/// of DEFINITE (unambiguous) discharges on any single reaching path. Counts
/// saturate at [`OBLIGATION_COUNT_SATURATION`].
///
/// The two release verdicts read different components so each fails in the
/// safe direction:
///   - UNDER-release (leak) reads `hi < mint_lo` when every reaching path is
///     short a release. For explicit retains it also reads `mint_hi > hi`,
///     which proves that even the largest discharge count cannot pay the path
///     carrying the largest retain-backed owner count;
///   - OVER-release (double-free) is memory-unsafe on ANY path, so it reads
///     `max_definite > mint_hi` (some single path definitely discharges more
///     owners than can exist). Reading `lo` (the per-path MINIMUM) here would
///     path-dilute a branch-conditional double-free away: a double-free on one
///     arm but not another leaves `lo == 1` and silently certifies. Ambiguous discharges
///     never raise `max_definite`, so widen-only events (mirrored plan drops,
///     aggregation operands, single-owner-resolved-elsewhere transfers)
///     cannot manufacture a false over-release.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg(test)]
struct ObligationState {
    /// Minimum/maximum owner mints present on reaching paths.  Ordinary
    /// defining writes reset this to one; explicit retain instructions add a
    /// second owner unless the retain is paired with a local move, in which
    /// case the destination receives its own independent state.
    mint_lo: u8,
    mint_hi: u8,
    lo: u8,
    hi: u8,
    /// Per-path maximum of DEFINITE discharges. `meet` takes the max across
    /// joining paths; only [`ObligationState::definite_discharge`] raises it.
    max_definite: u8,
    /// Whether every/any reaching mint generation was created by an explicit
    /// retain. A definitely leaked retained generation is a compiler invariant
    /// failure, not a legacy advisory ownership hole.
    explicit_retain_lo: bool,
    explicit_retain_hi: bool,
    neutralized: PayloadNeutralized,
}

#[cfg(test)]
impl ObligationState {
    fn minted() -> Self {
        Self {
            mint_lo: 1,
            mint_hi: 1,
            lo: 0,
            hi: 0,
            max_definite: 0,
            explicit_retain_lo: false,
            explicit_retain_hi: false,
            neutralized: PayloadNeutralized::No,
        }
    }

    fn minted_by_retain() -> Self {
        Self {
            explicit_retain_lo: true,
            explicit_retain_hi: true,
            ..Self::minted()
        }
    }

    fn meet(self, other: Self) -> Self {
        Self {
            mint_lo: self.mint_lo.min(other.mint_lo),
            mint_hi: self.mint_hi.max(other.mint_hi),
            lo: self.lo.min(other.lo),
            hi: self.hi.max(other.hi),
            max_definite: self.max_definite.max(other.max_definite),
            explicit_retain_lo: self.explicit_retain_lo && other.explicit_retain_lo,
            explicit_retain_hi: self.explicit_retain_hi || other.explicit_retain_hi,
            neutralized: self.neutralized.meet(other.neutralized),
        }
    }

    /// Mint one additional co-owner over this live value. The release count is
    /// intentionally unchanged: a later owning sink and the original local's
    /// terminal drop must independently discharge the two references.
    fn retain_mint(&mut self) {
        self.mint_lo = self
            .mint_lo
            .saturating_add(1)
            .min(OBLIGATION_COUNT_SATURATION);
        self.mint_hi = self
            .mint_hi
            .saturating_add(1)
            .min(OBLIGATION_COUNT_SATURATION);
        self.explicit_retain_lo = true;
        self.explicit_retain_hi = true;
    }

    /// A discharge that fires on every modelling of the current path.
    fn definite_discharge(&mut self) {
        self.lo = self.lo.saturating_add(1).min(OBLIGATION_COUNT_SATURATION);
        self.hi = self.hi.saturating_add(1).min(OBLIGATION_COUNT_SATURATION);
        self.max_definite = self
            .max_definite
            .saturating_add(1)
            .min(OBLIGATION_COUNT_SATURATION);
    }

    /// Confirm that the current generation has transferred exactly once.
    /// The string aggregate/capture lowering writes an empty static string
    /// into the moved-from slot after publishing its bytes into the successor
    /// owner. Aggregation is initially modelled as an ambiguous discharge;
    /// this commit marker turns that same event into a definite one instead of
    /// counting a second discharge. If no ambiguous event preceded it, the
    /// marker still establishes the one transfer it commits.
    fn confirm_transfer_discharge(&mut self) {
        self.lo = self.lo.max(1);
        self.hi = self.hi.max(1);
        self.max_definite = self.max_definite.max(1);
    }

    /// A discharge whose single-owner resolution belongs to another
    /// authority: widens the interval upward only, so it can never produce
    /// a definite under- or over-release verdict by itself.
    fn ambiguous_discharge(&mut self) {
        self.hi = self.hi.saturating_add(1).min(OBLIGATION_COUNT_SATURATION);
    }

    /// A drop observed on this local (inline `Instr::Drop` or an exit-plan
    /// terminal drop): a real release on un-neutralized paths, a
    /// null-tolerant no-op on neutralized ones.
    fn drop_discharge(&mut self) {
        match self.neutralized {
            PayloadNeutralized::No => self.definite_discharge(),
            PayloadNeutralized::Yes => {}
            PayloadNeutralized::Maybe => self.ambiguous_discharge(),
        }
    }
}

/// Per-block obligation state: tracked local (root) → interval. Absent key =
/// unminted on every path reaching the point.
#[cfg(test)]
type ObligationMap = BTreeMap<u32, ObligationState>;

/// The local id a place addresses AS A WHOLE OWNED VALUE — the granularity
/// the balance model counts at. Half-handles are direction sub-objects (the
/// duplex split checker owns their exactly-once story) and projections are
/// sub-object reads; neither is a whole-owner address.
fn whole_owner_local(place: Place) -> Option<u32> {
    match place {
        Place::Local(n)
        | Place::DuplexHandle(n)
        | Place::LambdaActorHandle(n)
        | Place::ActorHandle(n) => Some(n),
        Place::SendHalf(_)
        | Place::RecvHalf(_)
        | Place::MachineTag(_)
        | Place::EnumTag(_)
        | Place::MachineVariant { .. }
        | Place::EnumVariant { .. }
        | Place::ReturnSlot => None,
    }
}

/// The carrier local behind an enum/machine payload-slot projection — the
/// address shape `NeutralizePayloadSlot` and variant-slot drops use.
fn payload_carrier_local(place: Place) -> Option<u32> {
    match place {
        Place::MachineVariant { local, .. } | Place::EnumVariant { local, .. } => Some(local),
        Place::Local(_)
        | Place::DuplexHandle(_)
        | Place::LambdaActorHandle(_)
        | Place::ActorHandle(_)
        | Place::SendHalf(_)
        | Place::RecvHalf(_)
        | Place::MachineTag(_)
        | Place::EnumTag(_)
        | Place::ReturnSlot => None,
    }
}

/// The local a WRITE to `place` (re-)mints. Whole-owner writes re-initialise
/// the slot; a payload write materialises the heap-owning generation of an
/// enum/machine carrier. A tag-only write selects an empty variant and must
/// not re-mint a release obligation that an earlier payload path discharged.
#[cfg(test)]
fn mint_target_local(place: Place) -> Option<u32> {
    match place {
        Place::Local(n)
        | Place::DuplexHandle(n)
        | Place::LambdaActorHandle(n)
        | Place::ActorHandle(n)
        | Place::MachineVariant { local: n, .. }
        | Place::EnumVariant { local: n, .. } => Some(n),
        Place::SendHalf(_)
        | Place::RecvHalf(_)
        | Place::MachineTag(_)
        | Place::EnumTag(_)
        | Place::ReturnSlot => None,
    }
}

/// Shared read-only context for the balance transfer functions.
#[cfg(test)]
struct ObligationCtx<'a> {
    /// Tracked mint set: root local → source-level name.
    tracked: &'a BTreeMap<u32, String>,
    /// Payload binder → carrier root, for move-outs of NON-neutralized
    /// carriers (see [`collect_payload_alias_map`]). Discharges of the binder
    /// attribute to the carrier's obligation.
    alias_to: &'a HashMap<u32, u32>,
    /// Parameter slots (`Builder::parameter_locals`): caller-retained
    /// borrows. A whole-local rebind FROM a parameter is a borrow alias, so
    /// the rebound dest mints with the hi-credit.
    parameter_locals: &'a HashSet<u32>,
    /// Exact `retain(source); move(destination, source)` instruction pairs.
    /// These are shares, not transfers: the source keeps its obligation and
    /// the defining move mints an independently retained destination owner.
    retained_move_sites: &'a HashSet<(u32, usize)>,
    /// Empty COW literals inserted immediately after an aggregate ownership
    /// handoff to clear the moved-from publication slot.
    cow_handoff_commit_sites: &'a HashSet<(u32, usize)>,
    /// Exact structural move paths whose neutralize transfers only one field
    /// of a multi-field active variant. The residual fields remain owned by
    /// the carrier. A sole-field active variant has no residual obligation:
    /// its payload handoff forwards the carrier generation in full.
    partial_transfer_payload_slots: &'a HashSet<Place>,
    /// Guarded projection binders that become owners only at their later,
    /// path-local carrier-neutralization commit.
    deferred_payload_transfer_binders: &'a HashSet<u32>,
}

#[cfg(test)]
impl ObligationCtx<'_> {
    /// Resolve a local through the payload-alias chain (hop-capped: the map
    /// is acyclic by construction, the cap is defensive).
    fn root_of(&self, local: u32) -> u32 {
        let mut cur = local;
        for _ in 0..32 {
            match self.alias_to.get(&cur) {
                Some(&next) => cur = next,
                None => break,
            }
        }
        cur
    }

    /// The tracked root a whole-owner place addresses, if any.
    fn tracked_root(&self, place: Place) -> Option<u32> {
        let root = self.root_of(whole_owner_local(place)?);
        self.tracked.contains_key(&root).then_some(root)
    }

    /// The tracked carrier root a variant-projection place addresses, if any.
    fn tracked_carrier(&self, place: Place) -> Option<u32> {
        let root = self.root_of(payload_carrier_local(place)?);
        self.tracked.contains_key(&root).then_some(root)
    }
}

#[cfg(test)]
fn obligation_entry(state: &mut ObligationMap, root: u32) -> &mut ObligationState {
    state.entry(root).or_insert_with(ObligationState::minted)
}

/// Fold one elaborated plan drop into the state: guard-gated closes are
/// path-sensitive at runtime (widen-only), unguarded drops discharge per the
/// neutralization flag, and a drop on an UNMINTED local belongs to paths
/// that never reach here with a live mint (skip rather than phantom-mint).
///
/// `inline_dropped` carries every place released by an inline `Instr::Drop`
/// anywhere in the function: an exit plan can MIRROR an inline drop (one
/// runtime release rendered on both surfaces — the inline release
/// null-clears the storage, so the plan copy is a null-tolerant no-op on
/// paths through it, and real only on paths that bypass it). Path-exact
/// resolution would need per-place release sets; the widen-only treatment
/// is AMBIGUOUS, which can neither certify balance nor manufacture a
/// definite verdict on either kind of path.
#[cfg(test)]
fn apply_plan_drop(
    state: &mut ObligationMap,
    drop: &ElabDrop,
    inline_dropped: &HashSet<Place>,
    cx: &ObligationCtx<'_>,
) {
    let Some(root) = cx
        .tracked_root(drop.place)
        .or_else(|| cx.tracked_carrier(drop.place))
    else {
        return;
    };
    let Some(entry) = state.get_mut(&root) else {
        return;
    };
    if drop.guard.is_some() || inline_dropped.contains(&drop.place) {
        entry.ambiguous_discharge();
    } else {
        entry.drop_discharge();
    }
}

/// Meet two per-block obligation maps. A local absent on one side is
/// unminted on that side's paths — no obligation there — so the present
/// side's interval carries through unchanged (identity meet).
#[cfg(test)]
fn meet_obligation_maps(a: &ObligationMap, b: &ObligationMap) -> ObligationMap {
    let mut out = a.clone();
    for (&local, &st) in b {
        out.entry(local)
            .and_modify(|cur| *cur = cur.meet(st))
            .or_insert(st);
    }
    out
}

/// Pre-scan: map payload-binder locals to their carrier root for move-outs
/// of payload slots that carry NO `NeutralizePayloadSlot`. Without a
/// neutralize on THAT slot, the binder's storage is a byte-copy of the
/// carrier's payload slot with two live release paths — one obligation, so
/// the binder's discharges must attribute to the carrier (this is exactly the
/// S1882 `move_out_arm` double-free shape). When a neutralize IS present on
/// the moved slot, the transfer is modelled path-sensitively by the
/// neutralize instruction itself and the binder is an independent mint.
///
/// Neutralization is keyed PER SLOT, not per carrier: since #2784 a
/// whole-carrier scrutinee neutralizes exactly the variant slot whose
/// ownership leaves, on the arm where it leaves. A move-out arm nulls its own
/// slot (`Ok(x) => x`) while a read-only sibling arm (`Err(y) => ..`) leaves
/// its slot live for the carrier's terminal drop. Keying on the carrier local
/// would let one arm's neutralize strip the read-only sibling's binder out of
/// the alias fold, leaving it a phantom independent mint that the shell drop
/// actually discharges — a false under-release on the sibling arm's exits.
fn collect_payload_alias_map(blocks: &[BasicBlock]) -> HashMap<u32, u32> {
    let mut neutralized_slots: HashSet<Place> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::NeutralizePayloadSlot { place, .. } = instr {
                neutralized_slots.insert(*place);
            }
        }
    }
    let mut seeds = Vec::new();
    for block in blocks {
        for instr in &block.instructions {
            let (dest, src) = match instr {
                Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. } => (*dest, *src),
                _ => continue,
            };
            let Some(carrier) = payload_carrier_local(src) else {
                continue;
            };
            let Some(binder) = whole_owner_local(dest) else {
                continue;
            };
            // The fold applies whether or not the carrier is itself tracked:
            // an EPHEMERAL scrutinee temp (a clone-getter `Option` result)
            // may sit outside the mint set, and its payload binder's storage
            // still traces to the carrier's slot — the binder is then also
            // outside the balance (its ownership story is the carrier's).
            if !neutralized_slots.contains(&src) && binder != carrier {
                seeds.push((binder, carrier));
            }
        }
    }
    // Retains and return publications mint or transfer an independently
    // discharged owner rather than extending this storage-alias chain. Keep
    // both boundaries out of the closure: folding a return publication back
    // into its carrier attributes the caller transfer and the carrier drop to
    // one obligation, producing a false double-release verdict.
    let mut ownership_boundary_moves = collect_retained_move_sites(blocks);
    let returned_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::Move {
                dest: Place::ReturnSlot,
                src,
            }
            | Instr::WitnessMove {
                dest: Place::ReturnSlot,
                src,
                ..
            } => whole_owner_local(*src),
            _ => None,
        })
        .collect();
    for block in blocks {
        for (index, instr) in block.instructions.iter().enumerate() {
            if matches!(instr, Instr::Move { dest, .. } | Instr::WitnessMove { dest, .. }
                if whole_owner_local(*dest).is_some_and(|local| returned_locals.contains(&local)))
            {
                ownership_boundary_moves.insert((block.id, index));
            }
        }
    }
    propagate_seeded_whole_value_alias_roots_excluding_moves(
        blocks,
        seeds,
        &ownership_boundary_moves,
    )
}

/// Restrict move-chain payload-alias folding to roots whose release obligation
/// is itself represented in the balance model. The direct projection binder is
/// always an alias of its carrier. A later source binding, however, can be
/// tracked while an ephemeral result carrier is absent from `tracked`; folding
/// that publication into the absent carrier would erase the only obligation
/// the validator can see and silently certify a real leak.
#[cfg(test)]
fn collect_balance_payload_alias_map(
    blocks: &[BasicBlock],
    tracked: &BTreeMap<u32, String>,
) -> HashMap<u32, u32> {
    let direct_payload_binders: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. }
                if payload_carrier_local(*src).is_some() =>
            {
                whole_owner_local(*dest)
            }
            _ => None,
        })
        .collect();
    collect_payload_alias_map(blocks)
        .into_iter()
        .filter(|(binder, carrier)| {
            direct_payload_binders.contains(binder)
                || !tracked.contains_key(binder)
                || tracked.contains_key(carrier)
        })
        .collect()
}

/// Projection binders whose ownership commit is deliberately deferred to a
/// different basic block (guarded match arms). The initial projection load is
/// a borrow: only the later `NeutralizePayloadSlot` path nulls the carrier and
/// turns the binder into an owner. Treating the load itself as a mint creates a
/// phantom owner on the guard-fallthrough edge.
#[cfg(test)]
fn collect_deferred_payload_transfer_binders(blocks: &[BasicBlock]) -> HashSet<u32> {
    let commits: HashSet<(Place, Place, u32)> = blocks
        .iter()
        .flat_map(|block| {
            block
                .instructions
                .iter()
                .filter_map(move |instr| match instr {
                    Instr::NeutralizePayloadSlot {
                        place,
                        transferee: Some(transferee),
                        ..
                    } => Some((*place, *transferee, block.id)),
                    _ => None,
                })
        })
        .collect();
    let mut deferred = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            let (dest, src) = match instr {
                Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. } => (*dest, *src),
                _ => continue,
            };
            let Some(dest_local) = whole_owner_local(dest) else {
                continue;
            };
            if payload_carrier_local(src).is_some()
                && commits.iter().any(|(place, transferee, commit_block)| {
                    *place == src && *transferee == dest && *commit_block != block.id
                })
            {
                deferred.insert(dest_local);
            }
        }
    }
    deferred
}

#[cfg(test)]
mod payload_alias_closure_tests {
    use super::*;

    #[test]
    fn return_publication_stops_projection_alias_closure() {
        let blocks = vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(8),
                    src: Place::MachineVariant {
                        local: 2,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                },
                Instr::Move {
                    dest: Place::Local(1),
                    src: Place::Local(8),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }];

        let aliases = collect_payload_alias_map(&blocks);
        assert_eq!(aliases.get(&8), Some(&2));
        assert_eq!(
            aliases.get(&1),
            None,
            "the returned publication owns its caller transfer independently"
        );
    }

    #[test]
    fn tracked_binding_does_not_fold_into_untracked_ephemeral_carrier() {
        let blocks = vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(8),
                    src: Place::MachineVariant {
                        local: 2,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                },
                Instr::Move {
                    dest: Place::Local(1),
                    src: Place::Local(8),
                },
            ],
            terminator: Terminator::Return,
        }];
        let tracked = BTreeMap::from([(1, "loop payload".to_string())]);

        let aliases = collect_balance_payload_alias_map(&blocks, &tracked);

        assert_eq!(aliases.get(&8), Some(&2));
        assert_eq!(
            aliases.get(&1),
            None,
            "a tracked publication must retain its obligation when its carrier is outside the balance"
        );
    }
}

/// Re-derive the explicit whole-local retain/share protocol from primitive
/// MIR. The pair is deliberately structural: codegen executes the retain on
/// every dynamic visit even when a destination slot is reused or the block is
/// cyclic, so the obligation model must count it on those paths rather than
/// inherit the stricter sole-owner-prover admission policy.
fn collect_retained_move_sites(blocks: &[BasicBlock]) -> HashSet<(u32, usize)> {
    let mut sites = HashSet::new();
    for block in blocks {
        for move_index in 1..block.instructions.len() {
            let retain_source = match &block.instructions[move_index - 1] {
                Instr::BytesRetain { value }
                | Instr::StringRetain {
                    value,
                    condition: crate::model::StringRetainCondition::Always,
                } => Some(*value),
                _ => None,
            };
            let Some(retain_source) = retain_source else {
                continue;
            };
            let (dest, move_source) = match &block.instructions[move_index] {
                Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. } => (*dest, *src),
                _ => continue,
            };
            if retain_source == move_source
                && whole_owner_local(dest).is_some()
                && whole_owner_local(move_source).is_some()
                && whole_owner_local(dest) != whole_owner_local(move_source)
            {
                sites.insert((block.id, move_index));
            }
        }
    }
    sites
}

#[cfg(test)]
fn collect_cow_handoff_commit_sites(blocks: &[BasicBlock]) -> HashSet<(u32, usize)> {
    let mut sites = HashSet::new();
    for block in blocks {
        for (index, instruction) in block.instructions.iter().enumerate() {
            let value = match instruction {
                Instr::StringLit { bytes, dest } | Instr::BytesLit { bytes, dest }
                    if bytes.is_empty() =>
                {
                    *dest
                }
                _ => continue,
            };
            let handoff = block.instructions[..index].iter().rev().find(|candidate| {
                !matches!(
                    candidate,
                    Instr::StringLit { bytes, .. } | Instr::BytesLit { bytes, .. }
                        if bytes.is_empty()
                )
            });
            if handoff.is_some_and(|handoff| {
                super::temp_drop::string_share_sink_places(handoff).contains(&value)
            }) {
                sites.insert((block.id, index));
            }
        }
    }
    sites
}

/// Forward transfer of one instruction: discharge events first, then mint
/// (whole-slot write) resets.
#[allow(
    clippy::too_many_lines,
    reason = "one exhaustive discharge-vocabulary classification; each arm is \
              a single distinct ownership decision and splitting scatters the \
              vocabulary the OWN-0 lesson requires to be reviewable in one place"
)]
#[allow(
    clippy::match_same_arms,
    reason = "arms sharing the ambiguous-discharge body model semantically \
              distinct transfer classes (aggregation vs capture vs dispatch); \
              merging them would erase the per-class rationale comments"
)]
#[cfg(test)]
fn apply_balance_instr(
    state: &mut ObligationMap,
    instr: &Instr,
    block: u32,
    instr_index: usize,
    cx: &ObligationCtx<'_>,
) {
    let retained_share_move = cx.retained_move_sites.contains(&(block, instr_index));
    // Derived mints take an hi-credit of 1 ("another owner's release may be
    // mine"), applied after the generic mint below:
    //   - a whole-local rebind FROM a tracked local (`let m2 = m;` — which
    //     slot's drop pays the one obligation is the alias machinery's
    //     decision) or FROM a parameter (a caller-retained borrow alias);
    //   - a whole-local minted by a field/tuple/env load (a byte-copy or
    //     handle transfer out of a base whose composite release may cover
    //     it — the `FieldLoadClass` fact is not re-derived here);
    //   - an actor-state load in `Borrowed` mode (a bare byte-copy alias;
    //     `Owned` mode retains a fresh owner and earns NO credit).
    // A fresh-producer mint (literal, call result, constructor) earns no
    // credit — those are the shapes the under-release net must keep.
    let credit_dest = |dest: &Place| {
        whole_owner_local(*dest)
            .filter(|d| cx.tracked.contains_key(d) && !cx.alias_to.contains_key(d))
    };
    let rebind_credit_dest = match instr {
        Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. } => whole_owner_local(*src)
            .filter(|s| cx.parameter_locals.contains(s) || cx.tracked.contains_key(&cx.root_of(*s)))
            .filter(|_| !retained_share_move)
            .and_then(|_| credit_dest(dest)),
        Instr::RecordFieldLoad { dest, .. }
        | Instr::TupleFieldLoad { dest, .. }
        | Instr::ClosureEnvFieldLoad { dest, .. } => credit_dest(dest),
        Instr::ActorStateFieldLoad {
            dest,
            mode: crate::model::ActorStateLoadMode::Borrowed,
            ..
        } => credit_dest(dest),
        _ => None,
    };
    match instr {
        Instr::Drop { place, .. } => {
            if let Some(root) = cx
                .tracked_root(*place)
                .or_else(|| cx.tracked_carrier(*place))
            {
                obligation_entry(state, root).drop_discharge();
            }
        }
        Instr::StringLit { bytes, dest } if bytes.is_empty() => {
            // `apply_string_retain_sites` uses an empty static string write as
            // the moved-from commit marker after an owned string enters a
            // record, tuple, or capture environment. A static string carries
            // no heap owner of its own, so confirm the preceding transfer and
            // do not let the generic write pass remint this slot below.
            if let Some(root) = cx.tracked_root(*dest) {
                if let Some(entry) = state.get_mut(&root) {
                    entry.confirm_transfer_discharge();
                    // The moved-from slot now holds an empty static string with
                    // no heap owner, so a later terminal drop of it (a loop
                    // back-edge, cancel, or panic scope-exit drop) walks a
                    // nulled slot and is a no-op, NOT a second discharge of the
                    // transferred buffer. Mark it neutralized (as
                    // `NeutralizePayloadSlot` does) so those drops cannot
                    // manufacture a phantom over-release; a later real defining
                    // write resets the obligation to a fresh `minted()`, so a
                    // genuine double-free on the next generation is still caught.
                    entry.neutralized = PayloadNeutralized::Yes;
                }
            }
        }
        Instr::BytesLit { bytes, dest }
            if bytes.is_empty() && cx.cow_handoff_commit_sites.contains(&(block, instr_index)) =>
        {
            if let Some(root) = cx.tracked_root(*dest) {
                if let Some(entry) = state.get_mut(&root) {
                    entry.confirm_transfer_discharge();
                    entry.neutralized = PayloadNeutralized::Yes;
                }
            }
        }
        Instr::ConstI64 {
            dest: Place::EnumTag(local),
            value: 1,
        } => {
            // `None` overwrites the carrier with an empty variant. Its old
            // payload, if any, has already been discharged by the path that
            // produced this terminal result; a following carrier drop must
            // remain a no-op instead of charging that release twice.
            if cx.tracked.contains_key(local) {
                obligation_entry(state, *local).neutralized = PayloadNeutralized::Yes;
            }
        }
        Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. } => {
            if let Some(root) = cx.tracked_root(*src) {
                if retained_share_move {
                    // The immediately preceding retain minted a distinct
                    // destination reference. The source remains owned here;
                    // the generic defining-write pass below creates the new
                    // destination obligation without a rebind credit.
                } else if matches!(dest, Place::ReturnSlot) {
                    // Return-transfer: the caller receives the one owner.
                    obligation_entry(state, root).definite_discharge();
                } else {
                    // Whole-local rebind: the payload pointer hands over with
                    // no retain, and WHICH slot carries the terminal drop is
                    // the alias machinery's decision — ambiguous here.
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
            // A move OUT of a variant projection is either alias-folded (no
            // neutralize anywhere — see `collect_payload_alias_map`) or paid
            // by the `NeutralizePayloadSlot` that follows it; no event here.
        }
        Instr::BytesRetain { value }
        | Instr::StringRetain {
            value,
            condition: crate::model::StringRetainCondition::Always,
        } => {
            // An exact retain+move pair mints the destination generation; do
            // not also charge the source with a second anonymous owner. Every
            // other whole-value retain creates a co-owner that an owning sink
            // must consume independently of the original local's terminal
            // drop. Conditional string retains are leaf-layout operations:
            // they retain only nested strings before a borrowed aggregate or
            // actor-state field copy, so they do not mint another owner of a
            // tracked non-string root (for example a Generator).
            let paired_move = cx
                .retained_move_sites
                .contains(&(block, instr_index.saturating_add(1)));
            if !paired_move {
                if let Some(root) = cx.tracked_root(*value) {
                    obligation_entry(state, root).retain_mint();
                }
            }
        }
        Instr::NeutralizePayloadSlot {
            place, transferee, ..
        } => {
            let destination_root = transferee.and_then(|destination| cx.tracked_root(destination));
            if let Some(dest) = transferee.and_then(whole_owner_local) {
                if cx.deferred_payload_transfer_binders.contains(&dest)
                    && cx.tracked.contains_key(&dest)
                {
                    state.insert(dest, ObligationState::minted());
                }
            }
            if let Some(root) = cx
                .tracked_root(*place)
                .or_else(|| cx.tracked_carrier(*place))
            {
                if cx.partial_transfer_payload_slots.contains(place) {
                    // The selected payload moved, but this minted call carrier
                    // still owns its shell and unselected slots. Only a later
                    // carrier drop discharges the whole-generation obligation.
                    return;
                }
                // A proven whole-value handoff is a generation rename, not a
                // terminal release. Move the outstanding obligation to the
                // transferee and remove the moved-from generation from the
                // normal-edge state. Earlier exceptional edges retain the
                // source state because this instruction has not executed
                // there. Counting the handoff as a discharge and then counting
                // the transferee's eventual drop as another discharge folds
                // two generations into one and manufactures a double-free.
                if let Some(destination_root) = destination_root.filter(|dest| *dest != root) {
                    let mut transferred =
                        state.remove(&root).unwrap_or_else(ObligationState::minted);
                    transferred.neutralized = PayloadNeutralized::No;
                    state.insert(destination_root, transferred);
                    return;
                }
                let entry = obligation_entry(state, root);
                match entry.neutralized {
                    PayloadNeutralized::No => {
                        // The payload's heap transferred to a new owner: the
                        // carrier's obligation discharges here; later drops
                        // of the carrier walk a nulled slot (no-ops).
                        entry.definite_discharge();
                        entry.neutralized = PayloadNeutralized::Yes;
                    }
                    // Re-nulling an already-nulled slot is idempotent.
                    PayloadNeutralized::Yes => {}
                    PayloadNeutralized::Maybe => {
                        entry.ambiguous_discharge();
                        entry.neutralized = PayloadNeutralized::Yes;
                    }
                }
            }
        }
        Instr::ValueSnapshotDrop { value, .. } => {
            if let Some(root) = cx.tracked_root(*value) {
                obligation_entry(state, root).definite_discharge();
            }
        }
        Instr::SpawnActor {
            state: spawn_state,
            init_args,
            ..
        } => {
            // The initial-state record is consumed by the spawn (the actor's
            // `state_drop_fn` becomes the single free site).
            if let Some(root) = spawn_state.as_ref().and_then(|p| cx.tracked_root(*p)) {
                obligation_entry(state, root).definite_discharge();
            }
            for arg in init_args {
                if let Some(root) = cx.tracked_root(*arg) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
        Instr::RcIntrinsic {
            op: hew_types::RcIntrinsicOp::New | hew_types::RcIntrinsicOp::Set,
            value: Some(value),
            ..
        } => {
            // `Rc::new(value)` and `Rc::set(value)` take the payload by
            // ownership.  The resulting/newly-installed Rc payload is the
            // sole successor authority, so the source mint is discharged
            // exactly here rather than left as an anonymous producer at the
            // function exit.
            if let Some(root) = cx.tracked_root(*value) {
                obligation_entry(state, root).definite_discharge();
            }
        }
        Instr::CallRuntimeAbi(call) => {
            for (i, arg) in call.args().iter().enumerate() {
                if let Some(root) = cx.tracked_root(*arg) {
                    if call.family().arg_consume_verdict(i)
                        == hew_types::runtime_call::ConsumeVerdict::ProvenConsume
                    {
                        obligation_entry(state, root).definite_discharge();
                    } else if i == 0 && call.symbol() == "hew_structural_format" {
                        // Structural formatting observes the value by address.
                        // It neither transfers nor clones any ownership authority.
                    } else {
                        // Runtime calls outside the consuming table borrow
                        // their heap args; widen only (a symbol this table
                        // does not know can never certify a leak away).
                        obligation_entry(state, root).ambiguous_discharge();
                    }
                }
            }
        }
        // Aggregation / capture / dispatch operands: single-owner resolution
        // for these belongs to the W3.053 prover family and the escape scans;
        // balance models each tracked whole-local operand as an AMBIGUOUS
        // transfer.
        Instr::RecordInit { fields, .. } => {
            for (_, place) in fields {
                if let Some(root) = cx.tracked_root(*place) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
        Instr::ClosureEnvInit { fields, .. } => {
            for field in fields {
                if let Some(root) = cx.tracked_root(field.src) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
        Instr::TupleConstruct { elements, .. } => {
            for place in elements {
                if let Some(root) = cx.tracked_root(*place) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
        Instr::RecordFieldStore { src, .. }
        | Instr::ActorStateFieldStore { src, .. }
        | Instr::ClosureEnvFieldStore { src, .. } => {
            if let Some(root) = cx.tracked_root(*src) {
                obligation_entry(state, root).ambiguous_discharge();
            }
        }
        Instr::MakeClosure { env, .. } => {
            if let Some(root) = cx.tracked_root(*env) {
                obligation_entry(state, root).ambiguous_discharge();
            }
        }
        Instr::CallClosure { args, .. } => {
            for arg in args {
                if let Some(root) = cx.tracked_root(*arg) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
        Instr::CallTraitMethod { args, .. } => {
            for arg in args {
                if let Some(root) = cx.tracked_root(*arg) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
        Instr::CoerceToDynTrait { value, .. } => {
            if let Some(root) = cx.tracked_root(*value) {
                obligation_entry(state, root).ambiguous_discharge();
            }
        }
        Instr::MachineEmitPlaceholder { payload, .. } => {
            for place in payload {
                if let Some(root) = cx.tracked_root(*place) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
        Instr::SpawnTaskDirect { task, .. } => {
            if let Some(root) = cx.tracked_root(*task) {
                obligation_entry(state, root).ambiguous_discharge();
            }
        }
        Instr::SpawnTaskClosure { task, env, .. } => {
            for place in [*task, *env] {
                if let Some(root) = cx.tracked_root(place) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
        // Sub-object releases of a tracked base: a destructured composite is
        // discharged through per-field in-place releases with its whole-root
        // composite drop SUPPRESSED (the exactly-once pairing is
        // `validate_field_drop_in_place`'s authority). Model each as an
        // AMBIGUOUS discharge of the base so a fully field-released root is
        // never a definite leak, while never certifying balance either.
        Instr::FieldDropInPlace { base, .. } | Instr::RecordFieldDrop { record: base, .. } => {
            if let Some(root) = cx.tracked_root(*base) {
                obligation_entry(state, root).ambiguous_discharge();
            }
        }
        // Field/tuple loads out of a tracked base: a `HandleTransfer`-class
        // load moves the base's single owned heap leaf into the binder (a
        // discharge of the base), while a `Retained` load clones and a
        // `ByteCopyAlias` load borrows. The class is a type-layout fact this
        // pass does not re-derive — ambiguous (widen-only).
        Instr::RecordFieldLoad { record: base, .. } | Instr::TupleFieldLoad { tuple: base, .. } => {
            if let Some(root) = cx.tracked_root(*base) {
                obligation_entry(state, root).ambiguous_discharge();
            }
        }
        // Everything else — arithmetic, comparisons, literals, casts, retains
        // (the co-owner mints on the OTHER side), in-place clones (source
        // borrowed), `GeneratorNext` (ctx borrowed), wire codecs, auto-locks,
        // context markers — reads no whole-value ownership out of a tracked
        // local: CoW reads are borrows (A278). A NEW consuming instruction
        // must be classified above; this default biases toward loud
        // UNDER-release reports for triage, never toward an
        // assumed-discharged suppress.
        _ => {}
    }
    let (_, writes, _) = dataflow::instr_reads_writes(instr);
    for write in writes {
        // A string literal points into read-only module storage; its drop is a
        // guarded no-op and therefore it never mints a heap obligation. Empty
        // literal writes over an already-minted slot are handled above as the
        // aggregate/capture transfer commit marker.
        if matches!(instr, Instr::StringLit { dest, .. } if *dest == write)
            || (cx.cow_handoff_commit_sites.contains(&(block, instr_index))
                && matches!(instr, Instr::BytesLit { dest, .. } if *dest == write))
            || matches!(instr, Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. }
            if payload_carrier_local(*src).is_some()
                && whole_owner_local(*dest).is_some_and(|local| {
                    cx.deferred_payload_transfer_binders.contains(&local)
                }))
        {
            continue;
        }
        if let Some(local) = mint_target_local(write) {
            // The defining write of a payload-alias binder is the transfer
            // moment of its carrier's payload, not a fresh mint.
            if cx.alias_to.contains_key(&local) {
                continue;
            }
            if cx.tracked.contains_key(&local) {
                let minted = if retained_share_move {
                    ObligationState::minted_by_retain()
                } else {
                    ObligationState::minted()
                };
                state.insert(local, minted);
            }
        }
    }
    if let Some(dest) = rebind_credit_dest {
        if let Some(entry) = state.get_mut(&dest) {
            entry.hi = entry.hi.max(1);
        }
    }
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

/// Normal (non-abandon) successors for ownership balance.
///
/// `BasicBlock::successors` includes a suspend carrier's cleanup target because
/// other CFG passes need to see the physical coroutine switch. Ownership
/// balance instead models `coro.destroy` as a terminal edge whose frame drops
/// are carried by `ExitPath::Suspend`; propagating the parked frame state into
/// `cleanup` would mix abandon-only execution into the resumed body.
#[cfg(test)]
fn obligation_successors(block: &BasicBlock) -> Vec<u32> {
    match &block.terminator {
        Terminator::Suspend { resume, .. } => vec![*resume],
        Terminator::SuspendingScopeDeadline {
            timeout_body_block,
            resume,
            ..
        } => vec![*timeout_body_block, *resume],
        Terminator::SuspendingSelect { arms, resume, .. } => {
            let mut successors: Vec<u32> = arms.iter().map(|arm| arm.body_block).collect();
            successors.push(*resume);
            successors
        }
        _ => block.successors(),
    }
}

/// Destinations written by the suspend ramp only after case-0 resume.
///
/// These cannot be minted in the suspend block's common state: the destroy
/// edge never initializes them, and charging them there would manufacture
/// frame obligations on abandon.
#[cfg(test)]
fn suspend_resume_mint_places(kind: &SuspendKind) -> Vec<Place> {
    match kind {
        SuspendKind::Ask {
            result_dest,
            reply_dest,
            error_dest,
            ..
        }
        | SuspendKind::RemoteAsk {
            result_dest,
            reply_dest,
            error_dest,
            ..
        } => vec![*result_dest, *reply_dest, *error_dest],
        SuspendKind::Read {
            result_dest,
            deadline_result_dest,
            error_dest,
            ..
        }
        | SuspendKind::Accept {
            result_dest,
            deadline_result_dest,
            error_dest,
            ..
        }
        | SuspendKind::StreamNext {
            result_dest,
            deadline_result_dest,
            error_dest,
            ..
        }
        | SuspendKind::ChannelRecv {
            result_dest,
            deadline_result_dest,
            error_dest,
            ..
        } => std::iter::once(*result_dest)
            .chain(deadline_result_dest.iter().copied())
            .chain(error_dest.iter().copied())
            .collect(),
        SuspendKind::CallClosure { result_dest, .. }
        | SuspendKind::TaskAwait { result_dest, .. } => result_dest.iter().copied().collect(),
        SuspendKind::RestartWait {
            result_dest,
            deadline_result_dest,
            ..
        } => std::iter::once(*result_dest)
            .chain(deadline_result_dest.iter().copied())
            .collect(),
        SuspendKind::ActorSend { .. }
        | SuspendKind::StreamSend { .. }
        | SuspendKind::Sleep { .. }
        | SuspendKind::SleepUntil { .. } => Vec::new(),
    }
}

#[cfg(test)]
fn apply_suspend_resume_mints(
    state: &mut ObligationMap,
    predecessor: &BasicBlock,
    successor: u32,
    kind: Option<&SuspendKind>,
    cx: &ObligationCtx<'_>,
) {
    let Terminator::Suspend { resume, .. } = &predecessor.terminator else {
        return;
    };
    if *resume != successor {
        return;
    }
    let Some(kind) = kind else {
        return;
    };
    for write in suspend_resume_mint_places(kind) {
        if let Some(local) = mint_target_local(write) {
            if !cx.alias_to.contains_key(&local) && cx.tracked.contains_key(&local) {
                state.insert(local, ObligationState::minted());
            }
        }
    }
}

/// Forward transfer of a block's terminator: discharge events, then mints.
#[cfg(test)]
fn apply_balance_terminator(
    state: &mut ObligationMap,
    block: &BasicBlock,
    suspend: Option<&SuspendKind>,
    cx: &ObligationCtx<'_>,
) {
    match &block.terminator {
        Terminator::Call { args, .. } => {
            // Consume-vs-borrow is a cross-function contract this LOCAL pass
            // does not verify (S2/OWN-V1's target): every tracked whole arg
            // is an ambiguous transfer. Deliberately NOT refined by the
            // `proven_borrow_call_args` summary — that summary also records
            // by-value `self` receiver slots whose method may be declared
            // `consuming` (the linear inherent-consume shape), so treating
            // it as a definite non-discharge manufactures phantom leaks.
            for arg in args {
                if let Some(root) = cx.tracked_root(*arg) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
        Terminator::Send { value, .. }
        | Terminator::Ask { value, .. }
        | Terminator::RemoteAsk { value, .. } => {
            // Whether transport consumes the outbound value is per-argument
            // (`SendAliasMode`): a prepared snapshot owner transfers, while a
            // `Copy`-mode handle payload (an actor ref forwarded by value)
            // stays caller-owned and keeps its scope-exit drop. Ambiguous
            // (widen-only) — the mode-exact refinement is S2 territory.
            if let Some(root) = cx.tracked_root(*value) {
                obligation_entry(state, root).ambiguous_discharge();
            }
        }
        Terminator::MakeLambdaActor {
            env: Some(env_place),
            ..
        } => {
            // The materialised capture-env record is heap-boxed and consumed
            // by the spawn; the synthesized state_drop_fn is its free site.
            if let Some(root) = cx.tracked_root(*env_place) {
                obligation_entry(state, root).definite_discharge();
            }
        }
        Terminator::MakeGenerator {
            env: Some(plan), ..
        } => {
            // The synthetic env shell is consumed by the generator ramp.
            if let Some(root) = cx.tracked_root(plan.place) {
                obligation_entry(state, root).definite_discharge();
            }
        }
        Terminator::Return
        | Terminator::Goto { .. }
        | Terminator::Branch { .. }
        | Terminator::Trap { .. }
        | Terminator::MakeGenerator { env: None, .. }
        | Terminator::MakeLambdaActor { env: None, .. } => {}
        // Yield / Select / Join and any future terminator: every source
        // operand is an ambiguous transfer (widen-only).
        other => {
            for place in terminator_source_places(other, suspend) {
                if let Some(root) = cx.tracked_root(place) {
                    obligation_entry(state, root).ambiguous_discharge();
                }
            }
        }
    }
    for write in terminator_mint_places(&block.terminator) {
        if let Some(local) = mint_target_local(write) {
            if cx.alias_to.contains_key(&local) {
                continue;
            }
            if cx.tracked.contains_key(&local) {
                state.insert(local, ObligationState::minted());
            }
        }
    }
}

/// The mint set comes from explicit ownership operations in MIR, never from
/// the mutable lowering ledger. Parameter slots are removed by the caller;
/// alias-only registrations emit no mint, while a provisional owner demoted to
/// an alias carries a program-point `Release` event that ends its generation.
fn tracked_obligation_locals_with_sites(
    builder: &Builder,
    blocks: &[BasicBlock],
) -> (BTreeMap<u32, String>, BTreeMap<u32, SiteId>) {
    let mut tracked: BTreeMap<u32, String> = BTreeMap::new();
    let mut mint_sites: BTreeMap<u32, SiteId> = BTreeMap::new();
    let binding_metadata: HashMap<BindingId, (String, SiteId)> = blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| match statement {
            MirStatement::Bind {
                binding,
                name,
                site,
                ..
            } => Some((*binding, (name.clone(), *site))),
            _ => None,
        })
        .collect();
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

/// Decomposed core of the retired obligation-balance validator. These
/// hand-constructed falsifiers remain test-only; production cleanup authority
/// is the exact `OwnerId` interpreter and frozen `DropRecipe` plan.
/// (hand-constructed blocks + drop plans + tracked set, no `Builder`).
/// Computes the default fixpoint iteration cap and forwards to
/// [`validate_obligation_balance_capped`].
#[cfg(test)]
fn validate_obligation_balance_with(
    elab: &ElaboratedMirFunction,
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    tracked_in: &BTreeMap<u32, String>,
    diagnostic_info: (&BTreeMap<u32, String>, &BTreeMap<u32, SiteId>),
    parameter_locals: &HashSet<u32>,
    partial_transfer_payload_slots: &HashSet<Place>,
) -> Vec<MirCheck> {
    // Iteration cap for the monotone worklist. The lattice is finite and the
    // transfer monotone, so convergence is guaranteed well within this bound;
    // the cap is a defensive ceiling whose exhaustion fails CLOSED (see the
    // capped core).
    let iteration_cap = blocks.len().saturating_mul(64).saturating_add(1024);
    validate_obligation_balance_capped(
        elab,
        blocks,
        suspend_kinds,
        tracked_in,
        diagnostic_info,
        parameter_locals,
        partial_transfer_payload_slots,
        iteration_cap,
    )
}

/// Balance-verdict core with an explicit fixpoint iteration cap. Split out so
/// the fail-closed cap-exhaustion path is unit-testable (a tiny cap forces the
/// unverified verdict a converging body would never reach).
#[allow(
    clippy::too_many_lines,
    clippy::too_many_arguments,
    reason = "single fixpoint + exit-verdict walk; splitting would obscure \
              the dataflow (mirrors validate_cross_block_split_consume)"
)]
#[cfg(test)]
fn validate_obligation_balance_capped(
    elab: &ElaboratedMirFunction,
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    tracked_in: &BTreeMap<u32, String>,
    diagnostic_info: (&BTreeMap<u32, String>, &BTreeMap<u32, SiteId>),
    parameter_locals: &HashSet<u32>,
    partial_transfer_payload_slots: &HashSet<Place>,
    iteration_cap: usize,
) -> Vec<MirCheck> {
    use std::collections::VecDeque;

    let (local_types, mint_sites) = diagnostic_info;
    let mut findings = Vec::new();
    if blocks.is_empty() || tracked_in.is_empty() {
        return findings;
    }

    let alias_to = collect_balance_payload_alias_map(blocks, tracked_in);
    let retained_move_sites = collect_retained_move_sites(blocks);
    let cow_handoff_commit_sites = collect_cow_handoff_commit_sites(blocks);
    let deferred_payload_transfer_binders = collect_deferred_payload_transfer_binders(blocks);
    // A payload-alias binder's discharges fold into its carrier; the binder
    // is not an independent obligation.
    let mut tracked = tracked_in.clone();
    for binder in alias_to.keys() {
        tracked.remove(binder);
    }
    // Two teardown surfaces live OUTSIDE the modelled streams and their
    // locals are therefore not balanceable here:
    //   - select-arm bindings: per-arm winner/loser cleanup is emitted at
    //     the codegen select join-dispatch, not in the `DropPlan`s;
    //   - stack/null-env closure pairs: the pair owns no heap (the env is a
    //     frame alloca or null) — the type-level mint fact over-approximates
    //     (only a `HeapBox` env pair carries a release obligation).
    // A third surface joins them: collection accessors whose result-payload
    // release choreography is emitted by the codegen callee intercept
    // (empirically leak-clean under `leaks --atExit` with NO release visible
    // in either MIR stream). Closed table — string/bytes producers (slice,
    // concat, to_upper, ...) mint fresh owners and deliberately stay
    // balance-checked.
    let mut excluded: HashSet<u32> = HashSet::new();
    for block in blocks {
        match &block.terminator {
            Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => {
                for arm in arms {
                    if let Some(local) = arm.binding.and_then(whole_owner_local) {
                        excluded.insert(local);
                    }
                }
            }
            Terminator::Call {
                callee,
                dest: Some(dest),
                ..
            } if matches!(
                callee.as_str(),
                "hew_hashmap_get_layout"
                    | "hew_hashmap_get_clone_layout"
                    | "hew_hashmap_remove_take_layout"
            ) =>
            {
                if let Some(local) = whole_owner_local(*dest) {
                    excluded.insert(local);
                }
            }
            _ => {}
        }
        for instr in &block.instructions {
            if let Instr::MakeClosure { dest, env_mode, .. } = instr {
                if !matches!(env_mode, crate::model::ClosureEnvMode::HeapBox) {
                    if let Some(local) = whole_owner_local(*dest) {
                        excluded.insert(local);
                    }
                }
            }
        }
    }
    // Propagate the exclusion through whole-local rebinds (`let f = <pair>`
    // moves the pair through a temp): a copy of a no-obligation value is
    // itself no obligation. Bounded fixpoint over the finite local set.
    loop {
        let mut grew = false;
        for block in blocks {
            for instr in &block.instructions {
                let (dest, src) = match instr {
                    Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. } => {
                        (*dest, *src)
                    }
                    _ => continue,
                };
                if let (Some(d), Some(s)) = (whole_owner_local(dest), whole_owner_local(src)) {
                    if excluded.contains(&s) && excluded.insert(d) {
                        grew = true;
                    }
                }
            }
        }
        if !grew {
            break;
        }
    }
    for local in &excluded {
        tracked.remove(local);
    }
    let cx = ObligationCtx {
        tracked: &tracked,
        alias_to: &alias_to,
        parameter_locals,
        retained_move_sites: &retained_move_sites,
        cow_handoff_commit_sites: &cow_handoff_commit_sites,
        partial_transfer_payload_slots,
        deferred_payload_transfer_binders: &deferred_payload_transfer_binders,
    };

    // Scope-exit releases ride the NORMAL-continuation exit plans (a
    // `goto[bbN->bbM]` edge closing an inner scope carries real drops), so
    // those plans participate in the dataflow at their owning block.
    // Exception edges (`Panic` / `Cancel` / `Unwind`) fire only on their
    // exceptional path. `Return`, `Suspend`, and `Unwind` plans are folded at
    // their terminal verdicts: a suspend plan belongs solely to the
    // coro.destroy abandon edge and must never discharge the still-live frame
    // state flowing into resume; an unwind plan belongs solely to LLVM
    // `invoke`'s cleanup successor and must never discharge the normal state.
    let mut edge_drops: HashMap<u32, Vec<&ElabDrop>> = HashMap::new();
    for (exit, plan) in &elab.drop_plans {
        if matches!(
            exit,
            ExitPath::Return { .. }
                | ExitPath::Panic { .. }
                | ExitPath::Cancel { .. }
                | ExitPath::Unwind { .. }
                | ExitPath::Suspend { .. }
        ) {
            continue;
        }
        edge_drops
            .entry(exit_block_id(exit))
            .or_default()
            .extend(plan.drops.iter());
    }
    // Every place inline-dropped anywhere in the function, for the
    // mirrored-plan widening in `apply_plan_drop`.
    let mut inline_dropped: HashSet<Place> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Drop { place, .. } = instr {
                inline_dropped.insert(*place);
            }
        }
    }

    // Forward interval fixpoint over the normal/resume CFG. Suspend cleanup
    // targets are excluded from predecessor and reachability construction:
    // the abandon edge is terminal and checked against its plan below.
    // Monotone: `lo` only decreases, `hi` only increases, `neutralized` only
    // coarsens — all bounded, so the worklist terminates (loops included).
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();
    let mut preds: HashMap<u32, Vec<u32>> = HashMap::new();
    for block in blocks {
        for successor in obligation_successors(block) {
            preds.entry(successor).or_default().push(block.id);
        }
    }
    let mut reachable: HashSet<u32> = HashSet::new();
    let mut reach_stack = Vec::new();
    if by_id.contains_key(&ENTRY_BLOCK_ID) {
        reachable.insert(ENTRY_BLOCK_ID);
        reach_stack.push(ENTRY_BLOCK_ID);
    }
    while let Some(block_id) = reach_stack.pop() {
        let Some(block) = by_id.get(&block_id) else {
            continue;
        };
        for successor in obligation_successors(block) {
            if reachable.insert(successor) {
                reach_stack.push(successor);
            }
        }
    }
    let mut exit_states: HashMap<u32, ObligationMap> = HashMap::new();
    let mut worklist: VecDeque<u32> = dataflow::compute_rpo(blocks).into();
    let mut iterations: usize = 0;
    while let Some(bb_id) = worklist.pop_front() {
        iterations += 1;
        if iterations > iteration_cap {
            // The lattice is finite and the transfer monotone, so convergence
            // is guaranteed within the cap; reaching it means the model is
            // defective. Fail CLOSED — a balance gate that cannot decide must
            // NOT certify the body as leak- and double-free-free. Emit an
            // unverified hard error (no allowlist escape) rather than the old
            // silent `return Vec::new()`, which certified the function as
            // balanced on the exact path where the verdict is unknown.
            return vec![MirCheck::ObligationBalanceUnverified {
                function: elab.name.clone(),
                reason: format!(
                    "discharge-interval fixpoint exceeded its iteration cap \
                     ({iteration_cap}) over {n} blocks before converging; the \
                     balance of this function is undecided",
                    n = blocks.len(),
                ),
            }];
        }
        let Some(block) = by_id.get(&bb_id) else {
            continue;
        };
        let empty = Vec::new();
        let entry = preds
            .get(&bb_id)
            .unwrap_or(&empty)
            .iter()
            .filter(|p| reachable.contains(p))
            .filter_map(|p| {
                let predecessor = by_id.get(p)?;
                let mut state = exit_states.get(p)?.clone();
                apply_suspend_resume_mints(
                    &mut state,
                    predecessor,
                    bb_id,
                    suspend_kinds.get(p),
                    &cx,
                );
                Some(state)
            })
            .fold(None::<ObligationMap>, |acc, m| match acc {
                None => Some(m),
                Some(cur) => Some(meet_obligation_maps(&cur, &m)),
            })
            .unwrap_or_default();
        let mut state = entry;
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            apply_balance_instr(&mut state, instr, bb_id, instr_index, &cx);
        }
        apply_balance_terminator(&mut state, block, suspend_kinds.get(&bb_id), &cx);
        if let Some(drops) = edge_drops.get(&bb_id) {
            for drop in drops {
                apply_plan_drop(&mut state, drop, &inline_dropped, &cx);
            }
        }
        let changed = exit_states.get(&bb_id) != Some(&state);
        exit_states.insert(bb_id, state);
        if changed {
            for succ in obligation_successors(block) {
                worklist.push_back(succ);
            }
        }
    }

    let mut under_released: BTreeMap<u32, UnderReleaseAggregate> = BTreeMap::new();

    // Verdict per terminal ownership edge: Return folds the ordinary function
    // exit plan; Suspend folds the abandon-only frame plan. The default park
    // edge is not terminal ownership transfer — the owner remains in frame.
    for (exit, plan) in &elab.drop_plans {
        let (block, exit_label) = match exit {
            ExitPath::Return { block } => (*block, "return"),
            ExitPath::Suspend { block, .. } => (*block, "suspend-abandon"),
            ExitPath::Unwind { block, .. } => (*block, "unwind"),
            _ => continue,
        };
        if !reachable.contains(&block) {
            continue;
        }
        let Some(block_state) = exit_states.get(&block) else {
            continue;
        };
        let mut state = block_state.clone();
        if matches!(exit, ExitPath::Unwind { .. }) {
            // `exit_states` includes the terminator's normal-edge destination
            // write. An `invoke` that unwinds never produces that result, so
            // the destination owner is uninitialized on the exceptional edge
            // and must not be diagnosed as a leaked mint. Argument handoffs do
            // happen at call entry and deliberately remain in the state.
            if let Some(Terminator::Call {
                dest: Some(dest), ..
            }) = blocks
                .iter()
                .find(|candidate| candidate.id == block)
                .map(|candidate| &candidate.terminator)
            {
                if let Some(local) = whole_owner_local(*dest) {
                    state.remove(&local);
                }
            }
        }
        for drop in &plan.drops {
            apply_plan_drop(&mut state, drop, &inline_dropped, &cx);
        }
        for (root, ob) in &state {
            let name = tracked
                .get(root)
                .cloned()
                .unwrap_or_else(|| format!("local_{root}"));
            // Two independently safe proofs feed the leak verdict:
            //   * `hi < mint_lo`: every reaching path is short a release;
            //   * `mint_hi > hi`: the path carrying the largest explicit
            //     retain count cannot be paid even by the largest possible
            //     discharge count on any reaching path.
            // The second form preserves a branch-local retain debt instead of
            // diluting it against an unretained sibling at the CFG join.
            let retained_path_under_released = ob.explicit_retain_hi && ob.mint_hi > ob.hi;
            if ob.hi < ob.mint_lo || retained_path_under_released {
                let reported_mints = if retained_path_under_released {
                    ob.mint_hi
                } else {
                    ob.mint_lo
                };
                let aggregate = under_released.entry(*root).or_default();
                aggregate.blocks.push(block);
                aggregate.exits.push(format!("{exit_label}[bb{block}]"));
                let exit_provenance = if ob.explicit_retain_lo {
                    crate::model::ObligationMintProvenance::ExplicitRetain
                } else if ob.explicit_retain_hi {
                    crate::model::ObligationMintProvenance::Mixed
                } else {
                    crate::model::ObligationMintProvenance::Ordinary
                };
                aggregate.mint_provenance = Some(
                    aggregate
                        .mint_provenance
                        .map_or(exit_provenance, |current| current.join(exit_provenance)),
                );
                aggregate.max_mints = aggregate.max_mints.max(reported_mints);
                aggregate.max_discharges = aggregate.max_discharges.max(ob.hi);
            } else if ob.max_definite > ob.mint_hi {
                findings.push(MirCheck::ObligationOverReleased {
                    function: elab.name.clone(),
                    block,
                    name: name.clone(),
                    reason: format!(
                        "owned local `{name}` accumulates {max_def} definite discharges for at \
                         most {mint_hi} owner mint(s) on a single path reaching \
                         {exit_label}[bb{block}] (discharge interval [{lo}, {hi}]): double release",
                        max_def = ob.max_definite,
                        mint_hi = ob.mint_hi,
                        lo = ob.lo,
                        hi = ob.hi,
                    ),
                });
            }
        }
    }
    for (root, mut aggregate) in under_released {
        aggregate.blocks.sort_unstable();
        aggregate.blocks.dedup();
        aggregate.exits.sort();
        aggregate.exits.dedup();
        let name = tracked
            .get(&root)
            .cloned()
            .unwrap_or_else(|| format!("local_{root}"));
        let mint_provenance = aggregate.mint_provenance.unwrap_or_default();
        findings.push(MirCheck::ObligationUnderReleased {
            function: elab.name.clone(),
            blocks: aggregate.blocks,
            site: mint_sites.get(&root).copied().unwrap_or(SiteId(0)),
            name: name.clone(),
            local_ty: local_types.get(&root).cloned().unwrap_or_default(),
            mint_provenance,
            reason: format!(
                "owned value `{name}` has up to {mints} owner mint(s), but at most \
                 {discharges} discharge(s), on {count} reachable exit path(s): {exits}; \
                 one or more owners have no terminal drop or ownership transfer before \
                 those exits (mint without discharge = leak)",
                mints = aggregate.max_mints,
                discharges = aggregate.max_discharges,
                count = aggregate.exits.len(),
                exits = aggregate.exits.join(", "),
            ),
        });
    }
    findings
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
/// Runtime ABIs that BORROW their owned-handle arguments by value rather than
/// taking ownership: the callee snapshots/reads the handle without retaining or
/// transferring it, so a borrowed arg does not alias a second free. The
/// active-mode transport `attach(handler)` surfaces lower to typed pseudo
/// calls whose `LocalPid` handler the runtime registers as a non-owning
/// actor-ref snapshot (real free-count 1, via the caller's source drop alone).
/// Kept as an explicit, narrow allowlist so the escape gate does not invent a
/// phantom second free for a ratified borrowing surface while every non-listed
/// call still fails closed by default. This allowlist is the PARTIAL form —
/// only the callee's non-handle-leaf args are borrowed; owned-handle-leaf args
/// are still poisoned. For callees that borrow EVERY arg including owned-handle
/// leaves, see [`is_handle_borrowing_call_abi`].
#[cfg(test)]
pub(super) fn is_borrowing_call_abi(
    builtin: Option<hew_types::runtime_call::RuntimeCallFamily>,
) -> bool {
    matches!(
        builtin,
        Some(
            hew_types::runtime_call::RuntimeCallFamily::TcpAttachLocal
                | hew_types::runtime_call::RuntimeCallFamily::TlsAttachLocal
                | hew_types::runtime_call::RuntimeCallFamily::WebSocketAttachLocal
        )
    )
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
/// hinge is the owned-record escape rule in `derive_owned_record_drop_allowed`,
/// not a tuple-specific shape whitelist.
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
/// A payload binder's attribution to the composite candidate it was
/// projected from. `Root` names the single candidate proven so far;
/// `Conflict` marks a binder that TWO DIFFERENT composite roots fed the same
/// local — deliberately not a missing map entry, so `note_payload_escape`'s
/// fail-closed "unknown root" branch still fires and excludes every
/// candidate instead of silently keeping whichever root arrived first.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PayloadBinderRoot {
    Root(u32),
    Conflict,
}
/// Attribute `binder` to `root`, or mark it `Conflict` when it is already
/// attributed to a DIFFERENT root. First-root-wins silently excluded only
/// the first root on escape when a raw-MIR shape fed one heap payload local
/// from two composite roots; `Conflict` instead falls back to the coarse
/// exclude-every-root posture for that binder. Once `Conflict`, the
/// attribution never reverts to a single root.
pub(super) fn attribute_payload_binder_root(
    payload_binder_candidate_root: &mut HashMap<u32, PayloadBinderRoot>,
    binder: u32,
    root: u32,
) {
    match payload_binder_candidate_root.get(&binder) {
        None => {
            payload_binder_candidate_root.insert(binder, PayloadBinderRoot::Root(root));
        }
        Some(PayloadBinderRoot::Root(existing)) if *existing != root => {
            payload_binder_candidate_root.insert(binder, PayloadBinderRoot::Conflict);
        }
        _ => {}
    }
}
/// Propagate `src_binder`'s attribution onward to `dest_binder` on a benign
/// hand-off Move. `Root` carries forward through [`attribute_payload_binder_root`]
/// (so a second, differing root already at `dest_binder` still resolves to
/// `Conflict`); `Conflict` propagates as `Conflict` rather than reverting to
/// no attribution; no attribution at `src_binder` leaves `dest_binder`
/// untouched.
pub(super) fn propagate_payload_binder_root(
    payload_binder_candidate_root: &mut HashMap<u32, PayloadBinderRoot>,
    src_binder: u32,
    dest_binder: u32,
) {
    match payload_binder_candidate_root.get(&src_binder).copied() {
        Some(PayloadBinderRoot::Root(root)) => {
            attribute_payload_binder_root(payload_binder_candidate_root, dest_binder, root);
        }
        Some(PayloadBinderRoot::Conflict) => {
            payload_binder_candidate_root.insert(dest_binder, PayloadBinderRoot::Conflict);
        }
        None => {}
    }
}
/// A payload binder read into an owning sink means the active payload escaped
/// its composite. Exclude only the root THAT binder was projected from, via the
/// per-candidate `payload_binder_candidate_root` attribution the borrow
/// exemption already trusts — so a legitimately-escaping non-synthesizable
/// sibling (a `Result<GlobResult, _>` resource binder consumed by
/// `matches.close()`) no longer strips a separately-admissible
/// `Result<bytes, string>` / `Result<CommandOutput, _>` of its `EnumInPlace`
/// drop. A binder with no known root, OR a `Conflict` attribution (two
/// composite roots fed the same binder), fails closed to the coarse
/// every-root posture. Over-exclusion leaks, never double-frees; this mirrors
/// the already per-root `note_alias_escape` whole-composite escape.
pub(super) fn note_payload_escape(
    payload_binder_candidate_root: &HashMap<u32, PayloadBinderRoot>,
    escaping_binder: u32,
    alias_of: &HashMap<u32, u32>,
    _blocks: &[BasicBlock],
    excluded_roots: &mut HashSet<u32>,
) {
    match payload_binder_candidate_root.get(&escaping_binder) {
        Some(PayloadBinderRoot::Root(root)) => {
            excluded_roots.insert(*root);
        }
        Some(PayloadBinderRoot::Conflict) | None => {
            for &root in alias_of.values() {
                excluded_roots.insert(root);
            }
        }
    }
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
        // hint is a structural fail-closed event — `build_lifo_drops`
        // refuses to emit a drop without a storage discriminator, and
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
        // validator re-derives against, so `build_lifo_drops` must emit the
        // identical kind (see `cow_value_leaf_drop_symbol`).
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
        // `build_lifo_drops` must emit the identical kind (admission authority:
        // `derive_local_bytes_drop_allowed`).
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
        // against, so `build_lifo_drops` must emit the identical kind. Dispatch
        // on the `builtin` discriminant (NOT the name string) so a user
        // `type HashMap { ... }` is never mistaken for the runtime handle.
        //
        // The release is an UNCONDITIONAL dealloc (the handle carries no
        // refcount); it is sound because the current M-COW spine is move-only —
        // exactly one live binding owns each handle, enforced by the move-checker
        // consuming the source on every share (see the SUBSTRATE INVARIANT /
        // REVISIT TRIGGER on `derive_local_collection_drop_allowed`). When
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
        // THIS function is only called from the **end-of-function LIFO
        // elaboration path** (`build_lifo_drops`), which operates on
        // `owned_locals` at binding granularity. A machine binding in
        // `owned_locals` should never be found here: machine `self` is a
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

fn builtin_resource_drop_descriptor(
    builtin: BuiltinType,
) -> Option<hew_types::runtime_call::RuntimeDropDescriptor> {
    use hew_types::runtime_call::RuntimeDropDescriptor;
    match builtin {
        BuiltinType::Duplex => Some(RuntimeDropDescriptor::DuplexClose),
        BuiltinType::Stream => Some(RuntimeDropDescriptor::StreamClose),
        BuiltinType::Sink => Some(RuntimeDropDescriptor::SinkClose),
        BuiltinType::Sender => Some(RuntimeDropDescriptor::SenderClose),
        BuiltinType::Receiver => Some(RuntimeDropDescriptor::ReceiverClose),
        BuiltinType::LambdaActorHandle | BuiltinType::LambdaPid => {
            Some(RuntimeDropDescriptor::LambdaActorHandleClose)
        }
        BuiltinType::SendHalf => Some(RuntimeDropDescriptor::SendHalfClose),
        BuiltinType::RecvHalf => Some(RuntimeDropDescriptor::RecvHalfClose),
        BuiltinType::CancellationToken => Some(RuntimeDropDescriptor::CancellationTokenRelease),
        BuiltinType::MonitorRef => Some(RuntimeDropDescriptor::MonitorRefClose),
        _ => None,
    }
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
        } => builtin_resource_drop_descriptor(*builtin).map(crate::model::DropFnSpec::Runtime),
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
/// skip, and the `build_lifo_drops` guard attachment), so they cannot
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
/// the ownership decision is the separate fail-closed
/// `derive_closure_pair_drop_allowed` authority.
pub(super) fn ty_is_closure_pair(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. })
}
/// Whole-value hand-off dedup for a Vec-handle drop allow-set. The
/// array-literal desugar binds the fresh vec to a synthetic let
/// (`__hew_array_N`) and the user binding then receives the SAME handle
/// through a chain of whole-value `Move`s (synthetic slot → expression temp →
/// binding slot) — TWO admitted bindings whose slots hold ONE handle, which
/// would emit two scope-exit releases (a double free; the dataflow does not
/// mark the synthetic source consumed). Ownership follows the `Move` chain:
/// strip every admitted binding whose handle transitively flows into ANOTHER
/// admitted binding's slot, so exactly the final owner releases.
/// LESSONS: raii-null-after-move, cleanup-all-exits.
/// Path-compressing union-find lookup over the undirected Move graph used by
/// the fan-out collapse in [`dedup_whole_value_handoff`].
fn move_component_root(parent: &mut HashMap<u32, u32>, x: u32) -> u32 {
    let p = *parent.entry(x).or_insert(x);
    if p == x {
        return x;
    }
    let root = move_component_root(parent, p);
    parent.insert(x, root);
    root
}
/// The whole-value `Move` graph [`dedup_whole_value_handoff`] walks: an edge
/// `(src_local, dest_local)` for every move that leaves both slots holding the
/// same live payload. Ownership TRANSFERS — moves whose source slot is nulled
/// immediately afterwards — are not edges, because after them the two locals
/// do not overlap and neither defers to the other.
fn whole_value_handoff_move_edges(blocks: &[BasicBlock]) -> Vec<(u32, u32)> {
    let mut move_edges: Vec<(u32, u32)> = Vec::new();
    // #2523 — an interior-projection move whose source slot is NEUTRALIZED
    // (`Instr::NeutralizePayloadSlot`) is an ownership TRANSFER, not a
    // shared-bits alias: the destination becomes the sole owner and the source
    // field is nulled, so the two locals do NOT overlap. Such edges must be
    // excluded from the hand-off/fan-out move graph. Otherwise two payload
    // fields moved out of the SAME aggregate (`V(x, y) => var wx = x; var wy = y;`)
    // collapse to `base_local(scrutinee)` and land in one undirected component,
    // and the fan-out collapse below wrongly strips BOTH their scope-exit drops
    // (leaking the reassigned owners — the #2523 two-field leak). This mirrors
    // the identical neutralized-source exclusion in `compute_projection_alias_taint`.
    let neutralized_sources: HashSet<Place> = blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::NeutralizePayloadSlot { place, .. } => Some(*place),
            _ => None,
        })
        .collect();
    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            if let Instr::Move { dest, src } = instr {
                if place_is_interior_projection(*src) && neutralized_sources.contains(src) {
                    continue;
                }
                // A typed produced-value handoff is a physical move when the
                // immediately following commit zeroes its whole source slot.
                // The two slots never alias after this point, so this is not an
                // edge in the shared-bits handoff graph.
                if block.instructions.get(instr_index + 1).is_some_and(|next| {
                    matches!(
                        next,
                        Instr::NeutralizePayloadSlot {
                            place,
                            transferee: Some(transferee),
                            authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
                        } if *place == *src && *transferee == *dest
                    )
                }) {
                    continue;
                }
                // A divergent-arm selection transfer nulls the whole source
                // local right after the move, so source and destination never
                // hold the same live payload: it is a transfer edge, not a
                // hand-off alias. Keeping it would make the source "handed
                // off" to the join slot and strip the release it still owes on
                // the paths that took a different arm.
                if super::split_consume::is_divergent_selection_transfer_move(block, instr_index) {
                    continue;
                }
                if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                    if sl != dl {
                        move_edges.push((sl, dl));
                    }
                }
            }
        }
    }
    move_edges
}

fn dedup_whole_value_handoff(
    blocks: &[BasicBlock],
    binding_locals: &HashMap<BindingId, Place>,
    allowed: &mut HashSet<BindingId>,
    guarded: &HashMap<BindingId, Place>,
) {
    let admitted_locals: HashMap<u32, BindingId> = allowed
        .iter()
        .filter_map(|b| {
            binding_locals
                .get(b)
                .and_then(|p| base_local(*p))
                .map(|l| (l, *b))
        })
        .collect();
    if admitted_locals.is_empty() {
        return;
    }
    let move_edges = whole_value_handoff_move_edges(blocks);
    for (&start_local, start_binding) in &admitted_locals {
        // BFS the Move graph from this admitted local; if the handle
        // reaches another admitted binding's slot, the downstream
        // binding owns the release and this one must not also fire.
        let mut frontier = vec![start_local];
        let mut seen: HashSet<u32> = HashSet::new();
        seen.insert(start_local);
        let mut handed_off = false;
        while let Some(cur) = frontier.pop() {
            for &(s, d) in &move_edges {
                if s == cur && seen.insert(d) {
                    if d != start_local && admitted_locals.contains_key(&d) {
                        handed_off = true;
                    }
                    frontier.push(d);
                }
            }
        }
        if handed_off && !guarded.contains_key(start_binding) {
            // #2418 — a source binding carrying a path-sensitive drop-flag is
            // NOT stripped: its hand-off is a dataflow-visible consume (the
            // flag is set 1 exactly where the move executes), so its
            // scope-exit release already fires only on paths where the handle
            // was never handed off. Stripping it would re-open the
            // conditional-move leak the flag exists to close. Unguarded
            // sources (the dataflow-invisible synthetic chains) keep the
            // strip: the downstream owner fires the single release.
            allowed.remove(start_binding);
        }
    }

    // Fan-out collapse (fail-closed). The hand-off strip above resolves a
    // CHAIN — every upstream binding whose handle flows into another admitted
    // binding defers to that downstream owner — but not a FAN-OUT: one source
    // whole-value-copied into several sibling bindings (`let s1 = v; … let
    // s2 = v;`, the Vec-pipeline receiver rebind `__hew_pipe_src_N`). No
    // sibling flows into another, so after the source is stripped EVERY
    // sibling stays admitted, and each fires its own scope-exit free of the
    // SAME handle (the exit-time invalid-free class). Sole ownership inside
    // such a group is unprovable under the move-only substrate, so remove
    // every remaining admitted binding in any undirected Move-connected
    // component that still holds more than one: each such handle leaks (as
    // before the plain-Vec lane), never double-frees. Single-owner components
    // (the array-literal chain, the pipeline-out → user-binding chain) are
    // untouched and keep their exactly-one free.
    // LESSONS: boundary-fail-closed, raii-null-after-move.
    let remaining: Vec<(u32, BindingId)> = allowed
        .iter()
        .filter_map(|b| {
            binding_locals
                .get(b)
                .and_then(|p| base_local(*p))
                .map(|l| (l, *b))
        })
        .collect();
    if remaining.len() < 2 {
        return;
    }
    // Union-find over the UNDIRECTED Move graph: two admitted bindings in one
    // component may share one handle's bits, so at most one free is provable
    // — and we cannot prove which, so none fires.
    let mut parent: HashMap<u32, u32> = HashMap::new();
    for &(s, d) in &move_edges {
        let rs = move_component_root(&mut parent, s);
        let rd = move_component_root(&mut parent, d);
        if rs != rd {
            parent.insert(rs, rd);
        }
    }
    let mut component_admitted: HashMap<u32, Vec<BindingId>> = HashMap::new();
    for &(local, binding) in &remaining {
        let root = move_component_root(&mut parent, local);
        component_admitted.entry(root).or_default().push(binding);
    }
    for bindings in component_admitted.values() {
        // #2418 — a flag-guarded binding (a conditional move's source) shares
        // its component with the move's DESTINATION(s) by construction, and
        // the group is exactly-once provable at runtime when the destinations
        // are on mutually-exclusive control-flow paths: each destination
        // releases on the arm where its move executed (its own scope-close
        // edge), the guarded source releases at scope exit only where the
        // flag is still 0 (the not-moved path). The move-checker already
        // rejects co-executable consume sites of one binding
        // (`UseAfterConsume` — straight-line re-consume and loop re-consume
        // without `break` both fail), so exclusive per-branch destinations
        // (`if a { let y = xs; } else if b { let z = xs; }`) are the only
        // accepted multi-destination shape — but the exclusivity is proven
        // HERE, in the CFG (no path executes two of the destinations' bind
        // sites), not assumed from the checker: a synthetic Move that never
        // went through the checker gets no benefit of the doubt.
        //
        // So: a component containing a guarded member keeps every member iff
        // every UNGUARDED member's bind sites are pairwise non-co-executable
        // (`unguarded_bind_sites_pairwise_exclusive`). A genuinely-parallel
        // fan-out — two unguarded destinations reachable on ONE path — still
        // takes the fail-closed collapse, guarded members included (a guarded
        // drop over an ambiguous share could double-free on the not-moved
        // path; leak instead, as before). Guardless components keep the
        // original >1-member collapse: sole ownership across a
        // dataflow-invisible fan-out is unprovable.
        let unguarded: Vec<BindingId> = bindings
            .iter()
            .filter(|b| !guarded.contains_key(b))
            .copied()
            .collect();
        if unguarded.len() <= 1 {
            continue;
        }
        let has_guarded = bindings.len() > unguarded.len();
        if has_guarded
            && unguarded_bind_sites_pairwise_exclusive(blocks, binding_locals, &unguarded)
        {
            continue;
        }
        for binding in bindings {
            allowed.remove(binding);
        }
    }
}
/// #2418 — base-parity fallback for a flag-guarded binding the escape scan
/// cannot admit. A flagged binding stays REGISTERED at its consume sites
/// (that is the fix), which also keeps it a CANDIDATE in the collection
/// escape scans — so an owning-sink read of it anywhere (an aggregate-literal
/// ingress on one branch, a by-value call, a return) excludes not just the
/// binding but its whole whole-value alias group, via the scan's
/// root-resolution. The legacy retract-at-consume compiler never presented
/// the binding as a candidate at all, so the move DESTINATION stood alone
/// with its own root and kept its release. Reproduce exactly that outcome:
/// re-derive the allow-set with every excluded flagged binding removed from
/// the candidate view, to a fixpoint (each round strictly shrinks the view,
/// and removing a candidate only removes escape notes, so admissions grow
/// monotonically). The excluded binding itself ends unregistered — the same
/// leak-not-double-free safety posture for that shape.
fn admit_with_flagged_fallback<C, F>(
    owned_locals_snapshot: &[(BindingId, String, ResolvedTy)],
    collection_drop_flags: &HashMap<BindingId, Place>,
    class_filter: C,
    derive: F,
) -> HashSet<BindingId>
where
    C: Fn(&ResolvedTy) -> bool,
    F: Fn(&[(BindingId, String, ResolvedTy)]) -> HashSet<BindingId>,
{
    let mut view: Vec<(BindingId, String, ResolvedTy)> = owned_locals_snapshot.to_vec();
    let mut allowed = derive(&view);
    loop {
        let excluded_flagged: HashSet<BindingId> = view
            .iter()
            .filter(|(binding, _, ty)| {
                class_filter(ty)
                    && collection_drop_flags.contains_key(binding)
                    && !allowed.contains(binding)
            })
            .map(|(binding, _, _)| *binding)
            .collect();
        if excluded_flagged.is_empty() {
            return allowed;
        }
        view.retain(|(binding, _, _)| !excluded_flagged.contains(binding));
        allowed = derive(&view);
    }
}
/// #2418 — CFG-grounded exclusivity test for the fan-out collapse in
/// [`dedup_whole_value_handoff`]: true iff the whole-value `Move` bind sites
/// of every binding in `unguarded` are pairwise non-co-executable, i.e. no
/// runtime path can execute two of them. Two sites co-execute when they share
/// a basic block, when one site's block reaches the other's, or when a site's
/// block sits on a cycle (it could re-execute); each case is tested directly
/// on the CFG via [`BasicBlock::successors`] reachability, so the answer is a
/// structural fact about this function's blocks — not an inference from the
/// move-checker's acceptance.
///
/// Fail-closed: a binding with no locatable bind site, an unresolvable Place,
/// or any co-executable pair returns `false` (the caller strips the whole
/// component — leak, never a double-free). Over-approximation direction: the
/// reachability walk follows EVERY successor edge (`successors()` is an
/// exhaustive match over `Terminator`), so exclusivity can only be
/// under-reported, never over-reported.
fn unguarded_bind_sites_pairwise_exclusive(
    blocks: &[BasicBlock],
    binding_locals: &HashMap<BindingId, Place>,
    unguarded: &[BindingId],
) -> bool {
    let mut dest_locals: HashSet<u32> = HashSet::new();
    for binding in unguarded {
        let Some(local) = binding_locals.get(binding).and_then(|p| base_local(*p)) else {
            return false;
        };
        dest_locals.insert(local);
    }
    // Every block holding a whole-value Move into one of the destinations.
    // Two sites (same or different destinations) in one block are trivially
    // co-executable, so a second site in a block fails immediately.
    let mut site_blocks: Vec<u32> = Vec::new();
    let mut located_dest_locals: HashSet<u32> = HashSet::new();
    for block in blocks {
        let mut block_has_site = false;
        for instr in &block.instructions {
            if let Instr::Move { dest, src } = instr {
                if let (Some(dl), Some(sl)) = (base_local(*dest), base_local(*src)) {
                    if dl != sl && dest_locals.contains(&dl) {
                        if block_has_site {
                            return false;
                        }
                        block_has_site = true;
                        located_dest_locals.insert(dl);
                    }
                }
            }
        }
        if block_has_site {
            site_blocks.push(block.id);
        }
    }
    // A destination the scan could not locate a bind site for cannot be
    // proven exclusive of anything — fail closed.
    if located_dest_locals.len() != dest_locals.len() {
        return false;
    }
    // Forward reachability from each site block. A site reaching ANY site
    // block — another's, or its own around a cycle — is co-executable.
    let successors: HashMap<u32, Vec<u32>> =
        blocks.iter().map(|b| (b.id, b.successors())).collect();
    let site_set: HashSet<u32> = site_blocks.iter().copied().collect();
    for &start in &site_blocks {
        let mut frontier: Vec<u32> = successors.get(&start).cloned().unwrap_or_default();
        let mut visited: HashSet<u32> = HashSet::new();
        while let Some(cur) = frontier.pop() {
            if !visited.insert(cur) {
                continue;
            }
            if site_set.contains(&cur) {
                return false;
            }
            if let Some(succs) = successors.get(&cur) {
                frontier.extend(succs.iter().copied());
            }
        }
    }
    true
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
/// Fail-closed sole-owner allow-set for `indirect enum` heap-node bindings
/// (spec §3.7.4). An indirect-enum local is a single heap `ptr` to a
/// tagged-union node; its scope-exit release (`DropKind::IndirectEnum`) frees
/// that node and recurses into owned child nodes. Because the move-checker
/// treats every by-value indirect-enum use as a borrow (`intent = Read`
/// everywhere — neither a return nor a call-argument marks the binding
/// `Consumed`), the consume-dataflow alone cannot prove sole ownership; this
/// derivation supplies the structural proof, defaulting every unproven binding
/// to EXCLUSION (leak, never double-free).
///
/// A binding is admitted ONLY IF all hold:
///
///  1. it has a `Place::Local` slot whose type is an indirect enum, AND
///  2. its local is a CONSTRUCTION site — it is written through a tag/variant
///     place (`Move { dest: MachineTag/MachineVariant/EnumTag/EnumVariant
///     (local), .. }`), i.e. THIS binding allocated and populated the node.
///     A binding that only RECEIVES a pointer (a destructure binder
///     `l = move mvar.payload`, a move-temp, a call-result binding) is not a
///     construction site and is excluded: it aliases a node another owner (the
///     parent node, or the callee) is responsible for, AND
///  3. its node is NOT moved INTO a parent node's variant payload
///     (`Move { dest: MachineVariant/EnumVariant { parent, .. }, src: this }`):
///     a child wired into a parent `Node(child, …)` is owned by the parent now
///     — the parent's recursive free reclaims it, so the child must not also
///     fire its own free (the recursive-free double-free this rule prevents).
///
/// The caller (`elaborate`) folds in the remaining fail-closed filters that
/// every other drop class shares: the `returned_aggregate_members` skip
/// (a node handed to the caller through the `ReturnSlot` or a returned
/// aggregate), and the dataflow `Consumed`/`MaybeConsumed` exit-state removal.
///
/// Every classifier consulted (`instr_*` Move-dest shapes) is a
/// compiler-exhaustive match over `Instr`, so a future construction- or
/// ingress-shaped instruction cannot be introduced without a classification
/// decision whose default direction is exclusion (a leak, never a double-free).
///
/// LESSONS: drop-allowset-from-value-flow (P0 — the default is no-drop; a drop
/// is earned only by a positive sole-owner proof), boundary-fail-closed,
/// cleanup-all-exits, raii-null-after-move.
#[must_use]
fn derive_indirect_enum_drop_allowed(
    blocks: &[BasicBlock],
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    enum_layouts: &[crate::model::EnumLayout],
    actor_message_ingress_locals: &HashSet<u32>,
) -> HashSet<BindingId> {
    // Pass 1 — classify the construction sites, the parent-ingress sources, and
    // the whole-value Move edges. A constructed node lands in a tag/variant
    // place's local, but the BINDING's slot is usually a later SSA local the
    // node is `Move`d into (`_ctor = Node(..); _binding = move _ctor`), so node
    // ownership must be PROPAGATED forward through Move to find the binding's
    // resting slot.
    let mut construction_sites: HashSet<u32> = HashSet::new();
    let mut moved_into_parent_node: HashSet<u32> = HashSet::new();
    // Whole-value Move edges `src -> dest` between plain locals (the node-pointer
    // flow graph). Tag/variant writes are construction, not flow, so excluded.
    let mut move_edges: Vec<(u32, u32)> = Vec::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Move { dest, src } = instr {
                match dest {
                    Place::MachineTag(l)
                    | Place::EnumTag(l)
                    | Place::MachineVariant { local: l, .. }
                    | Place::EnumVariant { local: l, .. } => {
                        construction_sites.insert(*l);
                        // The SOURCE of a variant-payload store is a child node
                        // wired into the parent `*l`'s node — the parent owns it.
                        if matches!(
                            dest,
                            Place::MachineVariant { .. } | Place::EnumVariant { .. }
                        ) {
                            if let Some(child) = base_local(*src) {
                                moved_into_parent_node.insert(child);
                            }
                        }
                    }
                    Place::Local(dl) => {
                        // Node-pointer FLOW edge — only a whole-value `Place::Local`
                        // src aliases the same node into `dl`. A SUB-place src
                        // (`l = move parent.payload.0`, src = MachineVariant/
                        // EnumVariant; or a tag read, src = MachineTag/EnumTag)
                        // COPIES a CHILD pointer the parent node still owns — it is
                        // NOT a whole-value alias of the parent. Seeding a flow edge
                        // from such a sub-place would (a) misclassify the child
                        // binder as a node owner (Pass 2) and (b) drag it into the
                        // parent's Pass-4 fan-out component, collapsing the parent's
                        // sole-owner free — the inline match-destructure leak (#46).
                        // The parent's recursive free already reclaims that child, so
                        // restricting to a whole-value `Local` src is fail-closed:
                        // whole-value rebinds (`let u = t;`) still flow and still
                        // collapse; destructure binders no longer entangle the parent.
                        if let Place::Local(sl) = src {
                            if *sl != *dl {
                                move_edges.push((*sl, *dl));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Pass 2 — forward-propagate node ownership from each construction-site
    // local along the Move edges to a fixpoint. `owns_node` is the set of
    // locals that hold a freshly-constructed node (the node's flow closure).
    let mut owns_node: HashSet<u32> = construction_sites
        .union(actor_message_ingress_locals)
        .copied()
        .collect();
    loop {
        let mut changed = false;
        for &(sl, dl) in &move_edges {
            if owns_node.contains(&sl) && owns_node.insert(dl) {
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    // Pass 3 — the locals that own a binding's resting slot. Only locals that
    // back an owned indirect-enum binding are candidates.
    let mut local_to_binding: HashMap<u32, BindingId> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !ty_is_indirect_enum(ty, enum_layouts) {
            continue;
        }
        if let Some(place) = binding_locals.get(binding) {
            if let Some(local) = base_local(*place) {
                local_to_binding.insert(local, *binding);
            }
        }
    }

    let mut allowed: HashSet<BindingId> = HashSet::new();
    for (&local, &binding) in &local_to_binding {
        // (a) the binding's slot must hold a freshly-constructed node, AND
        // (b) that node must not have been handed to a parent node (the parent's
        //     recursive free owns it). A binding whose node was moved into a
        //     parent (a child `let left = ...; Node(left, right)`) is excluded.
        if !owns_node.contains(&local) || moved_into_parent_node.contains(&local) {
            continue;
        }
        allowed.insert(binding);
    }

    // Pass 4 — fan-out / hand-off collapse (fail-closed). If the SAME node flows
    // into two admitted binding slots (whole-value rebind `let u = t;`, or a
    // construction-site local that is itself a separate admitted binding), only
    // one may free it and we cannot prove which — so drop BOTH (leak, never
    // double-free). Union-find over the undirected Move graph restricted to
    // admitted bindings' locals.
    let admitted_locals: HashMap<u32, BindingId> = allowed
        .iter()
        .filter_map(|b| {
            binding_locals
                .get(b)
                .and_then(|p| base_local(*p))
                .map(|l| (l, *b))
        })
        .collect();
    if admitted_locals.len() > 1 {
        let mut parent: HashMap<u32, u32> = HashMap::new();
        for &(s, d) in &move_edges {
            let rs = move_component_root(&mut parent, s);
            let rd = move_component_root(&mut parent, d);
            if rs != rd {
                parent.insert(rs, rd);
            }
        }
        let mut component_admitted: HashMap<u32, Vec<BindingId>> = HashMap::new();
        for (&local, &binding) in &admitted_locals {
            let root = move_component_root(&mut parent, local);
            component_admitted.entry(root).or_default().push(binding);
        }
        for bindings in component_admitted.values() {
            if bindings.len() > 1 {
                for binding in bindings {
                    allowed.remove(binding);
                }
            }
        }
    }

    allowed
}
#[allow(
    clippy::too_many_arguments,
    reason = "drop elaboration threads the binding ledgers, the per-class \
              allow-sets, and the enum layouts the W5.020 enum-composite arm \
              needs; each is a distinct authority, not foldable"
)]
#[expect(
    clippy::too_many_lines,
    reason = "one flat match over the per-binding drop classes (dyn / \
              owned-string-record / enum-composite / owned-Vec / value-class); \
              splitting per-class helpers would scatter the fail-closed arms"
)]
fn build_lifo_drops(
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    type_classes: &hew_hir::TypeClassTable,
    dyn_trait_storage: &HashMap<BindingId, TraitObjectStorage>,
    owned_record_drop_allowed: &HashSet<BindingId>,
    cow_drop_allowed: &HashSet<BindingId>,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
    enum_composite_drop_allowed: &HashSet<BindingId>,
    machine_composite_drop_allowed: &HashSet<BindingId>,
    owned_vec_drop_allowed: &HashSet<BindingId>,
    local_collection_drop_allowed: &HashSet<BindingId>,
    local_bytes_drop_allowed: &HashSet<BindingId>,
    tuple_composite_drop_allowed: &HashSet<BindingId>,
    _returned_aggregate_members: &HashSet<BindingId>,
    _consumed_local_aggregate_members: &HashSet<BindingId>,
    _spawn_consumed_handle_members: &HashSet<BindingId>,
    closure_vec_drop_allowed: &HashSet<BindingId>,
    plain_vec_drop_allowed: &HashSet<BindingId>,
    indirect_enum_drop_allowed: &HashSet<BindingId>,
    affine_release_flags: &HashMap<BindingId, Place>,
    overwrite_guard_flags: &HashMap<BindingId, Place>,
    collection_drop_flags: &HashMap<BindingId, Place>,
    actor_message_cow_drop_flags: &HashMap<BindingId, Place>,
    conditional_record_drop_flags: &HashMap<BindingId, Place>,
    vec_iter_drop_flags: &HashMap<BindingId, Place>,
    latest_owner_by_binding: &HashMap<BindingId, crate::model::OwnerId>,
    projection_alias_tainted: &HashSet<u32>,
    borrowed_builtin_handle_projection_aliases: &HashSet<BindingId>,
    collection_borrow_getter_aliases: &HashSet<u32>,
) -> Vec<ElabDrop> {
    let mut drops = Vec::new();
    let guarded = |binding: &BindingId, flag: Option<Place>| {
        flag.and_then(|flag| {
            latest_owner_by_binding
                .get(binding)
                .copied()
                .map(|owner| crate::model::ElabDropGuard { owner, flag })
        })
    };
    for (binding, _name, ty) in owned_locals.iter().rev() {
        // `VecIter` is an inline cursor whose field-0 ownership is discharged
        // by the dedicated `VecIterCursor` protocol below. Its explicit Guard
        // event makes it a real Checked-MIR owner, but must not also route it
        // through the generic named-record destructor template.
        if vec_iter_drop_flags.contains_key(binding) {
            continue;
        }
        // A binding whose value came from a BORROWING element getter
        // (`hew_vec_get_owned` / `hew_vec_get_ptr` — contract:
        // `returns_receiver_interior_alias`) is an interior alias of a
        // still-live collection. The collection's own release is the single
        // discharge authority for that element (for a close-obligated element,
        // the exactly-once `close`); emitting this binding's LIFO drop would
        // mint a second authority over one context (double-close/free).
        // Skipping only ever un-emits a drop for a value the parent releases —
        // never a leak (`boundary-fail-closed`).
        if binding_locals
            .get(binding)
            .and_then(|place| base_local(*place))
            .is_some_and(|local| collection_borrow_getter_aliases.contains(&local))
        {
            continue;
        }
        // Return, aggregate-ingress, and spawn handoffs do not mutate this
        // destructor catalogue. Their exact Checked-MIR Transfer operations
        // end or relocate the corresponding OwnerId at the program point where
        // ownership changes; per-exit owner state below then excludes the old
        // generation. Whole-function ancestry sets are intentionally not
        // cleanup authority because they erase the owner from earlier unwind
        // edges and misclassify retain-backed return copies.
        // A typed builtin handle projected without retain from a live
        // aggregate remains owned by that aggregate. The projection-alias
        // derivation excludes neutralized move-outs, so this skip applies only
        // to borrow aliases; a transferred payload keeps its destination drop.
        // Place this before every drop-class arm because field-bearing handles
        // such as MonitorRef may otherwise route through the recursive record
        // arm before reaching the scalar affine-resource arm.
        if borrowed_builtin_handle_projection_aliases.contains(binding) {
            continue;
        }
        // W5.016 — owned-element `Vec<T>` local (an element that owns heap:
        // record/enum/tuple with a string field, etc.). Its scope-exit release
        // is `hew_vec_free_owned`, which drops every live element exactly once
        // via the per-element descriptor `drop_fn` then frees the buffer. A Vec
        // is `ValueClass::CowValue` but `cow_value_leaf_drop_symbol` only
        // handles the leaf `string` case, so an owned Vec would otherwise leak.
        // Gated fail-closed on `owned_vec_drop_allowed`: a Vec consumed by
        // for-in (`into_iter` moves it) or returned (moved to the ReturnSlot)
        // is excluded, so it is never double-freed. The drop is a `CowHeap`
        // runtime release; the per-element drop logic lives in the runtime.
        // `owned_vec_drop_allowed` already encodes the owned-element decision
        // (derived from the builder's `is_owned_vec_element` authority); the
        // local guard here only confirms the binding's type is a `Vec` so the
        // `hew_vec_free_owned` ABI is correct for the handle.
        // Closure-pair `Vec<fn(...)>` handle: each slot owns a heap-boxed
        // pair (and its env box). The stamped descriptor drop thunk walks the
        // elements (env free-thunk + pair-box free per slot), then frees the
        // buffer and the handle. Intercept BEFORE the owned-Vec arm so the
        // closure-element class never routes through the descriptor-driven
        // `hew_vec_free_owned` ABI it was not constructed under.
        if closure_vec_drop_allowed.contains(binding) && ty_is_closure_pair_vec(ty) {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: closure-pair Vec binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::CowHeap {
                    release: crate::ownership::CowHeapRelease::VecClosurePairs,
                },
                guard: None,
            });
            continue;
        }
        if owned_vec_drop_allowed.contains(binding) && ty_is_vec(ty) {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: owned-Vec binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::CowHeap {
                    release: crate::ownership::CowHeapRelease::VecOwnedElement,
                },
                // #2418 — a conditionally-moved handle carries its
                // path-sensitive drop-flag so the release fires exactly once:
                // skipped where the flag reads 1 (moved out on this path),
                // fired where it reads 0 (still owned). `None` for every
                // unflagged binding (the common case) — byte-identical to the
                // pre-#2418 drop.
                guard: guarded(binding, collection_drop_flags.get(binding).copied()),
            });
            continue;
        }
        // Plain `Vec<T>` local (BitCopy-scalar or `string` element — the
        // shapes neither the closure-pair nor the owned-element class above
        // claims). Pre-fix it fell through to the no-op `CowValue` arm and
        // LEAKED its backing buffer (and, for `Vec<string>`, every element)
        // on every exit path. Its scope-exit release is the plain
        // `hew_vec_free`: frees the buffer and the handle, with the runtime's
        // own `ElemKind::String` element walk for string vecs. Intercept
        // AFTER the closure-pair and owned-Vec arms so those specialised
        // releases are never displaced (the allow-set's default-deny element
        // filter `binding_ty_is_plain_vec` already excludes both shapes; the
        // ordering is belt-and-suspenders). Gated fail-closed on
        // `plain_vec_drop_allowed`: a handle that escapes (returned, moved
        // into an aggregate / actor state, consumed by a by-value call or
        // for-in `into_iter`) is excluded by the escape-scan + dataflow
        // consume filter, so it is never double-freed. A binding the prover
        // did not clear leaks (as before this fix).
        // LESSONS: cleanup-all-exits, raii-null-after-move,
        // boundary-fail-closed.
        if plain_vec_drop_allowed.contains(binding) && ty_is_vec(ty) {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: plain Vec binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::CowHeap {
                    release: crate::ownership::CowHeapRelease::VecPlain,
                },
                // #2418 — path-sensitive exactly-once gate for a
                // conditionally-moved handle; see the owned-Vec arm above.
                guard: guarded(binding, collection_drop_flags.get(binding).copied()),
            });
            continue;
        }
        // Local `HashMap<K, V>` / `HashSet<E>` handle. A collection is
        // `ValueClass::CowValue`, but `cow_value_leaf_drop_symbol` only handles
        // the leaf `string` case, so a local map/set would otherwise fall through
        // to the no-op `CowValue` arm and LEAK its layout-keyed backing storage
        // on every normal-return AND cancel/cooperate path (the bug this fix
        // fixes). Intercept BEFORE the value-class match (mirroring the owned-Vec
        // arm above) and emit the `DropKind::CowHeap` runtime release
        // (`hew_hashmap_free_layout` / `hew_hashset_free_layout`, selected by the
        // builtin discriminant in `drop_kind_for`) ONLY when the fail-closed
        // sole-owner derivation proved the handle still solely owns its storage
        // at scope exit (`local_collection_drop_allowed`). A handle moved into an
        // actor's initial state (`spawn A(f: m)`) or otherwise escaped is
        // excluded by the escape-scan, so the actor's synthesised `state_drop_fn`
        // remains the sole owner of that free — no double-drop. A binding the
        // prover did not clear leaks (as before this fix); it never double-frees.
        // The local guard here only confirms the binding's type is a collection
        // handle so the `*_free_layout` ABI is correct; route the kind through
        // `drop_kind_for` (the single source of truth the drop-plan validator
        // re-derives against) so the emitted kind cannot drift.
        // LESSONS: cleanup-all-exits, raii-null-after-move, boundary-fail-closed,
        // container-ingress-ownership-is-per-container.
        if local_collection_drop_allowed.contains(binding) && ty_is_local_collection_handle(ty) {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: local collection binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: drop_kind_for(place, ty, None),
                // #2418 — path-sensitive exactly-once gate for a
                // conditionally-moved handle; see the owned-Vec arm above.
                guard: guarded(binding, collection_drop_flags.get(binding).copied()),
            });
            continue;
        }
        // Local `bytes` binding. A bytes value is `ValueClass::CowValue`, but
        // `cow_value_leaf_drop_symbol` only handles the leaf `string` case, so
        // a local bytes triple would otherwise fall through to the no-op
        // CowValue arm and LEAK its refcounted data buffer on every exit path
        // (the sender-local leak the bytes ownership probe pins). Intercept
        // BEFORE the value-class match (mirroring the owned-Vec / collection
        // arms) and emit the `DropKind::CowHeap { "hew_bytes_drop" }` release
        // (lowered by codegen's BytesTriple-aware `emit_bytes_inplace_drop`)
        // ONLY when the fail-closed sole-owner derivation proved the binding
        // still solely owns its buffer at scope exit
        // (`local_bytes_drop_allowed`). A triple consumed by an actor send
        // (mailbox `memcpy` hand-off) or otherwise escaped is excluded by the
        // escape-scan + dataflow `Consumed` filter, so the receive side /
        // `state_drop_fn` remains the sole owner of that release — no
        // double-drop. A binding the prover did not clear leaks (as before
        // this fix); it never double-frees. The local guard here only confirms
        // the binding's type is `bytes` so the triple-field-0 ABI is correct;
        // route the kind through `drop_kind_for` (the single source of truth
        // the drop-plan validator re-derives against) so the emitted kind
        // cannot drift.
        // LESSONS: cleanup-all-exits, raii-null-after-move,
        // boundary-fail-closed.
        if local_bytes_drop_allowed.contains(binding) && matches!(ty, ResolvedTy::Bytes) {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: local bytes binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: drop_kind_for(place, ty, None),
                guard: guarded(binding, actor_message_cow_drop_flags.get(binding).copied()),
            });
            continue;
        }
        // W5.020 — heap-owning enum composite (`Result<T, string>` /
        // `Option<string>` / user enum with an owned-payload variant). A user
        // enum is `ValueClass::Unknown` and so would otherwise fall into the
        // no-drop arm below (leak). Intercept BEFORE the value-class match,
        // mirroring the dyn-trait and owned-string-record arms, and emit the
        // tag-aware `DropKind::EnumInPlace` drop ONLY when the fail-closed
        // sole-owner derivation proved this composite still owns its active
        // payload at scope exit (`enum_composite_drop_allowed`). A binding the
        // prover did not clear leaks (as before W5.020); it never double-frees.
        if enum_composite_drop_allowed.contains(binding)
            && ty_is_heap_owning_enum_composite(
                ty,
                record_field_orders,
                enum_layouts,
                type_classes.lifecycle_registry(),
            )
        {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: heap-owning enum composite binding {binding:?} \
                     is in owned_locals but missing from binding_locals; lowering must wire a \
                     Place before drop elaboration observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::EnumInPlace,
                guard: guarded(binding, overwrite_guard_flags.get(binding).copied()),
            });
            continue;
        }
        // Machine-typed local whose active state payload carries a release
        // obligation. A machine is `ValueClass::Unknown`, so without this arm
        // the binding falls through to the no-drop arm below and the
        // `#[resource]` handle held in the state the machine ends its scope in
        // is never closed — the same handle in a bare local closes correctly.
        // Machines are enums at the value-classification layer
        // (`machine_enum_views`), so the release is the same tag-aware
        // `DropKind::EnumInPlace` helper family a user enum uses: it walks only
        // the ACTIVE variant's owned fields and frees no wrapper. Gated on the
        // fail-closed `derive_machine_composite_drop_allowed` prover, which
        // admits only a binding whose every read is a step round-trip, a
        // state-name read, or a tag test — a machine whose payload was matched
        // out, returned, stored, or sent keeps the pre-existing leak posture and
        // is never double-closed. Intercept BEFORE the value-class match,
        // mirroring the enum-composite arm.
        if machine_composite_drop_allowed.contains(binding) {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: machine composite binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before drop elaboration observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::EnumInPlace,
                guard: None,
            });
            continue;
        }
        // `indirect enum` heap-node local (spec §3.7.4). An indirect enum is a
        // single heap `ptr` to a tagged-union node, so it is `ValueClass::
        // Unknown` and would otherwise fall into the no-drop arm below (leak the
        // heap node — and, for a recursive `Node(Tree, Tree)`, every child node
        // too). Intercept BEFORE the value-class match (mirroring the
        // enum-composite arm) and emit the recursive `DropKind::IndirectEnum`
        // free ONLY when the fail-closed sole-owner derivation proved this
        // binding constructed and still solely owns its node at scope exit
        // (`indirect_enum_drop_allowed`): a destructure/alias binder, a child
        // wired into a parent node, a returned node, and a consumed node are all
        // excluded so the node is freed by exactly one owner. A binding the
        // prover did not clear leaks (as before this kind); it never
        // double-frees.
        // LESSONS: drop-allowset-from-value-flow, cleanup-all-exits,
        // raii-null-after-move, boundary-fail-closed.
        if indirect_enum_drop_allowed.contains(binding) && ty_is_indirect_enum(ty, enum_layouts) {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: indirect-enum binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::IndirectEnum,
                guard: None,
            });
            continue;
        }
        // Owned-aggregate record by value (RC-4 / RC-6 / G12). The
        // `owned_record_drop_allowed` set is the fail-closed sole-owner gate:
        // it contains every owned record binding (legacy owned-string records
        // plus the unified owned-aggregate records admitted by the value-class
        // gate) that the dataflow proved is NOT consumed/maybe-consumed at any
        // exit, so the whole-record `DropKind::RecordInPlace` thunk
        // (`__hew_record_drop_inplace_<R>`, which recurses through every owned
        // field — string/bytes/Vec/HashMap/HashSet/nested record/enum) runs
        // exactly once on the owner and never on a moved-out record. A returned
        // record is excluded (the `ReturnSlot` owns it); a field-read-only
        // record stays in the set and is dropped here.
        // A field-bearing user `#[resource]` record must stay on this structural
        // arm: its `RecordInPlace` helper runs the user close first and then
        // recursively tears down every owned field.  The scalar
        // `AffineResource` arm below runs only the close ritual and would leak
        // heap fields and skip nested resource closes.
        //
        // `affine_release_flags` is populated by the single
        // `affine_release_needs_drop_flag` predicate for user closes. Attach
        // that flag to the WHOLE recursive helper so a conditional by-value
        // hand-off skips both the close and field teardown on the consumed
        // path, while the live path performs the complete resource-record
        // ritual exactly once.
        if owned_record_drop_allowed.contains(binding) && user_record_layout_key(ty).is_some() {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: owned record binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::RecordInPlace,
                // Resource-record flags take the same precedence as the
                // consume hook in `lower_value_for_move`; ordinary conditional
                // record flags cover the disjoint String/BitCopy record class.
                guard: guarded(
                    binding,
                    affine_release_flags
                        .get(binding)
                        .or_else(|| conditional_record_drop_flags.get(binding))
                        .copied(),
                ),
            });
            continue;
        }
        // W5.021 — heap-owning tuple by value (the tuple-of-owned-handles drop
        // spine). A tuple is `ValueClass::CowValue` but `cow_value_leaf_drop_
        // symbol` only handles the leaf `string` case, so an owned tuple would
        // otherwise fall through to the no-op CowValue arm and leak its members.
        // Intercept BEFORE the value-class match (mirroring the owned-Vec /
        // enum-composite / owned-record arms) and emit the per-element
        // `DropKind::TupleInPlace` drop ONLY when the fail-closed sole-owner
        // derivation proved this tuple still owns its members at scope exit
        // (`tuple_composite_drop_allowed`). The `__tuple_N` destructure temp
        // (elements moved out) and a returned tuple are both excluded, so the
        // helper frees each member exactly once. A binding the prover did not
        // clear leaks (as before); it never double-frees. Tuples whose fields
        // transitively hold `Option` / user-enum payloads are included by the
        // same structural authority and remain sound for the same reason: the
        // nested payloads are still dropped through the tuple's one member-drop
        // path, not by a separate ad hoc whitelist.
        if tuple_composite_drop_allowed.contains(binding)
            && ty_is_heap_owning_tuple(
                ty,
                record_field_orders,
                enum_layouts,
                type_classes.lifecycle_registry(),
            )
        {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: heap-owning tuple binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::TupleInPlace,
                guard: None,
            });
            continue;
        }
        // Escaping-closure pair (the closure env heap-lifetime contract). A
        // fn-typed binding is `ValueClass::PersistentShare` and would fall
        // into the no-drop arm below, leaking its heap env-box on every
        // scope exit. Intercept BEFORE the value-class match (mirroring the
        // dyn-trait arm) and emit the `DropKind::ClosurePair` drop ONLY for
        // bindings the fail-closed sole-owner derivation admitted
        // (`derive_closure_pair_drop_allowed`): the pair is heap-or-null by
        // construction at its producing site, and its env-box has exactly
        // this one owner left at scope exit. The drop protocol null-checks
        // the env pointer, so named-fn pairs and capture-free escaping
        // closures are no-ops. A binding the prover did not clear leaks (as
        // before this fix); it never double-frees.
        // LESSONS: cleanup-all-exits, raii-null-after-move,
        // boundary-fail-closed, ffi-ownership-contracts.
        // Checked-MIR owner construction is the admission proof here:
        // `register_owned_local` mints a closure-pair OwnerId only for a
        // HeapBox `MakeClosure` (or its exact transferred successor). Named
        // functions and stack/null pairs never enter this owner catalogue.
        // Per-exit OwnerId replay then follows Relocate/Transfer into the
        // actual carrier, so a function-global source-operand alias scan is
        // neither necessary nor authoritative.
        if ty_is_closure_pair(ty) {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: closure-pair binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::ClosurePair,
                guard: None,
            });
            continue;
        }
        // W3.031 Stage 1: `dyn Trait` owned locals (ValueClass::PersistentShare
        // by `ValueClass::of_ty`) carry their drop ritual on the vtable's
        // slot 0 (`drop_in_place`) plus a storage-discriminated release
        // ritual. Without this arm, the PersistentShare match below skips
        // them — the pre-Stage-1 gap that left every owned `dyn Trait`
        // binding with no scope-exit drop elaborated.
        //
        // Place the dyn-trait arm BEFORE the value-class match so it
        // intercepts regardless of `ValueClass::of_ty`'s classification.
        // The storage discriminator is sourced from the per-binding
        // side table populated by the introducing `let` statement; a
        // binding that reached `owned_locals` without a side-table
        // entry is a builder invariant violation (the `Let` arm that
        // adds to `owned_locals` is the same arm that populates
        // `dyn_trait_storage` for dyn-typed bindings) — fail-closed
        // with a panic so the gap surfaces at MIR construction time.
        if matches!(ty, ResolvedTy::TraitObject { .. }) {
            let place = *binding_locals.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: dyn Trait binding {binding:?} is in \
                     owned_locals but missing from binding_locals; lowering must wire a \
                     Place before the drop-elaboration pass observes the binding"
                )
            });
            let storage = *dyn_trait_storage.get(binding).unwrap_or_else(|| {
                panic!(
                    "build_lifo_drops invariant: dyn Trait binding {binding:?} is in \
                     owned_locals but missing from dyn_trait_storage; the introducing \
                     `HirStmtKind::Let` arm must populate the storage discriminator \
                     before pushing the binding into owned_locals (W3.031 Stage 1)"
                )
            });
            drops.push(ElabDrop {
                place,
                ty: ty.clone(),
                drop_fn: None,
                kind: DropKind::TraitObject { storage },
                guard: None,
            });
            continue;
        }
        match ValueClass::of_ty(ty, type_classes) {
            ValueClass::AffineResource => {
                let place = *binding_locals.get(binding).unwrap_or_else(|| {
                    panic!(
                        "build_lifo_drops invariant: binding {binding:?} is in owned_locals \
                         but missing from binding_locals; lowering must wire a Place before \
                         the drop-elaboration pass observes the binding"
                    )
                });
                // A match payload binder is an interior alias of its owning
                // enum shell unless the scrutinee itself was consumed and
                // neutralized. The shell's recursive drop releases Rc/Weak
                // payloads; a second affine release on the binder would
                // underflow the reference count on cleanup edges.
                if matches!(
                    ty,
                    ResolvedTy::Named {
                        builtin: Some(BuiltinType::Rc | BuiltinType::Weak),
                        ..
                    }
                ) && base_local(place)
                    .is_some_and(|local| projection_alias_tainted.contains(&local))
                {
                    continue;
                }
                // Registry-driven drop_fn dispatch. The HIR-lowering pass
                // populates `type_classes` with `(marker, Some(close_method))`
                // for every `#[resource]` type; reaching this arm without
                // a `close_method` is structurally unreachable because the
                // `E_RESOURCE_MISSING_CLOSE` HIR diagnostic short-circuits
                // the pipeline upstream. The string form is preserved as a
                // failsafe; codegen rejects `Some(_)` until runtime drop
                // dispatch lands (`hew-codegen-rs/src/llvm.rs:471`).
                let ty_derived_drop_fn = resource_drop_fn(ty, type_classes);
                // Resolve to the binding's real backend place. Falling
                // back to `ReturnSlot` for an unmapped binding would
                // drop the wrong slot — fail closed instead. The
                // `stmt` handler always populates `binding_locals` for
                // any binding that reaches `owned_locals` (see
                // `HirStmtKind::Let` arm), so this expect is a builder
                // invariant. A future surface that grows
                // `owned_locals` ahead of `binding_locals` must wire
                // a real `Place` before reaching here. LESSONS:
                // boundary-fail-closed.
                // Place-aware override of the type-derived drop_fn. A
                // `Place::LambdaActorHandle` carries a `Named{"Duplex"}` ty
                // (the surface type of an `actor |..|{..}` expression), but
                // its release ritual is `hew_lambda_actor_release`, not
                // `hew_duplex_close`. SendHalf/RecvHalf get the same
                // treatment — direction discriminant materialised at
                // call site from the Place variant.
                let drop_fn = place_aware_drop_fn(place, ty_derived_drop_fn);
                // Drop-kind classification for the M2 substrate. The
                // pre-M2 generic `@resource` path keeps `DropKind::Resource`;
                // M2 Duplex / lambda-actor / half-handle Places select
                // the specialised kinds so codegen (slice 5) and the
                // runtime (slice 4) emit the right close protocol. The
                // dyn-trait arm above intercepts `ResolvedTy::TraitObject`
                // before reaching here, so the dispatcher never observes
                // a dyn type from this AffineResource arm — pass `None`
                // for the storage hint.
                // LESSONS: cleanup-all-exits, raii-null-after-move.
                let kind = drop_kind_for(place, ty, None);
                // #1933 / #1941 — gate a non-idempotent user `#[resource]`
                // close on its path-sensitive runtime drop-flag so it fires
                // exactly once on a `MaybeConsumed` control-flow join (Live on
                // one predecessor, Consumed on another). The flag presence in
                // `affine_release_flags` is the authority: it is populated iff
                // `affine_release_needs_drop_flag` held at the binding's `let`, the
                // same predicate that decided to KEEP this binding in
                // `owned_locals` across its consume (no `mark_binding_moved`).
                // So a flagged binding is exactly one that survived to here and
                // must be guarded; an unflagged AffineResource (Duplex / lambda
                // / half / `Runtime`-descriptor close) is idempotent and drops
                // unguarded as before. `drops_for_exit` independently excludes
                // the drop entirely on an unconditionally-`Consumed` exit, so
                // the guard only does runtime work at a genuine join.
                let guard = guarded(binding, affine_release_flags.get(binding).copied());
                drops.push(ElabDrop {
                    place,
                    ty: ty.clone(),
                    drop_fn,
                    kind,
                    guard,
                });
            }
            // Linear, BitCopy, PersistentShare, View, Unknown: no implicit
            // drop. Linear is enforced by MustConsume; the rest have no drop
            // semantics by value-class definition.
            ValueClass::Linear
            | ValueClass::BitCopy
            | ValueClass::PersistentShare
            | ValueClass::View
            | ValueClass::Unknown => {}
            // CowValue — W5-011 P3. A heap-owning value-class local whose
            // single owner is this scope. This slice elaborates the
            // function-scope release for the leaf `string` case only (the
            // accumulating helper-local leak the lane targets). The drop is
            // gated fail-CLOSED on the sole-owner ALLOW-set: a binding is
            // dropped here ONLY IF it appears in `cow_drop_allowed`, which
            // `elaborate` populates by proving — against the finalised MIR
            // instruction + terminator stream — that the binding's pointer is
            // never aliased out (never read as a source operand) and is not a
            // projection alias of a still-live aggregate, then removing any
            // binding consumed/maybe-consumed on a path. A binding absent from
            // the allow-set leaks (as before this fix); it never double-frees.
            // The default for any binding the prover did not positively clear
            // is exclusion, so an un-enumerated future alias producer cannot
            // re-open the double-free. Aggregate/container `CowValue` drops
            // (Vec, HashMap, HashSet, composite tuples/records, Bytes) are
            // admitted by their separate structural authorities and
            // interception arms; they remain no-ops in THIS scalar-leaf path.
            // LESSONS: cleanup-all-exits, raii-null-after-move,
            // boundary-fail-closed.
            ValueClass::CowValue => {
                if cow_value_leaf_drop_symbol(ty).is_some() && cow_drop_allowed.contains(binding) {
                    if let Some(place) = binding_locals.get(binding) {
                        // `drop_kind_for` is the single source of truth for the
                        // Place+type → DropKind mapping (the drop-plan validator
                        // re-derives against it); route through it so the emitted
                        // kind cannot drift from the validator's expectation.
                        drops.push(ElabDrop {
                            place: *place,
                            ty: ty.clone(),
                            drop_fn: None,
                            kind: drop_kind_for(*place, ty, None),
                            // Projected-payload guards are generation-specific
                            // Checked-MIR events and are attached per exit by
                            // `enumerate_exits`. Actor-message flags predate
                            // that stream but are still qualified with the
                            // current generation before the plan is sealed.
                            guard: guarded(
                                binding,
                                actor_message_cow_drop_flags.get(binding).copied(),
                            ),
                        });
                    }
                }
            }
        }
    }
    drops
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
/// scope-exit drop is live, with its own dedicated admission authority
/// (`derive_local_bytes_drop_allowed`) and its own `build_lifo_drops`
/// interception arm — keeping it out of this table keeps exactly ONE prover
/// in charge of bytes admission. Adding a `Bytes` arm here would make bytes
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
        //     admission authority (`derive_local_bytes_drop_allowed`) and
        //     `build_lifo_drops` arm — kept out so exactly ONE prover owns bytes
        //     admission (a second union-admitting authority would risk a
        //     double-free; LESSONS: boundary-fail-closed).
        //   - `Named` containers/handles (`Vec`/`HashMap`/`HashSet`/`Generator`/
        //     records/enums): NOT scalar leaves — their drop is the
        //     `binding_ty_is_*_vec` / `derive_local_collection_drop_allowed`
        //     release buckets, NOT this per-leaf symbol picker.
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
    use hew_types::runtime_call::RuntimeDropDescriptor;
    let ResolvedTy::Named {
        builtin: Some(builtin),
        ..
    } = ty
    else {
        return None;
    };
    match builtin {
        hew_types::BuiltinType::Stream => Some(RuntimeDropDescriptor::StreamClose),
        hew_types::BuiltinType::Receiver => Some(RuntimeDropDescriptor::ReceiverClose),
        _ => None,
    }
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
///     scope-exit drop (`derive_local_collection_drop_allowed` exempts the
///     cursor ingress), so the cursor BORROWS and must NOT drop. Returns false.
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
/// `build_lifo_drops` are free fns): `ty_is_closure_pair_vec(Vec<E>)` ≡
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
/// is the correct release for the binding's single owned handle. The
/// sole-owner / no-escape decision is the separate fail-closed authority
/// `derive_local_collection_drop_allowed`.
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
/// ownership escape, so `derive_local_collection_drop_allowed` skips arg[0] when
/// scanning these calls (it still scans arg[1..], which carry by-value keys /
/// elements that genuinely flow elsewhere).
///
/// This is an EXPLICIT allow-list, deliberately NOT a `hew_hashmap_` /
/// `hew_hashset_` prefix test (LESSONS: `boundary-fail-closed`). The consuming
/// release `hew_hashmap_free_layout` / `hew_hashset_free_layout` and the
/// constructors `*_new_with_layout` (which write a fresh handle, never read an
/// existing one as arg[0]) are intentionally absent: a future runtime op that
/// consumes its receiver must be classified here deliberately. An op left out of
/// this list is treated as a receiver ESCAPE, which over-excludes the binding
/// from its scope-exit drop — a leak, never a double-free. Every entry below is
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
/// Without this, `match mk() { Some(s) => println(f"v={s}") }` classified `s` as
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
/// Build the elaborated block list + per-`ExitPath` drop plans for a
/// function's CFG. Every basic block becomes one `ElabBlock` of
/// `BlockKind::Normal`; `Terminator::Panic` synthesises a sibling
/// `BlockKind::Cleanup` block. Every runtime-reachable terminator maps to one
/// `(ExitPath, DropPlan)` entry. `Terminator::Unreachable` is instead a
/// compiler-proven semantic endpoint: it deliberately has no `ExitPath`,
/// cleanup block, or drop plan. `Return`-terminated blocks narrow
/// the function-wide LIFO `lifo` sequence to bindings whose state at
/// that block's exit is `Live` — bindings already `Consumed` on
/// every reaching path do not need their drop fired again
/// (LESSONS `raii-null-after-move`). `MaybeConsumed` at a Return
/// exit is rejected upstream by the move-checker; the elaborator
/// treats it as if `Live` for drop-plan purposes, but the program
/// would have already been rejected before reaching codegen so the
/// drop list is informational.
pub(super) type ExactOwnerState = HashMap<crate::model::OwnerId, Place>;
pub(super) type MaybeOwnerState = HashSet<(crate::model::OwnerId, Place)>;
pub(super) type MustBindingOwnerState = HashMap<BindingId, Place>;

#[derive(Clone)]
enum OwnerStateOperation {
    Mint {
        owner: crate::model::OwnerId,
        place: Place,
    },
    Transfer {
        owner: crate::model::OwnerId,
        successor: Option<(crate::model::OwnerId, Place)>,
    },
    RelocateOwner {
        owner: crate::model::OwnerId,
        to: Place,
    },
    End {
        owner: crate::model::OwnerId,
    },
    Reset {
        previous: crate::model::OwnerId,
        replacement: crate::model::OwnerId,
        place: Place,
    },
    Rearm {
        previous: crate::model::OwnerId,
        replacement: crate::model::OwnerId,
        place: Place,
    },
    Join {
        incoming: Vec<crate::model::OwnerId>,
        replacement: crate::model::OwnerId,
        place: Place,
    },
    None,
}

#[allow(
    clippy::match_same_arms,
    reason = "explicit physical-copy and unclassified-operation arms document distinct ownership semantics"
)]
fn owner_state_operation(instruction: &Instr) -> OwnerStateOperation {
    use crate::model::OwnershipEvent;

    match instruction {
        // Physical copies are backend mechanics. They never mutate ownership
        // state implicitly: lowering must publish an exact `Relocate` or
        // `Transfer` event at the same program point. This distinction also
        // permits borrowed ABI copies without accidentally moving the owner.
        Instr::Move { .. } | Instr::WitnessMove { .. } => OwnerStateOperation::None,
        Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, .. }) => {
            OwnerStateOperation::Mint {
                owner: *owner,
                place: *place,
            }
        }
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner,
            to,
            to_owner,
            ..
        }) => OwnerStateOperation::Transfer {
            owner: *owner,
            successor: to_owner.zip(*to),
        },
        Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, to, .. }) => {
            OwnerStateOperation::RelocateOwner {
                owner: *owner,
                to: *to,
            }
        }
        Instr::OwnershipEvent(
            OwnershipEvent::Release { owner, .. }
            | OwnershipEvent::GuardedRelease { owner, .. }
            | OwnershipEvent::DemoteToAlias { owner, .. },
        ) => OwnerStateOperation::End { owner: *owner },
        Instr::OwnershipEvent(OwnershipEvent::Reset {
            previous,
            replacement,
            place,
            ..
        }) => OwnerStateOperation::Reset {
            previous: *previous,
            replacement: *replacement,
            place: *place,
        },
        Instr::OwnershipEvent(OwnershipEvent::Rearm {
            previous,
            replacement,
            place,
            ..
        }) => OwnerStateOperation::Rearm {
            previous: *previous,
            replacement: *replacement,
            place: *place,
        },
        Instr::OwnershipEvent(OwnershipEvent::Join {
            incoming,
            replacement,
            place,
            ..
        }) => OwnerStateOperation::Join {
            incoming: incoming.clone(),
            replacement: *replacement,
            place: *place,
        },
        _ => OwnerStateOperation::None,
    }
}

pub(super) fn apply_exact_owner_ops(instructions: &[Instr], live: &mut ExactOwnerState) {
    for instruction in instructions {
        match owner_state_operation(instruction) {
            OwnerStateOperation::Mint { owner, place } => {
                live.insert(owner, place);
            }
            OwnerStateOperation::Transfer { owner, successor } => {
                live.remove(&owner);
                if let Some((next, destination)) = successor {
                    live.insert(next, destination);
                }
            }
            OwnerStateOperation::RelocateOwner { owner, to } => {
                if let Some(place) = live.get_mut(&owner) {
                    *place = to;
                }
            }
            OwnerStateOperation::End { owner } => {
                live.remove(&owner);
            }
            OwnerStateOperation::Reset {
                previous,
                replacement,
                place,
            }
            | OwnerStateOperation::Rearm {
                previous,
                replacement,
                place,
            } => {
                live.remove(&previous);
                live.insert(replacement, place);
            }
            OwnerStateOperation::Join {
                incoming: _,
                replacement,
                place,
            } => {
                live.retain(|owner, _| owner.binding != replacement.binding);
                live.insert(replacement, place);
            }
            OwnerStateOperation::None => {}
        }
    }
}

pub(super) fn apply_maybe_owner_ops(instructions: &[Instr], live: &mut MaybeOwnerState) {
    for instruction in instructions {
        match owner_state_operation(instruction) {
            OwnerStateOperation::Mint { owner, place } => {
                live.insert((owner, place));
            }
            OwnerStateOperation::Transfer { owner, successor } => {
                live.retain(|(candidate, _)| *candidate != owner);
                if let Some(next) = successor {
                    live.insert(next);
                }
            }
            OwnerStateOperation::RelocateOwner { owner, to } => {
                let was_live = live.iter().any(|(candidate, _)| *candidate == owner);
                live.retain(|(candidate, _)| *candidate != owner);
                if was_live {
                    live.insert((owner, to));
                }
            }
            OwnerStateOperation::End { owner } => {
                live.retain(|(candidate, _)| *candidate != owner);
            }
            OwnerStateOperation::Reset {
                previous,
                replacement,
                place,
            } => {
                live.retain(|(candidate, _)| *candidate != previous);
                live.insert((replacement, place));
            }
            OwnerStateOperation::Rearm {
                previous,
                replacement,
                place,
            } => {
                live.retain(|(candidate, _)| *candidate != previous);
                live.insert((replacement, place));
            }
            OwnerStateOperation::Join {
                incoming: _,
                replacement,
                place,
            } => {
                live.retain(|(owner, _)| owner.binding != replacement.binding);
                live.insert((replacement, place));
            }
            OwnerStateOperation::None => {}
        }
    }
}

pub(super) fn exact_owner_states(
    blocks: &[BasicBlock],
) -> (HashMap<u32, ExactOwnerState>, HashMap<u32, ExactOwnerState>) {
    let mut entries = HashMap::from([(ENTRY_BLOCK_ID, ExactOwnerState::new())]);
    let mut exits = HashMap::new();
    let mut queue = std::collections::VecDeque::from([ENTRY_BLOCK_ID]);
    while let Some(block_id) = queue.pop_front() {
        let Some(block) = blocks.iter().find(|block| block.id == block_id) else {
            continue;
        };
        let mut outgoing = entries.get(&block_id).cloned().unwrap_or_default();
        apply_exact_owner_ops(&block.instructions, &mut outgoing);
        exits.insert(block_id, outgoing.clone());
        for successor in block.successors() {
            let changed = if let Some(existing) = entries.get_mut(&successor) {
                let joined: ExactOwnerState = existing
                    .iter()
                    .filter_map(|(owner, place)| {
                        (outgoing.get(owner) == Some(place)).then_some((*owner, *place))
                    })
                    .collect();
                if *existing == joined {
                    false
                } else {
                    *existing = joined;
                    true
                }
            } else {
                entries.insert(successor, outgoing.clone());
                true
            };
            if changed {
                queue.push_back(successor);
            }
        }
    }
    (entries, exits)
}

pub(super) fn maybe_owner_states(
    blocks: &[BasicBlock],
) -> (HashMap<u32, MaybeOwnerState>, HashMap<u32, MaybeOwnerState>) {
    let mut entries = HashMap::from([(ENTRY_BLOCK_ID, MaybeOwnerState::new())]);
    let mut exits = HashMap::new();
    let mut queue = std::collections::VecDeque::from([ENTRY_BLOCK_ID]);
    while let Some(block_id) = queue.pop_front() {
        let Some(block) = blocks.iter().find(|block| block.id == block_id) else {
            continue;
        };
        let mut outgoing = entries.get(&block_id).cloned().unwrap_or_default();
        apply_maybe_owner_ops(&block.instructions, &mut outgoing);
        exits.insert(block_id, outgoing.clone());
        for successor in block.successors() {
            let changed = if let Some(existing) = entries.get_mut(&successor) {
                let before = existing.len();
                existing.extend(outgoing.iter().copied());
                existing.len() != before
            } else {
                entries.insert(successor, outgoing.clone());
                true
            };
            if changed {
                queue.push_back(successor);
            }
        }
    }
    (entries, exits)
}

fn apply_must_binding_owner_ops(instructions: &[Instr], live: &mut MustBindingOwnerState) {
    for instruction in instructions {
        match owner_state_operation(instruction) {
            OwnerStateOperation::Mint { owner, place } => {
                live.insert(owner.binding, place);
            }
            OwnerStateOperation::Transfer { owner, successor } => {
                live.remove(&owner.binding);
                if let Some((next, destination)) = successor {
                    live.insert(next.binding, destination);
                }
            }
            OwnerStateOperation::RelocateOwner { owner, to } => {
                if let Some(place) = live.get_mut(&owner.binding) {
                    *place = to;
                }
            }
            OwnerStateOperation::End { owner } => {
                live.remove(&owner.binding);
            }
            OwnerStateOperation::Reset {
                previous,
                replacement,
                place,
            }
            | OwnerStateOperation::Rearm {
                previous,
                replacement,
                place,
            } => {
                live.remove(&previous.binding);
                live.insert(replacement.binding, place);
            }
            OwnerStateOperation::Join {
                incoming,
                replacement,
                place,
            } => {
                for owner in incoming {
                    live.remove(&owner.binding);
                }
                live.insert(replacement.binding, place);
            }
            OwnerStateOperation::None => {}
        }
    }
}

/// Generation-erased must-own state used only to justify an ownership-SSA
/// Join. Unlike exact `OwnerId` intersection, this lattice preserves a binding
/// when every predecessor owns it in the same physical place even if their
/// generations differ. It is derived solely from explicit ownership events.
pub(super) fn must_binding_owner_states(
    blocks: &[BasicBlock],
) -> (
    HashMap<u32, MustBindingOwnerState>,
    HashMap<u32, MustBindingOwnerState>,
) {
    let mut entries = HashMap::from([(ENTRY_BLOCK_ID, MustBindingOwnerState::new())]);
    let mut exits = HashMap::new();
    let mut queue = std::collections::VecDeque::from([ENTRY_BLOCK_ID]);
    while let Some(block_id) = queue.pop_front() {
        let Some(block) = blocks.iter().find(|block| block.id == block_id) else {
            continue;
        };
        let mut outgoing = entries.get(&block_id).cloned().unwrap_or_default();
        apply_must_binding_owner_ops(&block.instructions, &mut outgoing);
        exits.insert(block_id, outgoing.clone());
        for successor in block.successors() {
            let changed = if let Some(existing) = entries.get_mut(&successor) {
                let joined = existing
                    .iter()
                    .filter_map(|(binding, place)| {
                        (outgoing.get(binding) == Some(place)).then_some((*binding, *place))
                    })
                    .collect::<MustBindingOwnerState>();
                if *existing == joined {
                    false
                } else {
                    *existing = joined;
                    true
                }
            } else {
                entries.insert(successor, outgoing.clone());
                true
            };
            if changed {
                queue.push_back(successor);
            }
        }
    }
    (entries, exits)
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
            apply_maybe_owner_ops(&block.instructions[..position], &mut state);
            return state;
        }
    }
    exits.get(&block_id).cloned().unwrap_or_default()
}

/// Exact generations that must be destroyed when one exit is taken.
///
/// This is the single continuation rule shared by elaboration and final
/// validation. It consumes only Checked-MIR ownership operations: Goto uses
/// source-side `EdgeCarry`, ordinary normal successors use their exact entry
/// state, and terminal/unwind/abandon exits retain every live non-return owner.
fn exact_required_owners_for_exit(
    exit: &ExitPath,
    blocks: &[BasicBlock],
    entries: &HashMap<u32, ExactOwnerState>,
    exits: &HashMap<u32, ExactOwnerState>,
) -> ExactOwnerState {
    let block_id = exit_block_id(exit);
    let live = exact_owner_state_for_exit(exit, blocks, entries, exits);
    live.into_iter()
        .filter(|(owner, place)| {
            if matches!(place, Place::ReturnSlot) {
                return false;
            }
            let continues = match exit {
                ExitPath::Goto { target, .. } => blocks
                    .iter()
                    .find(|block| block.id == block_id)
                    .is_some_and(|block| {
                        block.instructions.iter().any(|instruction| {
                            matches!(
                                instruction,
                                Instr::OwnershipEvent(crate::model::OwnershipEvent::EdgeCarry {
                                    owner: carried,
                                    place: carried_place,
                                    target: carried_target,
                                }) if carried == owner
                                    && carried_place == place
                                    && carried_target == target
                            )
                        })
                    }),
                ExitPath::Call { next, .. }
                | ExitPath::Send { next, .. }
                | ExitPath::Ask { next, .. }
                | ExitPath::Select { next, .. }
                | ExitPath::Join { next, .. } => entries
                    .get(next)
                    .is_some_and(|state| state.get(owner) == Some(place)),
                ExitPath::Branch { .. } => true,
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

fn guarded_owner_groups_for_exit(
    exit: &ExitPath,
    blocks: &[BasicBlock],
    maybe_entries: &HashMap<u32, MaybeOwnerState>,
    maybe_exits: &HashMap<u32, MaybeOwnerState>,
    guarded_owners: &HashSet<crate::model::OwnerId>,
) -> GuardedOwnerGroups {
    if !matches!(
        exit,
        ExitPath::Return { .. }
            | ExitPath::Panic { .. }
            | ExitPath::Unwind { .. }
            | ExitPath::Cancel { .. }
            | ExitPath::Yield { .. }
            | ExitPath::Suspend { .. }
    ) {
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

#[allow(
    clippy::too_many_lines,
    reason = "enumerate_exits is a flat match over Terminator variants \
              with per-arm payload construction; the line count is the \
              variant count, not deep nesting"
)]
#[allow(
    clippy::too_many_arguments,
    reason = "each argument is a distinct producer-supplied input the per-exit \
              drop planner reads independently (the LIFO template, the exit- and \
              entry-state dataflow maps, the binding→Place / binding→scope tables, \
              the cancellation + loop-back-edge block sets); bundling them into a \
              struct would add indirection without clarifying the data flow"
)]
pub(super) fn enumerate_exits(
    blocks: &[BasicBlock],
    lifo: &[ElabDrop],
    exit_states: &std::collections::HashMap<
        u32,
        std::collections::BTreeMap<hew_hir::BindingId, dataflow::BindingState>,
    >,
    entry_states: &std::collections::HashMap<
        u32,
        std::collections::BTreeMap<hew_hir::BindingId, dataflow::BindingState>,
    >,
    binding_locals: &HashMap<BindingId, Place>,
    cancellation_blocks: &HashSet<u32>,
    projection_alias_tainted: &HashSet<u32>,
    owner_guards: &HashMap<crate::model::OwnerId, Place>,
) -> (Vec<ElabBlock>, Vec<(ExitPath, DropPlan)>) {
    // Track the highest block id observed so cleanup-block ids can
    // start past it. Slice 2 onwards may emit multiple non-trivial
    // blocks; reserving cleanup ids past the max keeps invariants from
    // the single-block era intact.
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
    let mut plans: Vec<(ExitPath, DropPlan)> = Vec::new();
    let drops_template = lifo.to_vec();
    let (owner_entry_states, owner_exit_states) = exact_owner_states(blocks);
    let empty_owner_state = ExactOwnerState::new();

    // Map each owned-local's Place back to its BindingId so the
    // per-exit filter can consult exit_states. The drops in `lifo`
    // already carry the binding's Place but not its id; reverse the
    // builder's `binding_locals` (BindingId -> Place) is the cleanest
    // bridge. Builds only as large as there are owned bindings.
    //
    // Adoption can point two lowering identities at one Place. Resolve that
    // only from the explicit old-generation → new-owner event emitted at the
    // adoption site. There is deliberately no BindingId ordering fallback:
    // identity order is not ownership authority.
    let adopted_owner_by_place: HashMap<Place, BindingId> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                from,
                to: Some(to),
                to_owner: Some(owner),
                ..
            }) if from == to => Some((*to, owner.binding)),
            _ => None,
        })
        .collect();
    let mut place_to_binding: std::collections::HashMap<Place, BindingId> =
        std::collections::HashMap::with_capacity(binding_locals.len());
    for (&binding, &place) in binding_locals {
        match place_to_binding.get(&place).copied() {
            None => {
                place_to_binding.insert(place, binding);
            }
            Some(existing) if existing == binding => {}
            Some(_) => {
                if let Some(adopted) = adopted_owner_by_place.get(&place).copied() {
                    place_to_binding.insert(place, adopted);
                } else {
                    // Ambiguous ownership is rejected by obligation balance;
                    // never pick an owner by hash iteration or numeric id.
                    place_to_binding.remove(&place);
                }
            }
        }
    }

    // Payload-alias → carrier composite map: `Ok(w)` / `Some(s)` binders whose
    // heap ownership was NOT transferred out by a `NeutralizePayloadSlot` are
    // non-owning interior aliases of the composite they were destructured from.
    // Reuses the exact authority the obligation checker folds discharges
    // through (`collect_payload_alias_map`), so the elaboration's exclusion and
    // the checker's balance accounting agree on which binders alias which
    // carrier.
    let payload_alias_carrier = collect_payload_alias_map(blocks);
    if std::env::var_os("HEW_DEBUG_DROP_ENUM").is_some() {
        eprintln!(
            "HEW_DEBUG_DROP_ENUM lifo={drops_template:#?} places={place_to_binding:#?} aliases={payload_alias_carrier:#?} owner_exits={owner_exit_states:#?}"
        );
    }

    // Carrier locals that are non-owning borrowed views of persistent actor
    // state (`ActorStateFieldLoad { mode: Borrowed }` — a bare byte-copy alias;
    // codegen retains nothing). Such a carrier never held an in-function drop
    // obligation, so "its drop is absent from this edge" carries no signal of
    // withheld admission: the actor FIELD owns the composite and frees its
    // payload recursively for the actor's whole lifetime. A payload binder
    // destructured out of one (`match self.field { Ok(r) => ... }`) must stay
    // alias-suppressed on every exit — granting it sole close authority would
    // release a handle the actor still owns (a `MonitorRef` scope-close here
    // demonitors immediately after arming).
    let borrowed_view_carrier_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::ActorStateFieldLoad {
                dest,
                mode: crate::model::ActorStateLoadMode::Borrowed,
                ..
            } => base_local(*dest),
            _ => None,
        })
        .collect();

    // Narrow the type/layout drop catalogue solely with the exact OwnerId
    // generations live at this program point.  HIR binding state is useful for
    // move diagnostics, but it is not cleanup authority: one binding may have
    // several sequential generations and a later successful transfer must not
    // erase the same binding's earlier unwind interval.
    let filter_drops_by_state = |_state_map: &std::collections::BTreeMap<
        hew_hir::BindingId,
        dataflow::BindingState,
    >,
                                 owner_state: &ExactOwnerState|
     -> Vec<ElabDrop> {
        let mut live: Vec<ElabDrop> = drops_template
            .iter()
            .filter(|drop| match place_to_binding.get(&drop.place) {
                Some(binding) => owner_state.keys().any(|owner| owner.binding == *binding),
                // No binding mapping → conservatively DROP the drop, not
                // keep it. This crate's posture is leak-not-double-free: an
                // unrecognised place with no dataflow-tracked owning binding
                // must not be assumed live, since firing an unproven drop
                // risks a double-free. `build_lifo_drops` panics on a
                // missing entry today, so this arm is unreachable — the
                // leak-safe polarity only guards future surfaces that build
                // drops outside the binding_locals registry.
                None => false,
            })
            .cloned()
            .collect();
        // Bind every guarded cleanup to the exact owner generation live at
        // this exit. A projected-payload guard exists only in the immutable
        // Checked-MIR event stream; legacy physical flag producers contribute
        // only the flag place already present on the template. In both cases
        // the exit-local OwnerId replaces any function-global generation.
        for drop in &mut live {
            let Some(binding) = place_to_binding.get(&drop.place).copied() else {
                continue;
            };
            let exact_owners = owner_state
                .iter()
                .filter_map(|(owner, place)| (owner.binding == binding).then_some((*owner, *place)))
                .collect::<Vec<_>>();
            let [(owner, current_place)] = exact_owners.as_slice() else {
                continue;
            };
            // Whole-place MIR moves relocate the same generation. Cleanup
            // follows that explicit program-point state instead of the
            // lexical slot captured by the pre-sealing LIFO template.
            drop.place = *current_place;
            if let Some(flag) = owner_guards.get(owner).copied() {
                drop.guard = Some(crate::model::ElabDropGuard {
                    owner: *owner,
                    flag,
                });
            } else if let Some(guard) = &mut drop.guard {
                guard.owner = *owner;
            }
        }
        // A projection-alias payload binder (`Ok(w)` destructured out of a
        // call-scrutinee `Result<Resource, _>`) is freed by its OWNING
        // composite's recursive `EnumInPlace` scope-exit drop. When the sibling
        // arm diverges (`Err(_) => panic()`/`return`) the `Return` block's only
        // reaching predecessor is the move-out arm, so the binder is still
        // `Live` here and its independent drop lands alongside the composite's —
        // a double-free of the shared payload. Exclude the binder's drop ONLY
        // when its carrier composite's drop is co-present at this same exit: the
        // composite covers the payload, so exactly one release fires. If the
        // carrier is absent (consumed / moved out / not admitted for its own
        // scope-exit drop), the binder is the live sole owner and keeps its
        // drop — leak-not-double-free. This is the `Return`/`Panic`/`Cancel`
        // sibling of the DI-020 exclusion `drops_for_scope_close_goto` already
        // applies on scope-close `Goto` edges. The alias set exempts neutralized
        // ownership transfers (`Ok(x) => x`) and fresh recv/generator/vec-iter
        // payloads, which remain independently dropped.
        let carrier_locals_present: HashSet<u32> = live
            .iter()
            .filter_map(|drop| base_local(drop.place))
            .collect();
        live.into_iter()
            .filter(|drop| {
                let Some(l) = base_local(drop.place) else {
                    return true;
                };
                if !projection_alias_tainted.contains(&l) {
                    return true;
                }
                match payload_alias_carrier.get(&l) {
                    Some(carrier) => {
                        !carrier_locals_present.contains(carrier)
                            && !borrowed_view_carrier_locals.contains(carrier)
                    }
                    None => true,
                }
            })
            .collect()
    };

    let drops_for_exit = |block_id: u32| -> Vec<ElabDrop> {
        let Some(state_map) = exit_states.get(&block_id) else {
            // No dataflow result for this block (defensive — every
            // reachable block has an exit_state entry after
            // analyze). Fall back to the function-wide LIFO.
            return drops_template.clone();
        };
        filter_drops_by_state(
            state_map,
            owner_exit_states
                .get(&block_id)
                .unwrap_or(&empty_owner_state),
        )
    };

    // An instruction-level may-unwind edge observes ownership immediately
    // before that instruction. Replaying only the immutable operation prefix
    // keeps a temporary owner live on the exceptional edge even when its
    // normal-path Release follows later in the same block.
    let drops_before_instruction = |block: &BasicBlock, instruction_index: usize| {
        let Some(state_map) = exit_states.get(&block.id) else {
            return drops_template.clone();
        };
        let mut owners = owner_entry_states
            .get(&block.id)
            .cloned()
            .unwrap_or_default();
        apply_exact_owner_ops(&block.instructions[..instruction_index], &mut owners);
        filter_drops_by_state(state_map, &owners)
    };

    // Drop set for a `CooperateKind::FunctionEntry` cancel exit. The cancel
    // branch leaves the function prologue BEFORE the entry block's own `Bind`
    // statements run, so the live set is the block's ENTRY (in-) state, not
    // its exit state. Using the exit state would over-include locals that the
    // entry block constructs after the cooperate site — for a struct-shaped
    // resource (`MonitorRef`) that means demonitoring an uninitialised stack
    // slot (fail-open: a garbage `ref_id` could cancel an unrelated monitor).
    // For a no-parameter function the entry state is empty, so the cancel exit
    // drops nothing — matching the established baseline for every other
    // resource. A by-value resource PARAMETER is `Live` at entry and is
    // correctly retained. LESSONS: cleanup-all-exits (P0), raii-null-after-move.
    let drops_for_entry_cancel = |block_id: u32| -> Vec<ElabDrop> {
        let Some(state_map) = entry_states.get(&block_id) else {
            return drops_template.clone();
        };
        filter_drops_by_state(
            state_map,
            owner_entry_states
                .get(&block_id)
                .unwrap_or(&empty_owner_state),
        )
    };

    // Per-iteration drops for a loop-body back-edge `Goto`. Restricts
    // `drops_for_exit` to bindings declared in this loop body's scope so the
    // back-edge releases ONLY per-iteration bindings (`let opt = await
    // Scope-close drops on every `Goto`, including loop backedges. A binding bound on
    // ONLY ONE arm of a `match`/`if` is `Live` at the arm's closing `Goto` but
    // goes OUT OF SCOPE crossing the join: at the join target's entry the dataflow
    // meets this arm's `Live` with the sibling arm's `Uninit`, yielding `Uninit`
    // (absent). The function-exit `Return`/`Trap` pass therefore never fires its
    // drop — the binding leaks on the normal path (it WAS correctly released on
    // the `cancel` cleanup path, which uses the source-block exit-state). This
    // closes that gap: a binding `Live` at the `Goto` source-exit but NOT `Live`
    // at the target-entry is released here, exactly where it was last provably the
    // live sole owner.
    //
    // Double-free safety (fail-closed): a binding still `Live` /
    // `MaybeConsumed` / `AliasedIntoAggregate` at the target-entry is NOT dropped
    // here — it stays in scope past the join, so its eventual `Return`/`Trap` exit
    // owns the single release. A binding `Consumed` / `Uninit` at the source-exit
    // is already excluded by `drops_for_exit`'s state filter (never freeing a
    // moved-out or uninitialised slot). So the release fires on exactly the path
    // where the binding is the live sole owner and on no other.
    // Projection-alias taint: a binding that is a non-owning interior alias of a
    // composite (a match/if-let payload binder destructured out of an enum
    // scrutinee — `Ok(inner)` / `Some(s)` — or a `*FieldLoad` interior pointer).
    // Such a binding NEVER solely owns its value; the OWNING composite frees it
    // through its recursive `EnumInPlace` / `RecordInPlace` / `TupleInPlace` drop.
    // A scope-close `Goto` must NOT fire a drop for one: when `Ok(inner)` is bound
    // and the inner `Option<string>` (`inner`) leaves scope crossing the join, the
    // outer `Result` composite is STILL live past the join and frees `inner`
    // recursively at the eventual `Return`. Dropping `inner` on the goto here AND
    // letting the composite free it at the return double-frees the payload string
    // (DI-020).
    //
    // The suppression stays UNCONDITIONAL here, unlike the terminal-exit filter
    // above. Carrier absence from a single edge's live drop set is not evidence
    // that carrier admission was withheld: the obligation checker folds every
    // alias discharge back onto the carrier mint, so a sibling binder of the same
    // carrier discharging at a LATER exit on the same path is invisible to this
    // edge. Reading absence as withheld admission double-freed
    // `std$net$connect_timeout`, whose `Ok`/`Err` binders both alias one
    // `__hew_call_scrutinee` mint. Conditional payload close authority is restored
    // by keeping the owned binder's Place reachable through
    // `deferred_drop_binding_locals` (see `lower_function`), not by relaxing this
    // filter.
    //
    // The single function-wide taint set was computed with the REAL `locals`
    // table (not an empty slice), so the `string`
    // `RecordFieldLoad`/`TupleFieldLoad` exemption stays active: a
    // `let name = r.name` reads a fresh `+1`-retained string owner that the
    // composite does NOT recursively free, so it must drop on the scope-close
    // edge. Tainting it (empty `locals` disables the exemption) would strand its
    // release at a join → `Uninit` → no `Return` recovery → leak. Enum/tuple
    // payload destructures stay tainted via the `Move`-from-interior arm
    // regardless of `locals`, so the double-free fix is unaffected. The exact
    // consumed-project binder set exempts only field-load destinations whose
    // parent composite is consume-marked on the selected arm; those destinations
    // are sole owners and must close on this edge before the join loses them.
    // Physical owners committed to `ReturnSlot` by this exact return block.
    // The binding-state analysis operates on HIR bindings and can leave the
    // original binding `Live` when expression lowering inserts anonymous move
    // carriers.  Follow the block-local whole-place move chain instead: every
    // source reaching `ReturnSlot` has transferred on this edge and must not
    // also run its cleanup drop.  Keeping this per block preserves owners on a
    // sibling early-return path that returns a different value.
    let return_transfer_places_for_block = |block: &BasicBlock| -> HashSet<Place> {
        let mut transferred = HashSet::from([Place::ReturnSlot]);
        loop {
            let mut changed = false;
            for instruction in &block.instructions {
                let (dest, src) = match instruction {
                    Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. } => {
                        (*dest, *src)
                    }
                    _ => continue,
                };
                if transferred.contains(&dest) {
                    changed |= transferred.insert(src);
                }
            }
            if !changed {
                break;
            }
        }
        transferred
    };

    for block in blocks {
        let block_id = block.id;
        let plan = match &block.terminator {
            Terminator::Unreachable => {
                // A semantic unreachable is not a language-visible exit. In
                // particular, do not reinterpret it as `Panic`/`Trap`, and do
                // not run ownership cleanup for a path the compiler has proved
                // impossible. The normal ElabBlock was still constructed above
                // so this stage preserves the Raw-MIR CFG identity explicitly.
                continue;
            }
            Terminator::Return => {
                let transferred = return_transfer_places_for_block(block);
                (
                    ExitPath::Return { block: block_id },
                    DropPlan {
                        drops: drops_for_exit(block_id)
                            .into_iter()
                            .filter(|drop| !transferred.contains(&drop.place))
                            .collect(),
                    },
                )
            }
            Terminator::Goto { target } => {
                // Lexical releases are executable Drop+Release operations at a
                // first-class ScopeExit program point. Every generation that
                // survives this source edge is named by EdgeCarry. There is no
                // target-block ownership rediscovery here.
                (
                    ExitPath::Goto {
                        block: block_id,
                        target: *target,
                    },
                    DropPlan::default(),
                )
            }
            Terminator::Branch {
                cond: _,
                then_target,
                else_target,
            } => (
                ExitPath::Branch {
                    block: block_id,
                    then_target: *then_target,
                    else_target: *else_target,
                },
                DropPlan::default(),
            ),
            Terminator::Call {
                callee,
                args: _,
                dest: _,
                next,
                ..
            } => (
                ExitPath::Call {
                    block: block_id,
                    callee: callee.clone(),
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Trap { .. } => {
                // Cleanup block: same drop plan as the NORMAL exit at this scope
                // depth — i.e. the init-aware `drops_for_exit` filter, NOT the
                // raw template. A binding that is `Uninit` at the trap point
                // (e.g. `let y = xs[i];` traps in the bounds check BEFORE the
                // getter initialises `y`) must not be dropped: its slot holds no
                // value, so an unfiltered `hew_string_drop`/record-drop would
                // dereference uninitialised stack memory and turn a clean OOB
                // trap (SIGTRAP, code 205) into a SIGSEGV. The filter also
                // excludes `Consumed` bindings (no double-free on the panic
                // path). `Live`/`MaybeConsumed`/`AliasedIntoAggregate` bindings
                // still drop, preserving cleanup-all-exits. No successor (trap is
                // terminal).
                let cleanup_id = next_cleanup_id;
                next_cleanup_id = next_cleanup_id.saturating_add(1);
                elab_blocks.push(ElabBlock {
                    id: cleanup_id,
                    kind: BlockKind::Cleanup,
                    drops: drops_for_exit(block_id),
                    successor: None,
                });
                (
                    ExitPath::Panic { block: block_id },
                    DropPlan {
                        drops: drops_for_exit(block_id),
                    },
                )
            }
            // Generator-body `yield`. The abandon (destroy-while-parked-at-yield)
            // edge must drop the frame-owned locals live across this yield, or a
            // generator destroyed before exhaustion (a `for await` consumer that
            // stops early, a supervisor tearing the producer down) leaks them.
            // The plan is the SAME `drops_for_exit` `BindingState`-filtered set the
            // `Return`/`Cancel` exits use, so a moved-out local is excluded (no
            // double-free). The just-yielded `value` is published to the companion
            // `out` slot as a MOVE, so its binding is `Consumed` at this exit and
            // the filter drops it here — its sole owner is `hew_gen_coro_destroy`'s
            // `out_drop_thunk`. Codegen fires this plan ONLY on the yield's case-1
            // destroy edge (never on resume): `emit_elab_drops` interposed before
            // the `br coro.cleanup`, with the block-loop's normal-flow emission
            // suppressed for suspend carriers.
            Terminator::Yield { value: _, next } => (
                ExitPath::Yield {
                    block: block_id,
                    next: *next,
                },
                DropPlan {
                    drops: drops_for_exit(block_id),
                },
            ),
            // Generator construction is a synchronous call into the runtime
            // (the coro companion alloc + the gen-body ramp call) with a single
            // `next` continuation. Model it as an `ExitPath::Call` so the
            // call-exit handling applies; the constructed value's own drop
            // (`hew_gen_coro_destroy`) is scheduled at scope exit by the
            // enclosing function's LIFO drop plan, not here.
            Terminator::MakeGenerator {
                dest: _,
                body_fn: _,
                next,
                env: _,
            } => (
                ExitPath::Call {
                    block: block_id,
                    callee: "hew_cont_frame_alloc".to_string(),
                    next: *next,
                },
                DropPlan::default(),
            ),
            // Lambda-actor construction is structurally identical to
            // MakeGenerator: a synchronous runtime call (`hew_lambda_actor_new`)
            // with a single `next` continuation. The constructed handle's drop
            // (`hew_lambda_actor_release`) is scheduled at scope exit by the
            // enclosing function's LIFO drop plan via `place_aware_drop_fn`,
            // not here.
            Terminator::MakeLambdaActor {
                dest: _,
                body_fn: _,
                state_drop_fn: _,
                shape: _,
                mailbox_capacity: _,
                next,
                env: _,
                env_field_drops: _,
            } => (
                ExitPath::Call {
                    block: block_id,
                    callee: "hew_lambda_actor_new".to_string(),
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Send {
                actor: _,
                stable_role: _,
                msg_type: _,
                value: _,
                next,
                arg_modes: _,
                cleanup_plan: _,
                result_dest: _,
            } => (
                // `actor` is a Place; the ExitPath::Send slot carries
                // the callee name. Spine has no Send construction
                // surface, so this is unreachable in practice — empty
                // placeholder name.
                ExitPath::Send {
                    block: block_id,
                    actor: String::new(),
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Ask {
                actor,
                stable_role: _,
                msg_type: _,
                value: _,
                arg_modes: _,
                cleanup_plan: _,
                result_dest: _,
                reply_dest: _,
                error_dest: _,
                next,
            }
            | Terminator::RemoteAsk { actor, next, .. } => (
                ExitPath::Ask {
                    block: block_id,
                    actor: *actor,
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Select { arms: _, next } => (
                // Per-arm select-loser cleanup lives in codegen, not in
                // the function-wide DropPlan. The DropPlan abstraction
                // models LIFO `@resource` drops over `place + drop_fn`;
                // select-loser cleanup needs two operands (the resource
                // and the runtime-allocated registration id returned by
                // the substrate primitive) and runs at the select
                // dispatch site, not the function exit. Keeping it out
                // of DropPlan avoids stretching the ElabDrop shape to
                // cover a case it was not designed for.
                //
                // The contract codegen must honour for each arm kind:
                //
                //   - StreamNext loser: emit
                //     `hew_stream_cancel_pending_read(stream, id)`
                //     where `id` is the PendingReadId returned by the
                //     winning-side `hew_stream_poll`. The stream
                //     binding remains usable in the enclosing scope
                //     (no item consumed). See `hew-runtime::stream`
                //     for the ABI and TOCTOU contract.
                //
                //   - ActorAsk loser: withdraw the envelope by
                //     correlation id if not yet dispatched, otherwise
                //     tombstone the reply sink; a late reply is
                //     classified as OrphanedAsk and dropped silently.
                //
                //   - TaskAwait loser: cancel the task at its next
                //     safepoint via the single-task cancel primitive;
                //     the awaitable handle is torn down.
                //
                //   - AfterTimer loser: cancel the timer; no callback
                //     fires.
                //
                // LESSONS: cleanup-all-exits — every select exit path
                // gets a non-empty cleanup at the codegen dispatch
                // site; the function-wide DropPlan is intentionally
                // empty for ExitPath::Select.
                ExitPath::Select {
                    block: block_id,
                    next: *next,
                },
                DropPlan::default(),
            ),
            // `join { }` exit — the wait-ALL sibling of `Terminator::Select`.
            // Per-branch cancel-rest cleanup lives at the codegen join-dispatch
            // site, not in the function-wide DropPlan, exactly as for `Select`.
            Terminator::Join { branches: _, result: _, next } => (
                ExitPath::Join {
                    block: block_id,
                    next: *next,
                },
                DropPlan::default(),
            ),
            // Stackless suspend (R326/R327). When the parked continuation is
            // DESTROYED without resuming (a supervisor stopping a parked actor,
            // teardown, `hew_cont_destroy`'s abandon edge), the coro frame is
            // freed but its frame-owned Hew heap values would leak (#2395) — the
            // never-implemented "cleanup outline". This plan carries the exit's
            // owned-local drops so codegen fires them on the case-1 (destroy)
            // edge, BEFORE the frame free, and ONLY there (never on resume: the
            // block-loop's normal-flow `emit_elab_drops` is suppressed for
            // suspend carriers, and the resume edge continues the still-owned
            // body). The drop set is the SAME `drops_for_exit` `BindingState`
            // filter the `Return`/`Cancel` exits use, so a local moved out across
            // the suspend is `Consumed` and excluded — no double-free. The
            // StreamSend in-flight value (escape-poisoned, so the generic filter
            // misses it) is appended kind-specifically after `enumerate_exits`
            // returns (see the `suspend_abandon_extra_drops` post-pass).
            Terminator::Suspend {
                resume, cleanup, ..
            }
            // `SuspendingScopeDeadline` shares the abandon-edge drop posture: the
            // frame-owned owned LOCALS are released by this plan; the scoped
            // children's drops + deadline cancel stay codegen-owned in the abandon
            // closure. The `timeout_body_block` deadline edge is a regular in-CFG
            // successor already covered by the successors walker; the plan keys
            // off the resume/cleanup coro edges exactly like the other carriers.
            | Terminator::SuspendingScopeDeadline {
                resume, cleanup, ..
            }
            // `SuspendingSelect` is a suspend point with the select drop posture:
            // the per-arm reply channels + readiness registrations + the shared
            // await-cancel arbiter ride the coro frame across the park. The
            // per-arm LOSER cleanup (win path) runs at the codegen resume-edge
            // dispatch site exactly as `Terminator::Select`'s does; the abandon
            // edge deregisters EVERY observer + cancels the timer + frees the
            // arbiter (the single-teardown owner) in the codegen abandon closure.
            // This plan adds ONLY the ordinary owned-local drops on top — the
            // `drops_for_exit` set (registered owned locals) cannot include those
            // select structures by construction.
            | Terminator::SuspendingSelect {
                resume, cleanup, ..
            } => (
                ExitPath::Suspend {
                    block: block_id,
                    resume: *resume,
                    cleanup: *cleanup,
                },
                DropPlan {
                    drops: drops_for_exit(block_id),
                },
            ),
        };
        if !matches!(
            &block.terminator,
            Terminator::Call { authority, .. } if authority.is_no_return()
        ) {
            plans.push(plan);
        }
        if let Terminator::Call { callee, .. } = &block.terminator {
            plans.push((
                ExitPath::Unwind {
                    block: block_id,
                    callee: callee.clone(),
                },
                DropPlan {
                    drops: drops_for_exit(block_id),
                },
            ));
        }
        for (instruction_index, call_site) in
            block
                .instructions
                .iter()
                .enumerate()
                .filter_map(|(instruction_index, instruction)| match instruction {
                    Instr::CallClosure { call_site, .. } => Some((instruction_index, *call_site)),
                    _ => None,
                })
        {
            plans.push((
                ExitPath::Unwind {
                    block: block_id,
                    callee: crate::model::indirect_closure_callee(call_site),
                },
                DropPlan {
                    drops: drops_before_instruction(block, instruction_index),
                },
            ));
        }
        if block
            .instructions
            .iter()
            .any(|instruction| matches!(instruction, Instr::GeneratorNext { .. }))
        {
            // `Builder::push_instr` seals a GeneratorNext as the last
            // instruction in its block. Its continuation resume is therefore
            // a unique may-unwind program point keyed by this block id, and
            // the successful destination ownership events live in the normal
            // successor rather than contaminating this exceptional live set.
            plans.push((
                ExitPath::Unwind {
                    block: block_id,
                    callee: "hew_cont_resume".to_string(),
                },
                DropPlan {
                    drops: drops_for_exit(block_id),
                },
            ));
        }
        if cancellation_blocks.contains(&block_id) {
            // The drop set depends on WHERE the cooperate-cancel branch leaves
            // the function (see `CooperateKind`). A `FunctionEntry` site (always
            // the entry block, id 0 — both `dataflow::analyze` and
            // `compute_cooperate_sites` pin the entry block to 0) fires in the
            // prologue, BEFORE this block's `Bind` statements run, so its live
            // set is the block ENTRY state. A `LoopBackEdge` site fires after the
            // back-edge block body, so its live set is the block EXIT state — the
            // established loop-cancel posture, unchanged here.
            let drops = if block_id == ENTRY_BLOCK_ID {
                drops_for_entry_cancel(block_id)
            } else {
                drops_for_exit(block_id)
            };
            plans.push((ExitPath::Cancel { block: block_id }, DropPlan { drops }));
        }
    }
    (elab_blocks, plans)
}

#[cfg(test)]
mod semantic_unreachable_tests {
    use super::*;

    #[test]
    fn semantic_unreachable_has_a_normal_block_but_no_exit_or_cleanup_plan() {
        let raw = [BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Unreachable,
        }];
        let (elaborated, plans) = enumerate_exits(
            &raw,
            &[],
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashMap::new(),
        );

        assert_eq!(elaborated.len(), 1);
        let block = &elaborated[0];
        assert_eq!(block.id, 0);
        assert_eq!(block.kind, BlockKind::Normal);
        assert!(block.drops.is_empty());
        assert_eq!(block.successor, None);
        assert!(
            plans.is_empty(),
            "unreachable must not manufacture an ExitPath"
        );
    }
}

// ============================================================================
// #2418 fan-out exclusivity boundary — direct-CFG tests against
// `dedup_whole_value_handoff`'s guarded-component collapse. Hand-constructed
// blocks are required because the co-executable (parallel) fan-out shapes are
// checker-rejected in source (`UseAfterConsume` on the second consume), so
// only a synthetic Move stream can reach the boundary — which is exactly why
// the exclusivity proof lives in the CFG walk and not in the checker's
// acceptance.
// ============================================================================

#[cfg(test)]
mod dedup_fanout_exclusivity_tests {
    use super::*;

    /// `BindingId(0) -> Local(10)` (the guarded source), `BindingId(1) ->
    /// Local(11)` and `BindingId(2) -> Local(12)` (the move destinations).
    fn locals() -> HashMap<BindingId, Place> {
        [
            (BindingId(0), Place::Local(10)),
            (BindingId(1), Place::Local(11)),
            (BindingId(2), Place::Local(12)),
        ]
        .into_iter()
        .collect()
    }

    fn all_allowed() -> HashSet<BindingId> {
        [BindingId(0), BindingId(1), BindingId(2)]
            .into_iter()
            .collect()
    }

    fn guarded_source() -> HashMap<BindingId, Place> {
        [(BindingId(0), Place::Local(90))].into_iter().collect()
    }

    fn mv(src: u32, dest: u32) -> Instr {
        Instr::Move {
            dest: Place::Local(dest),
            src: Place::Local(src),
        }
    }

    fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![],
            instructions,
            terminator,
        }
    }

    /// Exclusive per-branch destinations of one guarded source (`if a
    /// { let y = xs; } else { let z = xs; }`): the destinations' bind sites
    /// sit on mutually-exclusive branch arms, so the whole component keeps
    /// its releases — the #2418 two-destination shape.
    #[test]
    fn exclusive_branch_destinations_of_guarded_source_are_kept() {
        let blocks = vec![
            block(
                0,
                vec![],
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 1,
                    else_target: 2,
                },
            ),
            block(1, vec![mv(10, 11)], Terminator::Goto { target: 3 }),
            block(2, vec![mv(10, 12)], Terminator::Goto { target: 3 }),
            block(3, vec![], Terminator::Return),
        ];
        let mut allowed = all_allowed();
        dedup_whole_value_handoff(&blocks, &locals(), &mut allowed, &guarded_source());
        assert_eq!(
            allowed,
            all_allowed(),
            "mutually-exclusive branch destinations of a guarded source must \
             keep every member's release"
        );
    }

    /// Genuinely-parallel fan-out (both destinations on ONE path): the
    /// fail-closed collapse strips the whole component, guarded source
    /// included — sole ownership across a co-executable fan is unprovable.
    /// This shape is checker-rejected in source; a synthetic Move stream
    /// gets no benefit of the doubt.
    #[test]
    fn parallel_fanout_from_guarded_source_still_strips_component() {
        let blocks = vec![
            block(0, vec![mv(10, 11)], Terminator::Goto { target: 1 }),
            block(1, vec![mv(10, 12)], Terminator::Goto { target: 2 }),
            block(2, vec![], Terminator::Return),
        ];
        let mut allowed = all_allowed();
        dedup_whole_value_handoff(&blocks, &locals(), &mut allowed, &guarded_source());
        assert!(
            allowed.is_empty(),
            "co-executable (sequential) fan-out must strip the whole \
             component, guarded source included; kept {allowed:?}"
        );
    }

    /// Two bind sites in one block are trivially co-executable — strip.
    #[test]
    fn same_block_fanout_from_guarded_source_strips_component() {
        let blocks = vec![block(0, vec![mv(10, 11), mv(10, 12)], Terminator::Return)];
        let mut allowed = all_allowed();
        dedup_whole_value_handoff(&blocks, &locals(), &mut allowed, &guarded_source());
        assert!(
            allowed.is_empty(),
            "same-block fan-out must strip the whole component; kept {allowed:?}"
        );
    }

    /// A bind site on a cycle can re-execute — co-executable with itself, so
    /// the pair (loop site, post-loop site) strips even though neither
    /// branch dominates the other.
    #[test]
    fn loop_bind_site_fanout_strips_component() {
        let blocks = vec![
            block(0, vec![], Terminator::Goto { target: 1 }),
            block(1, vec![mv(10, 11)], Terminator::Goto { target: 2 }),
            block(
                2,
                vec![],
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 1,
                    else_target: 3,
                },
            ),
            block(3, vec![mv(10, 12)], Terminator::Return),
        ];
        let mut allowed = all_allowed();
        dedup_whole_value_handoff(&blocks, &locals(), &mut allowed, &guarded_source());
        assert!(
            allowed.is_empty(),
            "a bind site inside a loop is reachable from itself and from the \
             post-loop site; the component must strip; kept {allowed:?}"
        );
    }

    /// Without a guarded member the original >1-member collapse holds even
    /// for exclusive destinations — the exclusivity exemption is earned by
    /// the guard flag, never by shape alone.
    #[test]
    fn guardless_component_keeps_original_collapse() {
        let blocks = vec![
            block(
                0,
                vec![],
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 1,
                    else_target: 2,
                },
            ),
            block(1, vec![mv(10, 11)], Terminator::Goto { target: 3 }),
            block(2, vec![mv(10, 12)], Terminator::Goto { target: 3 }),
            block(3, vec![], Terminator::Return),
        ];
        let mut allowed = all_allowed();
        dedup_whole_value_handoff(&blocks, &locals(), &mut allowed, &HashMap::new());
        // The chain strip removes the source (its handle reaches admitted
        // destinations); the fan-out collapse then strips the ambiguous
        // sibling pair.
        assert!(
            allowed.is_empty(),
            "a guardless multi-destination component keeps the fail-closed \
             collapse; kept {allowed:?}"
        );
    }
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
mod returned_member_read_aliases {
    use super::*;

    fn block(id: u32, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator,
        }
    }

    /// Counterfactual for non-terminal re-admission: the candidate owner is
    /// copied through unretained MIR handoff slots before a branch, and only
    /// the final alias is read after the join. A direct-local scan sees no
    /// future read of `_1` from either arm Goto and could free it there; the
    /// alias-closed scan must attribute the joined `_3` read back to `_1`.
    #[test]
    fn unretained_move_alias_read_after_join_is_attributed_to_candidate() {
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                }],
                terminator: Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 1,
                    else_target: 2,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![Instr::Move {
                    dest: Place::Local(3),
                    src: Place::Local(2),
                }],
                terminator: Terminator::Goto { target: 3 },
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: vec![Instr::Move {
                    dest: Place::Local(3),
                    src: Place::Local(2),
                }],
                terminator: Terminator::Goto { target: 3 },
            },
            BasicBlock {
                id: 3,
                statements: Vec::new(),
                instructions: vec![Instr::Drop {
                    place: Place::Local(3),
                    ty: ResolvedTy::String,
                    drop_fn: None,
                }],
                terminator: Terminator::Return,
            },
        ];
        let binding = BindingId(7);
        let candidate_locals = [(1_u32, binding)].into_iter().collect();
        let reads = returned_member_alias_read_blocks(&blocks, &HashMap::new(), &candidate_locals);

        assert!(
            reads
                .get(&binding)
                .is_some_and(|blocks| blocks.contains(&3)),
            "the post-join read through `_3` must be attributed to returned-member \
             candidate `_1`; otherwise an arm Goto may release the shared heap early: \
             {reads:?}"
        );
    }

    /// Diamond counterfactual: only the then arm has an earlier candidate A,
    /// while both arms merge at candidate B. Reachability alone must not keep
    /// A and suppress B (that leaks the else arm). B postdominates A's target,
    /// so the selector keeps only B and both paths cross exactly one release.
    #[test]
    fn merged_later_candidate_covers_sibling_that_bypasses_earlier_candidate() {
        let blocks = vec![
            block(
                0,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 1,
                    else_target: 2,
                },
            ),
            block(1, Terminator::Goto { target: 3 }),
            block(2, Terminator::Goto { target: 3 }),
            block(3, Terminator::Goto { target: 4 }),
            block(4, Terminator::Return),
        ];
        let block_reach: HashMap<u32, HashSet<u32>> = blocks
            .iter()
            .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
            .collect();
        let candidates = [
            ReturnedMemberReAdmission {
                plan_index: 10,
                block: 1,
                path: ReturnedMemberReAdmissionPath::Normal,
            },
            ReturnedMemberReAdmission {
                plan_index: 11,
                block: 3,
                path: ReturnedMemberReAdmissionPath::Normal,
            },
        ];

        let selected = select_returned_member_re_admissions(&blocks, &block_reach, &candidates)
            .expect("the later common candidate is unambiguous");
        assert_eq!(
            selected,
            vec![candidates[1]],
            "the common later candidate must replace the one-arm predecessor"
        );

        for path in [[0_u32, 1, 3, 4], [0_u32, 2, 3, 4]] {
            let releases = selected
                .iter()
                .filter(|candidate| path.contains(&candidate.block))
                .count();
            assert_eq!(
                releases, 1,
                "both diamond paths must cross exactly one selected release: \
                 path={path:?}, selected={selected:?}"
            );
        }
    }

    /// Existing-plan diamond counterfactual: the then arm already releases the
    /// owner and a later common candidate covers both arms. A reachability veto
    /// would delete the common candidate and leak the else arm. Because the
    /// common block postdominates the existing arm's continuation, it may
    /// replace that arm-local release and both paths retain exactly one release.
    #[test]
    fn common_candidate_replaces_branch_local_existing_release() {
        let blocks = vec![
            block(
                0,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 1,
                    else_target: 2,
                },
            ),
            block(1, Terminator::Goto { target: 3 }),
            block(2, Terminator::Goto { target: 3 }),
            block(3, Terminator::Goto { target: 4 }),
            block(4, Terminator::Return),
        ];
        let block_reach: HashMap<u32, HashSet<u32>> = blocks
            .iter()
            .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
            .collect();
        let existing = [ReturnedMemberReAdmission {
            plan_index: 20,
            block: 1,
            path: ReturnedMemberReAdmissionPath::Normal,
        }];
        let candidate = ReturnedMemberReAdmission {
            plan_index: 10,
            block: 3,
            path: ReturnedMemberReAdmissionPath::Normal,
        };

        let replaced =
            existing_releases_replaced_by_candidate(&blocks, &block_reach, candidate, &existing)
                .expect("the common postdominator must be comparable")
                .expect("the common postdominator can replace the arm-local release");
        assert_eq!(replaced, HashSet::from([existing[0].plan_index]));

        for path in [[0_u32, 1, 3, 4], [0_u32, 2, 3, 4]] {
            let releases = usize::from(
                path.contains(&existing[0].block) && !replaced.contains(&existing[0].plan_index),
            ) + usize::from(path.contains(&candidate.block));
            assert_eq!(
                releases, 1,
                "both diamond paths must cross exactly one release after relocation: \
                 path={path:?}, replaced={replaced:?}"
            );
        }
    }

    /// A normal scope-closing Goto can precede a later loop cancellation. The
    /// later cancel must not duplicate the already-completed release, but an
    /// independent cancellation route that bypasses the Goto still owns one.
    #[test]
    fn normal_goto_replaces_only_the_later_loop_cancellation_release() {
        let blocks = vec![
            block(
                0,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 1,
                    else_target: 4,
                },
            ),
            block(1, Terminator::Goto { target: 2 }),
            block(2, Terminator::Goto { target: 2 }),
            block(4, Terminator::Goto { target: 5 }),
            block(5, Terminator::Return),
        ];
        let block_reach: HashMap<u32, HashSet<u32>> = blocks
            .iter()
            .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
            .collect();
        let candidates = [
            ReturnedMemberReAdmission {
                plan_index: 10,
                block: 1,
                path: ReturnedMemberReAdmissionPath::Normal,
            },
            ReturnedMemberReAdmission {
                plan_index: 11,
                block: 2,
                path: ReturnedMemberReAdmissionPath::Abandonment,
            },
            ReturnedMemberReAdmission {
                plan_index: 12,
                block: 4,
                path: ReturnedMemberReAdmissionPath::Abandonment,
            },
        ];

        let selected = select_returned_member_re_admissions(&blocks, &block_reach, &candidates)
            .expect("the dominated loop cancellation has one prior release owner");
        assert_eq!(
            selected,
            vec![candidates[0], candidates[2]],
            "the post-Goto loop cancellation is redundant, while the route that bypasses \
             the Goto retains cancellation coverage"
        );
        let abandoned_existing = existing_releases_replaced_by_candidate(
            &blocks,
            &block_reach,
            candidates[0],
            &candidates[1..],
        )
        .expect("the normal Goto is comparable to both cancellation plans")
        .expect("cross-path authority arbitration occurs after candidate selection");
        assert_eq!(
            abandoned_existing,
            HashSet::new(),
            "normal/cancellation replacements must wait until normal candidates are final"
        );
        assert_eq!(
            existing_releases_replaced_by_candidate(
                &blocks,
                &block_reach,
                candidates[1],
                &candidates[..1],
            )
            .expect("the cancellation is comparable to the preceding Goto"),
            Some(HashSet::new()),
            "normal/cancellation arbitration must wait until normal candidates are final"
        );

        for path in [&[0_u32, 1, 2][..], &[0_u32, 4][..]] {
            let releases = selected
                .iter()
                .filter(|candidate| path.contains(&candidate.block))
                .count();
            assert_eq!(
                releases, 1,
                "each normal or cancellation path must have exactly one release: \
                 path={path:?}, selected={selected:?}"
            );
        }
    }

    /// If a downstream normal Goto replaces A, a cancellation between A and the
    /// replacement bypasses the final normal authority and must stay selected.
    #[test]
    fn later_normal_replacement_does_not_suppress_intermediate_cancellation() {
        let blocks = vec![
            block(0, Terminator::Goto { target: 1 }),
            block(1, Terminator::Goto { target: 2 }),
            block(2, Terminator::Goto { target: 3 }),
            block(3, Terminator::Goto { target: 4 }),
            block(4, Terminator::Return),
        ];
        let block_reach: HashMap<u32, HashSet<u32>> = blocks
            .iter()
            .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
            .collect();
        let candidates = [
            ReturnedMemberReAdmission {
                plan_index: 10,
                block: 1,
                path: ReturnedMemberReAdmissionPath::Normal,
            },
            ReturnedMemberReAdmission {
                plan_index: 11,
                block: 2,
                path: ReturnedMemberReAdmissionPath::Abandonment,
            },
            ReturnedMemberReAdmission {
                plan_index: 12,
                block: 3,
                path: ReturnedMemberReAdmissionPath::Normal,
            },
        ];

        let selected = select_returned_member_re_admissions(&blocks, &block_reach, &candidates)
            .expect("the normal replacement and intermediate cancellation are unambiguous");
        assert_eq!(
            selected,
            vec![candidates[1], candidates[2]],
            "the replacement owns normal completion while the intermediate cancel \
             retains its bypass cleanup"
        );
        let cancellation_releases = selected
            .iter()
            .filter(|candidate| {
                matches!(candidate.path, ReturnedMemberReAdmissionPath::Abandonment)
                    && candidate.block == 2
            })
            .count();
        assert_eq!(
            cancellation_releases, 1,
            "the cancellation at bb2 must retain exactly one release: {selected:?}"
        );
        let normal_releases = selected
            .iter()
            .filter(|candidate| {
                matches!(candidate.path, ReturnedMemberReAdmissionPath::Normal)
                    && [0_u32, 1, 2, 3, 4].contains(&candidate.block)
            })
            .count();
        assert_eq!(
            normal_releases, 1,
            "the normal continuation must retain exactly one release: {selected:?}"
        );
    }

    /// Partial overlap counterfactual: one release can reach the other, but
    /// neither covers all of the other's paths. Selecting either candidate
    /// leaks one path and selecting both double-releases their overlap, so this
    /// topology must reject instead of silently omitting every cleanup.
    #[test]
    fn partial_overlap_re_admission_is_rejected() {
        let blocks = vec![
            block(
                0,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 1,
                    else_target: 2,
                },
            ),
            block(1, Terminator::Goto { target: 3 }),
            block(2, Terminator::Goto { target: 4 }),
            block(
                3,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 4,
                    else_target: 5,
                },
            ),
            block(4, Terminator::Goto { target: 5 }),
            block(5, Terminator::Return),
        ];
        let block_reach: HashMap<u32, HashSet<u32>> = blocks
            .iter()
            .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
            .collect();
        let candidates = [
            ReturnedMemberReAdmission {
                plan_index: 10,
                block: 1,
                path: ReturnedMemberReAdmissionPath::Normal,
            },
            ReturnedMemberReAdmission {
                plan_index: 11,
                block: 4,
                path: ReturnedMemberReAdmissionPath::Normal,
            },
        ];

        let ambiguity = select_returned_member_re_admissions(&blocks, &block_reach, &candidates)
            .expect_err("partial overlap has no exactly-once cleanup owner");
        assert_eq!(ambiguity.first, candidates[0]);
        assert_eq!(ambiguity.second, candidates[1]);
    }
}
#[cfg(test)]
mod obligation_balance_validator;
#[cfg(test)]
#[derive(Default)]
struct UnderReleaseAggregate {
    blocks: Vec<u32>,
    exits: Vec<String>,
    mint_provenance: Option<crate::model::ObligationMintProvenance>,
    max_mints: u8,
    max_discharges: u8,
}
