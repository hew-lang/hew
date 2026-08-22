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
    derive_tuple_composite_drop_allowed, instr_source_places, outbound_record_layouts,
    place_is_interior_projection, place_refs_local,
    propagate_seeded_whole_value_alias_roots_excluding_moves, propagate_whole_value_alias_roots,
    retained_string_terminator_drop_safe, short_name, string_call_borrows,
    terminator_source_places, user_record_layout_key, vec_iter_record_init_vec_source, BTreeMap,
    BasicBlock, BindingId, BlockKind, Builder, BuiltinType, CheckedMirFunction,
    ClosureEnvFieldOwnership, ClosurePairRhs, Disposition, DropKind, DropPlan, ElabBlock, ElabDrop,
    ElaboratedMirFunction, ExitPath, HashMap, HashSet, HirExpr, HirExprKind, Instr, IntentKind,
    LambdaCapture, MirCheck, MirDiagnostic, MirDiagnosticKind, MirStatement, ParamCrashCleanupKind,
    Place, RawMirFunction, ResolvedRef, ResolvedTy, ScopeId, SiteId, SuspendKind, Terminator,
    TraitObjectStorage, ValueClass, ENTRY_BLOCK_ID,
};
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
///   1. Walk the builder's `owned_locals` ledger (the per-function
///      ordered list of non-`BitCopy` bindings introduced by `let`).
///      The ledger is already maintained in source/declaration order
///      with bindings removed when consumed (`mark_binding_moved`).
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
    clippy::too_many_lines,
    reason = "elaborate threads each per-class drop-allow derivation (cow / enum \
              / owned-Vec / owned-record / tuple-composite / returned-aggregate \
              members) into one ordered pass; each is a distinct fail-closed \
              authority and splitting them scatters the ordering contract"
)]
pub(super) fn elaborate(
    checked: &CheckedMirFunction,
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
    // The scope-exit-live owned-locals view, materialised once for the
    // reverse-declaration drop stream and every per-class allow-set derivation
    // below — the same `(binding, name, ty)` tuples the provers read before the
    // ledger carried richer facts, in the same declaration order.
    let owned_locals_snapshot = builder.owned_locals_snapshot();
    let aggregate_member_neutralized_bindings: HashSet<BindingId> = checked
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::NeutralizePayloadSlot {
                place,
                authority: crate::model::NeutralizeAuthority::AggregateMemberConsume,
                ..
            } => builder
                .binding_locals
                .iter()
                .find_map(|(binding, binding_place)| {
                    (*binding_place == *place).then_some(*binding)
                }),
            _ => None,
        })
        .collect();
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
    // Consume facts narrow the allow-set further: a binding `Consumed` or
    // `MaybeConsumed` at any block exit is removed, because `enumerate_exits`
    // treats `MaybeConsumed` as Live (the move-checker rejects that only for
    // `MustConsume`/Linear types, not CoW values) and would otherwise fire the
    // drop on a branch where the buffer was already moved out.
    let mut cow_drop_allowed = if let Some(precomputed) = precomputed_cow_drop_allowed {
        precomputed.clone()
    } else {
        let fresh_owner_dest_locals = builder.fresh_owner_dest_locals();
        let mut derived = derive_cow_sole_owner(
            &checked.blocks,
            &builder.suspend_kinds,
            &owned_locals_snapshot,
            &builder.binding_locals,
            &builder.match_project_consumed_binder_locals,
            &fresh_owner_dest_locals,
            &builder.locals,
            &builder.borrowed_string_param_locals,
            &builder.parameter_locals,
            &builder.actor_message_cow_drop_flags,
            &builder.module_fn_names,
            &builder.module_generic_fn_names,
            &builder.call_scrutinee_provenance.extern_table,
            &builder
                .call_scrutinee_provenance
                .owned_string_return_carrier_symbols,
        )
        .allowed;
        derived.extend(derive_cow_fresh_borrowed_owner(
            &checked.blocks,
            &builder.suspend_kinds,
            &owned_locals_snapshot,
            &builder.binding_locals,
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
                ) && !builder.actor_message_cow_drop_flags.contains_key(binding)
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
        &checked.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
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
    enum_composite_drop_allowed.extend(builder.overwrite_guard_flags.keys().copied());
    // A call scrutinee held across a `while let` iteration has an explicit,
    // per-edge consume recorded by the loop lowerer.  Its short lifetime can
    // make the whole-function sole-owner scan conservatively decline the
    // normal scope-exit class, but that must not erase the typed drop template
    // the already-recorded back-edge, mismatch, panic, and early-exit plans
    // need.  These bindings come only from `record_iteration_owner_drop`,
    // which is reached after the call-result admission proof; re-admitting
    // them here gives those exact consume edges the same EnumInPlace drop they
    // had before the scan without granting an arbitrary local a release.
    enum_composite_drop_allowed.extend(
        builder
            .iteration_owner_drop_blocks
            .values()
            .flatten()
            .copied(),
    );
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
    enum_composite_drop_allowed.extend(builder.binding_locals.iter().filter_map(
        |(binding, place)| {
            let local = base_local(*place)?;
            let ty = builder.locals.get(local as usize)?;
            (builder.call_scrutinee_carrier_mint_locals.contains(&local)
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
    let partially_transferred_carriers: HashSet<u32> = checked
        .blocks
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
    enum_composite_drop_allowed.extend(builder.binding_locals.iter().filter_map(
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
        &checked.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
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
        &builder.collection_drop_flags,
        |ty| builder.binding_ty_is_owned_element_vec(ty),
        |view| {
            derive_local_collection_drop_allowed(
                &checked.blocks,
                &builder.suspend_kinds,
                view,
                &builder.binding_locals,
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
    let vec_iter_borrowed_owned_sources: HashSet<BindingId> = checked
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(vec_iter_record_init_vec_source)
        .filter_map(base_local)
        .filter_map(|local| {
            builder.binding_locals.iter().find_map(|(binding, place)| {
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
            ) && !builder.collection_drop_flags.contains_key(binding)
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
        let owned_vec_interior_alias = compute_collection_interior_alias_taint(&checked.blocks);
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
        &checked.blocks,
        &builder.binding_locals,
        &mut owned_vec_drop_allowed,
        &builder.collection_drop_flags,
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
        &builder.collection_drop_flags,
        ty_is_local_collection_handle,
        |view| {
            derive_local_collection_drop_allowed(
                &checked.blocks,
                &builder.suspend_kinds,
                view,
                &builder.binding_locals,
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
            ) && !builder.collection_drop_flags.contains_key(binding)
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
    let local_bytes_drop_allowed = if let Some(precomputed) = precomputed_local_bytes_drop_allowed {
        precomputed.clone()
    } else {
        let mut derived = derive_local_bytes_drop_allowed(
            &checked.blocks,
            &builder.suspend_kinds,
            &owned_locals_snapshot,
            &builder.binding_locals,
            &builder.locals,
            &builder.borrowed_bytes_param_locals,
        )
        .allowed;
        derived.extend(
            owned_locals_snapshot
                .iter()
                .filter(|(binding, _, ty)| {
                    matches!(ty, ResolvedTy::Bytes)
                        && builder.actor_message_cow_drop_flags.contains_key(binding)
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
                ) && !builder.actor_message_cow_drop_flags.contains_key(binding)
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
        &checked.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
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
        &builder.collection_drop_flags,
        |ty| builder.binding_ty_is_plain_vec(ty),
        |view| {
            derive_local_collection_drop_allowed(
                &checked.blocks,
                &builder.suspend_kinds,
                view,
                &builder.binding_locals,
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
            ) && !builder.collection_drop_flags.contains_key(binding)
            {
                plain_vec_drop_allowed.remove(binding);
            }
        }
    }

    // Whole-value hand-off dedup for both Vec-handle allow-sets (the
    // closure-pair set and the plain set; a handle never changes element
    // class across a `Move`, so the two sets cannot hand off to each other).
    dedup_whole_value_handoff(
        &checked.blocks,
        &builder.binding_locals,
        &mut closure_vec_drop_allowed,
        &builder.collection_drop_flags,
    );
    dedup_whole_value_handoff(
        &checked.blocks,
        &builder.binding_locals,
        &mut plain_vec_drop_allowed,
        &builder.collection_drop_flags,
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
        &checked.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
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
    owned_record_drop_allowed.extend(builder.conditional_record_drop_flags.keys().copied());
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
        &checked.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
        &alias_field_binders,
        &builder.proven_borrow_call_args,
    );

    for allowed in [
        &mut enum_composite_drop_allowed,
        &mut owned_vec_drop_allowed,
        &mut local_collection_drop_allowed,
        &mut closure_vec_drop_allowed,
        &mut plain_vec_drop_allowed,
        &mut owned_record_drop_allowed,
        &mut tuple_composite_drop_allowed,
    ] {
        allowed.extend(aggregate_member_neutralized_bindings.iter().copied());
    }

    // W5.021 (defect #1) — owned members the caller now owns via a returned
    // aggregate; excluded from every drop class below (see the function doc).
    let mut returned_aggregate_members = derive_returned_aggregate_member_bindings(
        &checked.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
    );
    returned_aggregate_members
        .retain(|binding| !aggregate_member_neutralized_bindings.contains(binding));
    // Path-sensitive re-admission map for values handed to the caller through the
    // return flow. The blanket exclusion (an aggregate member the return handoff
    // removes, `semver::try_parse`; or a whole-value return that retracts its
    // binding to `ConsumedAt`, `base64::decode`) is correct on the return path but
    // leaks the value on a guard early-return that exits BEFORE the hand-off and
    // still owns it locally. Sourced from the returned-candidate view (scope-exit
    // OR consume-retracted owners) so BOTH shapes are covered; this locates where
    // each candidate enters the return flow so the elaborator can restore its
    // scope-exit drop on exactly the `Return` exits that transfer cannot reach.
    let returned_member_candidates = builder.owned_locals_exit_candidates();
    let returned_member_transfer_blocks = derive_returned_member_transfer_blocks(
        &checked.blocks,
        &returned_member_candidates,
        &builder.binding_locals,
    );

    // W3.053 — owned-handle members moved into a LOCAL aggregate and then
    // extracted-and-consumed back out (for-in / `let` extraction) by a downstream
    // release-consumer; the consumer owns the single free, so the source binding
    // must not also drop. The local-aggregate analogue of
    // `returned_aggregate_members` (see the function doc).
    let mut consumed_local_aggregate_members = derive_consumed_local_aggregate_member_bindings(
        &checked.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
    );
    consumed_local_aggregate_members
        .retain(|binding| !aggregate_member_neutralized_bindings.contains(binding));
    // CAP-08 — owned handle-leaf bindings moved into an actor initial-state
    // record consumed by `SpawnActor`. The actor's synthesised `state_drop_fn`
    // is the single free site (Stream→`hew_stream_close` / Sink→`hew_sink_close`),
    // so the source binding's own scope-exit drop is removed here. The W3.053
    // gate consumes the SAME derivation via `source_excluded` so its free-count
    // model matches the drop this removal actually elides.
    let mut spawn_consumed_handle_members = derive_spawn_consumed_handle_bindings(
        &checked.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
    );
    spawn_consumed_handle_members
        .retain(|binding| !aggregate_member_neutralized_bindings.contains(binding));

    // Escaping-closure pair env-box drop allow-set. Starts from the
    // `Let`-admitted ownership ledger (heap-mode literal / call result /
    // admitted rebind — see `closure_pair_owned`), then removes every
    // binding whose pair bits are aliased or moved out of the slot
    // (returned, passed as a call argument, captured into an aggregate) via
    // the fail-closed source-operand scan, and finally every binding the
    // dataflow proves consumed or maybe-consumed at any exit (the same
    // belt-and-suspenders net the owned-Vec / cow arms use). Both
    // directions only ever over-EXCLUDE (leak), never re-admit
    // (`boundary-fail-closed`, `cleanup-all-exits`).
    let mut closure_pair_drop_allowed = derive_closure_pair_drop_allowed(
        &checked.blocks,
        &builder.suspend_kinds,
        &builder.closure_pair_owned,
        &builder.binding_locals,
    );
    for states in dataflow_result.exit_states.values() {
        for (binding, state) in states {
            if matches!(
                state,
                dataflow::BindingState::Discharged(_)
                    | dataflow::BindingState::Consumed(_)
                    | dataflow::BindingState::MaybeConsumed(_)
            ) {
                closure_pair_drop_allowed.remove(binding);
            }
        }
    }

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
        &checked.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
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

    // A parent overwrite or borrow-spine call forwarding can transfer a direct
    // string payload's release authority to its still-live binder. Re-admit
    // exactly those delayed owners into the leaf drop class and remove their
    // projection-alias suppression; their per-binder flag remains one on every
    // path where no transfer occurred, so the resulting scope-exit drop is
    // skipped there and cannot compete with the parent's recursive release.
    cow_drop_allowed.extend(builder.projected_payload_delayed_releases.iter().copied());
    let mut projection_alias_tainted = compute_projection_alias_taint(
        &checked.blocks,
        &builder.match_project_consumed_binder_locals,
        &builder.fresh_variant_payload_binder_locals,
        &builder.locals,
    );
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
            &builder.binding_locals,
            &builder.locals,
            &projection_alias_tainted,
        );
    // A projected builtin handle can defer to its enum carrier only when that
    // carrier actually earned an `EnumInPlace` drop. If carrier admission was
    // withheld, suppressing the binder as an alias removes both release paths.
    // Keep alias suppression only for the exact carrier bindings present in the
    // enum allow-set; otherwise the binder remains the sole close authority.
    let payload_alias_carriers = collect_payload_alias_map(&checked.blocks);
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
        &checked.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &tuple_composite_drop_allowed,
    );
    borrowed_builtin_handle_projection_aliases
        .retain(|binding| !owned_tuple_handle_projections.contains(binding));
    // A `ConsumedAt` disposition proves the value is absent only AFTER its
    // consuming instruction.  It can still be live at an earlier Return or a
    // coroutine-abandon edge, so the LIFO template must retain it and let the
    // CFG state filter select the exact owning exits.  `BodyEndReleased`,
    // `ScopeReleased`, and interior aliases stay excluded by this view.
    let owned_locals_exit_candidates = builder.owned_locals_exit_candidates();
    // Borrowed-element aliases of a live collection, SCOPED to close-obligated
    // element types: their LIFO drop is suppressed (the collection is the sole
    // discharge authority), which is sound only under the use validation
    // below. Nested-collection handle borrows keep their pre-existing
    // exclusion machinery and are deliberately NOT in this set.
    let mut borrow_getter_aliases = collection_borrow_getter_alias_locals(&checked.blocks);
    borrow_getter_aliases.retain(|local| {
        builder.locals.get(*local as usize).is_some_and(|ty| {
            crate::model::ty_drop_obligation(
                ty,
                &crate::model::MirHeapLayouts {
                    record_field_orders: &builder.record_field_orders,
                    enum_layouts: &builder.enum_layouts,
                },
                builder.type_classes.lifecycle_registry(),
            )
            .needs_close
        })
    });
    // Fail-closed floor for the suppression: every use of a close-obligated
    // borrow must be a proven-safe read. An escape (return/store), a consume
    // (`e.close()`, `w.push(e)`, any call argument), or a reassignment refuses
    // the function -- suppression without this proof converted a silent
    // double-close into a use-after-close (the alias outliving the
    // collection's release) and stays structurally unreachable only by
    // rejecting the unprovable shapes.
    for violation in close_obligated_borrow_alias_violations(
        &checked.blocks,
        &borrow_getter_aliases,
        &tracked_obligation_locals(builder),
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
    let lifo_drops = build_lifo_drops(
        &owned_locals_exit_candidates,
        &builder.binding_locals,
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
        &closure_pair_drop_allowed,
        &closure_vec_drop_allowed,
        &plain_vec_drop_allowed,
        &indirect_enum_drop_allowed,
        &builder.affine_release_flags,
        &builder.overwrite_guard_flags,
        &builder.collection_drop_flags,
        &builder.actor_message_cow_drop_flags,
        &builder.conditional_record_drop_flags,
        &builder.projected_payload_overwrite_flags,
        &projection_alias_tainted,
        &borrowed_builtin_handle_projection_aliases,
        &borrow_getter_aliases,
    );
    let ordinary_lifo_drops: Vec<ElabDrop> = lifo_drops
        .iter()
        .filter(|drop| {
            !builder
                .back_edge_only_iteration_owners
                .iter()
                .any(|binding| builder.binding_locals.get(binding).copied() == Some(drop.place))
        })
        .cloned()
        .collect();
    let (elab_blocks, mut drop_plans) = enumerate_exits(
        &checked.blocks,
        &ordinary_lifo_drops,
        &dataflow_result.exit_states,
        &dataflow_result.entry_states,
        &builder.binding_locals,
        &checked
            .cooperate_sites
            .iter()
            .map(|site| site.bb_id)
            .collect::<HashSet<_>>(),
        &builder.binding_scope,
        &builder.loop_back_edge_blocks,
        &projection_alias_tainted,
    );

    // A first-class VecIter cursor is an inline record whose field-0 Vec
    // snapshot is released on ordinary lexical/explicit exits by a
    // flag-gated RecordFieldDrop. Cancellation, panic, yield-destroy, and
    // suspend-destroy can abandon the frame without traversing that inline
    // cleanup. Re-admit the same typed field release on those alternate
    // terminal edges only while dataflow says the cursor binding has been
    // initialised and may still be live. The existing ownership sidecar is
    // carried as ElabDrop::guard, so a conditional move, a borrowing cursor,
    // or an earlier lexical release skips the drop at runtime.
    //
    // Function-entry cancellation observes the block ENTRY state because its
    // check runs before entry-block instructions. Every other abandonment
    // point observes EXIT state, matching enumerate_exits and codegen's
    // cooperate/suspend placement.
    for (binding, cursor_ty) in builder.vec_iter_scope_owner_ledger.iter().rev() {
        let Some(&place) = builder.binding_locals.get(binding) else {
            continue;
        };
        let Some(&guard) = builder.vec_iter_drop_flags.get(binding) else {
            continue;
        };
        let Some(release) = builder.vec_iter_cursor_release_protocol(cursor_ty) else {
            continue;
        };
        for (exit, plan) in &mut drop_plans {
            let block = match exit {
                ExitPath::Cancel { block }
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
            let is_entry_cancel =
                matches!(exit, ExitPath::Cancel { .. }) && block == ENTRY_BLOCK_ID;
            let state_maps = if is_entry_cancel {
                &dataflow_result.entry_states
            } else {
                &dataflow_result.exit_states
            };
            let binding_may_own = matches!(
                state_maps
                    .get(&block)
                    .and_then(|states| states.get(binding))
                    .copied(),
                Some(
                    dataflow::BindingState::Live
                        | dataflow::BindingState::MaybeConsumed(_)
                        | dataflow::BindingState::AliasedIntoAggregate(_)
                )
            );
            if !binding_may_own
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
                guard: Some(guard),
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
        let region = vec_iter_yield_body_region(&checked.blocks, exit_drop);

        for (exit, plan) in &mut drop_plans {
            let block = match exit {
                ExitPath::Cancel { block }
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
            let binding_live = dataflow_result
                .exit_states
                .get(&block)
                .and_then(|states| states.get(&exit_drop.binding))
                .copied()
                == Some(dataflow::BindingState::Live);
            if !binding_live || plan.drops.iter().any(|drop| drop.place == exit_drop.place) {
                continue;
            }
            plan.drops.push(ElabDrop {
                place: exit_drop.place,
                ty: exit_drop.ty.clone(),
                drop_fn: None,
                kind: exit_drop.kind,
                guard: None,
            });
        }
    }

    super::for_await_drop_plan::admit_terminal_handoff_drops(
        checked,
        builder,
        dataflow_result,
        &returned_member_candidates,
        &mut drop_plans,
    );

    for (exit, plan) in &mut drop_plans {
        let block = match exit {
            ExitPath::Goto { block, .. } | ExitPath::Return { block } => *block,
            _ => continue,
        };
        let Some(bindings) = builder.iteration_owner_drop_blocks.get(&block) else {
            continue;
        };
        for binding in bindings {
            let Some(place) = builder.binding_locals.get(binding) else {
                continue;
            };
            if plan.drops.iter().any(|drop| drop.place == *place) {
                continue;
            }
            if let Some(drop) = lifo_drops.iter().find(|drop| drop.place == *place) {
                plan.drops.push(drop.clone());
            }
        }
    }

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
        let block_reach: HashMap<u32, HashSet<u32>> = checked
            .blocks
            .iter()
            .map(|block| (block.id, blocks_reachable_from(&checked.blocks, block.id)))
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
        for block in &checked.blocks {
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
        let member_read_blocks = returned_member_alias_read_blocks(
            &checked.blocks,
            &builder.suspend_kinds,
            &candidate_locals,
        );
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
            let Some(&place) = builder.binding_locals.get(binding) else {
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
                    guard: builder.affine_release_flags.get(binding).copied(),
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
                    &checked.blocks,
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
                &checked.blocks,
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
                                &checked.blocks,
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
                    if normal_goto_precedes_abandonment(
                        &checked.blocks,
                        &block_reach,
                        *candidate,
                        *release,
                    ) {
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
        &checked.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
    );
    if !bytes_mailbox_transfer_blocks.is_empty() {
        let mut transfer_reach: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for (binding, transfer_blocks) in &bytes_mailbox_transfer_blocks {
            let mut reach: HashSet<u32> = HashSet::new();
            for &transfer_block in transfer_blocks {
                reach.insert(transfer_block);
                reach.extend(blocks_reachable_from(&checked.blocks, transfer_block));
            }
            transfer_reach.insert(*binding, reach);
        }
        // Reverse CFG once, then walk backwards from each binding's transfer
        // sites. This is the exact "can normally reach the transfer itself"
        // set (not its downstream reach) in O(B+E) per binding.
        let mut reverse_cfg: HashMap<u32, Vec<u32>> = HashMap::new();
        for block in &checked.blocks {
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
            propagate_whole_value_alias_roots(&checked.blocks, candidate_roots.keys().copied());
        let mut read_blocks: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        let mut aggregate_owner_blocks: HashMap<BindingId, HashSet<u32>> = HashMap::new();
        for block in &checked.blocks {
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
                reach.extend(blocks_reachable_from(&checked.blocks, *owner_block));
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
                // A guarded actor-message Bytes binding already remains in the
                // LIFO template across a `MaybeConsumed` join. Its shared exit
                // drop is the sole release on the live path; adding the legacy
                // frontier re-admission as well would release once at the
                // non-transfer Goto and again at the shared exit while the
                // flag is still zero. The only exception is function-entry
                // cancellation, which precedes flag initialisation and is
                // converted to an unconditional drop below.
                if builder.actor_message_cow_drop_flags.contains_key(binding) && !is_entry_cancel {
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
                let Some(&place) = builder.binding_locals.get(binding) else {
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
                plan.drops.extend(extra.iter().cloned());
            }
        }
    }

    (
        ElaboratedMirFunction {
            name: checked.name.clone(),
            return_ty: checked.return_ty.clone(),
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
enum PayloadNeutralized {
    /// No neutralization on any reaching path.
    No,
    /// Neutralized on every reaching path: later drops of the local no-op.
    Yes,
    /// Neutralized on some-but-not-all reaching paths: later drops are
    /// ambiguous (fire on the un-neutralized paths only).
    Maybe,
}

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

/// Runtime-ABI symbols that CONSUME (take ownership of) exactly one argument:
/// the `*_move` copy-in family byte-transfers the element's heap into the
/// collection. Returns the consumed argument's index in C-ABI order.
fn runtime_consumed_arg_index(symbol: &str) -> Option<usize> {
    match symbol {
        // hew_vec_push_owned_move(v, data): `data`'s heap moves into the Vec.
        "hew_vec_push_owned_move" => Some(1),
        // hew_vec_set_owned_move(v, index, data): `data`'s heap moves in.
        "hew_vec_set_owned_move" => Some(2),
        _ => None,
    }
}

/// Shared read-only context for the balance transfer functions.
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
    /// Exact call/await result generations whose projected-slot neutralize is
    /// only a partial transfer; the remaining carrier still needs a drop.
    partial_transfer_carrier_mints: &'a HashSet<u32>,
}

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
        Instr::NeutralizePayloadSlot { place, .. } => {
            if let Some(root) = cx
                .tracked_root(*place)
                .or_else(|| cx.tracked_carrier(*place))
            {
                if payload_carrier_local(*place).is_some()
                    && cx.partial_transfer_carrier_mints.contains(&root)
                {
                    // The selected payload moved, but this minted call carrier
                    // still owns its shell and unselected slots. Only a later
                    // carrier drop discharges the whole-generation obligation.
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
            let consumed = runtime_consumed_arg_index(call.symbol());
            for (i, arg) in call.args().iter().enumerate() {
                if let Some(root) = cx.tracked_root(*arg) {
                    if consumed == Some(i) {
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
        SuspendKind::StreamSend { .. }
        | SuspendKind::Sleep { .. }
        | SuspendKind::SleepUntil { .. } => Vec::new(),
    }
}

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

/// The mint set: every heap-owning owned local from the per-function
/// registration ledger (INCLUDING entries dispositioned off `ScopeExit` —
/// the dispositions are under test), minus the trusted exclusions:
/// `Disposition::AliasOf` interior aliases (not independent mints) and
/// parameter slots (by-value params are caller-retained `CoW` borrows — the
/// caller owns the release; A278).
fn tracked_obligation_locals_with_sites(
    builder: &Builder,
    blocks: &[BasicBlock],
) -> (BTreeMap<u32, String>, BTreeMap<u32, SiteId>) {
    let mut tracked: BTreeMap<u32, String> = BTreeMap::new();
    let mut mint_sites: BTreeMap<u32, SiteId> = BTreeMap::new();
    for entry in builder.owned_locals_ledger() {
        if matches!(entry.disposition, Disposition::AliasOf) {
            continue;
        }
        // Re-apply the heap-ownership authority: the seed test admits every
        // non-BitCopy value class, which includes heap-free direct enums
        // (`Result<i64, AskError>`, `Colour`) that carry NO release
        // obligation. Track a binding only when its type transitively owns
        // heap (`ty_owns_heap` — the single structural authority) or its
        // class carries a non-heap drop ritual (`@resource` close /
        // `@linear` consume — the all-bitcopy resource record case).
        let class = ValueClass::of_ty(&entry.ty, &builder.type_classes);
        let owns_heap = crate::model::ty_owns_heap_mir(
            &entry.ty,
            &builder.record_field_orders,
            &builder.enum_layouts,
        );
        if !owns_heap && !matches!(class, ValueClass::AffineResource | ValueClass::Linear) {
            continue;
        }
        let Some(place) = builder.binding_locals.get(&entry.binding) else {
            continue;
        };
        let Some(local) = base_local(*place) else {
            continue;
        };
        if builder.parameter_locals.contains(&local) {
            continue;
        }
        let binding_site = blocks.iter().flat_map(|block| &block.statements).find_map(
            |statement| match statement {
                MirStatement::Bind { binding, site, .. } if *binding == entry.binding => {
                    Some(*site)
                }
                _ => None,
            },
        );
        let diagnostic = builder.call_scrutinee_diagnostics.get(&local);
        let name = diagnostic.map_or_else(|| entry.name.clone(), |(_, label)| label.clone());
        let site = diagnostic.map(|(site, _)| *site).or(binding_site);
        tracked.entry(local).or_insert(name);
        if let Some(site) = site {
            mint_sites.entry(local).or_insert(site);
        }
    }
    (tracked, mint_sites)
}

fn tracked_obligation_locals(builder: &Builder) -> BTreeMap<u32, String> {
    tracked_obligation_locals_with_sites(builder, &[]).0
}

/// S1 obligation-balance validation over one elaborated function. See the
/// module-section comment above for the model; called at the `mod.rs` gate
/// site immediately after `validate_drop_plan`. Additive sibling pass —
/// `validate_drop_plan` is not consulted or modified.
#[must_use]
pub(super) fn validate_obligation_balance(
    elab: &ElaboratedMirFunction,
    raw: &RawMirFunction,
    builder: &Builder,
) -> Vec<MirCheck> {
    let (mut tracked, mut mint_sites) = tracked_obligation_locals_with_sites(builder, &raw.blocks);
    // Structural parameter exclusion: `locals[0..params.len()]` ARE the
    // parameter slots (the RawMirFunction invariant). Synthesized bodies can
    // register a param-backed binding without a `parameter_locals` entry;
    // by-value params are caller-retained borrows either way.
    let n_params = u32::try_from(raw.params.len()).unwrap_or(u32::MAX);
    tracked.retain(|local, _| *local >= n_params);
    mint_sites.retain(|local, _| tracked.contains_key(local));
    if tracked.is_empty() {
        return Vec::new();
    }
    // Rendered types of the tracked locals, keyed by root, for the registry
    // scoping discriminator carried on each under-release finding. The type
    // narrows an allowlist entry to its minting site so a same-named local of
    // a different type in another compilation unit cannot ride it.
    let local_types: BTreeMap<u32, String> = tracked
        .keys()
        .filter_map(|&root| {
            raw.locals
                .get(root as usize)
                .map(|ty| (root, format!("{ty}")))
        })
        .collect();
    let partial_transfer_carrier_mints = builder
        .call_scrutinee_carrier_mint_locals
        .iter()
        .copied()
        .filter(|local| {
            raw.locals.get(*local as usize).is_none_or(|ty| {
                !super::composite_own::direct_payload_has_registered_resource_record(
                    ty,
                    &builder.enum_layouts,
                    &builder.lifecycle_registry,
                )
            })
        })
        .collect();
    validate_obligation_balance_with(
        elab,
        &raw.blocks,
        &raw.suspend_kinds,
        &tracked,
        (&local_types, &mint_sites),
        &builder.parameter_locals,
        &partial_transfer_carrier_mints,
    )
}

/// Decomposed core of [`validate_obligation_balance`] — the unit-test entry
/// (hand-constructed blocks + drop plans + tracked set, no `Builder`).
/// Computes the default fixpoint iteration cap and forwards to
/// [`validate_obligation_balance_capped`].
fn validate_obligation_balance_with(
    elab: &ElaboratedMirFunction,
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    tracked_in: &BTreeMap<u32, String>,
    diagnostic_info: (&BTreeMap<u32, String>, &BTreeMap<u32, SiteId>),
    parameter_locals: &HashSet<u32>,
    partial_transfer_carrier_mints: &HashSet<u32>,
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
        partial_transfer_carrier_mints,
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
fn validate_obligation_balance_capped(
    elab: &ElaboratedMirFunction,
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    tracked_in: &BTreeMap<u32, String>,
    diagnostic_info: (&BTreeMap<u32, String>, &BTreeMap<u32, SiteId>),
    parameter_locals: &HashSet<u32>,
    partial_transfer_carrier_mints: &HashSet<u32>,
    iteration_cap: usize,
) -> Vec<MirCheck> {
    use std::collections::VecDeque;

    let (local_types, mint_sites) = diagnostic_info;
    let mut findings = Vec::new();
    if blocks.is_empty() || tracked_in.is_empty() {
        return findings;
    }

    let alias_to = collect_payload_alias_map(blocks);
    let retained_move_sites = collect_retained_move_sites(blocks);
    let cow_handoff_commit_sites = collect_cow_handoff_commit_sites(blocks);
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
        partial_transfer_carrier_mints,
    };

    // Scope-exit releases ride the NORMAL-continuation exit plans (a
    // `goto[bbN->bbM]` edge closing an inner scope carries real drops), so
    // those plans participate in the dataflow at their owning block.
    // Exception edges (`Panic` / `Cancel`) fire only on unwind. `Return`
    // and `Suspend` plans are folded at their terminal verdicts: a suspend
    // plan belongs solely to the coro.destroy abandon edge and must never
    // discharge the still-live frame state flowing into resume.
    let mut edge_drops: HashMap<u32, Vec<&ElabDrop>> = HashMap::new();
    for (exit, plan) in &elab.drop_plans {
        if matches!(
            exit,
            ExitPath::Return { .. }
                | ExitPath::Panic { .. }
                | ExitPath::Cancel { .. }
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
            _ => continue,
        };
        if !reachable.contains(&block) {
            continue;
        }
        let Some(block_state) = exit_states.get(&block) else {
            continue;
        };
        let mut state = block_state.clone();
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
            hard: mint_provenance.is_blocking(),
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

/// Discharge-authority corroboration pin (D159 dual-carrier / L211). INDEPENDENT
/// of the carried authority: it re-derives, from the primitive `Instr` stream
/// ALONE, whether each named `transferee` is a real destination the routing
/// actually writes, and reports drift when the two carriers of the one
/// ownership-transfer fact disagree.
///
/// S1's [`validate_obligation_balance`] does NOT read these facts — it re-derives
/// the discharge set from the primitive stream and never consults the carried
/// authority (independence preserved; a ledger-trusting validator inherits ledger
/// bugs). This is a THIRD pass comparing the carried authority against a
/// from-primitives re-derivation. A `transferee` the stream never writes is a
/// fabricated transfer — the routing-vs-disposition drift class (S1889-F3).
pub(super) fn validate_discharge_authority_corroboration(
    elab: &ElaboratedMirFunction,
    raw: &RawMirFunction,
) -> Vec<MirCheck> {
    validate_discharge_authority_corroboration_over(&elab.name, &raw.blocks)
}

/// Testable core of [`validate_discharge_authority_corroboration`] — hand-
/// constructed blocks, no `RawMirFunction`.
#[allow(
    clippy::too_many_lines,
    reason = "one corroboration proof keeps its primitive routing facts and findings together"
)]
fn validate_discharge_authority_corroboration_over(
    function: &str,
    blocks: &[BasicBlock],
) -> Vec<MirCheck> {
    // Carrier 2 (independent): every local the primitive stream writes through
    // Move/WitnessMove, plus each exact source/destination pair written by a
    // tuple or record constructor. Derived WITHOUT reading any
    // NeutralizePayloadSlot authority.
    let mut move_destinations: HashSet<u32> = HashSet::new();
    let mut aggregate_member_destinations: HashSet<(Place, Place)> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            match instr {
                Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. } => {
                    if let Some(local) = whole_owner_local(*dest) {
                        move_destinations.insert(local);
                    }
                    if let Place::MachineVariant { local, .. } | Place::EnumVariant { local, .. } =
                        dest
                    {
                        aggregate_member_destinations.insert((*src, Place::Local(*local)));
                    }
                }
                Instr::TupleConstruct { elements, dest } => {
                    aggregate_member_destinations
                        .extend(elements.iter().map(|source| (*source, *dest)));
                }
                Instr::RecordInit { fields, dest, .. } => {
                    aggregate_member_destinations
                        .extend(fields.iter().map(|(_offset, source)| (*source, *dest)));
                }
                Instr::RecordFieldStore { record, src, .. } => {
                    aggregate_member_destinations.insert((*src, *record));
                }
                _ => {}
            }
        }
    }
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                let (Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. }) = instr
                else {
                    continue;
                };
                let Some(dest_local) = whole_owner_local(*dest) else {
                    continue;
                };
                let inherited: Vec<Place> = aggregate_member_destinations
                    .iter()
                    .filter_map(|(source, aggregate)| {
                        (whole_owner_local(*source) == Some(dest_local)).then_some(*aggregate)
                    })
                    .collect();
                for aggregate in inherited {
                    changed |= aggregate_member_destinations.insert((*src, aggregate));
                }
            }
        }
        if !changed {
            break;
        }
    }
    let mut findings = Vec::new();
    for block in blocks {
        for instr in &block.instructions {
            // Carrier 1: the carried transferee fact.
            let Instr::NeutralizePayloadSlot {
                place,
                transferee: Some(transferee),
                authority,
            } = instr
            else {
                continue;
            };
            let Some(dest_local) = whole_owner_local(*transferee) else {
                continue;
            };
            let corroborated = if matches!(
                authority,
                crate::model::NeutralizeAuthority::ReturnedAggregateMemberConsume
                    | crate::model::NeutralizeAuthority::AggregateMemberConsume
            ) {
                aggregate_member_destinations.contains(&(*place, *transferee))
            } else {
                move_destinations.contains(&dest_local)
            };
            if !corroborated {
                let primitive = if matches!(
                    authority,
                    crate::model::NeutralizeAuthority::ReturnedAggregateMemberConsume
                        | crate::model::NeutralizeAuthority::AggregateMemberConsume
                ) {
                    format!(
                        "the primitive instruction stream never constructs {transferee:?} from \
                         source {place:?}"
                    )
                } else {
                    format!(
                        "the primitive instruction stream never moves any value into \
                         local_{dest_local}"
                    )
                };
                findings.push(MirCheck::DischargeAuthorityDrift {
                    function: function.to_string(),
                    block: block.id,
                    name: format!("local_{dest_local}"),
                    reason: format!(
                        "NeutralizePayloadSlot ({authority:?}) names transferee local_{dest_local} \
                         as the new owner of the neutralized slot, but {primitive}: the carried \
                         transfer fact and the actual routing disagree (dual-carrier drift)"
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
/// True when `ty` is an owned-HANDLE LEAF the W3.053 gate guards: a
/// `Generator`/`AsyncGenerator` context, a `CancellationToken`, or a
/// `Resource`-marker builtin handle (Stream/Sink/Duplex/SendHalf/RecvHalf/
/// `LambdaActorHandle`). Each owns a single runtime context released only by its
/// handle drop, so aliasing it into an aggregate / container / storing call
/// creates the two-free hazard this gate guards.
///
/// Deliberately EXCLUDES the NON-OWNING actor-pid leaves
/// (`Pid`/`LocalPid`) and the inline `RemotePid` identity aggregate. None has
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
/// Fail-closed sole-owner narrowing for closure-pair drops. Starts from the
/// `Let`-admitted `closure_pair_owned` ledger and KEEPS a binding only when
/// its backing local is never read as a source operand by anything other
/// than the closure-call drivers' callee read (`Instr::CallClosure` /
/// `Terminator::SuspendingCallClosure` — calling the pair is the borrow it
/// exists for). Every other read aliases or moves the pair bits out of the
/// slot with no retain:
///
/// - `Move { dest: ReturnSlot }` — the caller owns the env now (function
///   tails do not emit a consume fact, so an exit-state gate alone would
///   double-free the returned pair);
/// - a call argument — the callee may return or store the same pair
///   (`let b = id_fn(a)` would otherwise free one env twice);
/// - `RecordInit` / aggregate ingress — a nested closure capturing the pair
///   or a record/Vec storing it owns the env through the aggregate.
///
/// Excluded bindings leak (as before this fix); they never double-free.
fn derive_closure_pair_drop_allowed(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    closure_pair_owned: &HashSet<BindingId>,
    binding_locals: &HashMap<BindingId, Place>,
) -> HashSet<BindingId> {
    if closure_pair_owned.is_empty() {
        return HashSet::new();
    }
    let mut aliased: HashSet<u32> = HashSet::new();
    let mark = |p: Place, aliased: &mut HashSet<u32>| {
        if let Some(l) = base_local(p) {
            aliased.insert(l);
        }
    };
    for block in blocks {
        for instr in &block.instructions {
            match instr {
                Instr::CallClosure { args, .. } => {
                    // The callee read is benign; args may alias a pair out.
                    for p in args {
                        mark(*p, &mut aliased);
                    }
                }
                _ => {
                    for p in instr_source_places(instr) {
                        mark(p, &mut aliased);
                    }
                }
            }
        }
        match &block.terminator {
            // The suspendable-callee driver (now a bare `Suspend` carrying a
            // `SuspendKind::CallClosure`) marks only its forwarded `args` as
            // aliased — the closure pair (`callee`) is a borrowed read, NOT
            // aliased out, so it stays drop-eligible. Folding it into the `other`
            // arm below would over-mark `callee` (which `terminator_source_places`
            // reports as a CallClosure source), wrongly excluding the pair.
            Terminator::Suspend { .. }
                if matches!(
                    suspend_kinds.get(&block.id),
                    Some(SuspendKind::CallClosure { .. })
                ) =>
            {
                if let Some(SuspendKind::CallClosure { args, .. }) = suspend_kinds.get(&block.id) {
                    for p in args {
                        mark(*p, &mut aliased);
                    }
                }
            }
            other => {
                for p in terminator_source_places(other, suspend_kinds.get(&block.id)) {
                    mark(p, &mut aliased);
                }
            }
        }
    }
    closure_pair_owned
        .iter()
        .filter(|binding| {
            binding_locals
                .get(binding)
                .and_then(|place| base_local(*place))
                .is_some_and(|local| !aliased.contains(&local))
        })
        .copied()
        .collect()
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
    returned_aggregate_members: &HashSet<BindingId>,
    consumed_local_aggregate_members: &HashSet<BindingId>,
    spawn_consumed_handle_members: &HashSet<BindingId>,
    closure_pair_drop_allowed: &HashSet<BindingId>,
    closure_vec_drop_allowed: &HashSet<BindingId>,
    plain_vec_drop_allowed: &HashSet<BindingId>,
    indirect_enum_drop_allowed: &HashSet<BindingId>,
    affine_release_flags: &HashMap<BindingId, Place>,
    overwrite_guard_flags: &HashMap<BindingId, Place>,
    collection_drop_flags: &HashMap<BindingId, Place>,
    actor_message_cow_drop_flags: &HashMap<BindingId, Place>,
    conditional_record_drop_flags: &HashMap<BindingId, Place>,
    projected_payload_overwrite_flags: &HashMap<BindingId, Place>,
    projection_alias_tainted: &HashSet<u32>,
    borrowed_builtin_handle_projection_aliases: &HashSet<BindingId>,
    collection_borrow_getter_aliases: &HashSet<u32>,
) -> Vec<ElabDrop> {
    let mut drops = Vec::new();
    for (binding, _name, ty) in owned_locals.iter().rev() {
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
        // W5.021 (defect #1) — a member handed to the caller through a returned
        // aggregate is owned by the caller now; the callee must NOT drop it or
        // it double-frees (the value-flow `derive_returned_aggregate_member_
        // bindings` authority covers every return shape, syntactic or not).
        // Skip BEFORE any drop-class arm so no class can re-admit it.
        if returned_aggregate_members.contains(binding) {
            continue;
        }
        // W3.053 — an owned handle moved into a LOCAL aggregate and then
        // extracted-and-consumed back out (for-in / `let` extraction) is owned by
        // the downstream consumer now; the source binding must NOT also drop it or
        // it double-frees the ctx the consumer's inline `hew_gen_coro_destroy` already
        // releases (the value-flow `derive_consumed_local_aggregate_member_
        // bindings` authority). Field-precise, so a no-consume sibling field keeps
        // the source binding's own sole drop. Skip BEFORE any drop-class arm.
        if consumed_local_aggregate_members.contains(binding) {
            continue;
        }
        // CAP-08 — an owned handle-leaf moved into an actor initial-state record
        // consumed by `SpawnActor` is owned by the spawned actor now: its
        // synthesised `state_drop_fn` frees the handle exactly once
        // (Stream→`hew_stream_close` / Sink→`hew_sink_close`). The M-COW spine
        // byte-copies the handle into the state record with no retain, so the
        // source binding must NOT also drop it (that is the double-free the
        // W3.053 gate refuses when this proof is absent). The
        // `derive_spawn_consumed_handle_bindings` authority admits only a handle
        // whose single owning ingress is the spawn-state record; the gate
        // consumes the SAME set via `source_excluded`. Skip BEFORE any drop-class
        // arm so the unconditional `AffineResource` handle drop below cannot
        // re-admit it. LESSONS: raii-null-after-move, cleanup-all-exits,
        // boundary-fail-closed.
        if spawn_consumed_handle_members.contains(binding) {
            continue;
        }
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
                guard: collection_drop_flags.get(binding).copied(),
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
                guard: collection_drop_flags.get(binding).copied(),
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
                guard: collection_drop_flags.get(binding).copied(),
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
                guard: actor_message_cow_drop_flags.get(binding).copied(),
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
                guard: overwrite_guard_flags.get(binding).copied(),
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
        if owned_record_drop_allowed.contains(binding) {
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
                guard: affine_release_flags
                    .get(binding)
                    .or_else(|| conditional_record_drop_flags.get(binding))
                    .copied(),
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
        if closure_pair_drop_allowed.contains(binding) && ty_is_closure_pair(ty) {
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
                let guard = affine_release_flags.get(binding).copied();
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
                            guard: actor_message_cow_drop_flags
                                .get(binding)
                                .or_else(|| projected_payload_overwrite_flags.get(binding))
                                .copied(),
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
/// `BlockKind::Cleanup` block. Each block's terminator maps to one
/// `(ExitPath, DropPlan)` entry. `Return`-terminated blocks narrow
/// the function-wide LIFO `lifo` sequence to bindings whose state at
/// that block's exit is `Live` — bindings already `Consumed` on
/// every reaching path do not need their drop fired again
/// (LESSONS `raii-null-after-move`). `MaybeConsumed` at a Return
/// exit is rejected upstream by the move-checker; the elaborator
/// treats it as if `Live` for drop-plan purposes, but the program
/// would have already been rejected before reaching codegen so the
/// drop list is informational.
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
    binding_scope: &HashMap<BindingId, ScopeId>,
    loop_back_edge_blocks: &HashMap<u32, ScopeId>,
    projection_alias_tainted: &HashSet<u32>,
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

    // Map each owned-local's Place back to its BindingId so the
    // per-exit filter can consult exit_states. The drops in `lifo`
    // already carry the binding's Place but not its id; reverse the
    // builder's `binding_locals` (BindingId -> Place) is the cleanest
    // bridge. Builds only as large as there are owned bindings.
    //
    // Adoption/transfer/machine-synth seams legitimately point two bindings
    // at one `Place`, so this reversal is not always injective. `.collect()`
    // over a `HashMap` iteration order would let a random one win —
    // `RandomState`-nondeterministic across runs, violating the compiler's
    // determinism doctrine. Fold with an explicit tie-break instead: the
    // lowest `BindingId` (first-declared) wins.
    let mut place_to_binding: std::collections::HashMap<Place, BindingId> =
        std::collections::HashMap::with_capacity(binding_locals.len());
    for (&binding, &place) in binding_locals {
        place_to_binding
            .entry(place)
            .and_modify(|existing| *existing = (*existing).min(binding))
            .or_insert(binding);
    }

    // Payload-alias → carrier composite map: `Ok(w)` / `Some(s)` binders whose
    // heap ownership was NOT transferred out by a `NeutralizePayloadSlot` are
    // non-owning interior aliases of the composite they were destructured from.
    // Reuses the exact authority the obligation checker folds discharges
    // through (`collect_payload_alias_map`), so the elaboration's exclusion and
    // the checker's balance accounting agree on which binders alias which
    // carrier.
    let payload_alias_carrier = collect_payload_alias_map(blocks);

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

    // Narrow the function-wide LIFO to the drops whose owning binding is
    // live (`Live` / `MaybeConsumed` / `AliasedIntoAggregate`) in `state_map`.
    // A binding `Consumed` (moved out) or `Uninit` (not yet, or never,
    // constructed) on the reaching path is excluded — firing its drop would
    // double-free a moved value or free/demonitor an uninitialised slot.
    let filter_drops_by_state = |state_map: &std::collections::BTreeMap<
        hew_hir::BindingId,
        dataflow::BindingState,
    >|
     -> Vec<ElabDrop> {
        let live: Vec<ElabDrop> = drops_template
            .iter()
            .filter(|drop| match place_to_binding.get(&drop.place) {
                Some(binding) => matches!(
                    state_map
                        .get(binding)
                        .copied()
                        .unwrap_or(dataflow::BindingState::Uninit),
                    dataflow::BindingState::Live
                        | dataflow::BindingState::MaybeConsumed(_)
                        | dataflow::BindingState::AliasedIntoAggregate(_)
                ),
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
        filter_drops_by_state(state_map)
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
        filter_drops_by_state(state_map)
    };

    // Per-iteration drops for a loop-body back-edge `Goto`. Restricts
    // `drops_for_exit` to bindings declared in this loop body's scope so the
    // back-edge releases ONLY per-iteration bindings (`let opt = await
    // rx.recv();` in a `while`, `let item = ...` in a `loop`) and never the
    // outer-scope bindings whose drop belongs at function exit (the receiver
    // itself, function parameters, bindings declared before the loop). The
    // body-scope match is exact, not transitive: nested block-scope bindings
    // already self-drop via the existing scope-exit pass when their inner
    // block closes, so this back-edge sees only the body's own bindings as
    // Live in `exit_states`.
    //
    // The escape / first-iteration / pass-by-value double-free corner cases
    // are handled by `drops_for_exit`'s `BindingState` filter (the same one
    // that gates `Return`/`Cancel`): a binding `Consumed` mid-body (moved out
    // to `break x;` or to a by-value call) is excluded — the consumer owns
    // the release; a binding `Uninit` on a path that misses its
    // initialisation (first iteration before the let runs) is excluded — no
    // value, nothing to free. So the back-edge plan is structurally safe.
    let drops_for_back_edge = |block_id: u32, body_scope: ScopeId| -> Vec<ElabDrop> {
        drops_for_exit(block_id)
            .into_iter()
            .filter(|drop| match place_to_binding.get(&drop.place) {
                Some(binding) => binding_scope.get(binding).copied() == Some(body_scope),
                // Unknown binding mapping — conservatively skip the
                // back-edge drop. The function-exit / cancel plans still
                // hold any unbound drop entries via their own paths, so a
                // miss here at worst leaves the value for the function-exit
                // pass to handle (leak-not-double-free posture matching the
                // existing `drops_for_exit` None arm).
                None => false,
            })
            .collect()
    };

    // Scope-close drops on a forward (non-back-edge) `Goto`. A binding bound on
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
    let target_entry_keeps_alive = |target: u32, binding: &BindingId| -> bool {
        entry_states
            .get(&target)
            .and_then(|state_map| state_map.get(binding))
            .copied()
            .is_some_and(|state| {
                matches!(
                    state,
                    dataflow::BindingState::Live
                        | dataflow::BindingState::MaybeConsumed(_)
                        | dataflow::BindingState::AliasedIntoAggregate(_)
                )
            })
    };
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
    let drops_for_scope_close_goto = |block_id: u32, target: u32| -> Vec<ElabDrop> {
        drops_for_exit(block_id)
            .into_iter()
            .filter(|drop| {
                if let Some(l) = base_local(drop.place) {
                    if projection_alias_tainted.contains(&l) {
                        return false;
                    }
                }
                match place_to_binding.get(&drop.place) {
                    // Drop here only when the binding leaves scope crossing this
                    // edge (not kept alive at the target). Kept-alive → its later
                    // exit owns the release (no double-free).
                    Some(binding) => !target_entry_keeps_alive(target, binding),
                    // No binding mapping → conservatively skip (leak-not-double-free,
                    // matching the back-edge None arm).
                    None => false,
                }
            })
            .collect()
    };

    for block in blocks {
        let block_id = block.id;
        let plan = match &block.terminator {
            Terminator::Return => (
                ExitPath::Return { block: block_id },
                DropPlan {
                    drops: drops_for_exit(block_id),
                },
            ),
            Terminator::Goto { target } => {
                // Per-iteration drops fire on loop-body back-edges; a forward
                // `Goto` that closes an inner (match/if-arm/block) scope releases
                // the bindings that leave scope crossing the join
                // (`drops_for_scope_close_goto`) — the bindings whose drop the
                // function-exit pass would otherwise miss because the join meets
                // them to `Uninit`. Bindings still live on the join stay for the
                // eventual exit.
                let drops = match loop_back_edge_blocks.get(&block_id) {
                    Some(&body_scope) => drops_for_back_edge(block_id, body_scope),
                    None => drops_for_scope_close_goto(block_id, *target),
                };
                (
                    ExitPath::Goto {
                        block: block_id,
                        target: *target,
                    },
                    DropPlan { drops },
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
                msg_type: _,
                value: _,
                next,
                arg_modes: _,
                cleanup_plan: _,
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
        plans.push(plan);
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
mod drop_admission_type_shape_pins {
    //! Frozen-verdict pins for the type-shape axis of MIR drop admission: the
    //! owned-locals seed gate, the collection-handle release bucket, and the
    //! two release-symbol pickers. Each pin enumerates its own function's
    //! decision domain and freezes today's verdict as a literal, so a moved
    //! admission decision is a named test failure — never a silent
    //! reclassification. An admission that widens over-drops (double-free,
    //! the worst outcome); one that narrows leaks.
    use super::*;
    use crate::ownership::{DropClass, HeapLeaf};

    fn vec_of(elem: ResolvedTy) -> ResolvedTy {
        ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![elem])
    }

    fn named(name: &str) -> ResolvedTy {
        ResolvedTy::named_user(name, vec![])
    }

    fn hashmap_str_i64() -> ResolvedTy {
        ResolvedTy::named_builtin(
            "HashMap",
            BuiltinType::HashMap,
            vec![ResolvedTy::String, ResolvedTy::I64],
        )
    }

    fn hashset_i64() -> ResolvedTy {
        ResolvedTy::named_builtin("HashSet", BuiltinType::HashSet, vec![ResolvedTy::I64])
    }

    fn generator_i64() -> ResolvedTy {
        ResolvedTy::named_builtin(
            "Generator",
            BuiltinType::Generator,
            vec![ResolvedTy::I64, ResolvedTy::Unit],
        )
    }

    fn bare_fn() -> ResolvedTy {
        ResolvedTy::Function {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
        }
    }

    fn empty_capture_closure() -> ResolvedTy {
        ResolvedTy::Closure {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
            captures: vec![],
        }
    }

    /// `indirect enum Foo { A(i64); B }` — a heap-boxed node whose per-element
    /// `Vec` release is unwired (`Unsupported(NoReleaseProtocol)`).
    fn builder_with_indirect_enum_foo() -> Builder {
        Builder {
            enum_layouts: vec![crate::model::EnumLayout {
                name: "Foo".to_string(),
                tag_width: 1,
                variants: vec![
                    crate::model::MachineVariantLayout {
                        name: "A".to_string(),
                        field_tys: vec![ResolvedTy::I64],
                        field_names: vec![],
                    },
                    crate::model::MachineVariantLayout {
                        name: "B".to_string(),
                        field_tys: vec![],
                        field_names: vec![],
                    },
                ],
                is_indirect: true,
            }],
            ..Builder::default()
        }
    }

    /// Builder carrying the field-drop classifier corpus's registered
    /// layouts: user records over every slot class, an inline enum, and the
    /// indirect enums `Foo` (from `builder_with_indirect_enum_foo`) and the
    /// self-recursive `ListNode`.
    fn builder_for_field_drop_classifier() -> Builder {
        let mut builder = builder_with_indirect_enum_foo();
        builder.enum_layouts.push(crate::model::EnumLayout {
            name: "Msg".to_string(),
            tag_width: 1,
            variants: vec![
                crate::model::MachineVariantLayout {
                    name: "Text".to_string(),
                    field_tys: vec![ResolvedTy::String],
                    field_names: vec![],
                },
                crate::model::MachineVariantLayout {
                    name: "Ping".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
            ],
            is_indirect: false,
        });
        builder.enum_layouts.push(crate::model::EnumLayout {
            name: "ListNode".to_string(),
            tag_width: 1,
            variants: vec![
                crate::model::MachineVariantLayout {
                    name: "Cons".to_string(),
                    field_tys: vec![ResolvedTy::I64, named("ListNode")],
                    field_names: vec![],
                },
                crate::model::MachineVariantLayout {
                    name: "Nil".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
            ],
            is_indirect: true,
        });
        for (record, fields) in [
            (
                "Row",
                vec![
                    ("name".to_string(), ResolvedTy::String),
                    ("n".to_string(), ResolvedTy::I64),
                ],
            ),
            (
                "Outer",
                vec![
                    ("row".to_string(), named("Row")),
                    ("k".to_string(), ResolvedTy::I64),
                ],
            ),
            ("HoldsFoo", vec![("f".to_string(), named("Foo"))]),
            (
                "HoldsBadVec",
                vec![("xs".to_string(), vec_of(named("Foo")))],
            ),
            (
                "HoldsSlice",
                vec![(
                    "s".to_string(),
                    ResolvedTy::Slice(Box::new(ResolvedTy::I64)),
                )],
            ),
            ("HoldsClosure", vec![("f".to_string(), bare_fn())]),
            (
                "HoldsToken",
                vec![("t".to_string(), ResolvedTy::CancellationToken)],
            ),
        ] {
            builder
                .record_field_orders
                .insert(record.to_string(), fields);
        }
        builder
    }

    /// The `FieldDropInPlace` admissibility classifier — the ONE predicate
    /// MIR admission and the drop-plan verifier consult — with the verdict
    /// frozen per shape. Admission mirrors codegen's `emit_heap_slot_drop`
    /// dispatch: the five aggregate shapes over registered layouts admit
    /// when every reachable slot is dischargeable; leaf COW types stay on
    /// their own authority (refused at top level); everything the dispatcher
    /// fail-closes on (slices, dyn traits, closure pairs, affine handles,
    /// unwired `Vec` elements, unregistered layouts, free type params) is
    /// refused. A widened verdict here is a wrong-ABI free at codegen; a
    /// narrowed one is a lost capability — both are named test failures.
    #[test]
    fn field_drop_classifier_verdicts_are_frozen_per_shape() {
        let builder = builder_for_field_drop_classifier();

        let corpus: Vec<(&str, ResolvedTy, bool)> = vec![
            // Admitted aggregate shapes.
            ("record of string+i64", named("Row"), true),
            ("record nesting an admissible record", named("Outer"), true),
            ("record with indirect-enum field", named("HoldsFoo"), true),
            (
                "tuple of (string, i64)",
                ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
                true,
            ),
            (
                "fixed array of string",
                ResolvedTy::Array(Box::new(ResolvedTy::String), 3),
                true,
            ),
            ("inline enum with string payload", named("Msg"), true),
            ("indirect enum", named("Foo"), true),
            (
                "self-recursive indirect enum (cycle guard)",
                named("ListNode"),
                true,
            ),
            (
                "tuple with a wired Vec element",
                ResolvedTy::Tuple(vec![vec_of(ResolvedTy::I64)]),
                true,
            ),
            // Refused: a reachable slot the dispatcher cannot discharge.
            (
                "record with unwired Vec<indirect enum> field",
                named("HoldsBadVec"),
                false,
            ),
            ("record with slice field", named("HoldsSlice"), false),
            ("record with closure field", named("HoldsClosure"), false),
            (
                "record with affine-handle field",
                named("HoldsToken"),
                false,
            ),
            (
                "tuple with dyn-trait element",
                ResolvedTy::Tuple(vec![ResolvedTy::TraitObject {
                    traits: vec![hew_types::ResolvedTraitBound {
                        trait_name: "Display".to_string(),
                        args: vec![],
                        assoc_bindings: vec![],
                    }],
                }]),
                false,
            ),
            (
                "tuple with free type-param element",
                ResolvedTy::Tuple(vec![ResolvedTy::TypeParam {
                    name: "T".to_string(),
                }]),
                false,
            ),
            // Refused: leaf / non-aggregate top levels (the admission OR
            // keeps leaves on `project_field_inline_drop_symbol`; `string`'s
            // reroute onto the op is its own decision, not a classifier
            // verdict).
            ("string top level", ResolvedTy::String, false),
            ("bytes top level", ResolvedTy::Bytes, false),
            ("Vec top level", vec_of(ResolvedTy::I64), false),
            ("i64 top level", ResolvedTy::I64, false),
            (
                "slice top level",
                ResolvedTy::Slice(Box::new(ResolvedTy::I64)),
                false,
            ),
            ("unregistered named type", named("Ghost"), false),
            (
                "free type param top level",
                ResolvedTy::TypeParam {
                    name: "T".to_string(),
                },
                false,
            ),
        ];

        for (label, ty, admitted) in corpus {
            assert_eq!(
                builder.field_drop_in_place_admissible(&ty),
                admitted,
                "field-drop admissibility verdict moved for `{label}` \
                 ({ty:?}); a widened verdict reaches codegen with no in-place \
                 release (wrong-ABI / fail-closed error), a narrowed one \
                 regresses an admitted discharge shape to the NYI refusal"
            );
        }
    }

    /// The owned-locals seed gate — "does a binding of this TYPE oblige drop
    /// elaboration?" — with the verdict frozen per shape over every class
    /// `ValueClass::of_ty` can answer. Only `BitCopy` declines to seed; every
    /// other class (including the record-blind `Unknown` for unmarked user
    /// records — a known, preserved limitation) enters `owned_locals`.
    #[test]
    fn seed_gate_matches_value_class_authority() {
        let mut type_classes = hew_hir::TypeClassTable::new();
        type_classes.insert("CopyRec".to_string(), (ResourceMarker::BitCopy, None));
        type_classes.insert("Sock".to_string(), (ResourceMarker::Resource, None));
        type_classes.insert("Once".to_string(), (ResourceMarker::Linear, None));
        let builder = Builder {
            type_classes,
            ..Builder::default()
        };

        // (shape, type, seeds-drop-elaboration) — the verdict column is the
        // FROZEN admission decision; a row here may only change together with
        // a deliberate, reviewed seed-rule change.
        let corpus: Vec<(&str, ResolvedTy, bool)> = vec![
            // BitCopy — the only class that does NOT seed.
            ("i64 scalar", ResolvedTy::I64, false),
            ("bool scalar", ResolvedTy::Bool, false),
            ("duration", ResolvedTy::Duration, false),
            ("unit", ResolvedTy::Unit, false),
            (
                "instant builtin",
                ResolvedTy::named_builtin("instant", BuiltinType::Instant, vec![]),
                false,
            ),
            ("bitcopy-marked record", named("CopyRec"), false),
            // CowValue seeds.
            ("string", ResolvedTy::String, true),
            ("bytes", ResolvedTy::Bytes, true),
            ("builtin Vec", vec_of(ResolvedTy::I64), true),
            (
                "tuple",
                ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
                true,
            ),
            // PersistentShare seeds.
            ("bare fn", bare_fn(), true),
            ("empty-capture closure", empty_capture_closure(), true),
            (
                "dyn trait",
                ResolvedTy::TraitObject {
                    traits: vec![hew_types::ResolvedTraitBound {
                        trait_name: "Display".to_string(),
                        args: vec![],
                        assoc_bindings: vec![],
                    }],
                },
                true,
            ),
            // AffineResource seeds.
            ("cancellation token", ResolvedTy::CancellationToken, true),
            ("generator handle", generator_i64(), true),
            ("resource-marked named", named("Sock"), true),
            // Linear seeds (its release is the move-checker's MustConsume,
            // but membership in the candidate ledger is what is decided here).
            (
                "task handle",
                ResolvedTy::Task(Box::new(ResolvedTy::I64)),
                true,
            ),
            ("linear-marked named", named("Once"), true),
            // View seeds (build_lifo_drops elaborates its no-retain no-op arm).
            (
                "borrow",
                ResolvedTy::Borrow {
                    pointee: Box::new(ResolvedTy::I64),
                },
                true,
            ),
            ("slice", ResolvedTy::Slice(Box::new(ResolvedTy::I64)), true),
            (
                "pointer",
                ResolvedTy::Pointer {
                    is_mutable: false,
                    pointee: Box::new(ResolvedTy::I64),
                },
                true,
            ),
            // Unknown seeds — the record-blind arm: an unmarked user record
            // classifies Unknown, not BitCopy, so it enters the ledger.
            ("unmarked named", named("Mystery"), true),
            (
                "type param",
                ResolvedTy::TypeParam {
                    name: "T".to_string(),
                },
                true,
            ),
        ];

        for (label, ty, seeds) in corpus {
            assert_eq!(
                builder.binding_seeds_drop_elaboration(&ty),
                seeds,
                "owned-locals seed verdict moved for `{label}` ({ty:?}); \
                 seeding decides drop-elaboration membership, so a flipped \
                 verdict is an over-drop (double-free) or an under-seed (leak)"
            );
            assert_eq!(
                builder.binding_seeds_drop_elaboration(&ty),
                ValueClass::of_ty(&ty, &builder.type_classes) != ValueClass::BitCopy,
                "the seed authority's verdict must remain the value-class \
                 seed for `{label}` ({ty:?})"
            );
        }
    }

    /// `ty_is_local_collection_handle` is a projection of the typed ownership
    /// classification: it answers `true` exactly when the decision's drop
    /// class is the `HashMap` / `HashSet` copy-on-write leaf. Corpus: every
    /// heap leaf the authority recognises, plus the user-Named collision
    /// negative (a user `type HashMap` shares the name but not the `builtin`
    /// discriminator and must never be mistaken for the runtime handle).
    #[test]
    fn collection_handle_predicate_projects_from_heap_leaf() {
        let records: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
        let type_classes = hew_hir::TypeClassTable::new();
        let ctx = OwnershipCtx::new(&records, &[], &type_classes);

        let corpus: Vec<(&str, ResolvedTy, bool)> = vec![
            ("string", ResolvedTy::String, false),
            ("bytes", ResolvedTy::Bytes, false),
            ("vec", vec_of(ResolvedTy::I64), false),
            ("hashmap", hashmap_str_i64(), true),
            ("hashset", hashset_i64(), true),
            ("generator", generator_i64(), false),
            ("cancellation token", ResolvedTy::CancellationToken, false),
            ("user-named HashMap collision", named("HashMap"), false),
        ];

        for (label, ty, expected) in corpus {
            assert_eq!(
                ty_is_local_collection_handle(&ty),
                expected,
                "collection-handle bucket membership moved for `{label}` ({ty:?})"
            );
            let projects = matches!(
                OwnershipDecision::classify(&ty, Place::Local(0), &ctx).drop_class(),
                Some(DropClass::CowHeapLeaf {
                    leaf: HeapLeaf::HashMap | HeapLeaf::HashSet
                })
            );
            assert_eq!(
                projects, expected,
                "`{label}` ({ty:?}): the typed classification and \
                 `ty_is_local_collection_handle` must answer identically — \
                 a future builtin collection added to one but not the other \
                 splits bucket admission from classification"
            );
        }

        // Symbol-agreement tripwire: the leaves' canonical release symbols are
        // exactly the two symbols the collection-handle bucket emits in
        // `build_lifo_drops` (via `drop_kind_for`).
        assert_eq!(
            HeapLeaf::HashMap.release_symbol(),
            "hew_hashmap_free_layout",
            "HashMap leaf release symbol must match the bucket's emission"
        );
        assert_eq!(
            HeapLeaf::HashSet.release_symbol(),
            "hew_hashset_free_layout",
            "HashSet leaf release symbol must match the bucket's emission"
        );
    }

    /// The complete release-verdict table for both Builder-side pickers —
    /// `generator_yield_drop_symbol` (matches the RAW type) and
    /// `project_field_inline_drop_symbol` (substitutes FIRST) — frozen per
    /// shape: the `Vec` arm over every `VecElementRelease` variant (both
    /// `FailClosedReason` arms represented), the defensive no-type-arg `Vec`,
    /// and the non-`Vec` arms.
    ///
    /// The `Unsupported(NoReleaseProtocol)` rows with no owned-ABI release
    /// (`Vec<bytes>`, `Vec<indirect enum>`) assert the FAIL-CLOSED verdict
    /// (`Unwired`): the per-element release for those shapes is unwired, so
    /// every consulting site must refuse the construct at compile time —
    /// never emit the buffer-only `hew_vec_free` over owned element nodes.
    /// The residual `Unsupported(UnenumeratedShape)` sub-domain deliberately
    /// keeps the buffer-only verdict, drawing the same boundary as the compile
    /// reject `unsupported_vec_element_walk`:
    ///   - `UnenumeratedShape` (`Vec<T>` unsubstituted): the element owns no
    ///     heap as a flat element, so the buffer free IS the complete
    ///     release — refusing would reject un-monomorphised generic bodies;
    ///
    /// A registered heap-owning record observed without this function's
    /// harvest key is instead classified harvest-independently and released
    /// through `hew_vec_free_owned`.
    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "the length is intrinsic: one frozen symbol matrix over every \
                  picker input shape, asserted against both pickers — splitting \
                  it would scatter the single-table proof across functions"
    )]
    fn yield_and_field_pickers_match_legacy_symbol_table() {
        use ReleaseSymbolVerdict::{NoDropPath, Unwired, Wired};

        let mut builder = builder_with_indirect_enum_foo();
        // A registered heap-owning record whose `Vec` is owned-ABI releasable
        // program-wide but whose key is NOT in this builder's per-function
        // harvest set — the boundary row for the releasable `Unsupported`
        // sub-domain.
        builder.record_field_orders.insert(
            "HeapRow".to_string(),
            vec![("s".to_string(), ResolvedTy::String)],
        );

        // (shape, type, generator-yield verdict, project-field verdict) —
        // every verdict column FROZEN. The two pickers agree on every row
        // here; the substitution-order asymmetry is pinned separately below.
        let corpus: Vec<(&str, ResolvedTy, ReleaseSymbolVerdict, ReleaseSymbolVerdict)> = vec![
            // Vec arm — Plain elements.
            (
                "Vec<i64> (Plain)",
                vec_of(ResolvedTy::I64),
                Wired("hew_vec_free"),
                Wired("hew_vec_free"),
            ),
            (
                "Vec<string> (Plain)",
                vec_of(ResolvedTy::String),
                Wired("hew_vec_free"),
                Wired("hew_vec_free"),
            ),
            // Vec arm — OwnedElement elements.
            (
                "Vec<Vec<i64>> (OwnedElement)",
                vec_of(vec_of(ResolvedTy::I64)),
                Wired("hew_vec_free_owned"),
                Wired("hew_vec_free_owned"),
            ),
            (
                "Vec<HashMap<string,i64>> (OwnedElement)",
                vec_of(hashmap_str_i64()),
                Wired("hew_vec_free_owned"),
                Wired("hew_vec_free_owned"),
            ),
            (
                "Vec<(string,i64)> (OwnedElement)",
                vec_of(ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64])),
                Wired("hew_vec_free_owned"),
                Wired("hew_vec_free_owned"),
            ),
            // Vec arm — ClosurePair elements.
            (
                "Vec<fn> (ClosurePair)",
                vec_of(bare_fn()),
                Wired("hew_vec_free_owned"),
                Wired("hew_vec_free_owned"),
            ),
            (
                "Vec<closure> (ClosurePair)",
                vec_of(empty_capture_closure()),
                Wired("hew_vec_free_owned"),
                Wired("hew_vec_free_owned"),
            ),
            // Vec arm — Unsupported elements with NO owned-ABI release: the
            // FAIL-CLOSED verdict. A buffer-only free over these element
            // nodes is a per-element leak, so the pickers refuse instead of
            // picking a symbol; every consulting site rejects at compile
            // time (see the test doc).
            (
                "Vec<bytes> (Unsupported/NoReleaseProtocol)",
                vec_of(ResolvedTy::Bytes),
                Unwired(FailClosedReason::NoReleaseProtocol),
                Unwired(FailClosedReason::NoReleaseProtocol),
            ),
            (
                "Vec<indirect enum> (Unsupported/NoReleaseProtocol)",
                vec_of(named("Foo")),
                Unwired(FailClosedReason::NoReleaseProtocol),
                Unwired(FailClosedReason::NoReleaseProtocol),
            ),
            // Vec arm — the residual Unsupported sub-domain that keeps the
            // buffer-only verdict (the boundary
            // `unsupported_vec_element_walk` draws; see the test doc).
            (
                "Vec<T> unsubstituted (Unsupported/UnenumeratedShape)",
                vec_of(ResolvedTy::TypeParam {
                    name: "T".to_string(),
                }),
                Wired("hew_vec_free"),
                Wired("hew_vec_free"),
            ),
            (
                "Vec<HeapRow> unharvested (Unsupported/NoReleaseProtocol, owned-ABI releasable)",
                vec_of(named("HeapRow")),
                Wired("hew_vec_free_owned"),
                Wired("hew_vec_free_owned"),
            ),
            // Vec arm — defensive no-type-arg fall-through.
            (
                "Vec with no type arg (defensive)",
                ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![]),
                Wired("hew_vec_free"),
                Wired("hew_vec_free"),
            ),
            // Non-Vec arms — must not move when the Vec arm reroutes.
            (
                "string",
                ResolvedTy::String,
                Wired("hew_string_drop"),
                Wired("hew_string_drop"),
            ),
            (
                "bytes",
                ResolvedTy::Bytes,
                Wired("hew_bytes_drop"),
                Wired("hew_bytes_drop"),
            ),
            // VecIter clone-out and the existing generator/receiver frame
            // contracts hand these collection values to the body as sole
            // owners. Their layout-aware releases close the common per-yield
            // lifecycle.
            (
                "HashMap",
                hashmap_str_i64(),
                Wired("hew_hashmap_free_layout"),
                Wired("hew_hashmap_free_layout"),
            ),
            (
                "HashSet",
                hashset_i64(),
                Wired("hew_hashset_free_layout"),
                Wired("hew_hashset_free_layout"),
            ),
            (
                "Generator",
                generator_i64(),
                NoDropPath,
                Wired("hew_gen_coro_destroy"),
            ),
            ("i64", ResolvedTy::I64, NoDropPath, NoDropPath),
            ("unmarked user record", named("Rec"), NoDropPath, NoDropPath),
        ];

        for (label, ty, want_yield, want_field) in corpus {
            assert_eq!(
                builder.generator_yield_drop_symbol(&ty),
                want_yield,
                "generator-yield release verdict moved for `{label}` ({ty:?})"
            );
            assert_eq!(
                builder.project_field_inline_drop_symbol(&ty),
                want_field,
                "project-field release verdict moved for `{label}` ({ty:?})"
            );
        }

        // The Unsupported rows above carry exactly the two fail-closed
        // reasons: the unwired release protocols and the anti-drift sentinel.
        assert_eq!(
            builder.classify_vec_element_release(&ResolvedTy::Bytes),
            VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol)
        );
        assert_eq!(
            builder.classify_vec_element_release(&named("Foo")),
            VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol)
        );
        assert_eq!(
            builder.classify_vec_element_release(&ResolvedTy::TypeParam {
                name: "T".to_string(),
            }),
            VecElementRelease::Unsupported(FailClosedReason::UnenumeratedShape)
        );
        // The releasable-boundary row rides `NoReleaseProtocol` too — it is
        // the `elem_is_owned_abi_releasable` exclusion, not the reason, that
        // keeps it off the fail-closed verdict.
        assert_eq!(
            builder.classify_vec_element_release(&named("HeapRow")),
            VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol)
        );
        assert!(builder.elem_is_owned_abi_releasable(&named("HeapRow")));
        assert!(!builder.elem_is_owned_abi_releasable(&named("Foo")));

        // Substitution-order asymmetry, frozen: `generator_yield_drop_symbol`
        // classifies the RAW type (a yield's type is already concrete at its
        // producer); `project_field_inline_drop_symbol` substitutes through
        // the monomorphisation map FIRST (a field type may still spell the
        // function's type parameter). With `T ↦ fn() -> unit` the two pickers
        // therefore answer differently for `Vec<T>` — harmonising them would
        // move release decisions.
        builder.subst = [("T".to_string(), bare_fn())].into_iter().collect();
        let vec_t = vec_of(ResolvedTy::TypeParam {
            name: "T".to_string(),
        });
        assert_eq!(
            builder.generator_yield_drop_symbol(&vec_t),
            Wired("hew_vec_free"),
            "the yield picker must classify the raw (unsubstituted) type"
        );
        assert_eq!(
            builder.project_field_inline_drop_symbol(&vec_t),
            Wired("hew_vec_free_owned"),
            "the field picker must substitute before classifying"
        );
    }

    /// The production (non-test) sources of the lower module. CRLF-normalised
    /// so a Windows checkout (`core.autocrlf=true`) still splits on the
    /// LF-anchored test-module boundary (mirrors `layout_key_shortening_guard`).
    fn production_source() -> String {
        [
            include_str!("mod.rs"),
            include_str!("drop_plan.rs"),
            include_str!("ownership.rs"),
            include_str!("scope.rs"),
            include_str!("expr.rs"),
            include_str!("pattern.rs"),
            include_str!("control_flow.rs"),
            include_str!("task.rs"),
            include_str!("actor.rs"),
            include_str!("closure_gen.rs"),
        ]
        .into_iter()
        .map(|src| {
            src.replace("\r\n", "\n")
                .split("\n#[cfg(test)]\nmod ")
                .next()
                .expect("lower module source has a non-test prefix")
                .to_string()
        })
        .collect::<Vec<_>>()
        .join("\n")
    }

    /// Structural inventory pin for the owned-locals seed fact: every
    /// occurrence of the non-`BitCopy` value-class polarity test in production
    /// code is named below, so a raw copy of the seed comparison cannot appear
    /// (or disappear) silently under any spelling — the whitespace-stripped
    /// scan catches line-wrapped and temp-variable forms alike.
    #[test]
    fn seed_fact_comparison_site_inventory_is_closed() {
        let squeezed: String = production_source()
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        // The seed fact has exactly one production spelling: the body of
        // `binding_seeds_drop_elaboration`, the authority the 11 seed sites
        // and the consume-side removal mirror all call.
        assert!(
            squeezed.contains("fnbinding_seeds_drop_elaboration"),
            "the owned-locals seed authority must exist in production code"
        );
        let count = squeezed.matches("!=ValueClass::BitCopy").count();
        // The complete allowlist of the non-`BitCopy` polarity test:
        //   - `binding_seeds_drop_elaboration` — the seed authority's own
        //     body, the single spelling of the seed fact;
        //   - `gen_env_capture_admissible` — generator-env capture
        //     flat-copyability, a DIFFERENT fact that must not follow a
        //     future seed-rule change;
        //   - the user-record value-class diagnostic reason builder — names
        //     the first non-BitCopy field in a rejection note (diagnostic
        //     wording, not admission).
        assert_eq!(
            count, 3,
            "a raw copy of the owned-locals seed comparison appeared in (or \
             an allowlisted use vanished from) lower module production code; \
             seed decisions route through `binding_seeds_drop_elaboration`, \
             never an inline class test — classify any change to this \
             population in the allowlist above deliberately"
        );
    }

    #[test]
    fn seed_fact_comparison_inventory_rejects_a_raw_counterfactual() {
        let squeezed: String = production_source()
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        let baseline = squeezed.matches("!=ValueClass::BitCopy").count();
        let counterfactual = format!("{squeezed}ifraw!=ValueClass::BitCopy{{}}");
        assert_eq!(baseline, 3, "the production inventory changed unexpectedly");
        assert_ne!(
            counterfactual.matches("!=ValueClass::BitCopy").count(),
            baseline,
            "a newly introduced raw seed comparison must make the inventory red"
        );
    }
}
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
#[derive(Default)]
struct UnderReleaseAggregate {
    blocks: Vec<u32>,
    exits: Vec<String>,
    mint_provenance: Option<crate::model::ObligationMintProvenance>,
    max_mints: u8,
    max_discharges: u8,
}
