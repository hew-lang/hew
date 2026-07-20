#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    base_local, check_duplex_split_state, compute_collection_interior_alias_taint,
    compute_projection_alias_taint, dataflow, derive_consumed_local_aggregate_member_bindings,
    derive_cow_fresh_borrowed_owner, derive_cow_sole_owner, derive_enum_composite_drop_allowed,
    derive_local_bytes_drop_allowed, derive_local_collection_drop_allowed,
    derive_owned_record_drop_allowed, derive_returned_aggregate_member_bindings,
    derive_spawn_consumed_handle_bindings, derive_tuple_composite_drop_allowed,
    instr_source_places, mangle_layout_key, place_is_interior_projection, place_refs_local,
    retained_string_terminator_drop_safe, short_name, terminator_source_places,
    user_record_layout_key, vec_iter_record_init_vec_source, BTreeMap, BasicBlock, BindingId,
    BlockKind, Builder, BuiltinType, CheckedMirFunction, ClosurePairRhs, DropKind, DropPlan,
    ElabBlock, ElabDrop, ElaboratedMirFunction, ExitPath, HashMap, HashSet, HirExpr, HirExprKind,
    Instr, IntentKind, LambdaCapture, MirCheck, MirDiagnostic, MirDiagnosticKind, MirStatement,
    Place, ResolvedRef, ResolvedTy, ResourceMarker, ScopeId, SuspendKind, Terminator,
    TraitObjectStorage, ValueClass, ENTRY_BLOCK_ID,
};

/// Project a Checked MIR finding to a `MirDiagnostic` for the CLI
/// rejection surface. `CheckedMirFunction::checks` is the single
/// source of truth for move/borrow/init legality; this function
/// adapts those findings to the older `MirDiagnostic` channel the
/// driver already consumes. Variants whose construction surface
/// isn't yet wired (`Aliasing`, `GeneratorBorrowAcrossYield`,
/// `ActorSendEscape`) cannot appear today; they yield `None` defensively.
#[allow(
    clippy::too_many_lines,
    reason = "one exhaustive MirCheck -> MirDiagnostic projection; each arm is a \
              single distinct mapping and splitting scatters the projection table"
)]
pub(super) fn check_to_diagnostic(check: &MirCheck) -> Option<MirDiagnostic> {
    match check {
        MirCheck::UseAfterConsume {
            binding,
            name,
            consumed_at,
            used_at,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::UseAfterConsume {
                binding: *binding,
                name: name.clone(),
                consumed_at: *consumed_at,
                used_at: *used_at,
            },
            note: "binding is used after an owned value move in checked MIR".to_string(),
        }),
        MirCheck::InitialisedBeforeUse {
            binding,
            name,
            use_site,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::InitialisedBeforeUse {
                binding: *binding,
                name: name.clone(),
                use_site: *use_site,
            },
            note: "binding is read before any initialising `let` for it appears".to_string(),
        }),
        MirCheck::DecisionMapTotal { offending_sites } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DecisionMapTotal {
                offending_sites: offending_sites.clone(),
            },
            note: "DecisionFact carries Strategy::UnknownBlocked at MIR boundary; \
                   the emitter must never receive an undecided value-class site"
                .to_string(),
        }),
        MirCheck::MustConsume {
            binding,
            name,
            bind_site,
            exit_site,
            ty,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::MustConsume {
                binding: *binding,
                name: name.clone(),
                bind_site: *bind_site,
                exit_site: *exit_site,
                ty: ty.clone(),
            },
            note: "@linear binding reached an exit without being consumed; \
                   declare a consuming method (e.g. `commit(consuming self)`) \
                   and ensure every reachable exit path invokes one"
                .to_string(),
        }),
        MirCheck::DropPlanUndetermined { block, reason } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DropPlanUndetermined {
                block: *block,
                reason: reason.clone(),
            },
            note: "drop-elaboration could not determine the per-exit live-set \
                   for an M2 substrate handle (Duplex / lambda-actor / \
                   half-handle); the elaborator aborts rather than emit a \
                   partial drop plan (LESSONS cleanup-all-exits)"
                .to_string(),
        }),
        MirCheck::ContextBoundaryViolation {
            function,
            block,
            kind,
            reason,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ContextBoundaryViolation {
                function: function.clone(),
                block: *block,
                kind,
                reason: reason.clone(),
            },
            note: "actor-handler execution context markers are structurally invalid".to_string(),
        }),
        MirCheck::ContextBindingEscapes { place, block } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ContextBindingEscapes {
                place: *place,
                block: *block,
            },
            note: "context-derived MIR place escapes past ExitContext".to_string(),
        }),
        MirCheck::OwnedHandleAggregateDoubleFree {
            name,
            handle_ty,
            overwrite,
            owner,
            ..
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::OwnedHandleAggregateExtractionUnsupported {
                name: name.clone(),
                handle_ty: handle_ty.clone(),
                overwrite: *overwrite,
                owner: *owner,
            },
            note: "the drop analysis could not prove this owned handle is freed \
                   exactly once after aggregate extraction; the compiler refuses \
                   rather than emit a double-free (LESSONS boundary-fail-closed, \
                   raii-null-after-move) — full aggregate-extraction support is \
                   tracked for v0.5.1"
                .to_string(),
        }),
        // No construction surface in the v0.5 integer spine. The
        // corresponding `MirDiagnosticKind` projections will land
        // alongside the construction surface for borrows, generators,
        // and actor sends.
        //
        // `WitnessOperandUnresolved` joins this group for a different
        // reason (W5.007a): witness instructions are produced only into
        // the gated polymorphic-MIR bucket, whose diagnostics are
        // discarded, so the finding never reaches a `CheckedMirFunction`
        // and has no user-visible projection in this slice. Its
        // fail-closed authority lives at the construction boundary
        // (`WitnessOperand::resolve`) and the MIR verifier.
        MirCheck::Aliasing { .. }
        | MirCheck::GeneratorBorrowAcrossYield { .. }
        | MirCheck::ActorSendEscape { .. }
        | MirCheck::ActorAskEscape { .. }
        | MirCheck::WitnessOperandUnresolved { .. } => None,
    }
}
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
) -> ElaboratedMirFunction {
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
    // derivation, so emission and admission cannot drift.
    //
    // Consume facts narrow the allow-set further: a binding `Consumed` or
    // `MaybeConsumed` at any block exit is removed, because `enumerate_exits`
    // treats `MaybeConsumed` as Live (the move-checker rejects that only for
    // `MustConsume`/Linear types, not CoW values) and would otherwise fire the
    // drop on a branch where the buffer was already moved out.
    let cow_drop_allowed = if let Some(precomputed) = precomputed_cow_drop_allowed {
        precomputed.clone()
    } else {
        let mut derived = derive_cow_sole_owner(
            &checked.blocks,
            &builder.suspend_kinds,
            &owned_locals_snapshot,
            &builder.binding_locals,
            &builder.match_project_consumed_binder_locals,
            &builder.locals,
            &builder.borrowed_string_param_locals,
            &builder.parameter_locals,
            &builder.module_fn_names,
            &builder.module_generic_fn_names,
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
        ));
        for states in dataflow_result.exit_states.values() {
            for (binding, state) in states {
                if matches!(
                    state,
                    dataflow::BindingState::Consumed(_) | dataflow::BindingState::MaybeConsumed(_)
                ) {
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
    let enum_composite_drop_allowed = derive_enum_composite_drop_allowed(
        &checked.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.binding_scope,
        &builder.transient_local_scopes,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        &builder.proven_borrow_call_args,
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
                dataflow::BindingState::Consumed(_) | dataflow::BindingState::MaybeConsumed(_)
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
                dataflow::BindingState::Consumed(_) | dataflow::BindingState::MaybeConsumed(_)
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
        for states in dataflow_result.exit_states.values() {
            for (binding, state) in states {
                if matches!(
                    state,
                    dataflow::BindingState::Consumed(_) | dataflow::BindingState::MaybeConsumed(_)
                ) {
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
                dataflow::BindingState::Consumed(_) | dataflow::BindingState::MaybeConsumed(_)
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
                dataflow::BindingState::Consumed(_) | dataflow::BindingState::MaybeConsumed(_)
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
    let owned_record_drop_allowed = derive_owned_record_drop_allowed(
        &checked.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &is_owned_record,
        &builder.record_field_orders,
        &builder.enum_layouts,
        &alias_field_binders,
        &builder.proven_borrow_call_args,
    );

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
    let tuple_composite_drop_allowed = derive_tuple_composite_drop_allowed(
        &checked.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        &alias_field_binders,
        &builder.proven_borrow_call_args,
    );

    // W5.021 (defect #1) — owned members the caller now owns via a returned
    // aggregate; excluded from every drop class below (see the function doc).
    let returned_aggregate_members = derive_returned_aggregate_member_bindings(
        &checked.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
    );

    // W3.053 — owned-handle members moved into a LOCAL aggregate and then
    // extracted-and-consumed back out (for-in / `let` extraction) by a downstream
    // release-consumer; the consumer owns the single free, so the source binding
    // must not also drop. The local-aggregate analogue of
    // `returned_aggregate_members` (see the function doc).
    let consumed_local_aggregate_members = derive_consumed_local_aggregate_member_bindings(
        &checked.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
    );

    // CAP-08 — owned handle-leaf bindings moved into an actor initial-state
    // record consumed by `SpawnActor`. The actor's synthesised `state_drop_fn`
    // is the single free site (Stream→`hew_stream_close` / Sink→`hew_sink_close`),
    // so the source binding's own scope-exit drop is removed here. The W3.053
    // gate consumes the SAME derivation via `source_excluded` so its free-count
    // model matches the drop this removal actually elides.
    let spawn_consumed_handle_members = derive_spawn_consumed_handle_bindings(
        &checked.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
    );

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
                dataflow::BindingState::Consumed(_) | dataflow::BindingState::MaybeConsumed(_)
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
    let mut indirect_enum_drop_allowed = derive_indirect_enum_drop_allowed(
        &checked.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.enum_layouts,
    );
    for states in dataflow_result.exit_states.values() {
        for (binding, state) in states {
            if matches!(
                state,
                dataflow::BindingState::Consumed(_) | dataflow::BindingState::MaybeConsumed(_)
            ) {
                indirect_enum_drop_allowed.remove(binding);
            }
        }
    }

    let projection_alias_tainted = compute_projection_alias_taint(
        &checked.blocks,
        &builder.match_project_consumed_binder_locals,
        &builder.locals,
    );
    let lifo_drops = build_lifo_drops(
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.type_classes,
        &builder.dyn_trait_storage,
        &owned_record_drop_allowed,
        &cow_drop_allowed,
        &builder.record_field_orders,
        &builder.enum_layouts,
        &enum_composite_drop_allowed,
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
        &builder.collection_drop_flags,
        &projection_alias_tainted,
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
        &builder.locals,
        &builder.match_project_consumed_binder_locals,
    );

    for (exit, plan) in &mut drop_plans {
        let ExitPath::Goto { block, .. } = exit else {
            continue;
        };
        let Some(bindings) = builder.iteration_owner_drop_blocks.get(block) else {
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

    // #2395 decision 2 — append each suspend's escape-poisoned abandon-edge drops
    // (today: the `SuspendKind::StreamSend` in-flight value) to its
    // `ExitPath::Suspend` plan. `drops_for_exit`'s `BindingState` filter cannot
    // see an escape-poisoned value, so it is stashed at the pump lowering site
    // (keyed by the suspend block id) and folded in here. Appended AFTER the
    // generic drops so it releases in the same LIFO order codegen walks; codegen
    // fires the whole plan on the case-1 destroy edge only.
    for (exit, plan) in &mut drop_plans {
        if let ExitPath::Suspend { block, .. } = exit {
            if let Some(extra) = builder.suspend_abandon_extra_drops.get(block) {
                plan.drops.extend(extra.iter().cloned());
            }
        }
    }

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
    }
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
/// active-mode `conn.attach(handler)` surface lowers to
/// `hew_tcp_attach_local(conn, handler)`, whose `LocalPid` handler the runtime
/// registers as a non-owning `HewActorRef::Local` by-value snapshot (real
/// free-count 1, via the caller's source drop alone). Kept as an explicit,
/// narrow allowlist so the escape gate does not invent a phantom second free
/// for a ratified borrowing surface while every non-listed call still fails
/// closed by default. This allowlist is the PARTIAL form — only the callee's
/// non-handle-leaf args are borrowed; owned-handle-leaf args are still poisoned
/// (e.g. `hew_tcp_attach_local`'s `conn`). For callees that borrow EVERY arg
/// including owned-handle leaves, see [`is_handle_borrowing_call_abi`].
pub(super) fn is_borrowing_call_abi(
    builtin: Option<hew_types::runtime_call::RuntimeCallFamily>,
) -> bool {
    matches!(
        builtin,
        Some(hew_types::runtime_call::RuntimeCallFamily::TcpAttachLocal)
    )
}
/// Runtime ABIs whose owned-handle-leaf arguments are ALSO borrowed (not just
/// the non-handle-leaf args of [`is_borrowing_call_abi`]). The layout-witness
/// stream recv / `try_recv` runtime entries — `hew_stream_next_layout` /
/// `hew_stream_try_next_layout` for `Stream<T>::recv` / `try_recv` — READ
/// one item from the stream and decode it into the consumer's `Option<T>`
/// slot; the stream handle itself is borrowed for the call and continues to
/// live in the caller's slot afterwards (the caller's source drop is still
/// the SOLE free of the stream's runtime context). Their suspending siblings
/// ([`Terminator::SuspendingStreamNext`]) are already exempt because they are
/// not [`Terminator::Call`]s; listing the blocking / non-suspending entries
/// here keeps the same borrow-not-consume semantics for the
/// `let (sink, input) = stream.pipe(N); input.try_recv()` shape that goes
/// through [`Terminator::Call`].
///
/// Kept narrow: each entry's Rust impl takes `*mut HewStream` and ONLY reads
/// one queued item without mutating the handle's ownership; adding a callee
/// that actually consumes the handle here would silently disable the
/// double-free gate for its caller.
pub(super) fn is_handle_borrowing_call_abi(
    builtin: Option<hew_types::runtime_call::RuntimeCallFamily>,
) -> bool {
    use hew_types::runtime_call::RuntimeCallFamily as F;
    matches!(builtin, Some(F::StreamNextLayout | F::StreamTryNextLayout))
}
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
/// inner buffer's release (DIV-1; a record-blind walker leaked it).
pub(super) fn ty_is_heap_owning_tuple(
    ty: &ResolvedTy,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
) -> bool {
    matches!(ty, ResolvedTy::Tuple(_))
        && crate::model::ty_owns_heap_mir(ty, record_field_orders, enum_layouts)
}
/// A payload binder read into an owning sink means the active payload escaped
/// the composite. We cannot cheaply attribute a binder to a single composite
/// root (a binder may be reachable from several composites through onward
/// copies), so — fail-closed — exclude EVERY heap-owning enum composite root
/// in the function when any payload binder escapes. Over-exclusion leaks; it
/// never double-frees. The vast majority of real handlers have at most one
/// matched heap-owning composite per scope, so this coarsening is rarely
/// observable, and the precise per-root attribution is a follow-on slice if a
/// fixture ever needs it.
pub(super) fn note_payload_escape(
    _payload_binders: &HashMap<u32, Option<ScopeId>>,
    _escaping_binder: u32,
    alias_of: &HashMap<u32, u32>,
    _blocks: &[BasicBlock],
    excluded_roots: &mut HashSet<u32>,
) {
    for &root in alias_of.values() {
        excluded_roots.insert(root);
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
) -> bool {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    // Must resolve to a registered tagged-union enum layout (not a record,
    // not an opaque/builtin handle). Indirect (heap-boxed) enums route their
    // payload drop through the boxed-storage release path, not this in-place
    // helper, so they are excluded here.
    let short = hew_types::short_name(name);
    let layout = if args.is_empty() {
        enum_layouts
            .iter()
            .find(|el| el.name == *name || hew_types::short_name(&el.name) == short)
    } else {
        let mangled = mangle_layout_key(short, args);
        enum_layouts
            .iter()
            .find(|el| el.name == mangled || el.name == *name)
    };
    let Some(layout) = layout else {
        return false;
    };
    if layout.is_indirect {
        return false;
    }
    crate::model::ty_owns_heap_mir(ty, record_field_orders, enum_layouts)
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
        // companion `{ ptr handle, ptr env, ptr out_drop_thunk, i8 started,
        // i8 pending, Y out }` (shared `Place::Local` storage, discriminated by
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
/// RAII-1 opaque-resource close registry: `(opaque_type_name, "<Type>::<close>")`
/// for every single-slot `#[resource] #[opaque]` handle whose close is a USER
/// method.
///
/// Parallel to the `resource_record_close` registry (which keys off
/// `record_layouts` for `#[resource]` RECORDS): a single-handle opaque
/// `#[resource]` has no record layout, so it is excluded from
/// `resource_record_close` — that exclusion is exactly the W3.029 leak this
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
/// `resource_record_close` / `resource_drop_fn` use.
///
/// Runtime-descriptor closes (where `RuntimeDropDescriptor::from_drop_fn_name`
/// matches, e.g. a builtin handle with a C-ABI release) are excluded: the
/// `Resource` drop arm calls the close as a user LLVM function, which a C-ABI
/// runtime symbol would not resolve to. Those keep their existing path.
pub(super) fn resource_opaque_close_registry(
    opaque_handle_names: &[String],
    type_classes: &hew_hir::TypeClassTable,
) -> Vec<(String, String)> {
    use hew_types::runtime_call::RuntimeDropDescriptor;
    opaque_handle_names
        .iter()
        .filter_map(|name| {
            let short = short_name(name);
            let (_, close) = type_classes
                .get(name.as_str())
                .or_else(|| type_classes.get(short))
                .and_then(|entry| matches!(entry.0, ResourceMarker::Resource).then_some(entry))?;
            let close_method = close.as_ref()?;
            let symbol = format!("{short}::{close_method}");
            // Open-set USER close only: a builtin runtime-descriptor close is a
            // C-ABI symbol the `Resource` drop arm cannot call via
            // `get_function`, so leave such a type on its existing path.
            if RuntimeDropDescriptor::from_drop_fn_name(&symbol).is_some() {
                return None;
            }
            Some((name.clone(), symbol))
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
        ResolvedTy::Named { name, .. } => {
            let short = short_name(name);
            let qualified_collision = short != name
                && type_classes
                    .keys()
                    .filter(|candidate| candidate.contains('.') && short_name(candidate) == short)
                    .count()
                    > 1;
            let class_entry = if short == name {
                type_classes.get_key_value(name)
            } else if qualified_collision {
                type_classes
                    .get_key_value(name)
                    .or_else(|| type_classes.get_key_value(short))
            } else {
                type_classes
                    .get_key_value(short)
                    .or_else(|| type_classes.get_key_value(name))
            };
            class_entry.and_then(|(class_name, (_, close))| {
                close.as_ref().map(|m| {
                    // Classify the close ritual at production: the type-class
                    // table's `<Type>::<method>` spelling lifts onto the typed
                    // runtime drop descriptor for the closed builtin set; user
                    // `#[resource]` close methods stay on the open-set
                    // generated-symbol arm.
                    let qualified = format!("{class_name}::{m}");
                    RuntimeDropDescriptor::from_drop_fn_name(&qualified).map_or(
                        crate::model::DropFnSpec::UserClose(qualified),
                        crate::model::DropFnSpec::Runtime,
                    )
                })
            })
        }
        // Task<T> and all other types have no user-visible close method.
        _ => None,
    }
}
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
/// - `HirExprKind::CoerceToDynTrait` → `FrameOwned`. The producer wraps
///   a concrete value into a fat pointer whose `data` word aliases the
///   concrete's frame slot; the coerced fat pointer's drop is the
///   slot-0 `drop_in_place` only.
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
        HirExprKind::CoerceToDynTrait { .. } => Ok(TraitObjectStorage::FrameOwned),
        HirExprKind::Call { .. }
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
            Instr::NeutralizePayloadSlot { place } => Some(*place),
            _ => None,
        })
        .collect();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Move { dest, src } = instr {
                if place_is_interior_projection(*src) && neutralized_sources.contains(src) {
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
/// leak-not-double-free posture the legacy path had for that shape.
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
    let short = short_name(name);
    enum_layouts
        .iter()
        .find(|el| el.name == name || short_name(&el.name) == short)
        .is_some_and(|el| el.is_indirect)
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
    let mut owns_node: HashSet<u32> = construction_sites.clone();
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
    collection_drop_flags: &HashMap<BindingId, Place>,
    projection_alias_tainted: &HashSet<u32>,
) -> Vec<ElabDrop> {
    let mut drops = Vec::new();
    for (binding, _name, ty) in owned_locals.iter().rev() {
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
                guard: None,
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
            && ty_is_heap_owning_enum_composite(ty, record_field_orders, enum_layouts)
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
                guard: None,
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
        // clear leaks (as before); it never double-frees.
        if tuple_composite_drop_allowed.contains(binding)
            && ty_is_heap_owning_tuple(ty, record_field_orders, enum_layouts)
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
            // re-open the double-free. Aggregate/container `CowValue` self-drops
            // (Vec, HashMap, HashSet, Tuple, Array, Bytes) are deferred to the
            // retain-on-share follow-on and remain no-ops here.
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
                            guard: None,
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
/// whose scope-exit drop is deferred to a later slice.
///
/// Slice 2 is intentionally restricted to the single `string` leaf — the
/// accumulating helper-local leak this fix targets. Aggregate and container
/// `CowValue` leaves (`Vec`, `HashMap`, `HashSet`, tuples, arrays) own nested
/// heap that, without retain-on-share at element-ingress sites, cannot be
/// released here without risking a double-free against the container's own
/// element-release path; they stay `None` (leak-as-before, never double-free)
/// until the retain-on-share spine lands.
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
/// The predicate selects every SINGLE-POINTER COW-heap field — `string`,
/// `Vec<T>`, `HashMap`, `HashSet`, and the `Generator` / `AsyncGenerator`
/// companion handle — whose runtime value is exactly one pointer, so the
/// `RecordFieldDrop` single-slot GEP + release is the whole destructor.
///
/// `bytes` is deliberately EXCLUDED: it is a fat `{ ptr, len, cap }` triple,
/// not a single pointer. Its destructor takes the by-value triple, so it is
/// reached through `RecordFieldLoad` + `Instr::Drop` (which materialises the
/// fat value into a temp). The set mirrors codegen's `resolved_ty_cow_heap_release`
/// (which returns `None` for `bytes`), so the `RecordFieldDrop` symbol/type
/// congruence assertion in codegen always agrees with what is emitted here.
///
/// Dispatch is on the `builtin` discriminant, never the `name` string, so a
/// user-defined `type Vec { ... }` (`builtin: None`) is never mis-routed to a
/// runtime release symbol (LESSONS: boundary-fail-closed, checker-authority).
pub(super) fn field_override_uses_record_field_drop(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::String
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
        name,
        fields,
        base: None,
        ..
    } = &value.kind
    else {
        return None;
    };
    if name != "VecIter" {
        return None;
    }
    fields
        .iter()
        .find(|(field, _)| field == "vec")
        .map(|(_, src)| src)
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
/// Instruction analogue of [`binder_read_is_borrow_safe_terminator`]. The `xs[i]`
/// bounds-check + getter path lowers `hew_vec_len` / `hew_vec_get_*` as
/// `Instr::CallRuntimeAbi` (not a terminator), so a field/element binder read
/// there must get the SAME arg[0] receiver-borrow exemption. Conservative:
/// `false` unless the only references to `binder` are the borrowed receiver.
pub(super) fn binder_read_is_borrow_safe_instr(instr: &Instr, binder: u32) -> bool {
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
    locals: &[ResolvedTy],
    match_project_consumed_binder_locals: &HashSet<u32>,
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
    let place_to_binding: std::collections::HashMap<Place, BindingId> = binding_locals
        .iter()
        .map(|(binding, place)| (*place, *binding))
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
        drops_template
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
                // No binding mapping → conservatively keep the drop.
                // This arm guards against future surfaces that build
                // drops outside the binding_locals registry; the
                // current `build_lifo_drops` `expect()` rules out
                // this path today, but keep it for forward safety.
                None => true,
            })
            .cloned()
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
    // (DI-020). The taint exactly identifies these non-sole-owner aliases, so the
    // scope-close release stays limited to genuinely sole-owned bindings that
    // leave scope (a resource handle, a leaf cow owner) — the leak the
    // scope-close pass exists to close — while a payload alias is left to its
    // composite's single recursive free.
    //
    // The REAL `locals` table is threaded through (not an empty slice) so the
    // `string` `RecordFieldLoad`/`TupleFieldLoad` exemption stays active: a
    // `let name = r.name` reads a fresh `+1`-retained string owner that the
    // composite does NOT recursively free, so it must drop on the scope-close
    // edge. Tainting it (empty `locals` disables the exemption) would strand its
    // release at a join → `Uninit` → no `Return` recovery → leak. Enum/tuple
    // payload destructures stay tainted via the `Move`-from-interior arm
    // regardless of `locals`, so the double-free fix is unaffected. The exact
    // consumed-project binder set exempts only field-load destinations whose
    // parent composite is consume-marked on the selected arm; those destinations
    // are sole owners and must close on this edge before the join loses them.
    let scope_close_alias_tainted =
        compute_projection_alias_taint(blocks, match_project_consumed_binder_locals, locals);
    let drops_for_scope_close_goto = |block_id: u32, target: u32| -> Vec<ElabDrop> {
        drops_for_exit(block_id)
            .into_iter()
            .filter(|drop| {
                // A projection-alias payload binder is owned by its composite,
                // which frees it recursively at its own exit — never drop it on a
                // scope-close edge (no double-free).
                if let Some(l) = base_local(drop.place) {
                    if scope_close_alias_tainted.contains(&l) {
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
                alias_mode: _,
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
                msg_type: _,
                value: _,
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
            // HashMap/HashSet yields have no validated consumer-drop path —
            // they leak-as-before (a frozen NoDropPath), never risk a
            // double-free.
            (
                "HashMap (yield leak-as-before)",
                hashmap_str_i64(),
                NoDropPath,
                Wired("hew_hashmap_free_layout"),
            ),
            (
                "HashSet (yield leak-as-before)",
                hashset_i64(),
                NoDropPath,
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
    use crate::return_provenance::{AliasBits, CallScrutineeProvenance, ExternContractTable};

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
        // A resolved module-fn callee whose precise summary carries PARAM forwards
        // a by-value heap parameter — the twin gate rejects it (defence-in-depth
        // for the #2523 forwarding-call twin; the preflight owns the same reject).
        let mut b = Builder::default();
        let mut provenance = HashMap::new();
        provenance.insert(hew_hir::ItemId(7), AliasBits::PARAM);
        b.call_scrutinee_provenance = Rc::new(CallScrutineeProvenance {
            provenance,
            extern_names: HashSet::new(),
            extern_table: ExternContractTable::default(),
            may_mutate: HashMap::new(),
        });
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

    #[test]
    fn call_forwarding_a_param_summary_over_fresh_arg_admits() {
        // S2b — the arg-scan rescue: the SAME `{PARAM}`-only callee over an
        // inline string literal is a fresh sole owner (the template/semver
        // stdlib shape) → the twin gate agrees with the preflight's Admit.
        let mut b = Builder::default();
        let mut provenance = HashMap::new();
        provenance.insert(hew_hir::ItemId(7), AliasBits::PARAM);
        b.call_scrutinee_provenance = Rc::new(CallScrutineeProvenance {
            provenance,
            extern_names: HashSet::new(),
            extern_table: ExternContractTable::default(),
            may_mutate: HashMap::new(),
        });
        let callee = expr(
            HirExprKind::BindingRef {
                name: "try_parse".to_string(),
                resolved: ResolvedRef::Item(hew_hir::ItemId(7)),
            },
            ResolvedTy::Unit,
        );
        let literal_arg = expr(
            HirExprKind::Literal(hew_hir::HirLiteral::String("hello {{.name".to_string())),
            ResolvedTy::String,
        );
        let call = expr(
            HirExprKind::Call {
                callee: Box::new(callee),
                args: vec![literal_arg],
            },
            ResolvedTy::String,
        );
        assert!(
            is_ephemeral(&b.classify_scrutinee_origin(&call)),
            "a ParamsOnly callee over an inline-fresh literal arg is a fresh sole owner"
        );
    }

    #[test]
    fn call_with_mixed_param_opaque_summary_rejects_despite_fresh_args() {
        // A mixed `PARAM|OPAQUE` summary is never arg-rescuable — the OPAQUE
        // component can alias a capture/global regardless of the arguments.
        let mut b = Builder::default();
        let mut provenance = HashMap::new();
        provenance.insert(hew_hir::ItemId(7), AliasBits::PARAM | AliasBits::OPAQUE);
        b.call_scrutinee_provenance = Rc::new(CallScrutineeProvenance {
            provenance,
            extern_names: HashSet::new(),
            extern_table: ExternContractTable::default(),
            may_mutate: HashMap::new(),
        });
        let callee = expr(
            HirExprKind::BindingRef {
                name: "mixed".to_string(),
                resolved: ResolvedRef::Item(hew_hir::ItemId(7)),
            },
            ResolvedTy::Unit,
        );
        let literal_arg = expr(
            HirExprKind::Literal(hew_hir::HirLiteral::String("x".to_string())),
            ResolvedTy::String,
        );
        let call = expr(
            HirExprKind::Call {
                callee: Box::new(callee),
                args: vec![literal_arg],
            },
            ResolvedTy::String,
        );
        assert!(
            is_alias_reject(&b.classify_scrutinee_origin(&call)),
            "a PARAM|OPAQUE summary must reject even over fresh args"
        );
    }

    #[test]
    fn call_to_a_fresh_summary_admits() {
        // A resolved module-fn callee with no PARAM bit keeps today's admission
        // (the interim legacy window).
        let mut b = Builder::default();
        let mut provenance = HashMap::new();
        provenance.insert(hew_hir::ItemId(7), AliasBits::EMPTY);
        b.call_scrutinee_provenance = Rc::new(CallScrutineeProvenance {
            provenance,
            extern_names: HashSet::new(),
            extern_table: ExternContractTable::default(),
            may_mutate: HashMap::new(),
        });
        let callee = expr(
            HirExprKind::BindingRef {
                name: "make_fresh".to_string(),
                resolved: ResolvedRef::Item(hew_hir::ItemId(7)),
            },
            ResolvedTy::Unit,
        );
        let call = expr(
            HirExprKind::Call {
                callee: Box::new(callee),
                args: vec![],
            },
            ResolvedTy::String,
        );
        assert!(
            is_ephemeral(&b.classify_scrutinee_origin(&call)),
            "a fresh-summary call scrutinee is an ephemeral fresh owner"
        );
    }
}
