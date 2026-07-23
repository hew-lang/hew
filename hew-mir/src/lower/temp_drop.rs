#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    base_local, binder_read_is_borrow_safe_instr, binder_read_is_borrow_safe_terminator,
    block_by_id, call_terminator_next, cow_value_leaf_drop_symbol, dataflow,
    derive_local_bytes_drop_allowed, generator_yield_instr_escapes,
    generator_yield_terminator_escapes, instr_source_places, local_is_used_after,
    place_is_interior_projection, place_refs_local, propagate_whole_value_alias_roots,
    shift_instr_spans_on_insert, terminator_source_places, vec_iter_record_layout_key,
    ActorStateLoadMode, BTreeMap, BasicBlock, BindingId, Builder, BytesDropDerivation,
    BytesRetainPlacement, BytesRetainSite, ClosureEnvFieldOwnership, HashMap, HashSet, Instr,
    IntentKind, MirStatement, NestedDefSite, NestedUseSite, Place, RawMirFunction, ResolvedTy,
    SiteId, StringDropDerivation, StringRetainSite, SuspendKind, Terminator,
};

pub(super) fn finalize_string_local_share_intents(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
) {
    let candidates: Vec<(SiteId, BindingId, BindingId)> = builder
        .string_local_share_sites
        .iter()
        .map(|(site, (source, dest))| (*site, *source, *dest))
        .collect();

    for (site, source_binding, dest_binding) in candidates {
        let Some(source_local) = builder
            .binding_locals
            .get(&source_binding)
            .and_then(|place| base_local(*place))
        else {
            continue;
        };
        let Some(dest_local) = builder
            .binding_locals
            .get(&dest_binding)
            .and_then(|place| base_local(*place))
        else {
            continue;
        };

        let move_sites: Vec<(u32, usize)> = blocks
            .iter()
            .flat_map(|block| {
                block
                    .instructions
                    .iter()
                    .enumerate()
                    .filter_map(move |(index, instr)| {
                        matches!(
                            instr,
                            Instr::Move {
                                dest: Place::Local(dl),
                                src: Place::Local(sl),
                            } if *dl == dest_local && *sl == source_local
                        )
                        .then_some((block.id, index))
                    })
            })
            .collect();
        let [(move_block, move_index)] = move_sites.as_slice() else {
            // Ambiguous lowering stays a retained share (the safe direction).
            continue;
        };
        if blocks.first().map(|block| block.id) != Some(*move_block) {
            // A branch-local last use is not a function-wide handoff. Keep it a
            // retained share so the source still drops on paths that skip it.
            continue;
        }

        let used_after = local_is_used_after(
            blocks,
            &builder.suspend_kinds,
            source_local,
            *move_block,
            *move_index,
        );
        if used_after {
            continue;
        }

        for block in blocks.iter_mut() {
            for statement in &mut block.statements {
                if let MirStatement::Use {
                    binding,
                    site: use_site,
                    intent,
                    ..
                } = statement
                {
                    if *binding == source_binding && *use_site == site {
                        *intent = IntentKind::Consume;
                    }
                }
            }
        }
    }
}
/// The destination of an instruction that loads an *interior pointer* out
/// of a still-live aggregate — a record field, a tuple element, a
/// closure-env capture, or an actor-state field. The loaded value aliases
/// the parent aggregate's owned heap (no retain is emitted on the M-COW
/// spine), so a `string` local that ultimately receives such a value is
/// NOT its own sole owner: dropping it would free a buffer the parent
/// still owns.
///
/// W5-011 P3 — `derive_cow_sole_owner` seeds projection-alias taint at
/// these dests and propagates it forward through `Move`. The match is
/// exhaustive with no wildcard so a future aggregate-projection load
/// cannot be added without deciding whether its dest aliases parent
/// storage — the same fail-closed guarantee `instr_source_places` carries.
///
/// CODEGEN-RETAIN PREMISE (string `*FieldLoad`): this classifier is purely
/// structural and still names `RecordFieldLoad` / `TupleFieldLoad` here, but a
/// *string-typed* `*FieldLoad` dest is NO LONGER a no-retain alias — codegen
/// emits `hew_string_clone` on it (`retain_string_field_load`, hew-codegen-rs),
/// making it a genuine fresh `+1` owner. `compute_projection_alias_taint`
/// therefore EXCLUDES string-typed `*FieldLoad` dests from the taint seed (via
/// the `locals` type gate) and `string_field_load_producer_dest` admits them as
/// fresh producers, so they earn exactly one balancing scope-exit drop — the
/// same class `hew_vec_get_str` (`xs.get(i)`) already occupies. NON-string
/// `*FieldLoad` dests (`bytes` / `Vec<T>` / nested record) are NOT retained by
/// codegen and stay tainted here (leak, never double-free) until the
/// retain-on-share spine lands. The two sides are coupled: if the codegen retain
/// is ever conditionally skipped for a string field load, the taint exclusion
/// and the fresh-producer admission must be skipped in lockstep, or the leak
/// inverts back into a double-free.
#[must_use]
fn projection_alias_dest(instr: &Instr) -> Option<Place> {
    match instr {
        Instr::RecordFieldLoad { dest, .. }
        | Instr::TupleFieldLoad { dest, .. }
        | Instr::ClosureEnvFieldLoad { dest, .. }
        | Instr::ActorStateFieldLoad { dest, .. } => Some(*dest),
        // Everything else either produces a fresh value (literals,
        // arithmetic, calls, aggregate construction) or writes through a
        // place that does not alias a live aggregate's interior. Listed
        // exhaustively (no wildcard) so a new projection-shaped load forces
        // a classification decision here.
        Instr::EnterContext
        | Instr::ExitContext
        | Instr::CheckCancellation
        | Instr::ContextField { .. }
        | Instr::ConstI64 { .. }
        | Instr::IntAdd { .. }
        | Instr::IntSub { .. }
        | Instr::IntMul { .. }
        | Instr::IntDiv { .. }
        | Instr::IntRem { .. }
        | Instr::IntBitAnd { .. }
        | Instr::IntBitOr { .. }
        | Instr::IntBitXor { .. }
        | Instr::BoolNot { .. }
        | Instr::IntNegChecked { .. }
        | Instr::FloatNeg { .. }
        | Instr::IntBitNot { .. }
        | Instr::IntShl { .. }
        | Instr::IntShr { .. }
        | Instr::IntArithChecked { .. }
        | Instr::IntArithCheckedOption { .. }
        | Instr::IntArithSaturating { .. }
        | Instr::IntCmp { .. }
        | Instr::IdentityCompare { .. }
        | Instr::CancellationTokenIsCancelled { .. }
        | Instr::RcIntrinsic { .. }
        | Instr::GeneratorNext { .. }
        | Instr::WireCodec { .. }
        | Instr::Move { .. }
        | Instr::BytesRetain { .. }
        | Instr::StringRetain { .. }
        | Instr::NumericCast { .. }
        | Instr::SaturatingWidthCast { .. }
        | Instr::TryWidthCast { .. }
        | Instr::CallRuntimeAbi(_)
        | Instr::AutoLockAcquire { .. }
        | Instr::AutoLockRelease { .. }
        | Instr::MakeClosure { .. }
        | Instr::ActorStateFieldStore { .. }
        | Instr::ClosureEnvFieldStore { .. }
        | Instr::SpawnActor { .. }
        | Instr::CallClosure { .. }
        | Instr::SpawnTaskDirect { .. }
        | Instr::SpawnTaskClosure { .. }
        | Instr::Drop { .. }
        | Instr::RecordFieldDrop { .. }
        // FieldDropInPlace releases one field slot in place through the base
        // pointer — it creates no dest and therefore no projection alias.
        | Instr::FieldDropInPlace { .. }
        | Instr::WitnessSizeOf { .. }
        | Instr::WitnessAlignOf { .. }
        | Instr::WitnessDropGlue { .. }
        | Instr::WitnessMove { .. }
        | Instr::StringLit { .. }
        | Instr::BytesLit { .. }
        | Instr::ConstGlobalLoad { .. }
        | Instr::RecordInit { .. }
        | Instr::ClosureEnvInit { .. }
        | Instr::RecordFieldStore { .. }
        | Instr::TupleConstruct { .. }
        | Instr::FloatLit { .. }
        | Instr::CharLit { .. }
        | Instr::UnitLit { .. }
        | Instr::DurationLit { .. }
        | Instr::FloatAdd { .. }
        | Instr::FloatSub { .. }
        | Instr::FloatMul { .. }
        | Instr::FloatDiv { .. }
        | Instr::FloatRem { .. }
        | Instr::FloatCmp { .. }
        | Instr::CoerceToDynTrait { .. }
        | Instr::CallTraitMethod { .. }
        | Instr::MachineEmitPlaceholder { .. }
        | Instr::EnumTagLoad { .. }
        | Instr::MachineStateName { .. }
        | Instr::MachineEmitTake { .. }
        // RecordCloneInplace allocates a fresh clone — its dest does NOT alias
        // the src or any parent aggregate field (the thunk overwrites heap
        // pointer fields in dst with independently-owned clones).
        | Instr::RecordCloneInplace { .. }
        // EnumCloneInplace allocates a fresh enum clone with the same
        // non-aliasing guarantee (tag-dispatched payload deep-clone).
        | Instr::EnumCloneInplace { .. }
        | Instr::ValueSnapshotClone { .. }
        | Instr::ValueSnapshotDrop { .. }
        // A payload-slot neutralize writes a constant null — it produces no
        // interior-alias dest (it retires one).
        | Instr::NeutralizePayloadSlot { .. }
        | Instr::AggregateProjectionNeutralize { .. } => None,
    }
}
fn string_place_is_typed(place: Place, local_tys: &[ResolvedTy]) -> bool {
    base_local(place)
        .is_some_and(|local| matches!(local_tys.get(local as usize), Some(ResolvedTy::String)))
}
fn string_share_sink_places(instr: &Instr) -> Vec<Place> {
    match instr {
        // Fork-entry environments preserve their existing move-in contract: the
        // parent consumes each owned argument and the child environment is the
        // sole release authority.
        Instr::RecordInit {
            ty: ResolvedTy::Named { name, .. },
            ..
        } if name.starts_with("__hew_fork_env_") => Vec::new(),
        Instr::RecordInit { fields, .. } => fields.iter().map(|(_, place)| *place).collect(),
        Instr::TupleConstruct { elements, .. } => elements.clone(),
        Instr::RecordFieldStore { src, .. }
        | Instr::ActorStateFieldStore { src, .. }
        | Instr::ClosureEnvFieldStore { src, .. } => vec![*src],
        Instr::SpawnActor {
            state, init_args, ..
        } => state.iter().chain(init_args).copied().collect(),
        Instr::ClosureEnvInit { fields, .. } => fields
            .iter()
            .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsClonedOrRetained)
            .map(|field| field.src)
            .collect(),
        _ => Vec::new(),
    }
}
fn apply_string_retain_sites(
    blocks: &mut [BasicBlock],
    instr_spans: &mut BTreeMap<(u32, u32), (u32, u32)>,
    retain_sites: &[StringRetainSite],
) {
    let mut before: HashMap<(u32, usize), Vec<Place>> = HashMap::new();
    for site in retain_sites {
        before
            .entry((site.block, site.instr_index))
            .or_default()
            .push(site.value);
    }

    let old_spans = std::mem::take(instr_spans);
    let mut new_spans = BTreeMap::new();
    for block in blocks {
        let old_instructions = std::mem::take(&mut block.instructions);
        let old_len = old_instructions.len();
        let mut rewritten = Vec::with_capacity(old_len);
        for (old_index, instr) in old_instructions.into_iter().enumerate() {
            let span = old_spans
                .get(&(block.id, u32::try_from(old_index).unwrap_or(u32::MAX)))
                .copied();
            if let Some(values) = before.get(&(block.id, old_index)) {
                for value in values {
                    let new_index = u32::try_from(rewritten.len()).unwrap_or(u32::MAX);
                    rewritten.push(Instr::StringRetain { value: *value });
                    if let Some(span) = span {
                        new_spans.insert((block.id, new_index), span);
                    }
                }
            }
            let new_index = u32::try_from(rewritten.len()).unwrap_or(u32::MAX);
            rewritten.push(instr);
            if let Some(span) = span {
                new_spans.insert((block.id, new_index), span);
            }
        }
        if let Some(span) = old_spans
            .get(&(block.id, u32::try_from(old_len).unwrap_or(u32::MAX)))
            .copied()
        {
            let term_index = u32::try_from(rewritten.len()).unwrap_or(u32::MAX);
            new_spans.insert((block.id, term_index), span);
        }
        block.instructions = rewritten;
    }
    *instr_spans = new_spans;
}
/// Retain-aware drop derivation for heap-owning `string` bindings.
///
/// By-value calls borrow. A genuine co-owner mint (aggregate ingress,
/// local-to-local share, or return of a borrowed parameter) gets an explicit
/// `StringRetain` marker, and the producing binding keeps its own drop
/// obligation. Existing retained producers (`*FieldLoad`, `hew_vec_get_str`,
/// and fresh string runtime results) already return `+1` and need no marker.
///
/// Unknown reads still fail closed to exclusion. Retain sites are filtered
/// against the final allow-set after dataflow consume facts are applied, so a
/// hand-off never gains an unbalanced retain.
#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "the string ownership derivation keeps candidate collection, alias \
              propagation, mint classification, drop admission, and retain-site \
              placement in one fail-closed pass so those decisions cannot drift"
)]
#[must_use]
pub(super) fn derive_cow_sole_owner(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    match_project_consumed_binder_locals: &HashSet<u32>,
    locals: &[ResolvedTy],
    borrowed_param_locals: &HashSet<u32>,
    parameter_locals: &HashSet<u32>,
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
) -> StringDropDerivation {
    let mut candidate_local_to_binding: HashMap<u32, BindingId> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !matches!(ty, ResolvedTy::String) {
            continue;
        }
        let Some(place) = binding_locals.get(binding) else {
            continue;
        };
        let Some(local) = base_local(*place) else {
            continue;
        };
        candidate_local_to_binding.insert(local, *binding);
    }

    let alias_of =
        propagate_whole_value_alias_roots(blocks, candidate_local_to_binding.keys().copied());
    let entry_block = blocks.first().map(|block| block.id);
    let mut handoff_move_sites: HashSet<(u32, usize)> = HashSet::new();
    let mut handoff_bindings: HashSet<BindingId> = HashSet::new();
    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            let Instr::Move {
                dest: Place::Local(dest_local),
                src,
            } = instr
            else {
                continue;
            };
            let (Some(src_local), Some(&dest_binding)) =
                (base_local(*src), candidate_local_to_binding.get(dest_local))
            else {
                continue;
            };
            let Some(&root) = alias_of.get(&src_local) else {
                continue;
            };
            let Some(&src_binding) = candidate_local_to_binding.get(&root) else {
                continue;
            };
            if src_binding == dest_binding {
                continue;
            }
            let used_after =
                local_is_used_after(blocks, suspend_kinds, src_local, block.id, instr_index);
            if !used_after && entry_block == Some(block.id) {
                handoff_move_sites.insert((block.id, instr_index));
                let handoff_binding = candidate_local_to_binding
                    .get(&src_local)
                    .copied()
                    .unwrap_or(src_binding);
                handoff_bindings.insert(handoff_binding);
            }
        }
    }
    // A variant-payload share is a hand-off (no retain) when the source is
    // never used afterwards AND the store site cannot re-execute for the same
    // binding instance — i.e. its block is not on a CFG cycle. The previous
    // entry-block-only proxy for "executes at most once" misclassified every
    // post-call return-position constructor (`let v = f(); Ok(v)` — the call
    // splits the CFG, so the constructor lands in a non-entry block) as a
    // co-own share. That minted a `StringRetain` while
    // `derive_returned_aggregate_member_bindings` simultaneously excluded the
    // binding from its scope-exit drop as a returned-aggregate member (that
    // authority's contract is a NO-retain byte-copy hand-off), so the payload
    // gained a reference nothing ever released: one leaked string per call.
    // Blocks on a cycle keep the retain-on-share posture — a loop body's store
    // can re-execute for a binding defined outside the loop, where a hand-off
    // would double-free.
    // Reachable-from-self is a deliberate OVER-APPROXIMATION of cycle membership,
    // and that is the fail-closed choice: a straight-line store wrongly judged
    // loop-carried merely keeps a retain (safe — at worst a missed hand-off), whereas
    // the dangerous direction is treating a re-executing store as a one-shot hand-off
    // (a double-free). Cost is O(blocks·(blocks+edges)) per store — fine at MIR
    // function sizes.
    let cyclic_blocks: HashSet<u32> = {
        let succs: HashMap<u32, Vec<u32>> = blocks.iter().map(|b| (b.id, b.successors())).collect();
        let mut cyclic = HashSet::new();
        for start in succs.keys().copied() {
            let mut stack: Vec<u32> = succs.get(&start).cloned().unwrap_or_default();
            let mut seen: HashSet<u32> = HashSet::new();
            while let Some(b) = stack.pop() {
                if b == start {
                    cyclic.insert(start);
                    break;
                }
                if seen.insert(b) {
                    if let Some(next) = succs.get(&b) {
                        stack.extend(next.iter().copied());
                    }
                }
            }
        }
        cyclic
    };
    let share_needs_retain = |local: u32, block: u32, instr_index: usize| {
        cyclic_blocks.contains(&block)
            || local_is_used_after(blocks, suspend_kinds, local, block, instr_index)
    };

    let borrowed_alias_of =
        propagate_whole_value_alias_roots(blocks, borrowed_param_locals.iter().copied());
    let binding_local_bases: HashSet<u32> = binding_locals
        .values()
        .filter_map(|place| base_local(*place))
        .collect();
    let tainted =
        compute_projection_alias_taint(blocks, match_project_consumed_binder_locals, locals);

    let mut excluded_roots: HashSet<u32> = HashSet::new();
    let mut pending_share_sites: Vec<(u32, usize, Place, Vec<BindingId>)> = Vec::new();
    let note_escape = |local: u32, excluded: &mut HashSet<u32>| {
        if let Some(&root) = alias_of.get(&local) {
            excluded.insert(root);
        }
    };

    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            if let Instr::Move { dest, src } = instr {
                if matches!(
                    dest,
                    Place::MachineVariant { .. } | Place::EnumVariant { .. }
                ) {
                    if let Some(src_local) = base_local(*src) {
                        if let Some(&root) = alias_of.get(&src_local) {
                            if let Some(&binding) = candidate_local_to_binding.get(&root) {
                                if share_needs_retain(src_local, block.id, instr_index) {
                                    pending_share_sites.push((
                                        block.id,
                                        instr_index,
                                        *src,
                                        Vec::new(),
                                    ));
                                } else {
                                    handoff_bindings.insert(binding);
                                }
                            }
                        } else if borrowed_alias_of.contains_key(&src_local)
                            || (string_place_is_typed(*src, locals)
                                && binding_local_bases.contains(&src_local)
                                && !parameter_locals.contains(&src_local)
                                && local_is_used_after(
                                    blocks,
                                    suspend_kinds,
                                    src_local,
                                    block.id,
                                    instr_index,
                                ))
                        {
                            pending_share_sites.push((block.id, instr_index, *src, Vec::new()));
                        }
                    }
                    continue;
                }
                if let Some(src_local) = base_local(*src) {
                    let src_is_member = alias_of.contains_key(&src_local)
                        && matches!(src, Place::Local(_) | Place::ReturnSlot);
                    let dest_is_member = base_local(*dest).is_some_and(|dest_local| {
                        alias_of.contains_key(&dest_local) && matches!(dest, Place::Local(_))
                    });
                    if src_is_member && !dest_is_member {
                        note_escape(src_local, &mut excluded_roots);
                    }
                }
                continue;
            }
            if matches!(
                instr,
                Instr::Drop { .. } | Instr::BytesRetain { .. } | Instr::StringRetain { .. }
            ) {
                continue;
            }

            if let Instr::ClosureEnvInit { fields, .. } = instr {
                for field in fields
                    .iter()
                    .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
                {
                    let Some(local) = base_local(field.src) else {
                        continue;
                    };
                    if borrowed_alias_of.contains_key(&local) {
                        pending_share_sites.push((block.id, instr_index, field.src, Vec::new()));
                    } else if alias_of.contains_key(&local) {
                        note_escape(local, &mut excluded_roots);
                    }
                }
            }

            let share_places = string_share_sink_places(instr);
            if !share_places.is_empty() {
                let mut candidate_groups: HashMap<u32, Vec<Place>> = HashMap::new();
                let mut external_groups: HashMap<u32, (bool, Vec<Place>)> = HashMap::new();
                for place in share_places {
                    let Some(local) = base_local(place) else {
                        continue;
                    };
                    if let Some(&root) = alias_of.get(&local) {
                        candidate_groups.entry(root).or_default().push(place);
                    } else if borrowed_alias_of.contains_key(&local)
                        && !matches!(instr, Instr::ActorStateFieldStore { .. })
                    {
                        external_groups
                            .entry(local)
                            .or_insert_with(|| (true, Vec::new()))
                            .1
                            .push(place);
                    } else if string_place_is_typed(place, locals)
                        && binding_local_bases.contains(&local)
                        && !parameter_locals.contains(&local)
                    {
                        let survives = local_is_used_after(
                            blocks,
                            suspend_kinds,
                            local,
                            block.id,
                            instr_index,
                        );
                        let entry = external_groups
                            .entry(local)
                            .or_insert_with(|| (survives, Vec::new()));
                        entry.0 |= survives;
                        entry.1.push(place);
                    }
                }
                for (root, values) in candidate_groups {
                    let Some(&binding) = candidate_local_to_binding.get(&root) else {
                        continue;
                    };
                    let source_local = base_local(values[0]).unwrap_or(root);
                    if share_needs_retain(source_local, block.id, instr_index) {
                        for value in values {
                            pending_share_sites.push((block.id, instr_index, value, Vec::new()));
                        }
                    } else {
                        handoff_bindings.insert(binding);
                        for value in values.into_iter().skip(1) {
                            pending_share_sites.push((block.id, instr_index, value, Vec::new()));
                        }
                    }
                }
                for (_local, (survives, values)) in external_groups {
                    let skip = usize::from(!survives);
                    for value in values.into_iter().skip(skip) {
                        pending_share_sites.push((block.id, instr_index, value, Vec::new()));
                    }
                }
                continue;
            }

            match instr {
                Instr::CallRuntimeAbi(call) => {
                    if crate::runtime_symbols::callee_ownership_contract(call.symbol())
                        .borrows_string_call_args()
                    {
                        continue;
                    }
                }
                // Hew by-value heap parameters are borrows. The callee retains
                // only if it mints a co-owner.
                Instr::CallClosure { .. }
                | Instr::CallTraitMethod { .. }
                // String comparisons borrow both operands.
                | Instr::IntCmp { .. } => continue,
                _ => {}
            }

            for place in instr_source_places(instr) {
                if let Some(local) = base_local(place) {
                    if alias_of.contains_key(&local)
                        && matches!(place, Place::Local(_) | Place::ReturnSlot)
                    {
                        note_escape(local, &mut excluded_roots);
                    }
                }
            }
        }

        match &block.terminator {
            Terminator::Call { callee, .. }
                if string_call_borrows(callee, module_fn_names, module_generic_fn_names) => {}
            other => {
                for place in terminator_source_places(other, suspend_kinds.get(&block.id)) {
                    if let Some(local) = base_local(place) {
                        if alias_of.contains_key(&local)
                            && matches!(place, Place::Local(_) | Place::ReturnSlot)
                        {
                            note_escape(local, &mut excluded_roots);
                        }
                    }
                }
            }
        }
    }

    let mut allowed = HashSet::new();
    for (&local, &binding) in &candidate_local_to_binding {
        if handoff_bindings.contains(&binding) {
            continue;
        }
        if tainted.contains(&local) {
            continue;
        }
        let root = alias_of.get(&local).copied().unwrap_or(local);
        if !excluded_roots.contains(&root) {
            allowed.insert(binding);
        }
    }

    let mut retain_sites = pending_share_sites
        .into_iter()
        .map(
            |(block, instr_index, value, required_bindings)| StringRetainSite {
                block,
                instr_index,
                value,
                required_bindings,
            },
        )
        .collect::<Vec<_>>();

    // Returning a by-value parameter duplicates the caller's live reference.
    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            let Instr::Move {
                dest: Place::ReturnSlot,
                src,
            } = instr
            else {
                continue;
            };
            if base_local(*src).is_some_and(|local| borrowed_alias_of.contains_key(&local)) {
                retain_sites.push(StringRetainSite {
                    block: block.id,
                    instr_index,
                    value: *src,
                    required_bindings: Vec::new(),
                });
            }
        }
    }

    // `let alias = source` keeps both string bindings live. Retain exactly once,
    // gated on every local owner that will actually reach a drop.
    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            let Instr::Move {
                dest: Place::Local(dest_local),
                src,
            } = instr
            else {
                continue;
            };
            let Some(src_local) = base_local(*src) else {
                continue;
            };
            let Some(&dest_binding) = candidate_local_to_binding.get(dest_local) else {
                continue;
            };
            if let Some(&root) = alias_of.get(&src_local) {
                if handoff_move_sites.contains(&(block.id, instr_index)) {
                    continue;
                }
                let Some(&src_binding) = candidate_local_to_binding.get(&root) else {
                    continue;
                };
                if src_binding == dest_binding {
                    continue;
                }
                retain_sites.push(StringRetainSite {
                    block: block.id,
                    instr_index,
                    value: *src,
                    required_bindings: Vec::new(),
                });
            } else {
                if !string_place_is_typed(*src, locals)
                    || !((binding_local_bases.contains(&src_local)
                        && !parameter_locals.contains(&src_local))
                        || borrowed_alias_of.contains_key(&src_local))
                {
                    continue;
                }
                retain_sites.push(StringRetainSite {
                    block: block.id,
                    instr_index,
                    value: *src,
                    required_bindings: Vec::new(),
                });
            }
        }
    }

    StringDropDerivation {
        allowed,
        retain_sites,
    }
}
/// P0 #2432 — per-function classifier that sets [`ActorStateLoadMode`] on
/// every `Instr::ActorStateFieldLoad` in `blocks`.
///
/// Called from `lower_function`, AFTER every splice pass that can add or
/// move instructions (`bracket_actor_handler_blocks`,
/// `apply_nested_fresh_string_temp_drops`,
/// `apply_escaped_record_sibling_field_drops`) and BEFORE `RawMirFunction`/
/// `CheckedMirFunction` are constructed — NOT from `elaborate`'s region
/// (despite operating on the identical finalised-block shape
/// `compute_collection_interior_alias_taint` consumes there). Codegen's
/// per-instruction lowering loop and `--dump-mir raw` both read
/// `RawMirFunction.blocks`; `CheckedMirFunction.blocks` is a clone taken
/// AFTER `RawMirFunction` is built. Setting `mode` on the checked-MIR clone
/// instead — after that clone already happened — would be a silent no-op
/// against codegen (LESSONS
/// `verify-ast-carries-discriminator-before-codegen-fix`), so this mutates
/// the SAME `blocks` local that gets moved into `RawMirFunction` and only
/// then cloned into `CheckedMirFunction`.
///
/// `Borrowed` iff EVERY use of a load's `dest`, scanned across every
/// instruction and terminator in the function, is one of:
///   - an interior projection of `dest` — `RecordFieldLoad`/`TupleFieldLoad`/
///     `ClosureEnvFieldLoad` whose base place's local is `dest`'s local, or a
///     `Move` whose `src` borrows per `move_src_borrows_actor_state_load` AND
///     `base_local(src) == base_local(dest)` (the enum/machine tag+payload
///     destructure shape, `lower_match_enum_tag`/`lower_while_let`); that
///     predicate is the direction-locked sibling of `place_is_interior_projection`
///     — handle-place srcs fail closed to the non-borrow (`Owned`/retain)
///     side here rather than the interior side (#2524);
///   - the receiver operand (arg[0]) of a runtime call
///     (`Instr::CallRuntimeAbi` or `Terminator::Call`) whose
///     `callee_ownership_contract` borrows the receiver
///     (`borrows_vec_receiver`/`borrows_collection_receiver`/
///     `borrows_bytes_receiver` — e.g. `hew_vec_len`);
///   - a string operand of a runtime call whose contract borrows its string
///     args (`borrows_string_call_args` — every string inspector/transform
///     and print sink, e.g. `hew_string_length`/`hew_string_concat`); string
///     borrows live on a separate contract axis (`string_args`) from the
///     Vec/bytes receiver axis (`call_arg_borrows_state_load` reads both);
///   - the `.0` (vec) field source of a `RecordInit VecIter { .. }` — the
///     `for x in <collection>` cursor, which BORROWS the source handle for the
///     loop and never frees it (see `vec_iter_record_init_vec_source`).
///
/// Any other use — a whole-value `Move`, a store src, a return/aggregate/
/// spawn/by-value-call operand, or any use the positive categories above
/// do not recognise — forces `Owned`, the fail-closed default every
/// construct site already seeds. A `dest` with NO observed use also stays
/// `Owned`: the positive `Borrowed` branch is reserved for an AFFIRMATIVELY
/// proven borrow, never a vacuous one (`boundary-fail-closed`).
/// Whether a `Move` whose `src` reads an actor-state load's `dest` is a
/// borrow-consumer (no whole-value escape) for [`classify_actor_state_load_modes`].
///
/// This is the classifier-local, DIRECTION-LOCKED sibling of the shared
/// [`place_is_interior_projection`] hand-off-vs-interior predicate. It exists
/// because the two predicates fail closed in OPPOSITE directions and must not
/// share an implementation (#2524):
///
///   - `place_is_interior_projection` answers "is this an interior alias I must
///     NOT scope-exit-drop as a hand-off?". Its fail-closed direction is
///     `true` (interior) — over-tainting only over-EXCLUDES a drop (leaks),
///     so it parks handle/tag places on the `true` arm.
///   - This predicate answers "is EVERY use of the load's `dest` a borrow, so
///     codegen may skip the retain?". Here `true` is the `Borrowed` verdict —
///     the bare-load/no-retain path, which is the USE-AFTER-FREE direction if
///     the read value is ever an owned whole-value alias. Its fail-closed
///     direction is therefore `false` (`Owned`/retain).
///
/// So a handle place (`Duplex/LambdaActor/Actor` handle, `Send/RecvHalf`)
/// must classify `false` here even though `place_is_interior_projection`
/// classifies it `true`: pinning it to `Owned` keeps the retain dispatch in
/// force regardless of any future handle `StateFieldCloneKind` clone-kind
/// change. This is behaviour-preserving today — every handle
/// `StateFieldCloneKind` (`OpaqueHandle`/`Resource`/`BitCopy`/`IoHandle`)
/// takes the no-retain arm in `lower_actor_state_field_load`, so the `Owned`
/// and `Borrowed` codegen paths are byte-identical for a handle-typed field —
/// but it removes the fragile coupling the shared predicate's handle arm
/// created (if a handle place ever gained a retaining clone kind, routing it
/// through `place_is_interior_projection` would silently over-borrow it into
/// the UAF direction). Only the genuine interior-destructure projections
/// (`MachineVariant`/`EnumVariant`) and the tag reads (`MachineTag`/`EnumTag`,
/// bitcopy ordinals that transfer no heap) borrow here.
fn move_src_borrows_actor_state_load(src: Place) -> bool {
    match src {
        // Interior payload destructure + tag reads: a bare alias / bitcopy
        // ordinal, no whole-value heap escape. These are the ONLY borrows.
        Place::MachineVariant { .. }
        | Place::EnumVariant { .. }
        | Place::MachineTag(_)
        | Place::EnumTag(_) => true,
        // Whole-value ownership slots AND handle places: fail closed to
        // `Owned` (retain). A `Local`/`ReturnSlot` move is a hand-off escape;
        // a handle place is direction-locked here (see doc comment) so it can
        // never be demoted into the no-retain UAF direction by a future
        // handle clone-kind change.
        Place::Local(_)
        | Place::ReturnSlot
        | Place::DuplexHandle(_)
        | Place::LambdaActorHandle(_)
        | Place::ActorHandle(_)
        | Place::SendHalf(_)
        | Place::RecvHalf(_) => false,
    }
}
#[cfg(test)]
mod actor_state_load_direction_lock_tests {
    use super::*;

    // Interior payload destructures + tag reads are the only borrows: a bare
    // alias / bitcopy ordinal, no whole-value heap escape.
    #[test]
    fn interior_variant_and_tag_srcs_borrow() {
        for src in [
            Place::MachineVariant {
                local: 3,
                variant_idx: 0,
                field_idx: 0,
            },
            Place::EnumVariant {
                local: 3,
                variant_idx: 1,
                field_idx: 0,
            },
            Place::MachineTag(3),
            Place::EnumTag(3),
        ] {
            assert!(
                move_src_borrows_actor_state_load(src),
                "{src:?} is an interior projection / tag read; it must borrow \
                 (no retain) so the whole-parent clone stays consumable"
            );
        }
    }

    // Whole-value ownership slots are hand-off escapes: fail closed to Owned
    // (retain), never Borrowed.
    #[test]
    fn whole_value_slots_do_not_borrow() {
        for src in [Place::Local(3), Place::ReturnSlot] {
            assert!(
                !move_src_borrows_actor_state_load(src),
                "{src:?} is a whole-value hand-off; it must classify Owned \
                 (retain) so a later escape does not alias freed heap"
            );
        }
    }

    // #2524 direction lock: handle places must fail closed to Owned here even
    // though `place_is_interior_projection` parks them on its `true` arm. The
    // two predicates fail closed in OPPOSITE directions and must not agree on
    // handle places.
    #[test]
    fn handle_places_are_direction_locked_to_owned() {
        for src in [
            Place::DuplexHandle(3),
            Place::LambdaActorHandle(3),
            Place::ActorHandle(3),
            Place::SendHalf(3),
            Place::RecvHalf(3),
        ] {
            assert!(
                place_is_interior_projection(src),
                "precondition: the shared hand-off predicate parks {src:?} on \
                 its fail-closed `true` (interior) arm"
            );
            assert!(
                !move_src_borrows_actor_state_load(src),
                "#2524: {src:?} must NOT borrow at the actor-state-load site \
                 (that is the no-retain UAF direction); it is direction-locked \
                 to Owned/retain regardless of any future handle clone kind"
            );
        }
    }
}
pub(super) fn classify_actor_state_load_modes(
    blocks: &mut [BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    locals: &[ResolvedTy],
) {
    let mut load_locals: HashSet<u32> = HashSet::new();
    for block in blocks.iter() {
        for instr in &block.instructions {
            if let Instr::ActorStateFieldLoad { dest, .. } = instr {
                if let Some(l) = base_local(*dest) {
                    load_locals.insert(l);
                }
            }
        }
    }
    if load_locals.is_empty() {
        // No actor-state loads in this function — nothing to classify. Every
        // OTHER lowering entry point (closure shims, task/fork adapters,
        // lambda-actor spawns, machine step synthesis) hits this early
        // return: `current_actor_state_fields` is only ever populated while
        // lowering an actor's own receive-handler body, so only
        // `lower_function`'s call site ever has non-empty `load_locals`.
        return;
    }

    // `all_borrow[l]` starts `true` and is AND-ed down by every observed use;
    // `any_use` records whether at least one use was observed at all.
    let mut all_borrow: HashMap<u32, bool> = HashMap::new();
    let mut any_use: HashSet<u32> = HashSet::new();

    for block in blocks.iter() {
        for instr in &block.instructions {
            record_actor_state_load_instr_uses(
                instr,
                locals,
                &load_locals,
                &mut any_use,
                &mut all_borrow,
            );
        }
        record_actor_state_load_terminator_uses(
            &block.terminator,
            suspend_kinds.get(&block.id),
            locals,
            &load_locals,
            &mut any_use,
            &mut all_borrow,
        );
    }

    for block in blocks.iter_mut() {
        for instr in &mut block.instructions {
            if let Instr::ActorStateFieldLoad { dest, mode, .. } = instr {
                if let Some(l) = base_local(*dest) {
                    let proven_borrow =
                        any_use.contains(&l) && *all_borrow.get(&l).unwrap_or(&false);
                    if proven_borrow {
                        *mode = ActorStateLoadMode::Borrowed;
                    }
                }
            }
        }
    }
}
#[cfg(test)]
mod actor_state_load_classifier_direction_lock_tests {
    use super::*;

    fn actor_state_load_mode_after_move(src: Place) -> ActorStateLoadMode {
        let load_dest = 3;
        let mut blocks = vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::ActorStateFieldLoad {
                    field_offset: FieldOffset(0),
                    dest: Place::Local(load_dest),
                    mode: ActorStateLoadMode::Owned,
                },
                Instr::Move {
                    dest: Place::Local(4),
                    src,
                },
            ],
            terminator: Terminator::Return,
        }];

        classify_actor_state_load_modes(&mut blocks, &HashMap::new(), &[]);

        match &blocks[0].instructions[0] {
            Instr::ActorStateFieldLoad { mode, .. } => *mode,
            _ => unreachable!("fixture's first instruction must be the actor-state load"),
        }
    }

    #[test]
    fn duplex_handle_move_keeps_actor_state_load_owned() {
        assert_eq!(
            actor_state_load_mode_after_move(Place::DuplexHandle(3)),
            ActorStateLoadMode::Owned,
            "#2524: the classifier must treat a duplex-handle whole-value move \
             as an owned escape, retaining the actor-state load"
        );
    }

    #[test]
    fn interior_variant_move_borrows_actor_state_load() {
        assert_eq!(
            actor_state_load_mode_after_move(Place::MachineVariant {
                local: 3,
                variant_idx: 0,
                field_idx: 0,
            }),
            ActorStateLoadMode::Borrowed,
            "an interior variant payload move is a genuine borrow-consumer"
        );
    }
}
/// Record `local`'s use as borrow-consumer (`is_borrow`) or escape,
/// AND-reducing the running per-local verdict `classify_actor_state_load_modes`
/// reads back once every instruction/terminator has been scanned. A no-op
/// for a local outside `load_locals` (not an actor-state load dest).
fn mark_actor_state_load_use(
    load_locals: &HashSet<u32>,
    any_use: &mut HashSet<u32>,
    all_borrow: &mut HashMap<u32, bool>,
    local: u32,
    is_borrow: bool,
) {
    if !load_locals.contains(&local) {
        return;
    }
    any_use.insert(local);
    let entry = all_borrow.entry(local).or_insert(true);
    *entry = *entry && is_borrow;
}
/// Classify one instruction's read(s) of any `load_locals` member as a
/// borrow-consumer or an escape (`classify_actor_state_load_modes`'s positive
/// categories: §RecordFieldLoad/TupleFieldLoad/ClosureEnvFieldLoad base,
/// interior-projection `Move` src, receiver-borrow / string-borrow call args,
/// and the `for x in <collection>` `VecIter` cursor source). The `_` arm falls
/// through to [`instr_source_places`] — itself a closed exhaustive match over
/// every `Instr` variant — so a future MIR variant that reads a load's dest
/// defaults to counting as a non-borrow use: over-classification always
/// pushes toward `Owned`, never toward `Borrowed`.
fn record_actor_state_load_instr_uses(
    instr: &Instr,
    locals: &[ResolvedTy],
    load_locals: &HashSet<u32>,
    any_use: &mut HashSet<u32>,
    all_borrow: &mut HashMap<u32, bool>,
) {
    match instr {
        Instr::RecordFieldLoad { record, .. } => {
            if let Some(l) = base_local(*record) {
                mark_actor_state_load_use(load_locals, any_use, all_borrow, l, true);
            }
        }
        Instr::TupleFieldLoad { tuple, .. } => {
            if let Some(l) = base_local(*tuple) {
                mark_actor_state_load_use(load_locals, any_use, all_borrow, l, true);
            }
        }
        Instr::ClosureEnvFieldLoad { env, .. } => {
            if let Some(l) = base_local(*env) {
                mark_actor_state_load_use(load_locals, any_use, all_borrow, l, true);
            }
        }
        Instr::Move { src, .. } => {
            if let Some(l) = base_local(*src) {
                let is_borrow = move_src_borrows_actor_state_load(*src);
                mark_actor_state_load_use(load_locals, any_use, all_borrow, l, is_borrow);
            }
        }
        Instr::CallRuntimeAbi(call) => {
            let contract = crate::runtime_symbols::callee_ownership_contract(call.symbol());
            for (i, place) in call.args().iter().enumerate() {
                if let Some(l) = base_local(*place) {
                    mark_actor_state_load_use(
                        load_locals,
                        any_use,
                        all_borrow,
                        l,
                        call_arg_borrows_state_load(contract, locals, i, l),
                    );
                }
            }
        }
        Instr::RecordInit { ty, fields, .. } if vec_iter_record_layout_key(ty).is_some() => {
            // A `for x in <collection>` desugar lowers the source collection
            // into a synthetic `VecIter { vec: <source>, idx: 0 }` cursor. The
            // cursor BORROWS the source's handle for the loop's duration — it
            // reads the handle via `len()`/`get(i)` and NEVER frees what it
            // borrows (see `vec_iter_record_init_vec_source`). So the `.0`
            // (vec) field source is a borrow-consumer of the loaded state
            // field, not a whole-value escape: classifying the load `Borrowed`
            // aliases the actor's still-owned state Vec (byte-identical to the
            // pre-#2432 behaviour) instead of deep-cloning a handle the cursor
            // never frees (the per-frame leak this arm closes). This holds for
            // every element type — the cursor borrows a drop-safe (BitCopy)
            // source it reads, and never frees an owned-element source at all.
            // Any non-`.0` field source (only the BitCopy `idx` in practice) is
            // an escape (fail-closed).
            for (offset, src) in fields {
                if let Some(l) = base_local(*src) {
                    mark_actor_state_load_use(load_locals, any_use, all_borrow, l, offset.0 == 0);
                }
            }
        }
        _ => {
            for place in instr_source_places(instr) {
                if let Some(l) = base_local(place) {
                    mark_actor_state_load_use(load_locals, any_use, all_borrow, l, false);
                }
            }
        }
    }
}
/// `true` when call argument `i` (a `load_locals` member, local `l`) is only
/// BORROWED by `contract` — either the receiver-borrow arg[0] (Vec/collection/
/// bytes receivers, e.g. `hew_vec_len`) or a string operand of a call that
/// borrows its string args (`borrows_string_call_args` — every string
/// inspector/transform and print sink; the string is read or refcount-copied,
/// the caller keeps the drop obligation). A string state-field value read
/// through such a call (`name.len()`, `name + "x"`, `println(name)`) therefore
/// classifies `Borrowed` instead of emitting an unbalanced `hew_string_clone`.
/// String borrows live in `string_args`, a separate contract axis from the
/// receiver axis the Vec/bytes categories read, so both axes are consulted.
fn call_arg_borrows_state_load(
    contract: crate::runtime_symbols::CalleeOwnershipContract,
    locals: &[ResolvedTy],
    arg_index: usize,
    arg_local: u32,
) -> bool {
    let borrows_receiver = contract.borrows_vec_receiver()
        || contract.borrows_collection_receiver()
        || contract.borrows_bytes_receiver();
    if arg_index == 0 && borrows_receiver {
        return true;
    }
    contract.borrows_string_call_args()
        && matches!(locals.get(arg_local as usize), Some(ResolvedTy::String))
}
/// The `Terminator::Call` analogue of [`record_actor_state_load_instr_uses`]
/// — the receiver-borrow-arg[0] and string-borrow-arg positive categories
/// (`call_arg_borrows_state_load`) apply identically to a block-terminating
/// call. Every other terminator variant falls through to
/// [`terminator_source_places`] (closed exhaustive match), marked as a
/// non-borrow use.
fn record_actor_state_load_terminator_uses(
    term: &Terminator,
    suspend_kind: Option<&SuspendKind>,
    locals: &[ResolvedTy],
    load_locals: &HashSet<u32>,
    any_use: &mut HashSet<u32>,
    all_borrow: &mut HashMap<u32, bool>,
) {
    match term {
        Terminator::Call { callee, args, .. } => {
            let contract = crate::runtime_symbols::callee_ownership_contract(callee);
            for (i, place) in args.iter().enumerate() {
                if let Some(l) = base_local(*place) {
                    mark_actor_state_load_use(
                        load_locals,
                        any_use,
                        all_borrow,
                        l,
                        call_arg_borrows_state_load(contract, locals, i, l),
                    );
                }
            }
        }
        _ => {
            for place in terminator_source_places(term, suspend_kind) {
                if let Some(l) = base_local(place) {
                    mark_actor_state_load_use(load_locals, any_use, all_borrow, l, false);
                }
            }
        }
    }
}
/// Projection-alias taint set: locals that hold an *interior pointer* of a
/// still-live aggregate, propagated forward through `Move` to a fixpoint.
///
/// Seeds:
/// - every interior-pointer load dest (`projection_alias_dest` — the four
///   `*FieldLoad` instrs: record / tuple / closure-env / actor-state field);
/// - every `Move` whose `src` is an interior projection (an enum/machine
///   payload destructure binder — `lower_match_enum_tag` / `lower_while_let` —
///   which aliases parent storage with no retain; `projection_alias_dest` does
///   not see these, so they are seeded directly).
///
/// Propagation: a value `Move`d from a tainted local is itself a parent-interior
/// alias, so taint flows forward through `Move` to a fixpoint. Dropping any
/// tainted local would free a buffer its parent aggregate still owns, so both
/// `derive_cow_sole_owner` and the W5.011 P3 fresh-borrowed admission
/// (`derive_cow_fresh_borrowed_owner`) exclude every tainted local.
///
/// `exempt_seed_locals` suppresses the SEED for match-destructure binders whose
/// scrutinee was consume-marked at the destructure site
/// (`match_project_consumed_binder_locals`): the parent's composite drop is
/// dataflow-suppressed, so the projection-alias-as-double-free reasoning does
/// not apply and the binder is itself the sole owner. Skipping the seed is
/// sufficient — the binder's slot is `alloc_local`-fresh, not a re-projection,
/// so no later `Move`-fixpoint round re-taints it. The exemption applies ONLY
/// to the `*FieldLoad` seed (`lower_match_project`'s shape); the
/// Move-from-interior seed (enum/machine destructures) is never exempted.
/// Passing an empty set is the conservative choice — it taints strictly more,
/// which only ever excludes more bindings (fail-closed).
///
/// `locals` is the function's declared local-type table. A `string`-typed
/// `*FieldLoad` dest is NOT seeded as a projection alias: codegen now retains it
/// (`retain_string_field_load`), so it is a genuine fresh `+1` owner admitted via
/// `string_field_load_producer_dest`, not a no-retain interior alias. Excluding
/// it from the taint is what lets `derive_cow_fresh_borrowed_owner` admit it for
/// its one balancing drop — the two-sided coupling. Non-string `*FieldLoad` dests
/// are NOT retained by codegen, so they stay tainted here (leak, never
/// double-free) until the retain-on-share spine lands. This exclusion is a
/// provable no-op for the collection/Vec callers (a string-typed local is never a
/// Vec/owned-record/HashMap drop candidate), so threading `locals` keeps one
/// taint authority with consistent semantics across all consumers.
#[must_use]
pub(super) fn compute_projection_alias_taint(
    blocks: &[BasicBlock],
    exempt_seed_locals: &HashSet<u32>,
    locals: &[ResolvedTy],
) -> HashSet<u32> {
    // #2523 — projection slots whose heap ownership was TRANSFERRED to a new
    // owner by a projected-payload move-out. The `Move` that copies such a slot
    // into the new owner's local is followed by an `Instr::NeutralizePayloadSlot`
    // that nulls the source, so the destination local is the SOLE owner, not a
    // parent-interior alias — it must NOT be tainted (that would suppress its
    // scope-exit drop and leak the transferred buffer, the very #2523 leak).
    let neutralized_sources: HashSet<Place> = blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::NeutralizePayloadSlot { place, .. } => Some(*place),
            _ => None,
        })
        .collect();
    let mut tainted: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Some(dest) = projection_alias_dest(instr) {
                if let Some(l) = base_local(dest) {
                    // A `string` field-load dest is a retained fresh producer, not
                    // a no-retain alias — exclude it from the taint so its
                    // balancing drop is admitted (see the doc above).
                    let is_retained_string_field_load =
                        string_field_load_producer_dest(instr, locals).is_some();
                    if !exempt_seed_locals.contains(&l) && !is_retained_string_field_load {
                        tainted.insert(l);
                    }
                }
            }
            if let Instr::Move { dest, src } = instr {
                // An interior-projection move whose source is neutralized
                // transfers ownership (#2523): the dest is the sole owner, not an
                // alias. Skip the taint seed so its drop is admitted exactly once.
                if place_is_interior_projection(*src) && !neutralized_sources.contains(src) {
                    if let Some(dl) = base_local(*dest) {
                        tainted.insert(dl);
                    }
                }
            }
        }
        if let Terminator::MakeGenerator { env: Some(env), .. } = &block.terminator {
            if let Some(local) = base_local(env.place) {
                tainted.insert(local);
            }
        }
    }
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        if tainted.contains(&sl) && tainted.insert(dl) {
                            changed = true;
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    tainted
}
/// Interior-alias taint for the COLLECTION / owned-vector sole-owner provers:
/// the locals that hold an interior pointer of a still-live aggregate and must
/// therefore never earn an independent scope-exit free.
///
/// Extends [`compute_projection_alias_taint`] (the record/tuple/closure-env/
/// actor-state field loads + enum/machine payload destructures the string and
/// bytes leaf provers already exclude) with the owned-element VECTOR getters
/// whose callee contract returns [`crate::runtime_symbols::ResultOwnership::InteriorAliasOfReceiver`],
/// which lower as either an
/// `Instr::CallRuntimeAbi` (the `xs[i]` index path) or a `Terminator::Call`
/// (the `xs.get(i)` method path). A handle loaded out of a parent vector's slot
/// is a borrow of that slot; dropping it double-frees the element the parent's
/// own drop path owns. Taint propagates forward through whole-value `Move` so a
/// rebind (`let r2 = r;`) of a borrowed element handle stays tainted.
///
/// Fail-closed: an empty exempt set is passed to the projection-alias seed
/// (taint strictly more), and over-tainting only ever over-EXCLUDES a binding
/// from drop (a leak, never a double-free).
///
/// An EMPTY `locals` slice is passed to `compute_projection_alias_taint`, which
/// disables its string `*FieldLoad` retain-exclusion: this collection taint must
/// keep tainting EVERY `*FieldLoad` dest (a Vec/owned-record/HashMap loaded from
/// a record field is an interior borrow that must never earn its own free). A
/// string-typed `*FieldLoad` dest is never a collection drop candidate, so NOT
/// excluding it here changes nothing — the string exclusion belongs only to the
/// string provers (`derive_cow_sole_owner` / `derive_cow_fresh_borrowed_owner`),
/// which pass the real `locals`.
#[must_use]
pub(super) fn compute_collection_interior_alias_taint(blocks: &[BasicBlock]) -> HashSet<u32> {
    let mut tainted = compute_projection_alias_taint(blocks, &HashSet::new(), &[]);
    // Seed the borrowing vector element getters (both lowering shapes).
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::CallRuntimeAbi(call) = instr {
                if crate::runtime_symbols::callee_ownership_contract(call.symbol())
                    .returns_receiver_interior_alias()
                {
                    if let Some(local) = call.dest().and_then(base_local) {
                        tainted.insert(local);
                    }
                }
            }
        }
        if let Terminator::Call { callee, dest, .. } = &block.terminator {
            if crate::runtime_symbols::callee_ownership_contract(callee)
                .returns_receiver_interior_alias()
            {
                if let Some(local) = dest.and_then(base_local) {
                    tainted.insert(local);
                }
            }
        }
    }
    // Forward-propagate the new seeds through whole-value `Move` to a fixpoint
    // (the field-load seeds were already propagated by the call above; the
    // getter seeds need a fresh pass).
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        if tainted.contains(&sl) && tainted.insert(dl) {
                            changed = true;
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    tainted
}
/// W5.011 P3 — the destination place of an instruction that produces a fresh,
/// solely-owned `string` (a `+1` owner the caller must balance with exactly one
/// `hew_string_drop`). Only the validated runtime-ABI producers
/// whose callee contract produces a fresh owned string qualify; everything else
/// returns `None` (the local is not seeded as fresh, so it is never admitted —
/// fail-closed).
fn fresh_string_producer_dest(instr: &Instr) -> Option<Place> {
    match instr {
        Instr::CallRuntimeAbi(call)
            if crate::runtime_symbols::callee_ownership_contract(call.symbol())
                .produces_fresh_owned_string() =>
        {
            call.dest()
        }
        _ => None,
    }
}
/// W5.011 P3 — the `Terminator::Call` analogue of [`fresh_string_producer_dest`]
/// (string transforms like `to_uppercase` / `slice` and the `Vec<string>`
/// getter `hew_vec_get_str` lower to block-terminating calls, not `Instr`s).
fn fresh_string_producer_term_dest(term: &Terminator) -> Option<Place> {
    match term {
        Terminator::Call {
            callee,
            builtin: None,
            dest,
            ..
        } if fresh_string_producer_term_admissible(callee) => *dest,
        _ => None,
    }
}
fn fresh_string_producer_term_admissible(callee: &str) -> bool {
    let contract = crate::runtime_symbols::callee_ownership_contract(callee);
    if contract.returns_receiver_interior_alias() {
        return false;
    }
    !crate::runtime_symbols::is_known_runtime_symbol(callee)
        || contract.produces_fresh_owned_string()
}
/// The dest of a `string`-typed record/tuple/closure-env field load — a fresh
/// `+1` owner once codegen retains it (`retain_string_field_load`,
/// hew-codegen-rs).
///
/// A `string` loaded from a record field (`RecordFieldLoad`), tuple element
/// (`TupleFieldLoad`), or closure capture (`ClosureEnvFieldLoad`) is emitted by
/// codegen with a `hew_string_clone` retain, so the loaded local is an
/// independent `+1` owner — the identical ownership shape as the `Vec<string>`
/// getter `hew_vec_get_str` (`xs.get(i)`), which is already admitted as a fresh
/// producer. This classifier moves the string field-load dest from the
/// "projection alias, never drop" class into the "fresh `+1`, drop once when
/// sole owner" class so `derive_cow_fresh_borrowed_owner` admits it for exactly
/// one balancing scope-exit `hew_string_drop`.
///
/// The two sides are coupled and MUST move together: codegen retains ONLY
/// string-typed `*FieldLoad` dests (detected by the dest's `ResolvedTy`), so this
/// classifier admits ONLY string-typed `*FieldLoad` dests (same type gate). A
/// non-string field load (`bytes` / `Vec<T>` / nested record) is NOT retained by
/// codegen and stays a projection alias here — it leaks rather than double-frees,
/// the safe default, until the retain-on-share spine lands. The type gate uses
/// the function's `locals` table (the dest local's declared `ResolvedTy`), the
/// same precise string distinguisher codegen uses.
pub(super) fn string_field_load_producer_dest(
    instr: &Instr,
    locals: &[ResolvedTy],
) -> Option<Place> {
    let dest = match instr {
        Instr::RecordFieldLoad { dest, .. }
        | Instr::TupleFieldLoad { dest, .. }
        | Instr::ClosureEnvFieldLoad { dest, .. } => *dest,
        _ => return None,
    };
    let local = base_local(dest)?;
    match locals.get(local as usize) {
        Some(ResolvedTy::String) => Some(dest),
        _ => None,
    }
}
/// The dest of a wire-codec text serialize (`value.to_json()` / `value.to_yaml()`
/// → `string`) — a fresh, solely-owned `string` the caller owes exactly one
/// `hew_string_drop`. Discriminate by the DEST TYPE (`string`), not by direction:
/// `ToJson`/`ToYaml` are the only text-serialize directions whose dest is a leaf
/// `string` (`From*`'s dest is `Result<T, string>`, `Encode`'s is `bytes`,
/// `Decode`'s is the value type). `Packet.from_json(p.to_json())` leaked the
/// `to_json` temp before this admission.
fn wire_codec_string_producer_dest(instr: &Instr, locals: &[ResolvedTy]) -> Option<Place> {
    match instr {
        Instr::WireCodec { dest, .. } if string_place_is_typed(*dest, locals) => Some(*dest),
        _ => None,
    }
}
/// `true` when `instr` is a wire-codec deserialize (`Type.from_json(s)` /
/// `Type.from_yaml(s)`) whose `operand` BORROWS the leaf-`string` temp `t`:
/// codegen's `lower_wire_codec_instr` loads the operand pointer to feed the text
/// parser and never frees it (model.rs: "The `operand` is borrowed"). So a fresh
/// `string` temp read only as a `from_json`/`from_yaml` operand keeps its single
/// drop obligation and is released once, immediately after the parse.
fn is_wire_codec_borrowing_string_use(instr: &Instr, t: u32) -> bool {
    matches!(instr, Instr::WireCodec { operand, .. } if place_refs_local(*operand, t))
}
/// `true` when `instr` is a string comparison (`==`/`!=`/`<`/`<=`/`>`/`>=`)
/// that **borrows** the leaf-`string` temp `t` as an operand. String compares
/// lower to `Instr::IntCmp { pred, lhs, rhs }` (see `lower_binary`); codegen
/// routes string-typed `IntCmp` operands through `hew_string_equals` /
/// `hew_string_compare` (`hew-codegen-rs/src/llvm.rs`), both of which `strcmp`
/// their arguments WITHOUT consuming the refcount (verified in
/// `hew-runtime/src/string.rs`: `CStr`/`strcmp` read, never free). So a fresh
/// `+1`-retained `hew_vec_get_str` temp fed into a compare still owns its single
/// drop obligation afterwards — `xs[i] == "hello"` leaked the retain before this
/// admission. The leaf-`string` type gate (rule 1) guarantees the operand takes
/// codegen's borrowing string-`icmp` path, not the aggregate structural-eq thunk
/// (which only fires for `Named`/struct operands a `hew_vec_get_str` temp can
/// never be).
fn is_borrowing_string_cmp_instr(instr: &Instr, t: u32) -> bool {
    matches!(
        instr,
        Instr::IntCmp { lhs, rhs, .. }
            if place_refs_local(*lhs, t) || place_refs_local(*rhs, t)
    )
}
fn is_hew_string_concat_runtime_call(call: &crate::model::RuntimeCall) -> bool {
    call.symbol() == "hew_string_concat"
}
fn is_fresh_string_producer_def(
    def: NestedDefSite,
    blocks: &[BasicBlock],
    locals: &[ResolvedTy],
    t: u32,
) -> bool {
    let dest = match def {
        NestedDefSite::Instr { block, idx } => block_by_id(blocks, block)
            .and_then(|b| b.instructions.get(idx))
            .and_then(|instr| {
                fresh_string_producer_dest(instr)
                    .or_else(|| string_field_load_producer_dest(instr, locals))
            }),
        NestedDefSite::Term { block } => {
            block_by_id(blocks, block).and_then(|b| fresh_string_producer_term_dest(&b.terminator))
        }
    };
    dest.and_then(base_local) == Some(t)
}
fn is_borrowing_string_concat_instr_use(instr: &Instr, t: u32) -> bool {
    let Instr::CallRuntimeAbi(call) = instr else {
        return false;
    };
    is_hew_string_concat_runtime_call(call)
        && call.args().iter().any(|p| place_refs_local(*p, t))
        && crate::runtime_symbols::callee_ownership_contract(call.symbol())
            .borrows_string_call_args()
}
/// `true` when control flows from `start` to `use_block` along a chain of
/// single-predecessor `Call`-terminated blocks — so reaching `use_block`
/// implies the block that produced `start` ran exactly once, and every block on
/// the chain (including `use_block`) has that single predecessor.
///
/// `start` is the def producer's `Call` continuation (the `next` of a
/// terminator-def, or the def instruction's own block terminator's `next`).
/// A single-step chain (`start == use_block`, `pred_count[use_block] == 1`) is
/// the shape the terminator-def domination check historically proved directly.
/// The multi-step walk is the #2726 f-string case: a `to_string_*` conversion
/// lowers to a block-terminating `Call` that SPLITS the concat chain, so the
/// concat consuming an intermediate result is separated from the concat that
/// produced it by that conversion block. Each intervening block is itself a
/// single-predecessor `Call` continuation, so the whole span is straight-line —
/// no branch merges in, so reaching the use implies the def ran once.
///
/// The walk is bounded by the block count: a single-predecessor chain cannot
/// revisit a block (a revisited block would have a second predecessor), so the
/// bound is a belt-and-braces guard, never a real limit.
fn dominates_use_via_single_pred_chain(
    blocks: &[BasicBlock],
    start: u32,
    use_block: u32,
    pred_count: &HashMap<u32, usize>,
) -> bool {
    let mut cur = start;
    for _ in 0..blocks.len() {
        if pred_count.get(&cur).copied().unwrap_or(0) != 1 {
            return false;
        }
        if cur == use_block {
            return true;
        }
        match block_by_id(blocks, cur).and_then(|b| call_terminator_next(&b.terminator)) {
            Some(n) => cur = n,
            None => return false,
        }
    }
    false
}
/// W5.011 P3 — `true` when an instruction transfers ownership of a fresh-owned
/// `string` `local` out of its slot, so a scope-exit drop of `local` would be a
/// double-free (over-decrement of the shared refcount).
///
/// The base classifier is [`generator_yield_instr_escapes`], which is exhaustive
/// over every `Instr` variant and already catches the ownership-transferring
/// shapes (`Move`, aggregate stores, spawns, re-drops). It treats EVERY call
/// (`CallRuntimeAbi` / `CallClosure` / `CallTraitMethod`) as a borrow — correct
/// for a per-iteration generator yield, but too permissive for a function-scope
/// owned string, which a consuming runtime call (`hew_hashmap_insert_layout`
/// moves the string) may take ownership of.
///
/// Runtime calls consult the closed ownership contract. User/closure/trait
/// by-value parameters borrow; a callee that mints a co-owner retains there.
fn cow_owned_string_instr_escapes(instr: &Instr, local: u32) -> bool {
    match instr {
        Instr::CallRuntimeAbi(call) => {
            call.args().iter().any(|p| place_refs_local(*p, local))
                && !crate::runtime_symbols::callee_ownership_contract(call.symbol())
                    .borrows_string_call_args()
        }
        // User/closure/trait by-value parameters borrow. A callee that mints a
        // co-owner retains at that mint point.
        Instr::CallClosure { .. } | Instr::CallTraitMethod { .. } => false,
        _ => generator_yield_instr_escapes(instr, local),
    }
}
/// W5.011 P3 — the `Terminator` analogue of [`cow_owned_string_instr_escapes`].
/// [`generator_yield_terminator_escapes`] is the exhaustive base (it catches
/// `Return`-via-`Move`, `Send`/`Ask`/`Yield`, `Select`/`Join`); it treats a
/// `Terminator::Call` as a borrow. This refines the `Call` arm by distinguishing
/// Hew-bodied borrowing calls from consuming runtime/release calls.
fn cow_owned_string_terminator_escapes(
    term: &Terminator,
    suspend_kind: Option<&SuspendKind>,
    local: u32,
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
) -> bool {
    match term {
        Terminator::Call { callee, args, .. } => {
            args.iter().any(|p| place_refs_local(*p, local))
                && !string_call_borrows(callee, module_fn_names, module_generic_fn_names)
        }
        _ => generator_yield_terminator_escapes(term, suspend_kind, local),
    }
}
fn string_call_borrows(
    callee: &str,
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
) -> bool {
    let contract = crate::runtime_symbols::callee_ownership_contract(callee);
    if contract.borrows_string_call_args() {
        return true;
    }
    if callee == "hew_string_drop" || crate::runtime_symbols::is_known_runtime_symbol(callee) {
        return false;
    }
    if module_fn_names.contains(callee) || module_generic_fn_names.contains(callee) {
        return true;
    }
    false
}
/// W5.011 P3 — borrowing-read-aware owned-`string` cleanup. Returns the subset
/// of `owned_locals` that hold a **proven fresh sole owner used only in
/// borrowing reads**, which therefore earns a scope-exit
/// `DropKind::CowHeap { release: String }` (`hew_string_drop`).
///
/// ## Why this exists (the leak `derive_cow_sole_owner` leaves behind)
///
/// `derive_cow_sole_owner` admits a `string` binding ONLY when its backing local
/// is NEVER read as a source operand. That blunt rule excludes the overwhelmingly
/// common shape `let y = a + b; y.len()` — `y` IS read (by `len`), so it leaks.
/// Relaxing the base rule to "read only by borrows" is unsound on its own: a
/// borrow-only-read local that ALIASES a caller-owned buffer (a `string`
/// parameter, an interior projection, a user-call result) must NOT be dropped,
/// or the shared refcount is over-decremented (premature free / use-after-free).
///
/// ## The ownership model (refcounted `string`, fail-closed)
///
/// `string` is refcounted: `hew_string_drop` decrements and frees at zero;
/// `hew_string_clone` / `hew_vec_get_str` bump the count and alias the same
/// buffer. Every fresh-owned-string contract hands the caller
/// exactly ONE drop obligation (fresh `rc == 1`, or a `+1` retain). A binding is
/// admitted here iff ALL of:
///
/// 1. it is a leaf `string` (`cow_value_leaf_drop_symbol` is `Some`);
/// 2. its backing local is **proven fresh** — it traces, through `Move` rebinds,
///    to the dest of a known fresh producer (rule keystone: this is what
///    excludes parameter aliases, interior projections, and user-call results,
///    none of which are fresh producers);
/// 3. it is not projection-alias tainted (`compute_projection_alias_taint`);
/// 4. **every** use of the local is a verified borrow
///    (`cow_owned_string_*_escapes` is `false` everywhere) — any
///    ownership-transferring escape (return, store, spawn, send, container-move
///    insert, user/closure call) excludes it.
///
/// This is **additive** to `derive_cow_sole_owner`: the caller unions the two
/// allow-sets. The base admits never-read sole owners; this admits read-only-by-
/// borrow fresh owners. The sets are disjoint by construction (never-read vs
/// read-by-borrow), and a `Move` chain's earlier owners are excluded (they appear
/// as a `Move` source — an escape), so a single buffer is never admitted twice.
///
/// Fail-closed throughout: an unrecognised producer (rule 2), an unrecognised
/// use (rule 4), or any taint (rule 3) leaves the binding out of the set — it
/// leaks, exactly as before this fix, and can never double-free.
///
/// LESSONS: boundary-fail-closed (P0), cleanup-all-exits, raii-null-after-move.
#[must_use]
pub(super) fn derive_cow_fresh_borrowed_owner(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    locals: &[ResolvedTy],
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
) -> HashSet<BindingId> {
    // 1. Fresh-owner locals: the dest of a known fresh `string` producer,
    //    propagated forward through `Move` (a `let y = <producer temp>` rebind
    //    transfers the single drop obligation from the temp to `y`). The
    //    string-typed `*FieldLoad` dest is a fresh producer too now that codegen
    //    retains it (`retain_string_field_load`): it is the identical retained-
    //    interior-load shape as `hew_vec_get_str` (`xs.get(i)`), so it earns the
    //    same one balancing drop. The companion taint exclusion below keeps it
    //    admissible (a string field-load dest is no longer projection-tainted).
    let mut fresh: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Some(dest) = fresh_string_producer_dest(instr) {
                if let Some(l) = base_local(dest) {
                    fresh.insert(l);
                }
            }
            if let Some(dest) = string_field_load_producer_dest(instr, locals) {
                if let Some(l) = base_local(dest) {
                    fresh.insert(l);
                }
            }
        }
        if let Some(dest) = fresh_string_producer_term_dest(&block.terminator) {
            if let Some(l) = base_local(dest) {
                fresh.insert(l);
            }
        }
        // A select terminator's value-bearing arm binding slot is a fresh
        // producer too: the winning edge materialises a fresh owned value
        // into the slot (an ask reply the reply channel hands over — the
        // consumed leg the channel destructor does NOT reap — or a channel
        // item popped by `try_recv`, which the queue neither clones nor
        // drops). The same +1 ownership shape as a fresh producer call.
        // Gated on the slot's declared type being a leaf `string`, matching
        // the leaf-string-only admission below.
        if let Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } =
            &block.terminator
        {
            for arm in arms {
                if let Some(binding) = arm.binding {
                    if let Some(l) = base_local(binding) {
                        if matches!(locals.get(l as usize), Some(ResolvedTy::String)) {
                            fresh.insert(l);
                        }
                    }
                }
            }
        }
    }
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        if fresh.contains(&sl) && fresh.insert(dl) {
                            changed = true;
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    // 2. Projection-alias taint — never admit an interior pointer of a live
    //    aggregate even if it traced through a producer-shaped `Move`. No
    //    consumed-binder exemption (conservative: taint strictly more). String
    //    `*FieldLoad` dests are excluded from the seed (they are retained fresh
    //    producers, admitted in step 1) via the `locals` type gate.
    let tainted = compute_projection_alias_taint(blocks, &HashSet::new(), locals);

    // 3. Admit a leaf-`string` owned binding iff it is a proven fresh owner, not
    //    a projection alias, and every use is a verified borrow.
    let mut allowed: HashSet<BindingId> = HashSet::new();
    for (binding, _name, ty) in owned_locals {
        if cow_value_leaf_drop_symbol(ty).is_none() {
            continue;
        }
        let Some(place) = binding_locals.get(binding) else {
            continue;
        };
        let Some(local) = base_local(*place) else {
            continue;
        };
        if !fresh.contains(&local) || tainted.contains(&local) {
            continue;
        }
        let all_uses_borrow = blocks.iter().all(|block| {
            block
                .instructions
                .iter()
                .all(|instr| !cow_owned_string_instr_escapes(instr, local))
                && !cow_owned_string_terminator_escapes(
                    &block.terminator,
                    suspend_kinds.get(&block.id),
                    local,
                    module_fn_names,
                    module_generic_fn_names,
                )
        });
        if all_uses_borrow {
            allowed.insert(*binding);
        }
    }
    allowed
}
pub(super) fn finalize_string_ownership(
    raw: &mut RawMirFunction,
    builder: &Builder,
    dataflow_result: &dataflow::DataflowResult,
) -> StringDropDerivation {
    let owned_locals_snapshot = builder.owned_locals_snapshot();
    let mut derivation = derive_cow_sole_owner(
        &raw.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.match_project_consumed_binder_locals,
        &builder.locals,
        &builder.borrowed_string_param_locals,
        &builder.parameter_locals,
        &builder.module_fn_names,
        &builder.module_generic_fn_names,
    );
    derivation.allowed.extend(derive_cow_fresh_borrowed_owner(
        &raw.blocks,
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
                derivation.allowed.remove(binding);
            }
        }
    }
    derivation.retain_sites.retain(|site| {
        site.required_bindings
            .iter()
            .all(|binding| derivation.allowed.contains(binding))
    });
    apply_string_retain_sites(
        &mut raw.blocks,
        &mut raw.instr_spans,
        &derivation.retain_sites,
    );
    derivation
}
/// W5.011 P3 — nested fresh-`string` temporary release. The bare-temp analogue
/// of [`derive_cow_fresh_borrowed_owner`]: it releases fresh-owned `string`
/// results that flow straight into a borrowing use (or are discarded) WITHOUT a
/// `let` binding, so the binding-scoped scope-exit derivation never sees them.
///
/// Shapes this releases exactly once (each leaked before this fix):
///
/// ```text
///   (a + b).len()           // concat temp, borrowed by hew_string_length
///   s.to_uppercase().len()  // to_uppercase temp, borrowed
///   xs[i].len()             // hew_vec_get_str temp (a +1 retain), borrowed
///   a + b;                  // discarded concat temp (zero uses)
/// ```
///
/// ## Ownership model (the same refcount keystone as the binding path)
///
/// `string` is refcounted. Every fresh-owned-string contract hands the caller
/// exactly ONE drop obligation (fresh `rc == 1`, or a `+1` retain for
/// `hew_vec_get_str` / `hew_string_clone`). A borrowing-use contract reads/copies
/// the buffer WITHOUT consuming
/// the refcount — verified in `hew-runtime/src/string.rs`: `hew_string_concat`,
/// `hew_string_to_uppercase`, `hew_string_trim`, … all `CStr::from_ptr(input)`
/// then allocate a FRESH result, never freeing the input. So a fresh temp used
/// once by a borrow (or never used) still owns its single obligation afterwards;
/// releasing it once balances the producer.
///
/// ## Fail-closed admission — a temp earns a drop iff ALL hold
///
/// 1. its local is a leaf `string` (`cow_value_leaf_drop_symbol` is `Some`);
/// 2. it is NOT a `let`/parameter binding local (those are the province of the
///    scope-exit `derive_cow_*` drops — the two sets are disjoint by
///    construction, so a buffer is never claimed by both);
/// 3. it has exactly ONE def, a known fresh producer (no instruction
///    re-definition; a producer-terminator temp has no instruction writer);
/// 4. it has AT MOST ONE use-site in the FULL dataflow reads
///    (`dataflow::instr_reads_writes(..).0` for instructions — which, unlike
///    `instr_source_places`, also sees borrow-excluded readers such as
///    `Instr::WireCodec`'s operand — plus `terminator_source_places` for
///    terminators), and (if present) that use is verified borrowing-only: a
///    borrowing `Terminator::Call`, a string compare instruction, or the
///    narrowly-proven nested `hew_string_concat` → `hew_string_concat`
///    instruction chain. Any other use (`Move`/store/return/send/user-call/
///    `WireCodec`) excludes it (leak, never double-free) — the reads-based
///    table is what keeps a `Packet.from_json(a + b)` operand ALIVE past the
///    parse instead of routing it down the discard branch;
/// 5. the drop site is provably dominated by the def and entered exactly once
///    (single-predecessor continuation), in one of these structural shapes:
///      * discard, instr def → drop right after the producer instruction (same
///        straight-line block);
///      * discard, terminator def `Call(next = N)` → drop at the front of `N`
///        (require `N` single-predecessor);
///      * single borrowing use at the terminator `Call(next = T)` of block `U`,
///        with the def either an instruction IN `U` (trivially precedes the
///        terminator) or a terminator `Call(next = U)` of the immediately
///        preceding block (require `U` single-predecessor) → drop at the front
///        of `T` (require `T` single-predecessor).
///
/// Any other CFG shape fails closed (the temp leaks, as before this fix). The
/// return is a list of `(block_id, insert_index, place, ty)` inline-drop
/// insertions for the caller to splice in. Pure over `blocks` (no mutation) so
/// it is unit-testable.
///
/// LESSONS: boundary-fail-closed (P0), cleanup-all-exits, raii-null-after-move.
#[must_use]
fn collect_nested_fresh_string_temp_drops(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    locals: &[ResolvedTy],
    binding_locals: &HashMap<BindingId, Place>,
) -> Vec<(u32, usize, Place, ResolvedTy)> {
    // A drop spliced at the FRONT of a block is sound only if that block is
    // entered from exactly one place — otherwise it could fire on a path where
    // the temp was never produced (free of an uninitialised slot / double-free).
    let mut pred_count: HashMap<u32, usize> = HashMap::new();
    for block in blocks {
        for succ in block.successors() {
            *pred_count.entry(succ).or_insert(0) += 1;
        }
    }

    // Binding/parameter locals are released by the scope-exit `derive_cow_*`
    // drops; never double-claim one here.
    let binding_local_ids: HashSet<u32> = binding_locals
        .values()
        .filter_map(|p| base_local(*p))
        .collect();

    // Instruction writers per local (rule 3): the sole admissible writer of an
    // instr-produced temp is its producer; a terminator-produced temp has none.
    let mut instr_writers: HashMap<u32, Vec<(u32, usize)>> = HashMap::new();
    for block in blocks {
        for (idx, instr) in block.instructions.iter().enumerate() {
            let (_, writes) = crate::dataflow::instr_reads_writes(instr);
            for w in writes {
                if let Some(l) = base_local(w) {
                    instr_writers.entry(l).or_default().push((block.id, idx));
                }
            }
        }
    }

    // Use sites per local (rule 4), deduplicated within each
    // instruction/terminator so a temp referenced twice by one call counts once.
    //
    // Instruction uses come from the full dataflow READS
    // (`dataflow::instr_reads_writes(..).0`), NOT from `instr_source_places` —
    // the same hidden-borrow-reader correction as the bytes collector
    // (`collect_nested_fresh_bytes_temp_drops`). `instr_source_places`
    // deliberately excludes borrow-only readers (`Instr::WireCodec`'s operand,
    // borrow-only `ClosureEnvInit` fields) so a NAMED binding keeps its
    // scope-exit drop past those reads; for a bare temp that exclusion
    // registered ZERO uses, took the discard branch, and spliced
    // `hew_string_drop` immediately after the producer — freeing the buffer
    // BEFORE the hidden reader ran. `Packet.from_json(a + b)` parsed a freed
    // buffer this way: a silent wrong-result (`Err`), while the named
    // `let joined = a + b` sibling was correct. With the reads table a hidden
    // reader is a real use-site the borrowing-use classification rejects, so
    // the temp fails CLOSED to the leak direction. Terminator uses keep
    // `terminator_source_places` (no borrow-exclusions for any reading
    // variant; `MakeGenerator`'s env — see its arm's note — never carries a
    // leaf string/bytes temp directly).
    let mut source_uses: HashMap<u32, Vec<NestedUseSite>> = HashMap::new();
    for block in blocks {
        for (idx, instr) in block.instructions.iter().enumerate() {
            let mut here: HashSet<u32> = HashSet::new();
            let (reads, _) = crate::dataflow::instr_reads_writes(instr);
            for p in reads {
                if let Some(l) = base_local(p) {
                    here.insert(l);
                }
            }
            for l in here {
                source_uses
                    .entry(l)
                    .or_default()
                    .push(NestedUseSite::Instr {
                        block: block.id,
                        idx,
                    });
            }
        }
        let mut here: HashSet<u32> = HashSet::new();
        for p in terminator_source_places(&block.terminator, suspend_kinds.get(&block.id)) {
            if let Some(l) = base_local(p) {
                here.insert(l);
            }
        }
        for l in here {
            source_uses
                .entry(l)
                .or_default()
                .push(NestedUseSite::Term { block: block.id });
        }
    }

    let mut result: Vec<(u32, usize, Place, ResolvedTy)> = Vec::new();
    let mut seen: HashSet<u32> = HashSet::new();
    for block in blocks {
        // Producer defs in this block: instruction producers first, then the
        // block-terminating producer (transform / Vec getter).
        let mut defs: Vec<(u32, NestedDefSite)> = Vec::new();
        for (idx, instr) in block.instructions.iter().enumerate() {
            // A runtime-ABI fresh producer (`hew_string_concat`/`…_to_uppercase`/…)
            // OR a retained string `*FieldLoad` dest — the #54 interior-alias
            // clone-out. `r.name` loaded out of a match-bound nested-record payload
            // is retained by codegen (`retain_string_field_load` →
            // `hew_string_clone`, a `+1` owner of the field's buffer); when it is
            // consumed only by a borrowing transform (`r.name.to_upper()` feeds
            // `hew_string_to_uppercase`, which reads its operand and allocates a
            // fresh result) the field-load temp keeps an unbalanced `+1` and leaks.
            // The parent composite's `EnumInPlace` drop frees the OTHER share (the
            // payload buffer), so this `+1` retain needs its own single balancing
            // drop. It is the identical fresh-`+1`-used-by-a-borrow shape this
            // collector already releases for runtime producers; admit it under the
            // SAME fail-closed rules (single def, at-most-one borrowing use, def
            // dominates the drop), so a temp that escapes
            // (Move/store/return/owning call) stays excluded (leak, never
            // double-free). `string_field_load_producer_dest`'s `string` type gate
            // is the exact congruence with the codegen retain — a non-string field
            // load is never seeded.
            if let Some(dest) = fresh_string_producer_dest(instr)
                .or_else(|| string_field_load_producer_dest(instr, locals))
                .or_else(|| wire_codec_string_producer_dest(instr, locals))
            {
                if let Some(t) = base_local(dest) {
                    defs.push((
                        t,
                        NestedDefSite::Instr {
                            block: block.id,
                            idx,
                        },
                    ));
                }
            }
        }
        if let Some(dest) = fresh_string_producer_term_dest(&block.terminator) {
            if let Some(t) = base_local(dest) {
                defs.push((t, NestedDefSite::Term { block: block.id }));
            }
        }
        for (t, def) in defs {
            if !seen.insert(t) {
                continue;
            }
            if let Some(ins) = nested_fresh_string_temp_drop(
                t,
                def,
                blocks,
                locals,
                &pred_count,
                &binding_local_ids,
                &instr_writers,
                &source_uses,
            ) {
                result.push(ins);
            }
        }
    }
    result
}
/// Per-candidate admission for [`collect_nested_fresh_string_temp_drops`].
/// Returns the inline-drop insertion `(block_id, insert_index, place, ty)` if
/// the fresh-`string` temp `t` (defined at `def`) satisfies every fail-closed
/// rule, else `None`.
#[must_use]
#[allow(
    clippy::too_many_arguments,
    reason = "admission threads the precomputed CFG/dataflow tables (pred counts, \
              binding locals, instr writers, source uses) needed to prove the temp \
              is a single-def, borrow-only, dominated fresh-owned string"
)]
fn nested_fresh_string_temp_drop(
    t: u32,
    def: NestedDefSite,
    blocks: &[BasicBlock],
    locals: &[ResolvedTy],
    pred_count: &HashMap<u32, usize>,
    binding_local_ids: &HashSet<u32>,
    instr_writers: &HashMap<u32, Vec<(u32, usize)>>,
    source_uses: &HashMap<u32, Vec<NestedUseSite>>,
) -> Option<(u32, usize, Place, ResolvedTy)> {
    // 1. leaf `string` — bail unless this resolves to a leaf drop symbol.
    let ty = locals.get(t as usize)?;
    cow_value_leaf_drop_symbol(ty)?;
    // 2. not a binding/parameter local.
    if binding_local_ids.contains(&t) {
        return None;
    }
    // 3. the sole instruction writer (if any) is the producer instruction.
    let writers: &[(u32, usize)] = instr_writers.get(&t).map_or(&[][..], Vec::as_slice);
    match def {
        NestedDefSite::Instr { block, idx } => {
            if !(writers.len() == 1 && writers[0] == (block, idx)) {
                return None;
            }
        }
        NestedDefSite::Term { .. } => {
            if !writers.is_empty() {
                return None;
            }
        }
    }
    // 4. at most one source-use.
    let uses: &[NestedUseSite] = source_uses.get(&t).map_or(&[][..], Vec::as_slice);
    if uses.len() > 1 {
        return None;
    }
    let drop_place = Place::Local(t);
    let drop_ty = ty.clone();

    match (def, uses.first()) {
        // Discarded instruction producer: drop right after it (straight-line).
        (NestedDefSite::Instr { block, idx }, None) => Some((block, idx + 1, drop_place, drop_ty)),
        // Discarded terminator producer `Call(next = N)`: drop at the front of
        // its single-predecessor continuation.
        (NestedDefSite::Term { block }, None) => {
            let n = call_terminator_next(&block_by_id(blocks, block)?.terminator)?;
            (pred_count.get(&n).copied().unwrap_or(0) == 1).then_some((n, 0, drop_place, drop_ty))
        }
        // Single use: it must be a borrowing terminator `Call`, and the def must
        // dominate it (so the drop runs iff the temp was produced).
        (_, Some(NestedUseSite::Term { block: use_block })) => {
            let ub = *use_block;
            let Terminator::Call {
                callee, args, next, ..
            } = &block_by_id(blocks, ub)?.terminator
            else {
                return None;
            };
            if !args.iter().any(|p| place_refs_local(*p, t)) {
                return None;
            }
            if !crate::runtime_symbols::callee_ownership_contract(callee).borrows_string_call_args()
            {
                return None;
            }
            if pred_count.get(next).copied().unwrap_or(0) != 1 {
                return None;
            }
            let def_dominates = match def {
                // Instruction def in the use's own block precedes the terminator;
                // otherwise the def block's `Call` continuation must reach the
                // use block along a single-predecessor chain (see below).
                NestedDefSite::Instr { block, .. } => {
                    block == ub
                        || block_by_id(blocks, block)
                            .and_then(|b| call_terminator_next(&b.terminator))
                            .is_some_and(|start| {
                                dominates_use_via_single_pred_chain(blocks, start, ub, pred_count)
                            })
                }
                // Terminator def `Call(next = U)`: `U` (and every block up to the
                // use) is reached only from the def block along a
                // single-predecessor chain, so the def ran. The multi-step chain
                // is the #2726 f-string case, where a `to_string_*` conversion
                // block splits the concat chain between the producing and
                // consuming concats.
                NestedDefSite::Term { block } => {
                    call_terminator_next(&block_by_id(blocks, block)?.terminator).is_some_and(
                        |start| dominates_use_via_single_pred_chain(blocks, start, ub, pred_count),
                    )
                }
            };
            def_dominates.then_some((*next, 0, drop_place, drop_ty))
        }
        // Single instruction use: admit ONLY known borrowing instruction uses:
        // a borrowing string compare (`xs[i] == "hello"`, `xs[i] != s`, and
        // the ordering family) or the reproduced nested-concat chain where a
        // fresh `hew_string_concat` instruction result is immediately borrowed
        // by another `hew_string_concat` instruction (`first + " " + last`).
        // Any other instruction use stays fail-closed (leak, never
        // double-free): a `Move`/store/user-call/closure-call/unknown runtime
        // call may take ownership, and the conservative leak is the safe side.
        (_, Some(NestedUseSite::Instr { block: ub, idx: ui })) => {
            let ub = *ub;
            let use_instr = block_by_id(blocks, ub)?.instructions.get(*ui)?;
            let borrowing_use = is_borrowing_string_cmp_instr(use_instr, t)
                || is_wire_codec_borrowing_string_use(use_instr, t)
                || (is_fresh_string_producer_def(def, blocks, locals, t)
                    && is_borrowing_string_concat_instr_use(use_instr, t));
            if !borrowing_use {
                return None;
            }
            let def_dominates = match def {
                // Instruction def dominates the use when EITHER:
                //   * it is an EARLIER instruction in the use's own block —
                //     same-block straight-line execution means reaching the use
                //     (which reads `t`) implies the def ran; OR
                //   * the def block's terminator is a `Call` whose sole
                //     continuation is the use block, and the use block has that
                //     single predecessor. The def is an instruction in the def
                //     block, which runs straight-line to its terminator; the
                //     terminator's normal edge reaches the use block, whose only
                //     predecessor is the def block — so reaching the use implies
                //     the def ran. This is the multi-interpolation f-string
                //     shape (#2726): a `to_string_*` conversion lowers to a
                //     block-terminating `Call` that SPLITS the concat chain, so
                //     the concat consuming an intermediate result lands one
                //     block past that split. Same single-step domination the
                //     terminator-def arm below already proves.
                NestedDefSite::Instr { block, idx } => {
                    (block == ub && idx < *ui)
                        || block_by_id(blocks, block)
                            .and_then(|b| call_terminator_next(&b.terminator))
                            .is_some_and(|start| {
                                dominates_use_via_single_pred_chain(blocks, start, ub, pred_count)
                            })
                }
                // Terminator def `Call(next = U)`: `U` (and every block up to the
                // use) is reached only from the def block along a
                // single-predecessor chain, so the def ran before the use.
                NestedDefSite::Term { block } => {
                    call_terminator_next(&block_by_id(blocks, block)?.terminator).is_some_and(
                        |start| dominates_use_via_single_pred_chain(blocks, start, ub, pred_count),
                    )
                }
            };
            // Drop immediately after the compare instruction (straight-line in
            // its block), so the release runs exactly once on the path that
            // produced and borrowed the temp.
            def_dominates.then_some((ub, ui + 1, drop_place, drop_ty))
        }
    }
}
/// W5.011 P3 — splice the inline `hew_string_drop`s computed by
/// [`collect_nested_fresh_string_temp_drops`] into `blocks`. Applied during
/// lowering BEFORE `check_function` / drop elaboration so the dataflow observes
/// each drop as a read of its temp (no use-after-free flag) and so codegen emits
/// the release. Per-block insertions are applied in descending index order so an
/// earlier splice does not shift a later (lower-index) one.
pub(super) fn apply_nested_fresh_string_temp_drops(
    blocks: &mut [BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    locals: &[ResolvedTy],
    binding_locals: &HashMap<BindingId, Place>,
    instr_spans: &mut BTreeMap<(u32, u32), (u32, u32)>,
) {
    let insertions =
        collect_nested_fresh_string_temp_drops(blocks, suspend_kinds, locals, binding_locals);
    if insertions.is_empty() {
        return;
    }
    let mut by_block: HashMap<u32, Vec<(usize, Place, ResolvedTy)>> = HashMap::new();
    for (bid, idx, place, ty) in insertions {
        by_block.entry(bid).or_default().push((idx, place, ty));
    }
    for block in blocks.iter_mut() {
        let Some(mut ins) = by_block.remove(&block.id) else {
            continue;
        };
        // Apply in descending index order so an earlier splice does not shift
        // the insertion point of a later (lower-index) one.
        ins.sort_by_key(|entry| std::cmp::Reverse(entry.0));
        for (idx, place, ty) in ins {
            let at = idx.min(block.instructions.len());
            block.instructions.insert(
                at,
                Instr::Drop {
                    place,
                    ty,
                    drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                },
            );
            // Keep the Stage 2 per-instruction line table aligned with the
            // splice: every side-table entry for this block at index `>= at`
            // shifts up by one. The inserted `hew_string_drop` itself gets no
            // entry — a synthesised drop inherits the nearest enclosing
            // location at codegen (fail-closed).
            shift_instr_spans_on_insert(
                instr_spans,
                block.id,
                u32::try_from(at).unwrap_or(u32::MAX),
            );
        }
    }
}
#[cfg(test)]
mod nested_fresh_string_temp_drop_admission {
    use super::*;

    fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![],
            instructions,
            terminator,
        }
    }

    fn ret_block(id: u32) -> BasicBlock {
        block(id, vec![], Terminator::Return)
    }

    fn runtime_call(symbol: &str, args: Vec<Place>, dest: Option<Place>) -> Instr {
        Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, args, dest)
                .unwrap_or_else(|_| panic!("{symbol} must be an allowlisted RuntimeCall")),
        )
    }

    fn concat(lhs: u32, rhs: u32, dest: u32) -> Instr {
        runtime_call(
            "hew_string_concat",
            vec![Place::Local(lhs), Place::Local(rhs)],
            Some(Place::Local(dest)),
        )
    }

    fn fresh_string_call(callee: &str, dest: u32, next: u32) -> Terminator {
        Terminator::Call {
            callee: callee.to_string(),
            builtin: None,
            args: vec![Place::Local(0)],
            dest: Some(Place::Local(dest)),
            next,
        }
    }

    fn user_record_ty() -> ResolvedTy {
        ResolvedTy::named_user("Holder", vec![])
    }

    fn vec_string_ty() -> ResolvedTy {
        ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String])
    }

    fn locals_with(overrides: &[(usize, ResolvedTy)]) -> Vec<ResolvedTy> {
        let mut locals = vec![ResolvedTy::String; 10];
        for (idx, ty) in overrides {
            locals[*idx] = ty.clone();
        }
        locals
    }

    fn collect(
        blocks: &[BasicBlock],
        locals: &[ResolvedTy],
        binding_locals: &HashMap<BindingId, Place>,
    ) -> Vec<(u32, usize, Place, ResolvedTy)> {
        collect_nested_fresh_string_temp_drops(blocks, &HashMap::new(), locals, binding_locals)
    }

    fn nested_concat_blocks() -> Vec<BasicBlock> {
        vec![block(
            0,
            vec![concat(0, 1, 2), concat(2, 3, 4)],
            Terminator::Return,
        )]
    }

    fn second_concat_binding() -> HashMap<BindingId, Place> {
        [(BindingId(44), Place::Local(4))].into_iter().collect()
    }

    #[test]
    fn nested_concat_instr_temp_gets_one_inline_drop_after_borrowing_concat_use() {
        let blocks = nested_concat_blocks();
        let drops = collect(&blocks, &locals_with(&[]), &second_concat_binding());

        assert_eq!(
            drops,
            vec![(0, 2, Place::Local(2), ResolvedTy::String)],
            "the first concat temp is a fresh owner used exactly once by the \
             second borrowing concat; it must be dropped immediately after that \
             use and only once"
        );
    }

    #[test]
    fn terminator_fresh_temp_gets_one_inline_drop_after_borrowing_concat_use() {
        let blocks = vec![
            block(
                0,
                vec![],
                fresh_string_call("hew_string_to_uppercase", 2, 1),
            ),
            block(1, vec![concat(3, 2, 4)], Terminator::Return),
        ];
        let drops = collect(&blocks, &locals_with(&[]), &second_concat_binding());

        assert_eq!(
            drops,
            vec![(1, 1, Place::Local(2), ResolvedTy::String)],
            "a proven fresh terminator-produced operand used once by borrowing concat \
             must be dropped immediately after that concat"
        );
    }

    fn concat_term(lhs: u32, rhs: u32, dest: u32, next: u32) -> Terminator {
        Terminator::Call {
            callee: "hew_string_concat".to_string(),
            builtin: None,
            args: vec![Place::Local(lhs), Place::Local(rhs)],
            dest: Some(Place::Local(dest)),
            next,
        }
    }

    // #2726: a multi-interpolation f-string lowers the concat chain as
    // block-terminating `Call`s, and each `to_string_*` conversion is itself a
    // terminator that SPLITS the chain. The intermediate concat result (t2)
    // produced in bb0 is consumed by the concat in bb2, one block past the
    // `to_string` split (bb1). Its producing and consuming concats are in
    // DIFFERENT blocks, so the single-step domination check missed it and it
    // leaked. The single-predecessor-chain walk proves bb0's continuation
    // reaches the use block bb2 straight-line, so t2 earns exactly one drop.
    #[test]
    fn cross_block_concat_intermediate_split_by_conversion_gets_one_drop() {
        let blocks = vec![
            // t2 = concat(t0, t1); the intermediate result.
            block(0, vec![], concat_term(0, 1, 2, 1)),
            // t3 = to_string(..); the conversion terminator that splits the chain.
            block(1, vec![], fresh_string_call("hew_i64_to_string", 3, 2)),
            // t4 = concat(t2, t3); consumes the cross-block intermediate t2.
            block(2, vec![], concat_term(2, 3, 4, 3)),
            ret_block(3),
        ];
        // t4 is the final result (a let/print binding); exclude it so the
        // assertion isolates the intermediate temps t2 and t3.
        let binding_locals: HashMap<BindingId, Place> =
            [(BindingId(44), Place::Local(4))].into_iter().collect();

        let mut drops = collect(&blocks, &locals_with(&[]), &binding_locals);
        drops.sort_by_key(|(_, _, place, _)| base_local(*place));

        assert_eq!(
            drops,
            vec![
                (3, 0, Place::Local(2), ResolvedTy::String),
                (3, 0, Place::Local(3), ResolvedTy::String),
            ],
            "the cross-block intermediate concat result (t2) and the conversion \
             temp (t3) are each fresh owners borrowed exactly once by the \
             consuming concat; both must be released once, at the front of the \
             consuming concat's continuation"
        );
    }

    #[test]
    fn binding_local_concat_result_stays_out_of_nested_temp_collector() {
        let blocks = nested_concat_blocks();
        let binding_locals: HashMap<BindingId, Place> = [
            (BindingId(22), Place::Local(2)),
            (BindingId(44), Place::Local(4)),
        ]
        .into_iter()
        .collect();
        let drops = collect(&blocks, &locals_with(&[]), &binding_locals);

        assert!(
            drops.is_empty(),
            "a local owned by a let/parameter binding belongs to scope-exit drop \
             derivation, not the bare-temp collector; got {drops:?}"
        );
    }

    #[test]
    fn concat_temp_moved_into_record_is_not_dropped_by_collector() {
        let blocks = vec![block(
            0,
            vec![
                concat(0, 1, 2),
                Instr::RecordInit {
                    ty: user_record_ty(),
                    fields: vec![(FieldOffset(0), Place::Local(2))],
                    dest: Place::Local(5),
                },
            ],
            Terminator::Return,
        )];
        let locals = locals_with(&[(5, user_record_ty())]);

        assert!(
            collect(&blocks, &locals, &HashMap::new()).is_empty(),
            "record ingress transfers ownership to the aggregate; an inline temp \
             drop would double-free the field"
        );
    }

    #[test]
    fn concat_temp_moved_into_vec_owned_store_is_not_dropped_by_collector() {
        let blocks = vec![
            block(
                0,
                vec![concat(0, 1, 2)],
                Terminator::Call {
                    callee: "hew_vec_push_owned".to_string(),
                    builtin: None,
                    args: vec![Place::Local(5), Place::Local(2)],
                    dest: None,
                    next: 1,
                },
            ),
            ret_block(1),
        ];
        let locals = locals_with(&[(5, vec_string_ty())]);

        assert!(
            collect(&blocks, &locals, &HashMap::new()).is_empty(),
            "Vec owned-element ingress is not the proven borrowing concat chain; \
             the collector must leave ownership with the container path"
        );
    }

    #[test]
    fn concat_temp_read_only_by_wire_from_json_gets_drop_after_the_parse() {
        // `Packet.from_json(a + b)`: the fresh concat temp's ONLY reader is
        // `Instr::WireCodec` (FromJson operand), a borrowing use — codegen loads
        // the operand pointer to feed the text parser and never frees it. The
        // temp keeps its single drop obligation past the parse, so it is released
        // exactly once immediately AFTER the WireCodec (idx 2), never at the
        // discard site (idx 1) before the parse reads it — that ordering is the
        // UAF this collector must never produce. Before this admission the temp
        // leaked one buffer per call (the confound the leak oracle worked around
        // by naming `let raw = …`).
        let blocks = vec![block(
            0,
            vec![
                concat(0, 1, 2),
                Instr::WireCodec {
                    dest: Place::Local(5),
                    operand: Place::Local(2),
                    direction: hew_types::WireCodecDirection::FromJson,
                    value_ty: user_record_ty(),
                },
            ],
            Terminator::Return,
        )];
        let locals = locals_with(&[(5, user_record_ty())]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(0, 2, Place::Local(2), ResolvedTy::String)],
            "a fresh string temp borrowed once by a from_json parse must be \
             dropped exactly once immediately after the parse, never before it"
        );
    }

    #[test]
    fn to_json_temp_read_only_by_from_json_gets_drop_after_the_parse() {
        // `Packet.from_json(p.to_json())`: `to_json` is a WireCodec text
        // serialize producing a fresh `string`; `from_json` borrows it. The
        // producer temp gains exactly one drop after the parse.
        let blocks = vec![block(
            0,
            vec![
                Instr::WireCodec {
                    dest: Place::Local(2),
                    operand: Place::Local(0),
                    direction: hew_types::WireCodecDirection::ToJson,
                    value_ty: user_record_ty(),
                },
                Instr::WireCodec {
                    dest: Place::Local(5),
                    operand: Place::Local(2),
                    direction: hew_types::WireCodecDirection::FromJson,
                    value_ty: user_record_ty(),
                },
            ],
            Terminator::Return,
        )];
        let locals = locals_with(&[(0, user_record_ty()), (5, user_record_ty())]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(0, 2, Place::Local(2), ResolvedTy::String)],
            "the to_json temp is a fresh owned string borrowed once by from_json; \
             it must be dropped once immediately after the parse"
        );
    }

    #[test]
    fn discarded_to_json_temp_dropped_right_after_producer() {
        // `p.to_json();` — a discarded text-serialize temp (zero uses) drops
        // immediately after the producer instruction.
        let blocks = vec![block(
            0,
            vec![Instr::WireCodec {
                dest: Place::Local(2),
                operand: Place::Local(0),
                direction: hew_types::WireCodecDirection::ToJson,
                value_ty: user_record_ty(),
            }],
            Terminator::Return,
        )];
        let locals = locals_with(&[(0, user_record_ty())]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(0, 1, Place::Local(2), ResolvedTy::String)],
            "a discarded to_json string temp is dropped right after its producer"
        );
    }

    #[test]
    fn named_to_json_binding_stays_out_of_string_collector() {
        // `let s = p.to_json(); Packet.from_json(s)` — the to_json result is a
        // binding local, released by scope-exit derivation; the bare-temp
        // collector must not double-claim it.
        let blocks = vec![block(
            0,
            vec![
                Instr::WireCodec {
                    dest: Place::Local(2),
                    operand: Place::Local(0),
                    direction: hew_types::WireCodecDirection::ToJson,
                    value_ty: user_record_ty(),
                },
                Instr::WireCodec {
                    dest: Place::Local(5),
                    operand: Place::Local(2),
                    direction: hew_types::WireCodecDirection::FromJson,
                    value_ty: user_record_ty(),
                },
            ],
            Terminator::Return,
        )];
        let locals = locals_with(&[(0, user_record_ty()), (5, user_record_ty())]);
        let binding_locals: HashMap<BindingId, Place> =
            [(BindingId(2), Place::Local(2))].into_iter().collect();
        assert!(
            collect(&blocks, &locals, &binding_locals).is_empty(),
            "a to_json result bound to a let belongs to scope-exit drop \
             derivation, not the bare-temp collector"
        );
    }

    #[test]
    fn concat_temp_return_move_is_not_dropped_by_collector() {
        let blocks = vec![block(
            0,
            vec![
                concat(0, 1, 2),
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(2),
                },
            ],
            Terminator::Return,
        )];

        assert!(
            collect(&blocks, &locals_with(&[]), &HashMap::new()).is_empty(),
            "moving the temp into the return slot transfers ownership to the \
             caller; no producer-side temp drop is admissible"
        );
    }

    #[test]
    fn concat_temp_sent_to_actor_is_not_dropped_by_collector() {
        let blocks = vec![
            block(
                0,
                vec![concat(0, 1, 2)],
                Terminator::Send {
                    actor: Place::Local(8),
                    msg_type: 0,
                    value: Place::Local(2),
                    next: 1,
                    arg_modes: vec![crate::model::SendAliasMode::SnapshotBitCopy],
                    cleanup_plan: None,
                },
            ),
            ret_block(1),
        ];

        assert!(
            collect(&blocks, &locals_with(&[]), &HashMap::new()).is_empty(),
            "actor send is an ownership-transfer edge, not a borrowing concat \
             instruction use"
        );
    }

    #[test]
    fn concat_temp_passed_to_unknown_user_call_is_not_dropped_by_collector() {
        let blocks = vec![
            block(
                0,
                vec![concat(0, 1, 2)],
                Terminator::Call {
                    callee: "user_takes_string".to_string(),
                    builtin: None,
                    args: vec![Place::Local(2)],
                    dest: None,
                    next: 1,
                },
            ),
            ret_block(1),
        ];

        assert!(
            collect(&blocks, &locals_with(&[]), &HashMap::new()).is_empty(),
            "unknown/user calls have no borrowing contract, so the collector must \
             fail closed"
        );
    }

    #[test]
    fn branch_around_concat_producer_does_not_insert_uninitialized_drop() {
        let blocks = vec![
            block(
                0,
                vec![],
                Terminator::Branch {
                    cond: Place::Local(9),
                    then_target: 1,
                    else_target: 2,
                },
            ),
            block(1, vec![concat(0, 1, 2)], Terminator::Goto { target: 2 }),
            block(2, vec![concat(2, 3, 4)], Terminator::Return),
        ];
        let mut locals = locals_with(&[(9, ResolvedTy::Bool)]);
        locals[4] = ResolvedTy::String;

        assert!(
            collect(&blocks, &locals, &second_concat_binding()).is_empty(),
            "the use block is reachable from a path that skipped the producer; \
             inserting a drop there would free an uninitialized temp"
        );
    }
}
/// #2542 — release a nested fresh-owned `bytes` CALL-RESULT temporary. The
/// bytes analogue of [`collect_nested_fresh_string_temp_drops`], scoped to the
/// one producer the string pass cannot see: a USER Hew function returning an
/// owned `bytes` value whose result flows straight into a borrowing use (or is
/// discarded) WITHOUT a `let` binding, so the binding-scoped
/// [`derive_local_bytes_drop_allowed`] never sees it.
///
/// Shapes this releases exactly once (each leaked before this fix):
///
/// ```text
///   mk().len()               // user-call bytes temp, borrowed by hew_vec_len
///   mk().get(i)              // borrowed by hew_bytes_get
///   mk();                    // discarded user-call bytes temp (zero uses)
///   "x".to_bytes().len()     // hew_string_to_bytes fresh temp, borrowed
/// ```
///
/// A `let b = mk(); b.len()` is UNCHANGED: `mk`'s dest is a separate temp
/// `move`d into `b`'s binding local, and a `Move` is not a borrowing use, so the
/// temp is excluded here (rule 4) and `b`'s scope-exit
/// `derive_local_bytes_drop_allowed` release remains the sole drop. No double
/// free.
///
/// ## Ownership model (the same refcount keystone as the binding path)
///
/// A user Hew function returning `bytes` hands the caller the SOLE drop
/// obligation: the callee retracts its own return-slot release
/// (`derive_returned_aggregate_member_bindings` / the `ReturnSlot` move), so the
/// caller owns exactly one `hew_bytes_drop`. A receiver-/all-args-borrowing
/// `bytes` runtime op (`hew_vec_len`, `hew_bytes_get`, `hew_bytes_contains`,
/// `hew_bytes_to_string`, `hew_bytes_slice`, … — every `borrows_bytes_receiver`
/// / `borrows_all_bytes_args` contract) reads the triple WITHOUT consuming its
/// refcount, so a fresh temp used once by such a borrow (or never used) still
/// owns its single obligation afterwards; releasing it once balances the callee.
///
/// ## Producer scope (fail-closed, safe direction)
///
/// A `builtin: None` `Terminator::Call` returning a `bytes`-typed dest whose
/// callee EITHER is not a known runtime symbol AND whose contract does not
/// classify its result as an interior alias of the receiver, OR is an
/// allowlisted runtime symbol whose contract classifies its result as
/// `FreshOwnedBytes`. Three producer shapes satisfy this:
///
///   * a USER Hew function returning `bytes` — its ABI hands the caller the sole
///     release (`FAIL_CLOSED` contract), and a Hew function can never return a
///     borrow (no lifetimes);
///   * `hew_string_to_bytes` (`"x".to_bytes()`), the one non-allowlisted fresh
///     `bytes` runtime helper — it allocates a fresh rc==1 buffer;
///   * `hew_bytes_slice` (`b.slice(..)`), an allowlisted runtime symbol whose
///     `FreshOwnedBytes` contract proves it hands back a fresh handle — the
///     non-empty path bumps the shared buffer's refcount so the slice owner
///     drops independently, and the empty path returns a null/0/0 triple whose
///     release is a no-op. This is what lets a `b.slice(..).len()` transient
///     earn its drop instead of leaking.
///
/// Allowlisted runtime callees WITHOUT a `FreshOwnedBytes` result
/// (`hew_bytes_get`, receiver-borrowing scans, …) are EXCLUDED: they may borrow
/// their receiver or return an interior alias, so admitting one risks dropping a
/// value that is actually a borrow (double free). The leak is the safe side.
///
/// ## Fail-closed admission — a temp earns a drop iff ALL hold
///
/// 1. its local is a `bytes` local;
/// 2. it is NOT a `let`/parameter binding local (those belong to the scope-exit
///    `derive_local_bytes_drop_allowed` drops — disjoint by construction, so a
///    buffer is never claimed by both);
/// 3. its sole def is its producer: a terminator producer has NO instruction
///    writer; an instruction producer (`hew_bytes_slice`) has exactly its own
///    producer instruction as the sole writer;
/// 4. it has AT MOST ONE use-site in the FULL dataflow reads
///    (`dataflow::instr_reads_writes(..).0` for instructions — which, unlike
///    `instr_source_places`, also sees the borrow-excluded readers
///    `Instr::WireCodec` / `Instr::BytesRetain` / `Instr::GeneratorNext` — plus
///    `terminator_source_places` for terminators), and (if present) that use is
///    a borrowing bytes op — an `Instr::CallRuntimeAbi` with a
///    receiver-/all-args-borrowing contract, or a `Terminator::Call`. Any other
///    use (`Move`/store/return/send/aggregate ingress/`WireCodec`/`BytesRetain`)
///    excludes it (leak, never double-free) — the reads-based table is what
///    keeps a `Packet.decode(mk())` operand ALIVE past the decode instead of
///    routing it down the discard branch;
/// 5. the drop site is provably dominated by the def and entered exactly once,
///    in one of these shapes (an instruction producer is straight-line, so its
///    def dominates any later same-block position; a terminator producer
///    dominates only its single-predecessor continuation):
///      * discard (zero uses) → for an instruction producer, drop immediately
///        after the producer instruction; for a terminator producer, drop at
///        the front of its single-predecessor continuation;
///      * single borrowing instruction use dominated by the def (an earlier
///        instruction in the same block for an instruction producer, or the
///        producer's single-predecessor continuation for a terminator producer)
///        → drop immediately after the use instruction;
///      * single borrowing terminator use dominated by the def → drop at the
///        front of that use's single-predecessor continuation.
///
/// Any other CFG shape fails closed (the temp leaks, as before this fix). The
/// return is a list of `(block_id, insert_index, place, ty)` inline-drop
/// insertions for the caller to splice in. Pure over `blocks` (no mutation) so
/// it is unit-testable.
///
/// LESSONS: boundary-fail-closed (P0), cleanup-all-exits, raii-null-after-move.
#[must_use]
fn collect_nested_fresh_bytes_temp_drops(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    locals: &[ResolvedTy],
    binding_locals: &HashMap<BindingId, Place>,
) -> Vec<(u32, usize, Place, ResolvedTy)> {
    // A drop spliced at the FRONT of a block is sound only if that block is
    // entered from exactly one place — otherwise it could fire on a path where
    // the temp was never produced (free of an uninitialised slot / double-free).
    let mut pred_count: HashMap<u32, usize> = HashMap::new();
    for block in blocks {
        for succ in block.successors() {
            *pred_count.entry(succ).or_insert(0) += 1;
        }
    }

    // Binding/parameter locals are released by the scope-exit
    // `derive_local_bytes_drop_allowed` drops; never double-claim one here.
    let binding_local_ids: HashSet<u32> = binding_locals
        .values()
        .filter_map(|p| base_local(*p))
        .collect();

    // Instruction writers per local (rule 3). The sole admissible writer of an
    // instr-produced temp (`hew_bytes_slice`) is its producer instruction; a
    // terminator-produced temp (user call / `hew_string_to_bytes`) has none.
    // Same shape as `collect_nested_fresh_string_temp_drops`.
    let mut instr_writers: HashMap<u32, Vec<(u32, usize)>> = HashMap::new();
    for block in blocks {
        for (idx, instr) in block.instructions.iter().enumerate() {
            let (_, writes) = crate::dataflow::instr_reads_writes(instr);
            for w in writes {
                if let Some(l) = base_local(w) {
                    instr_writers.entry(l).or_default().push((block.id, idx));
                }
            }
        }
    }

    // Use sites per local (rule 4), deduplicated within each
    // instruction/terminator so a temp referenced twice by one call counts once.
    //
    // Instruction uses come from the full dataflow READS
    // (`dataflow::instr_reads_writes(..).0`), NOT from `instr_source_places`.
    // The two tables encode different facts: `instr_source_places` deliberately
    // EXCLUDES borrow-only readers (`Instr::WireCodec`'s decode operand,
    // `Instr::BytesRetain`'s value, `Instr::GeneratorNext`'s ctx) so a NAMED
    // binding keeps its scope-exit drop past those reads — but this collector
    // needs "is the temp actually read at all / where". A hidden reader the
    // source table hides would register ZERO uses, route the temp down the
    // discard branch, and splice `hew_bytes_drop` at the front of the
    // continuation BEFORE the reader runs — a use-after-free
    // (`Packet.decode(mk())` trapped exactly this way). With the reads table, a
    // hidden reader is a real use-site that the borrow-only classification
    // rejects (not an admissible borrowing bytes runtime op), so the temp fails
    // CLOSED to the leak direction. Terminator uses keep
    // `terminator_source_places`, which has no borrow-exclusions (every reading
    // variant lists its operands).
    let mut source_uses: HashMap<u32, Vec<NestedUseSite>> = HashMap::new();
    for block in blocks {
        for (idx, instr) in block.instructions.iter().enumerate() {
            let mut here: HashSet<u32> = HashSet::new();
            let (reads, _) = crate::dataflow::instr_reads_writes(instr);
            for p in reads {
                if let Some(l) = base_local(p) {
                    here.insert(l);
                }
            }
            for l in here {
                source_uses
                    .entry(l)
                    .or_default()
                    .push(NestedUseSite::Instr {
                        block: block.id,
                        idx,
                    });
            }
        }
        let mut here: HashSet<u32> = HashSet::new();
        for p in terminator_source_places(&block.terminator, suspend_kinds.get(&block.id)) {
            if let Some(l) = base_local(p) {
                here.insert(l);
            }
        }
        for l in here {
            source_uses
                .entry(l)
                .or_default()
                .push(NestedUseSite::Term { block: block.id });
        }
    }

    let mut result: Vec<(u32, usize, Place, ResolvedTy)> = Vec::new();
    let mut seen: HashSet<u32> = HashSet::new();
    for block in blocks {
        // Producer defs in this block: instruction producers first, then the
        // block-terminating producer.
        //
        // Instruction producer: an `Instr::CallRuntimeAbi` whose contract
        // classifies its result as `FreshOwnedBytes` (`hew_bytes_slice`) — it
        // bumps the shared buffer's refcount (or returns a null/0/0 empty
        // triple), handing the caller a fresh rc==1 handle it owes exactly one
        // release. This is the transient `b[a..b].len()` receiver the #2559
        // leak reports (the slice lowers straight-line as a `call_rt`, so its
        // borrowing `.len()` use lives in the SAME block, not a continuation).
        //
        // Terminator producer: a `builtin: None` `Terminator::Call` returning an
        // owned `bytes` dest that is EITHER not a known runtime symbol — a user
        // Hew function (its ABI hands the caller the sole release —
        // `FAIL_CLOSED`), or a non-allowlisted fresh-`bytes` helper such as
        // `hew_string_to_bytes` (`"x".to_bytes()`) — OR an allowlisted runtime
        // symbol whose contract is `FreshOwnedBytes`.
        //
        // Allowlisted runtime callees WITHOUT a `FreshOwnedBytes` result are
        // excluded (they may borrow their receiver or return an interior alias,
        // so admitting one risks a double-free). Defense-in-depth: a callee
        // whose contract classifies its result as an interior alias of arg[0]
        // is ALSO excluded, so a borrow-returner can never be dropped even if a
        // future one lands without the allowlist entry (a Hew user fn can never
        // return a borrow — no lifetimes).
        let defs = fresh_bytes_producer_defs_in_block(block, locals);
        for (t, def) in defs {
            if !seen.insert(t) {
                continue;
            }
            if let Some(ins) = nested_fresh_bytes_temp_drop(
                t,
                def,
                blocks,
                &pred_count,
                &binding_local_ids,
                &instr_writers,
                &source_uses,
            ) {
                result.push(ins);
            }
        }
    }
    result
}
/// The fresh-owned-`bytes` producer defs in `block`: instruction producers
/// (`hew_bytes_slice`, straight-line `call_rt`) first, then the block-terminating
/// producer (user call / `hew_string_to_bytes` / allowlisted `FreshOwnedBytes`).
/// Each admissible `bytes`-typed dest is returned with its [`NestedDefSite`].
fn fresh_bytes_producer_defs_in_block(
    block: &BasicBlock,
    locals: &[ResolvedTy],
) -> Vec<(u32, NestedDefSite)> {
    let mut defs: Vec<(u32, NestedDefSite)> = Vec::new();
    for (idx, instr) in block.instructions.iter().enumerate() {
        if let Some(dest) = fresh_bytes_producer_instr_dest(instr) {
            if bytes_place_is_typed(dest, locals) {
                if let Some(t) = base_local(dest) {
                    defs.push((
                        t,
                        NestedDefSite::Instr {
                            block: block.id,
                            idx,
                        },
                    ));
                }
            }
        }
    }
    if let Terminator::Call {
        callee,
        builtin: None,
        dest: Some(dest),
        ..
    } = &block.terminator
    {
        if fresh_bytes_producer_term_admissible(callee) && bytes_place_is_typed(*dest, locals) {
            if let Some(t) = base_local(*dest) {
                defs.push((t, NestedDefSite::Term { block: block.id }));
            }
        }
    }
    defs
}
/// The destination place of an `Instr::CallRuntimeAbi` whose ownership contract
/// classifies its result as a fresh, solely-owned `bytes` (`hew_bytes_slice`) —
/// a `+1` owner the caller must balance with exactly one `hew_bytes_drop`. Only
/// validated runtime-ABI producers whose contract is `FreshOwnedBytes` qualify;
/// everything else returns `None` (fail-closed — the local is never seeded).
fn fresh_bytes_producer_instr_dest(instr: &Instr) -> Option<Place> {
    match instr {
        Instr::CallRuntimeAbi(call)
            if crate::runtime_symbols::callee_ownership_contract(call.symbol())
                .produces_fresh_owned_bytes() =>
        {
            call.dest()
        }
        // A wire-codec serialize hands the caller a fresh, solely-owned CBOR
        // buffer (`value.encode() -> bytes`, rc==1, one `hew_bytes_drop` owed).
        // Discriminate by the DEST TYPE at the caller's `bytes_place_is_typed`
        // gate rather than by direction: `Encode` is the only direction whose
        // dest is `bytes` (`Decode`'s dest is the value type, `ToJson`/`ToYaml`'s
        // is `string`, `From*`'s is `Result`), so a bytes-typed WireCodec dest is
        // exactly the fresh-owned CBOR temp. `Packet.decode(p.encode())` /
        // `p.encode();` leaked this buffer before this admission.
        Instr::WireCodec { dest, .. } => Some(*dest),
        _ => None,
    }
}
/// True iff a `builtin: None` `Terminator::Call` to `callee` is an admissible
/// fresh-owned-`bytes` producer: either NOT a known runtime symbol (a user Hew
/// fn or the non-allowlisted `hew_string_to_bytes`, both fresh rc==1), or an
/// allowlisted runtime symbol whose contract is `FreshOwnedBytes`. A callee
/// whose result is an interior alias of the receiver is always excluded
/// (defense-in-depth — a borrow-returner is never dropped).
fn fresh_bytes_producer_term_admissible(callee: &str) -> bool {
    let contract = crate::runtime_symbols::callee_ownership_contract(callee);
    if contract.returns_receiver_interior_alias() {
        return false;
    }
    !crate::runtime_symbols::is_known_runtime_symbol(callee)
        || contract.produces_fresh_owned_bytes()
}
/// Per-candidate admission for [`collect_nested_fresh_bytes_temp_drops`].
/// Returns the inline-drop insertion `(block_id, insert_index, place, ty)` if
/// the fresh-owned `bytes` temp `t` (defined at `def`) satisfies every
/// fail-closed rule, else `None`.
#[must_use]
#[allow(
    clippy::too_many_arguments,
    reason = "admission threads the precomputed CFG/dataflow tables (pred counts, \
              binding locals, instr writers, source uses) needed to prove the temp \
              is a single-def, borrow-only, dominated fresh-owned bytes"
)]
fn nested_fresh_bytes_temp_drop(
    t: u32,
    def: NestedDefSite,
    blocks: &[BasicBlock],
    pred_count: &HashMap<u32, usize>,
    binding_local_ids: &HashSet<u32>,
    instr_writers: &HashMap<u32, Vec<(u32, usize)>>,
    source_uses: &HashMap<u32, Vec<NestedUseSite>>,
) -> Option<(u32, usize, Place, ResolvedTy)> {
    // 2. not a binding/parameter local.
    if binding_local_ids.contains(&t) {
        return None;
    }
    // 3. the sole instruction writer (if any) is the producer instruction. A
    // terminator-produced temp has no instruction writer; an instr-produced
    // temp (`hew_bytes_slice`) has exactly its producer.
    let writers: &[(u32, usize)] = instr_writers.get(&t).map_or(&[][..], Vec::as_slice);
    match def {
        NestedDefSite::Instr { block, idx } => {
            if !(writers.len() == 1 && writers[0] == (block, idx)) {
                return None;
            }
        }
        NestedDefSite::Term { .. } => {
            if !writers.is_empty() {
                return None;
            }
        }
    }
    // 4. at most one source-use.
    let uses: &[NestedUseSite] = source_uses.get(&t).map_or(&[][..], Vec::as_slice);
    if uses.len() > 1 {
        return None;
    }
    let drop_place = Place::Local(t);
    let drop_ty = ResolvedTy::Bytes;

    match (def, uses.first()) {
        // Discarded instruction producer: drop right after it (straight-line in
        // its block).
        (NestedDefSite::Instr { block, idx }, None) => Some((block, idx + 1, drop_place, drop_ty)),
        // Discarded terminator producer `Call(next = N)`: drop at the front of
        // its single-predecessor continuation, so the temp was provably produced.
        (NestedDefSite::Term { block }, None) => {
            let n = call_terminator_next(&block_by_id(blocks, block)?.terminator)?;
            (pred_count.get(&n).copied().unwrap_or(0) == 1).then_some((n, 0, drop_place, drop_ty))
        }
        // Single instruction use: it must be a borrowing `bytes` runtime op, and
        // the def must dominate it.
        (_, Some(NestedUseSite::Instr { block: ub, idx: ui })) => {
            let ub = *ub;
            let use_instr = block_by_id(blocks, ub)?.instructions.get(*ui)?;
            if !bytes_temp_instr_use_is_borrow_only(use_instr, t) {
                return None;
            }
            let def_dominates = match def {
                // Instruction def must be an EARLIER instruction in the use's own
                // block: same-block straight-line execution means reaching the
                // use (which reads `t`) implies the def ran. This is the
                // `b[a..b].len()` shape — `hew_bytes_slice` then `hew_vec_len`
                // in one block.
                NestedDefSite::Instr { block, idx } => block == ub && idx < *ui,
                // Terminator def `Call(next = U)`: a single-predecessor `U` is
                // reached only from the def block, so the def ran before the use.
                NestedDefSite::Term { block } => {
                    call_terminator_next(&block_by_id(blocks, block)?.terminator) == Some(ub)
                        && pred_count.get(&ub).copied().unwrap_or(0) == 1
                }
            };
            // Drop immediately after the borrowing use (straight-line in `ub`),
            // so the release runs exactly once on the path that produced and
            // borrowed the temp.
            def_dominates.then_some((ub, ui + 1, drop_place, drop_ty))
        }
        // Single terminator use: a `Terminator::Call` reading `t`. Hew's
        // by-value heap parameters are BORROWS — a plain function/method call
        // never consumes the caller's `bytes` reference (a callee that shares it
        // mints its own retain), so EVERY `Terminator::Call` bytes arg is a
        // borrow. This mirrors `derive_local_bytes_drop_allowed`'s uniform
        // `Terminator::Call { .. } => {}` (no-escape) posture, which is why a
        // named `let b = mk(); b.get(0)` keeps its scope-exit drop; the bare temp
        // needs the identical release. Actor `Send`/`Ask` (which DO consume the
        // triple via the mailbox hand-off) are distinct terminators, so the
        // pattern below excludes them fail-closed. Drop at the front of the use's
        // single-predecessor continuation.
        (_, Some(NestedUseSite::Term { block: ub })) => {
            let ub = *ub;
            let Terminator::Call { args, next, .. } = &block_by_id(blocks, ub)?.terminator else {
                return None;
            };
            if !args.iter().any(|p| place_refs_local(*p, t)) {
                return None;
            }
            if pred_count.get(next).copied().unwrap_or(0) != 1 {
                return None;
            }
            let def_dominates = match def {
                // Instruction def in the use's own block precedes the terminator.
                NestedDefSite::Instr { block, .. } => block == ub,
                // Terminator def `Call(next = U)`: a single-predecessor `U` is
                // reached only from the def block, so the def ran.
                NestedDefSite::Term { block } => {
                    call_terminator_next(&block_by_id(blocks, block)?.terminator) == Some(ub)
                        && pred_count.get(&ub).copied().unwrap_or(0) == 1
                }
            };
            def_dominates.then_some((*next, 0, drop_place, drop_ty))
        }
    }
}
/// A `bytes` runtime-op argument position is a BORROW (reads the triple without
/// consuming its refcount) iff the callee's ownership contract borrows all bytes
/// args, or borrows the receiver and `t` sits at arg[0]. The
/// `VecCopyInElementStore` receiver class is deliberately NOT admitted here:
/// that is container-ingress ownership, out of scope for this fix.
#[must_use]
fn bytes_call_arg_is_borrow(callee: &str, arg_index: usize) -> bool {
    let contract = crate::runtime_symbols::callee_ownership_contract(callee);
    contract.borrows_all_bytes_args() || (arg_index == 0 && contract.borrows_bytes_receiver())
}
/// True iff every position at which `t` appears in `args` is a borrowing `bytes`
/// position for `callee`, and `t` appears at least once. A single non-borrow
/// position (an escaping tail operand) fails the whole use closed.
#[must_use]
fn bytes_temp_call_use_is_borrow_only(callee: &str, args: &[Place], t: u32) -> bool {
    let mut appears = false;
    for (arg_index, place) in args.iter().enumerate() {
        if place_refs_local(*place, t) {
            appears = true;
            if !bytes_call_arg_is_borrow(callee, arg_index) {
                return false;
            }
        }
    }
    appears
}
/// True iff `instr` is an `Instr::CallRuntimeAbi` whose every use of `t` is a
/// borrowing `bytes` position. Every non-runtime-call instruction use
/// (`Move`/store/aggregate ingress/…) is an ownership transfer or escape and
/// fails closed — the temp keeps its leak (never a double-free).
#[must_use]
fn bytes_temp_instr_use_is_borrow_only(instr: &Instr, t: u32) -> bool {
    match instr {
        Instr::CallRuntimeAbi(call) => {
            bytes_temp_call_use_is_borrow_only(call.symbol(), call.args(), t)
        }
        // A wire-codec deserialize BORROWS its `operand` bytes: codegen's
        // `lower_wire_codec_instr` loads the operand pointer to feed
        // `__hew_deserialize_<key>(data, len, ..)` and never frees it (model.rs:
        // "The `operand` is borrowed"). So a fresh bytes temp read only as a
        // decode operand keeps its single drop obligation and is released once
        // here, immediately after the decode. `Packet.decode(mk())` /
        // `Packet.decode(p.encode())` leaked the operand before this admission.
        Instr::WireCodec { operand, .. } => place_refs_local(*operand, t),
        _ => false,
    }
}
/// #2542 — splice the inline `hew_bytes_drop`s computed by
/// [`collect_nested_fresh_bytes_temp_drops`] into `blocks`. Applied during
/// lowering BEFORE `check_function` / drop elaboration so the dataflow observes
/// each drop as a read of its temp (no use-after-free flag) and so codegen emits
/// the release. Per-block insertions are applied in descending index order so an
/// earlier splice does not shift a later (lower-index) one. Codegen routes a
/// `bytes`-typed inline `Instr::Drop` carrying `Release("hew_bytes_drop")`
/// through `emit_bytes_inplace_drop` (the `BytesTriple` field-0 release).
pub(super) fn apply_nested_fresh_bytes_temp_drops(
    blocks: &mut [BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    locals: &[ResolvedTy],
    binding_locals: &HashMap<BindingId, Place>,
    instr_spans: &mut BTreeMap<(u32, u32), (u32, u32)>,
) {
    let insertions =
        collect_nested_fresh_bytes_temp_drops(blocks, suspend_kinds, locals, binding_locals);
    if insertions.is_empty() {
        return;
    }
    let mut by_block: HashMap<u32, Vec<(usize, Place, ResolvedTy)>> = HashMap::new();
    for (bid, idx, place, ty) in insertions {
        by_block.entry(bid).or_default().push((idx, place, ty));
    }
    for block in blocks.iter_mut() {
        let Some(mut ins) = by_block.remove(&block.id) else {
            continue;
        };
        // Apply in descending index order so an earlier splice does not shift
        // the insertion point of a later (lower-index) one.
        ins.sort_by_key(|entry| std::cmp::Reverse(entry.0));
        for (idx, place, ty) in ins {
            let at = idx.min(block.instructions.len());
            block.instructions.insert(
                at,
                Instr::Drop {
                    place,
                    ty,
                    drop_fn: Some(crate::model::DropFnSpec::Release("hew_bytes_drop")),
                },
            );
            shift_instr_spans_on_insert(
                instr_spans,
                block.id,
                u32::try_from(at).unwrap_or(u32::MAX),
            );
        }
    }
}
#[cfg(test)]
mod nested_fresh_bytes_temp_drop_admission {
    use super::*;

    fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![],
            instructions,
            terminator,
        }
    }

    fn ret_block(id: u32) -> BasicBlock {
        block(id, vec![], Terminator::Return)
    }

    /// A user Hew callee producing a `bytes` dest — the #2542 shape.
    fn user_call(callee: &str, dest: u32, next: u32) -> Terminator {
        Terminator::Call {
            callee: callee.to_string(),
            builtin: None,
            args: vec![],
            dest: Some(Place::Local(dest)),
            next,
        }
    }

    fn runtime_call(symbol: &str, args: Vec<Place>, dest: Option<Place>) -> Instr {
        Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, args, dest)
                .unwrap_or_else(|_| panic!("{symbol} must be an allowlisted RuntimeCall")),
        )
    }

    /// `hew_vec_len` is the polymorphic bytes/vec length borrow (`.len()`).
    fn bytes_len(recv: u32, dest: u32) -> Instr {
        runtime_call(
            "hew_vec_len",
            vec![Place::Local(recv)],
            Some(Place::Local(dest)),
        )
    }

    fn boxed_ty() -> ResolvedTy {
        ResolvedTy::named_user("Boxed", vec![])
    }

    /// All locals `bytes` except the listed overrides (e.g. an `i64` length).
    fn bytes_locals_with(overrides: &[(usize, ResolvedTy)]) -> Vec<ResolvedTy> {
        let mut locals = vec![ResolvedTy::Bytes; 10];
        for (idx, ty) in overrides {
            locals[*idx] = ty.clone();
        }
        locals
    }

    fn collect(
        blocks: &[BasicBlock],
        locals: &[ResolvedTy],
        binding_locals: &HashMap<BindingId, Place>,
    ) -> Vec<(u32, usize, Place, ResolvedTy)> {
        collect_nested_fresh_bytes_temp_drops(blocks, &HashMap::new(), locals, binding_locals)
    }

    #[test]
    fn slice_instr_transient_receiver_gets_one_inline_drop_after_borrowing_use() {
        // The #2559 `b[a..b].len()` shape. `hew_bytes_slice` is an allowlisted
        // runtime symbol whose result is `FreshOwnedBytes`; it lowers
        // straight-line as an `Instr::CallRuntimeAbi`, so its borrowing `.len()`
        // (`hew_vec_len`) use lives in the SAME block, not a continuation.
        // bb0: _2 = hew_bytes_slice(_5, _3, _4)  (fresh +1, instr)
        //      _6 = hew_vec_len(_2)              (borrow, instr)
        //      Return
        let blocks = vec![block(
            0,
            vec![
                runtime_call(
                    "hew_bytes_slice",
                    vec![Place::Local(5), Place::Local(3), Place::Local(4)],
                    Some(Place::Local(2)),
                ),
                bytes_len(2, 6),
            ],
            Terminator::Return,
        )];
        let locals = bytes_locals_with(&[
            (3, ResolvedTy::I64),
            (4, ResolvedTy::I64),
            (6, ResolvedTy::I64),
        ]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(0, 2, Place::Local(2), ResolvedTy::Bytes)],
            "the fresh-owned-bytes slice temp is borrowed once by hew_vec_len in \
             its own block; it must be dropped immediately after that use"
        );
    }

    #[test]
    fn discarded_slice_instr_result_dropped_right_after_producer() {
        // A discarded `hew_bytes_slice` instruction result (zero uses) drops
        // immediately after the producer instruction (straight-line).
        let blocks = vec![block(
            0,
            vec![runtime_call(
                "hew_bytes_slice",
                vec![Place::Local(5), Place::Local(3), Place::Local(4)],
                Some(Place::Local(2)),
            )],
            Terminator::Return,
        )];
        let locals = bytes_locals_with(&[(3, ResolvedTy::I64), (4, ResolvedTy::I64)]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(0, 1, Place::Local(2), ResolvedTy::Bytes)],
            "a discarded fresh-owned-bytes slice temp is dropped right after its \
             producer instruction"
        );
    }

    #[test]
    fn slice_instr_result_moved_into_local_is_not_dropped_by_collector() {
        // `let s = b[a..b]` — the slice result is moved into a binding local,
        // which is released by the scope-exit `derive_local_bytes_drop_allowed`
        // path. A `Move` is not a borrowing use, so the collector must leave it
        // alone (no double-free).
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(BindingId(0), Place::Local(7));
        let blocks = vec![block(
            0,
            vec![
                runtime_call(
                    "hew_bytes_slice",
                    vec![Place::Local(5), Place::Local(3), Place::Local(4)],
                    Some(Place::Local(2)),
                ),
                Instr::Move {
                    dest: Place::Local(7),
                    src: Place::Local(2),
                },
            ],
            Terminator::Return,
        )];
        let locals = bytes_locals_with(&[(3, ResolvedTy::I64), (4, ResolvedTy::I64)]);
        assert!(
            collect(&blocks, &locals, &binding_locals).is_empty(),
            "a slice result moved into a binding local is owned by that binding; \
             the collector must not drop it (double-free otherwise)"
        );
    }

    #[test]
    fn usercall_transient_receiver_gets_one_inline_drop_after_borrowing_use() {
        // bb0: _2 = call mk() -> bb1
        // bb1: _3 = hew_vec_len(_2)  (borrow)  -> Return
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            block(1, vec![bytes_len(2, 3)], Terminator::Return),
        ];
        let locals = bytes_locals_with(&[(3, ResolvedTy::I64)]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(1, 1, Place::Local(2), ResolvedTy::Bytes)],
            "the user-call bytes temp is used once by a borrowing hew_vec_len; it \
             must be dropped immediately after that use, exactly once"
        );
    }

    #[test]
    fn usercall_result_moved_into_local_is_not_dropped_by_collector() {
        // The `let b = mk(); b.len()` shape: mk's dest _2 is `move`d into b's
        // local _4, then _4 is borrowed. _2's only use is the Move (ownership
        // transfer), so dropping _2 would double-free b's buffer.
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            block(
                1,
                vec![
                    Instr::Move {
                        dest: Place::Local(4),
                        src: Place::Local(2),
                    },
                    bytes_len(4, 3),
                ],
                Terminator::Return,
            ),
        ];
        let locals = bytes_locals_with(&[(3, ResolvedTy::I64)]);
        let binding_locals: HashMap<BindingId, Place> =
            [(BindingId(7), Place::Local(4))].into_iter().collect();
        assert!(
            collect(&blocks, &locals, &binding_locals).is_empty(),
            "the mk() temp's only use is a Move (ownership transfer to b); dropping \
             it would double-free the buffer b's scope-exit release owns"
        );
    }

    #[test]
    fn binding_local_usercall_result_stays_out_of_collector() {
        // mk()'s dest IS a binding local directly — owned by the scope-exit
        // derive_local_bytes_drop_allowed release, not the bare-temp collector.
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            block(1, vec![bytes_len(2, 3)], Terminator::Return),
        ];
        let locals = bytes_locals_with(&[(3, ResolvedTy::I64)]);
        let binding_locals: HashMap<BindingId, Place> =
            [(BindingId(9), Place::Local(2))].into_iter().collect();
        assert!(
            collect(&blocks, &locals, &binding_locals).is_empty(),
            "a bytes local owned by a let/parameter binding belongs to \
             derive_local_bytes_drop_allowed, not the bare-temp collector"
        );
    }

    #[test]
    fn fresh_bytes_helper_transient_is_admitted() {
        // `"x".to_bytes().len()`: hew_string_to_bytes is a non-allowlisted fresh
        // bytes helper (builtin: None, result not an interior alias), so its
        // transient result is admitted like a user call.
        let blocks = vec![
            block(
                0,
                vec![],
                Terminator::Call {
                    callee: "hew_string_to_bytes".to_string(),
                    builtin: None,
                    args: vec![Place::Local(5)],
                    dest: Some(Place::Local(2)),
                    next: 1,
                },
            ),
            block(1, vec![bytes_len(2, 3)], Terminator::Return),
        ];
        let locals = bytes_locals_with(&[(3, ResolvedTy::I64)]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(1, 1, Place::Local(2), ResolvedTy::Bytes)],
            "hew_string_to_bytes hands the caller a fresh rc==1 buffer; its \
             transient result borrowed once must be dropped exactly once"
        );
    }

    #[test]
    fn allowlisted_fresh_owned_bytes_terminator_is_admitted() {
        // hew_bytes_slice is an ALLOWLISTED runtime symbol, but its ownership
        // contract now classifies its result as `FreshOwnedBytes`: the non-empty
        // path bumps the shared buffer's refcount and the empty path returns a
        // null/0/0 triple, so the caller owes exactly one release. A transient
        // `b.slice(..).len()` result must therefore be dropped exactly once.
        let blocks = vec![
            block(
                0,
                vec![],
                Terminator::Call {
                    callee: "hew_bytes_slice".to_string(),
                    builtin: None,
                    args: vec![Place::Local(5)],
                    dest: Some(Place::Local(2)),
                    next: 1,
                },
            ),
            block(1, vec![bytes_len(2, 3)], Terminator::Return),
        ];
        let locals = bytes_locals_with(&[(3, ResolvedTy::I64)]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(1, 1, Place::Local(2), ResolvedTy::Bytes)],
            "an allowlisted fresh-owned-bytes runtime callee (hew_bytes_slice) \
             borrowed once must be dropped exactly once"
        );
    }

    #[test]
    fn allowlisted_borrowing_bytes_terminator_is_not_admitted() {
        // hew_bytes_get is an ALLOWLISTED runtime symbol whose result is NOT
        // `FreshOwnedBytes` (it borrows its receiver / returns a scalar); it
        // stays excluded fail-closed so a borrow-returner is never dropped.
        let blocks = vec![
            block(
                0,
                vec![],
                Terminator::Call {
                    callee: "hew_bytes_get".to_string(),
                    builtin: None,
                    args: vec![Place::Local(5)],
                    dest: Some(Place::Local(2)),
                    next: 1,
                },
            ),
            block(1, vec![bytes_len(2, 3)], Terminator::Return),
        ];
        let locals = bytes_locals_with(&[(3, ResolvedTy::I64)]);
        assert!(
            collect(&blocks, &locals, &HashMap::new()).is_empty(),
            "an allowlisted runtime bytes callee without a FreshOwnedBytes \
             result is excluded (leak-safe) — it may borrow or alias"
        );
    }

    #[test]
    fn discarded_usercall_result_dropped_at_single_pred_continuation() {
        // bb0: _2 = call mk() -> bb1 ; bb1 never reads _2 (discard).
        let blocks = vec![block(0, vec![], user_call("mk", 2, 1)), ret_block(1)];
        let locals = bytes_locals_with(&[]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(1, 0, Place::Local(2), ResolvedTy::Bytes)],
            "a discarded user-call bytes temp is dropped at the front of its \
             single-predecessor continuation"
        );
    }

    #[test]
    fn discarded_usercall_result_at_multipred_continuation_not_dropped() {
        // bb1 (the continuation) has two predecessors: bb0 (producer) and bb2. A
        // front-of-block drop could fire on the bb2 path that never produced _2.
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            ret_block(1),
            block(2, vec![], Terminator::Goto { target: 1 }),
        ];
        let locals = bytes_locals_with(&[]);
        assert!(
            collect(&blocks, &locals, &HashMap::new()).is_empty(),
            "a front-of-block drop on a multi-predecessor continuation could free \
             an uninitialised temp — fail closed"
        );
    }

    #[test]
    fn usercall_result_moved_into_record_not_dropped() {
        // mk()'s result is stored into a record field (aggregate ingress); the
        // aggregate owns the release. RecordInit is not a borrowing runtime call.
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            block(
                1,
                vec![Instr::RecordInit {
                    ty: boxed_ty(),
                    fields: vec![(FieldOffset(0), Place::Local(2))],
                    dest: Place::Local(5),
                }],
                Terminator::Return,
            ),
        ];
        let locals = bytes_locals_with(&[(5, boxed_ty())]);
        assert!(
            collect(&blocks, &locals, &HashMap::new()).is_empty(),
            "record ingress transfers ownership to the aggregate; an inline temp \
             drop would double-free the field"
        );
    }

    #[test]
    fn usercall_result_borrowed_by_terminator_call_gets_dropped_at_continuation() {
        // `mk().get(0)`: mk's dest _2 is borrowed by a `Terminator::Call`
        // (`hew_bytes_get`, a COLLECTION-scan receiver borrow). Every
        // Terminator::Call bytes arg is a borrow (Hew by-value heap params), so
        // the temp is dropped at the front of the call's continuation.
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            block(
                1,
                vec![],
                Terminator::Call {
                    callee: "hew_bytes_get".to_string(),
                    builtin: None,
                    args: vec![Place::Local(2), Place::Local(6)],
                    dest: Some(Place::Local(7)),
                    next: 2,
                },
            ),
            ret_block(2),
        ];
        let locals = bytes_locals_with(&[(6, ResolvedTy::I64)]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(2, 0, Place::Local(2), ResolvedTy::Bytes)],
            "a bytes temp borrowed by a Terminator::Call is released at the front \
             of the call's single-predecessor continuation"
        );
    }

    #[test]
    fn usercall_result_sent_to_actor_is_not_dropped() {
        // A `Terminator::Send` CONSUMES the bytes (mailbox hand-off); the sender
        // must NOT drop it. Only `Terminator::Call` is a borrow — `Send` is
        // excluded fail-closed.
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            block(
                1,
                vec![],
                Terminator::Send {
                    actor: Place::Local(6),
                    msg_type: 0,
                    value: Place::Local(2),
                    next: 2,
                    arg_modes: vec![crate::model::SendAliasMode::SnapshotBitCopy],
                    cleanup_plan: None,
                },
            ),
            ret_block(2),
        ];
        let locals = bytes_locals_with(&[]);
        assert!(
            collect(&blocks, &locals, &HashMap::new()).is_empty(),
            "a bytes temp consumed by an actor send must not be dropped by the \
             sender (the mailbox copy / state_drop owns the release)"
        );
    }

    #[test]
    fn usercall_result_read_only_by_wire_decode_gets_drop_after_the_decode() {
        // `Packet.decode(mk())`: the fresh bytes temp's ONLY reader is
        // `Instr::WireCodec` (decode operand), a borrowing use — codegen loads
        // the operand pointer to feed `__hew_deserialize_<key>` and never frees
        // it. The temp keeps its single drop obligation past the decode, so it is
        // released exactly once immediately AFTER the WireCodec (front of the
        // single-predecessor continuation, at idx 1), never at a discard site
        // before the decode reads the buffer — that ordering would be the
        // use-after-free this collector must never produce. Before this admission
        // the temp leaked one CBOR buffer per call.
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            block(
                1,
                vec![Instr::WireCodec {
                    dest: Place::Local(5),
                    operand: Place::Local(2),
                    direction: hew_types::WireCodecDirection::Decode,
                    value_ty: ResolvedTy::named_user("Packet", vec![]),
                }],
                Terminator::Return,
            ),
        ];
        let locals = bytes_locals_with(&[(5, ResolvedTy::named_user("Packet", vec![]))]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(1, 1, Place::Local(2), ResolvedTy::Bytes)],
            "a bytes temp borrowed once by a WireCodec decode must be dropped \
             exactly once immediately after the decode, never before it"
        );
    }

    #[test]
    fn encode_then_decode_anonymous_temp_gets_drop_after_the_decode() {
        // `Packet.decode(p.encode())`: `p.encode()` is a WireCodec serialize
        // producing a fresh CBOR bytes temp (_2), immediately borrowed as the
        // `Packet.decode(_2)` operand in the SAME block. The temp is released once
        // right after the decode instruction (idx 2). This is the anonymous
        // round-trip shape the leak oracle measures at exact-zero.
        let blocks = vec![block(
            0,
            vec![
                Instr::WireCodec {
                    dest: Place::Local(2),
                    operand: Place::Local(0),
                    direction: hew_types::WireCodecDirection::Encode,
                    value_ty: ResolvedTy::named_user("Packet", vec![]),
                },
                Instr::WireCodec {
                    dest: Place::Local(5),
                    operand: Place::Local(2),
                    direction: hew_types::WireCodecDirection::Decode,
                    value_ty: ResolvedTy::named_user("Packet", vec![]),
                },
            ],
            Terminator::Return,
        )];
        let locals = bytes_locals_with(&[
            (0, ResolvedTy::named_user("Packet", vec![])),
            (5, ResolvedTy::named_user("Packet", vec![])),
        ]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(0, 2, Place::Local(2), ResolvedTy::Bytes)],
            "the encode temp is a fresh owned CBOR buffer borrowed once by decode; \
             it must be dropped once immediately after the decode"
        );
    }

    #[test]
    fn discarded_encode_temp_dropped_right_after_producer() {
        // `p.encode();` — a discarded WireCodec serialize temp (zero uses) drops
        // immediately after the producer instruction (straight-line).
        let blocks = vec![block(
            0,
            vec![Instr::WireCodec {
                dest: Place::Local(2),
                operand: Place::Local(0),
                direction: hew_types::WireCodecDirection::Encode,
                value_ty: ResolvedTy::named_user("Packet", vec![]),
            }],
            Terminator::Return,
        )];
        let locals = bytes_locals_with(&[(0, ResolvedTy::named_user("Packet", vec![]))]);
        assert_eq!(
            collect(&blocks, &locals, &HashMap::new()),
            vec![(0, 1, Place::Local(2), ResolvedTy::Bytes)],
            "a discarded encode CBOR temp is dropped right after its producer"
        );
    }

    #[test]
    fn named_encode_binding_stays_out_of_bytes_collector() {
        // `let raw = p.encode(); Packet.decode(raw)` — the encode result is a
        // binding local, released by the scope-exit `derive_local_bytes_drop_allowed`
        // path; the bare-temp collector must not double-claim it.
        let blocks = vec![block(
            0,
            vec![
                Instr::WireCodec {
                    dest: Place::Local(2),
                    operand: Place::Local(0),
                    direction: hew_types::WireCodecDirection::Encode,
                    value_ty: ResolvedTy::named_user("Packet", vec![]),
                },
                Instr::WireCodec {
                    dest: Place::Local(5),
                    operand: Place::Local(2),
                    direction: hew_types::WireCodecDirection::Decode,
                    value_ty: ResolvedTy::named_user("Packet", vec![]),
                },
            ],
            Terminator::Return,
        )];
        let locals = bytes_locals_with(&[
            (0, ResolvedTy::named_user("Packet", vec![])),
            (5, ResolvedTy::named_user("Packet", vec![])),
        ]);
        let binding_locals: HashMap<BindingId, Place> =
            [(BindingId(2), Place::Local(2))].into_iter().collect();
        assert!(
            collect(&blocks, &locals, &binding_locals).is_empty(),
            "a named `let raw = p.encode()` binding is released by scope-exit \
             derivation, not the bare-temp collector"
        );
    }

    #[test]
    fn usercall_result_read_only_by_bytes_retain_is_not_discard_dropped() {
        // Same hidden-reader class: `Instr::BytesRetain` reads its value but is
        // excluded from `instr_source_places`. The reads-based use table must
        // see it and fail closed (no discard drop), never splice a drop the
        // retain's co-owner accounting did not budget for.
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            block(
                1,
                vec![Instr::BytesRetain {
                    value: Place::Local(2),
                }],
                Terminator::Return,
            ),
        ];
        let locals = bytes_locals_with(&[]);
        assert!(
            collect(&blocks, &locals, &HashMap::new()).is_empty(),
            "a bytes temp read by a BytesRetain must not take the discard drop; \
             the retain implies a co-owner mint this collector cannot balance"
        );
    }

    #[test]
    fn usercall_result_used_twice_not_dropped() {
        // Two source-uses (rule 4: at most one). Fail closed rather than guess
        // which use is the last.
        let blocks = vec![
            block(0, vec![], user_call("mk", 2, 1)),
            block(
                1,
                vec![bytes_len(2, 3), bytes_len(2, 6)],
                Terminator::Return,
            ),
        ];
        let locals = bytes_locals_with(&[(3, ResolvedTy::I64), (6, ResolvedTy::I64)]);
        assert!(
            collect(&blocks, &locals, &HashMap::new()).is_empty(),
            "a temp with more than one source-use is not the single-borrow shape; \
             fail closed (leak, never double-free)"
        );
    }
}
pub(super) fn bytes_place_is_typed(place: Place, local_tys: &[ResolvedTy]) -> bool {
    base_local(place)
        .is_some_and(|local| matches!(local_tys.get(local as usize), Some(ResolvedTy::Bytes)))
}
pub(super) fn bytes_interior_producer_dest(
    instr: &Instr,
    local_tys: &[ResolvedTy],
) -> Option<Place> {
    let dest = match instr {
        Instr::RecordFieldLoad { dest, .. }
        | Instr::TupleFieldLoad { dest, .. }
        | Instr::ClosureEnvFieldLoad { dest, .. } => *dest,
        Instr::CallRuntimeAbi(call)
            if matches!(call.symbol(), "hew_vec_get_owned" | "hew_vec_get_ptr") =>
        {
            call.dest()?
        }
        _ => return None,
    };
    bytes_place_is_typed(dest, local_tys).then_some(dest)
}
pub(super) fn bytes_runtime_arg_is_borrow(
    call: &crate::model::RuntimeCall,
    arg_index: usize,
) -> bool {
    let contract = crate::runtime_symbols::callee_ownership_contract(call.symbol());
    contract.borrows_all_bytes_args()
        || (arg_index == 0 && contract.borrows_bytes_receiver())
        || contract.is_vec_copy_in_element_store()
}
pub(super) fn bytes_share_sink_places(instr: &Instr) -> Vec<Place> {
    match instr {
        Instr::RecordInit { fields, .. } => fields.iter().map(|(_, place)| *place).collect(),
        Instr::TupleConstruct { elements, .. } => elements.clone(),
        Instr::RecordFieldStore { src, .. }
        | Instr::ActorStateFieldStore { src, .. }
        | Instr::ClosureEnvFieldStore { src, .. } => vec![*src],
        Instr::SpawnActor {
            state, init_args, ..
        } => state.iter().chain(init_args).copied().collect(),
        Instr::ClosureEnvInit { fields, .. } => fields
            .iter()
            .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsClonedOrRetained)
            .map(|field| field.src)
            .collect(),
        _ => Vec::new(),
    }
}
pub(super) fn readmit_retained_bytes_tuple_roots(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    alias_of: &HashMap<u32, u32>,
    retained_roots: &HashSet<u32>,
    excluded_roots: &mut HashSet<u32>,
) {
    for &retained_root in retained_roots {
        let root_members: HashSet<u32> = alias_of
            .iter()
            .filter_map(|(&local, &root)| (root == retained_root).then_some(local))
            .collect();
        let mut escapes = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if base_local(*src).is_some_and(|local| root_members.contains(&local)) {
                        let benign = base_local(*dest)
                            .is_some_and(|local| root_members.contains(&local))
                            && matches!(dest, Place::Local(_));
                        if !benign {
                            escapes = true;
                            break;
                        }
                    }
                    continue;
                }
                if matches!(
                    instr,
                    Instr::TupleFieldLoad { .. }
                        | Instr::BytesRetain { .. }
                        | Instr::StringRetain { .. }
                        | Instr::Drop { .. }
                        | Instr::FieldDropInPlace { .. }
                ) {
                    continue;
                }
                for place in instr_source_places(instr) {
                    let Some(local) = base_local(place) else {
                        continue;
                    };
                    if root_members.contains(&local)
                        && !binder_read_is_borrow_safe_instr(instr, local)
                    {
                        escapes = true;
                        break;
                    }
                }
                if escapes {
                    break;
                }
            }
            if escapes {
                break;
            }
            for place in terminator_source_places(&block.terminator, suspend_kinds.get(&block.id)) {
                let Some(local) = base_local(place) else {
                    continue;
                };
                if root_members.contains(&local)
                    && !binder_read_is_borrow_safe_terminator(
                        &block.terminator,
                        suspend_kinds.get(&block.id),
                        local,
                    )
                {
                    escapes = true;
                    break;
                }
            }
            if escapes {
                break;
            }
        }
        if !escapes {
            excluded_roots.remove(&retained_root);
        }
    }
}
fn apply_bytes_retain_sites(
    blocks: &mut [BasicBlock],
    instr_spans: &mut BTreeMap<(u32, u32), (u32, u32)>,
    retain_sites: &[BytesRetainSite],
) {
    let mut before: HashMap<(u32, usize), Vec<Place>> = HashMap::new();
    let mut after: HashMap<(u32, usize), Vec<Place>> = HashMap::new();
    for site in retain_sites {
        let table = match site.placement {
            BytesRetainPlacement::Before => &mut before,
            BytesRetainPlacement::After => &mut after,
        };
        table
            .entry((site.block, site.instr_index))
            .or_default()
            .push(site.value);
    }

    let old_spans = std::mem::take(instr_spans);
    let mut new_spans = BTreeMap::new();
    for block in blocks {
        let old_instructions = std::mem::take(&mut block.instructions);
        let old_len = old_instructions.len();
        let mut rewritten = Vec::with_capacity(old_len);
        for (old_index, instr) in old_instructions.into_iter().enumerate() {
            let span = old_spans
                .get(&(block.id, u32::try_from(old_index).unwrap_or(u32::MAX)))
                .copied();
            if let Some(values) = before.get(&(block.id, old_index)) {
                for value in values {
                    let new_index = u32::try_from(rewritten.len()).unwrap_or(u32::MAX);
                    rewritten.push(Instr::BytesRetain { value: *value });
                    if let Some(span) = span {
                        new_spans.insert((block.id, new_index), span);
                    }
                }
            }
            let new_index = u32::try_from(rewritten.len()).unwrap_or(u32::MAX);
            rewritten.push(instr);
            if let Some(span) = span {
                new_spans.insert((block.id, new_index), span);
            }
            if let Some(values) = after.get(&(block.id, old_index)) {
                for value in values {
                    let new_index = u32::try_from(rewritten.len()).unwrap_or(u32::MAX);
                    rewritten.push(Instr::BytesRetain { value: *value });
                    if let Some(span) = span {
                        new_spans.insert((block.id, new_index), span);
                    }
                }
            }
        }
        // Stage 2 (gdb `-g`): `record_terminator_span` keys the block
        // terminator's span one slot PAST the last instruction (at
        // `instructions.len()`). The rewrite above only re-keys indices
        // `0..old_len`, so without carrying this entry over, every block's
        // terminator span is silently dropped — call statements and
        // control-flow exits lose their line-table entry, collapsing the
        // breakpoint's `DILocation` onto the previous instruction's scope and
        // hiding shadowed inner bindings. Re-key it to the new terminator slot
        // (`rewritten.len()`), which grows by any `after`-placed retains on the
        // final instruction.
        if let Some(span) = old_spans
            .get(&(block.id, u32::try_from(old_len).unwrap_or(u32::MAX)))
            .copied()
        {
            let term_index = u32::try_from(rewritten.len()).unwrap_or(u32::MAX);
            new_spans.insert((block.id, term_index), span);
        }
        block.instructions = rewritten;
    }
    *instr_spans = new_spans;
}
pub(super) fn finalize_bytes_ownership(
    raw: &mut RawMirFunction,
    builder: &Builder,
    dataflow_result: &dataflow::DataflowResult,
) -> BytesDropDerivation {
    let owned_locals_snapshot = builder.owned_locals_snapshot();
    let mut derivation = derive_local_bytes_drop_allowed(
        &raw.blocks,
        &builder.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &builder.borrowed_bytes_param_locals,
    );
    for states in dataflow_result.exit_states.values() {
        for (binding, state) in states {
            if matches!(
                state,
                dataflow::BindingState::Consumed(_) | dataflow::BindingState::MaybeConsumed(_)
            ) {
                derivation.allowed.remove(binding);
            }
        }
    }
    derivation.retain_sites.retain(|site| {
        site.required_bindings
            .iter()
            .all(|binding| derivation.allowed.contains(binding))
    });
    apply_bytes_retain_sites(
        &mut raw.blocks,
        &mut raw.instr_spans,
        &derivation.retain_sites,
    );
    derivation
}
#[cfg(test)]
mod cow_sole_owner_derivation {
    //! Direct structural tests for `derive_cow_sole_owner` — the fail-closed
    //! layer-1 allow-set. These poke the derivation with synthetic MIR
    //! blocks because the buggy shape (an enum/machine payload-destructure
    //! binder) cannot be built through the `TypeCheckOutput::default()`
    //! minimal `elaborate.rs` pipeline (variant literals need populated
    //! `expr_types`), and the runtime liveness layer would mask a layer-1
    //! admission anyway. Asserting on the allow-set directly is the only
    //! place the projection-alias-via-`Move` hole is observable.
    use super::*;

    fn block(instructions: Vec<Instr>) -> BasicBlock {
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions,
            terminator: Terminator::Return,
        }
    }

    /// A `string` owned local whose backing local is never touched by any
    /// instruction is the canonical sole owner — it must be admitted.
    #[test]
    fn untouched_string_local_is_admitted() {
        let b = BindingId(1);
        let owned = vec![(b, "s".to_string(), ResolvedTy::String)];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(7));

        let allowed = derive_cow_sole_owner(
            &[block(vec![])],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &HashSet::new(),
            &[],
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
        )
        .allowed;
        assert!(
            allowed.contains(&b),
            "an untouched string local is its own sole owner and must be admitted"
        );
    }

    /// Two payload-destructure binders aliasing the same scrutinee
    /// (`Move {{ dest: Local(d), src: MachineVariant {{ local: scrutinee }} }}`,
    /// the `lower_match_enum_tag` shape) must BOTH be excluded: each aliases
    /// the parent's rc=1 buffer with no retain, so admitting either to a
    /// scope-exit `hew_string_drop` double-frees the shared buffer. This is
    /// the regression for the projection-alias-via-`Move` hole — before the
    /// fix `projection_alias_dest` saw only the four `*FieldLoad` instrs and
    /// these binders were admitted.
    #[test]
    fn machine_variant_destructure_binders_are_excluded() {
        let f = BindingId(10);
        let g = BindingId(11);
        let scrutinee = 5u32;
        let proj = |field_idx| Place::MachineVariant {
            local: scrutinee,
            variant_idx: 0,
            field_idx,
        };
        let instrs = vec![
            Instr::Move {
                dest: Place::Local(20),
                src: proj(0),
            },
            Instr::Move {
                dest: Place::Local(21),
                src: proj(0),
            },
        ];
        let owned = vec![
            (f, "f".to_string(), ResolvedTy::String),
            (g, "g".to_string(), ResolvedTy::String),
        ];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(f, Place::Local(20));
        binding_locals.insert(g, Place::Local(21));

        let allowed = derive_cow_sole_owner(
            &[block(instrs)],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &HashSet::new(),
            &[],
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
        )
        .allowed;
        assert!(
            !allowed.contains(&f) && !allowed.contains(&g),
            "payload-destructure binders alias parent storage (no retain) and \
             must be excluded from the scope-exit drop plan; got {allowed:?}"
        );
    }

    /// The same exclusion holds for the user-enum (`EnumVariant`) interior-projection
    /// load form, and the
    /// taint propagates one `Move` hop further (binder copied into a second
    /// local).
    #[test]
    fn enum_variant_destructure_taint_propagates_through_move() {
        let payload = BindingId(30);
        let copy = BindingId(31);
        let instrs = vec![
            // copy <- EnumVariant{scrutinee} : payload-destructure binder.
            Instr::Move {
                dest: Place::Local(40),
                src: Place::EnumVariant {
                    local: 3,
                    variant_idx: 0,
                    field_idx: 0,
                },
            },
            // local 41 <- Local(40) : plain hand-off Move, must carry taint.
            Instr::Move {
                dest: Place::Local(41),
                src: Place::Local(40),
            },
        ];
        let owned = vec![
            (payload, "p".to_string(), ResolvedTy::String),
            (copy, "c".to_string(), ResolvedTy::String),
        ];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(payload, Place::Local(40));
        binding_locals.insert(copy, Place::Local(41));

        let allowed = derive_cow_sole_owner(
            &[block(instrs)],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &HashSet::new(),
            &[],
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
        )
        .allowed;
        assert!(
            allowed.is_empty(),
            "the enum-variant binder and the local it is moved into both alias \
             parent storage and must be excluded; got {allowed:?}"
        );
    }

    /// A plain `Local -> Local` string `Move` is a retained co-owner share:
    /// both bindings keep a drop obligation and one retain is emitted.
    #[test]
    fn plain_local_move_retains_and_admits_both_owners() {
        let src = BindingId(50);
        let dst = BindingId(51);
        let instrs = vec![
            Instr::Move {
                dest: Place::Local(61),
                src: Place::Local(60),
            },
            Instr::IntCmp {
                dest: Place::Local(62),
                pred: CmpPred::Eq,
                lhs: Place::Local(60),
                rhs: Place::Local(61),
            },
        ];
        let owned = vec![
            (src, "src".to_string(), ResolvedTy::String),
            (dst, "dst".to_string(), ResolvedTy::String),
        ];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(src, Place::Local(60));
        binding_locals.insert(dst, Place::Local(61));

        let derivation = derive_cow_sole_owner(
            &[block(instrs)],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &HashSet::new(),
            &[],
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
        );
        assert!(
            derivation.allowed.contains(&src) && derivation.allowed.contains(&dst),
            "both retained string co-owners must keep their scope-exit drops"
        );
        assert_eq!(
            derivation.retain_sites,
            vec![StringRetainSite {
                block: 0,
                instr_index: 0,
                value: Place::Local(60),
                required_bindings: Vec::new(),
            }]
        );
    }

    #[test]
    fn record_ingress_retains_and_keeps_source_drop() {
        let source = BindingId(70);
        let owned = vec![(source, "source".to_string(), ResolvedTy::String)];
        let binding_locals = HashMap::from([(source, Place::Local(3))]);
        let derivation = derive_cow_sole_owner(
            &[block(vec![
                Instr::RecordInit {
                    ty: ResolvedTy::named_user("Holder", vec![]),
                    fields: vec![(FieldOffset(0), Place::Local(3))],
                    dest: Place::Local(4),
                },
                Instr::IntCmp {
                    dest: Place::Local(5),
                    pred: CmpPred::Eq,
                    lhs: Place::Local(3),
                    rhs: Place::Local(3),
                },
            ])],
            &HashMap::new(),
            &owned,
            &binding_locals,
            &HashSet::new(),
            &[
                ResolvedTy::String,
                ResolvedTy::String,
                ResolvedTy::String,
                ResolvedTy::String,
            ],
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
        );
        assert!(derivation.allowed.contains(&source));
        assert_eq!(
            derivation.retain_sites,
            vec![StringRetainSite {
                block: 0,
                instr_index: 0,
                value: Place::Local(3),
                required_bindings: Vec::new(),
            }]
        );
    }

    #[test]
    fn borrowed_param_variant_ingress_retains_for_the_payload_owner() {
        let derivation = derive_cow_sole_owner(
            &[block(vec![Instr::Move {
                dest: Place::EnumVariant {
                    local: 5,
                    variant_idx: 0,
                    field_idx: 0,
                },
                src: Place::Local(2),
            }])],
            &HashMap::new(),
            &[],
            &HashMap::new(),
            &HashSet::new(),
            &[ResolvedTy::String, ResolvedTy::String, ResolvedTy::String],
            &HashSet::from([2]),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
        );
        assert_eq!(
            derivation.retain_sites,
            vec![StringRetainSite {
                block: 0,
                instr_index: 0,
                value: Place::Local(2),
                required_bindings: Vec::new(),
            }]
        );
    }

    #[test]
    fn explicit_string_drop_call_excludes_the_released_owner() {
        let binding = BindingId(80);
        let blocks = vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: "hew_string_drop".to_string(),
                builtin: None,
                args: vec![Place::Local(3)],
                dest: None,
                next: 1,
            },
        }];
        let derivation = derive_cow_sole_owner(
            &blocks,
            &HashMap::new(),
            &[(binding, "value".to_string(), ResolvedTy::String)],
            &HashMap::from([(binding, Place::Local(3))]),
            &HashSet::new(),
            &[
                ResolvedTy::String,
                ResolvedTy::String,
                ResolvedTy::String,
                ResolvedTy::String,
            ],
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::from(["hew_string_drop".to_string()]),
            &HashSet::new(),
        );
        assert!(
            !derivation.allowed.contains(&binding),
            "an explicit release owns the drop; scope exit must not release again"
        );
    }
}
#[cfg(test)]
mod call_args_borrow_safe_bytes_append_pins {
    //! #2474: `call_args_borrow_safe` (the escape scan the
    //! for-await/generator loop-variable exit-edge release consults on
    //! `return`/`break`/`continue`) must recognise the all-args-borrow bytes
    //! contract. `hew_bytes_append` borrows its receiver AND the unpacked
    //! source triple — no argument position is consumed — so a loop variable
    //! threaded into the source triple (args[1..], NOT the receiver) stays
    //! owned by its binding and its exit-edge drop must fire. The sibling
    //! composite-drop provers
    //! (`binder_read_is_borrow_safe_terminator`/`_instr`) already carry this
    //! exemption; before the fix this function did not, so that shape leaked.
    use super::*;

    fn contract(sym: &str) -> crate::runtime_symbols::CalleeOwnershipContract {
        crate::runtime_symbols::callee_ownership_contract(sym)
    }

    #[test]
    fn bytes_append_receiver_reference_is_borrow_safe() {
        // args[0] is the receiver triple's base local.
        let args = [Place::Local(7)];
        assert!(
            call_args_borrow_safe(contract("hew_bytes_append"), &args, 7),
            "the hew_bytes_append receiver is a read-only borrow"
        );
    }

    #[test]
    fn bytes_append_source_triple_reference_is_borrow_safe() {
        // A for-await/generator loop variable (local 9) threaded into the
        // source-triple positions (args[1..]) — the exact #2474 leak shape.
        // hew_bytes_append(&mut dst_triple, src_ptr, src_offset, src_len):
        // model the loop var appearing in the by-value source tail.
        let args = [
            Place::Local(3), // receiver dst triple
            Place::Local(9), // src ptr (from loop var)
            Place::Local(9), // src offset
            Place::Local(9), // src len
        ];
        assert!(
            call_args_borrow_safe(contract("hew_bytes_append"), &args, 9),
            "every hew_bytes_append argument position is a read-only borrow; \
             a source-triple reference does not escape (was leaking pre-#2474)"
        );
    }

    #[test]
    fn local_absent_is_borrow_safe_regardless_of_contract() {
        // Baseline: a local that never appears in args is trivially safe.
        let args = [Place::Local(1), Place::Local(2)];
        assert!(call_args_borrow_safe(
            contract("hew_bytes_append"),
            &args,
            42
        ));
    }

    #[test]
    fn non_borrowing_callee_reference_still_escapes() {
        // Fail-closed anchor: a callee WITHOUT a proven borrow contract, with
        // the tracked local in an argument, is still unproven (escape). This
        // pins that the fix widened ONLY the bytes-all-args-borrow case and
        // did not relax the fail-closed default for arbitrary callees.
        let args = [Place::Local(5), Place::Local(9)];
        let c = contract("hew_vec_push");
        assert!(
            !c.borrows_all_bytes_args(),
            "guard: hew_vec_push must not be an all-args-borrow contract"
        );
        assert!(
            !call_args_borrow_safe(c, &args, 9),
            "a non-borrowing callee with the tracked local as a by-value arg \
             must stay unproven (fail-closed leak, never re-admitted)"
        );
    }
}
