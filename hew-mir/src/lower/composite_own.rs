#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    alias_projection_chain_owner_seeds, attribute_field_binder_provenance, base_local,
    binder_read_is_borrow_safe_instr, binder_read_is_borrow_safe_terminator, blocks_reachable_from,
    bytes_interior_producer_dest, bytes_place_is_typed, bytes_runtime_arg_is_borrow,
    bytes_share_sink_places, close_alias_binders_forward, collect_record_field_binders,
    compute_collection_interior_alias_taint, descend_match_bound_hop_alias_chain,
    descend_match_bound_hop_aliases, instr_escape_places, instr_source_places,
    local_is_byte_copy_aggregate, note_payload_escape, place_is_interior_projection,
    place_is_tag_read, propagate_whole_value_alias_roots, readmit_retained_bytes_tuple_roots,
    render_owned_handle_ty, retained_string_terminator_drop_safe, shift_instr_spans_on_insert,
    short_name, string_field_load_producer_dest, terminator_escape_places,
    terminator_source_places, ty_is_heap_owning_enum_composite, ty_is_heap_owning_tuple,
    ty_is_owned_handle_leaf, vec_iter_record_init_vec_source, AggregateOwner, BTreeMap, BasicBlock,
    BindingId, BytesDropDerivation, BytesRetainPlacement, BytesRetainSite,
    ClosureEnvFieldOwnership, FieldBinderProvenance, FieldOffset, HashMap, HashSet, Instr,
    MirCheck, MirStatement, Place, ResolvedTy, RootScan, ScopeId, SuspendKind, Terminator,
    FOR_ITER_CURSOR_NAME_PREFIX,
};

/// #2212 — discharge the non-escaped owned sibling fields of a record whose
/// composite drop the sole-owner prover excludes because ONE of its fields
/// escaped through a field binder.
///
/// `derive_owned_record_drop_allowed` excludes a record root from its
/// scope-exit `RecordInPlace` drop when an owned-field binder loaded from it
/// escapes (the escapee owns that field now). The exclusion is
/// function-scoped, so every OTHER owned field of the record — still solely
/// owned by the record slot — leaked (#2212: one 64 B `tag` buffer per
/// frame at slope 1). This pass emits one `Instr::FieldDropInPlace` per
/// non-escaped owned sibling right after the escape instruction, where the
/// value flow proves that point is past the record's last use.
///
/// Runs post-seal, after `apply_nested_fresh_string_temp_drops` and before
/// `check_function` / drop elaboration, so the dataflow observes each
/// discharge as a read of the record local and codegen emits the release.
///
/// ## Fail-closed admission (ALL conditions required; any miss keeps
/// today's whole-record leak — never a double-free)
///
/// 1. The root is an `owned_locals` owned-aggregate-record candidate whose
///    whole-value alias set is the root alone (no `let b2 = b` copies —
///    copies byte-share field pointers and can diverge; the discharge frees
///    through the root slot only).
/// 2. Exactly ONE escape event exists across the root's binders, it is an
///    instruction (a terminator escape has no post-escape insertion point),
///    and the escaping binder's provenance is `Unique { root, field }` —
///    the value flow proves both the root and WHICH field escaped. The
///    escaped field is never discharged: for a moved-out binder the escapee
///    owns it; for a retained `string` clone the original keeps its
///    pre-existing leak.
/// 3. No binder of the root is the base local of another `owned_locals`
///    binding and none is the place of an inline `Drop` — an extracted
///    field with its own release path (`let g = b.gen`) is a second owner
///    whose release this pass must not race (the aggregate-extraction
///    double-free class).
/// 4. The escape's block is not reachable from itself (a loop would re-run
///    the discharge, and inline-composite fields have no null-store to make
///    that idempotent), and NO use of the root or its binders — field
///    loads, alias moves, clone reads, borrow-safe binder reads — lies
///    after the escape (a later position in its block, its block's
///    terminator, or any transitively reachable block). Discharging before
///    a live read would free a slot the read still observes.
///
/// The discharged sibling set is the record's owned fields minus the
/// escaped field, narrowed to the shapes the field-drop contract covers
/// (`string`, or a classifier-admitted aggregate —
/// `field_drop_in_place_admissible`); an owned sibling outside that set
/// keeps its leak. The emitted op's base is the root local, so the
/// composite-drop prover's direct `FieldDropInPlace` exclusion rule keeps
/// the root excluded when it re-derives over these blocks — admission and
/// discharge cannot disagree.
///
/// Binder-local reuse through a terminator dest (a call result written into
/// a spent binder slot) is not tracked as a defining write: stale
/// provenance can only redirect WHICH field is treated as escaped (that
/// field keeps its leak), never which record owns the siblings, so the
/// discharge stays sound.
#[allow(
    clippy::too_many_arguments,
    reason = "each argument is a distinct Builder-owned input (blocks, the \
              ownership ledgers, the layout tables, the four type-shape \
              predicates, the debug line table); bundling them into a struct \
              would add indirection at the single call site"
)]
#[allow(
    clippy::too_many_lines,
    reason = "one exhaustive event-classification walk over every \
              instruction and terminator, then the per-root admission checks \
              and the splice; splitting the walk from the checks would \
              scatter the fail-closed poison rules the soundness argument \
              enumerates in the doc comment"
)]
pub(super) fn apply_escaped_record_sibling_field_drops(
    blocks: &mut [BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
    alias_chain: &[(u32, u32, u32)],
    is_owned_record: &dyn Fn(&ResolvedTy) -> bool,
    owned_field_list: &dyn Fn(&ResolvedTy) -> Vec<(u32, ResolvedTy)>,
    owned_tuple_field_list: &dyn Fn(&ResolvedTy) -> Vec<(u32, ResolvedTy)>,
    field_dischargeable: &dyn Fn(&ResolvedTy) -> bool,
    instr_spans: &mut BTreeMap<(u32, u32), (u32, u32)>,
) {
    // Candidate roots: base locals of owned-aggregate-record bindings — the
    // same candidate set the composite-drop prover derives from.
    let mut root_record_ty: HashMap<u32, ResolvedTy> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !is_owned_record(ty) {
            continue;
        }
        let Some(place) = binding_locals.get(binding) else {
            continue;
        };
        let Some(local) = base_local(*place) else {
            continue;
        };
        root_record_ty.insert(local, ty.clone());
    }
    // Tuple candidate roots (#2383): base locals of owned heap-owning TUPLE
    // bindings — the tuple composite prover's candidate set. The one-hop scan
    // below is record-only; the multi-hop chain compensator walks BOTH root
    // kinds, because `derive_tuple_composite_drop_allowed` folds the recorded
    // deep aliases into its exclusion exactly as the record prover does, and
    // a widened exclusion without compensation leaks every chain sibling.
    let mut root_tuple_ty: HashMap<u32, ResolvedTy> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !ty_is_heap_owning_tuple(ty, record_field_orders, enum_layouts) {
            continue;
        }
        let Some(place) = binding_locals.get(binding) else {
            continue;
        };
        let Some(local) = base_local(*place) else {
            continue;
        };
        root_tuple_ty.insert(local, ty.clone());
    }
    if root_record_ty.is_empty() && root_tuple_ty.is_empty() {
        return;
    }

    let alias_of = propagate_whole_value_alias_roots(blocks, root_record_ty.keys().copied());
    let tuple_alias_of = propagate_whole_value_alias_roots(blocks, root_tuple_ty.keys().copied());
    let local_is_heap_owning = |local: u32| -> bool {
        local_tys
            .get(local as usize)
            .is_some_and(|ty| crate::model::ty_owns_heap_mir(ty, record_field_orders, enum_layouts))
    };
    let field_binders = collect_record_field_binders(blocks, &alias_of, &local_is_heap_owning);
    let provenance = attribute_field_binder_provenance(blocks, &alias_of, &field_binders);
    let binder_root = |binder: u32| -> Option<u32> {
        match provenance.get(&binder) {
            Some(
                FieldBinderProvenance::Unique { root, .. }
                | FieldBinderProvenance::RootOnly { root },
            ) => Some(*root),
            _ => None,
        }
    };

    // Condition 1 — member count per root (only singleton alias sets admit).
    let mut member_count: HashMap<u32, u32> = HashMap::new();
    for &root in alias_of.values() {
        *member_count.entry(root).or_insert(0) += 1;
    }
    // Condition 3 — base locals of every owned binding (a binder in this set
    // is an extracted field with its own release path).
    let owned_binding_bases: HashSet<u32> = owned_locals
        .iter()
        .filter_map(|(binding, _, _)| binding_locals.get(binding).and_then(|p| base_local(*p)))
        .collect();

    let mut scans: HashMap<u32, RootScan> = root_record_ty
        .keys()
        .map(|&r| (r, RootScan::default()))
        .collect();
    // Uses of a binder whose provenance names no single root: dangerous for
    // EVERY root's after-escape region.
    let mut global_sites: Vec<(u32, Option<usize>)> = Vec::new();
    // An ambiguous binder escaping (or any event this walk cannot attribute)
    // refuses every discharge in the function.
    let mut poison_all = false;

    for block in blocks.iter() {
        let bid = block.id;
        for (idx, instr) in block.instructions.iter().enumerate() {
            // Per-event closures would fight the borrow checker over `scans`;
            // small macros keep the classification readable instead.
            macro_rules! poison {
                ($root:expr) => {
                    if let Some(scan) = scans.get_mut(&$root) {
                        scan.poisoned = true;
                    }
                };
            }
            macro_rules! site {
                ($root:expr, $pos:expr) => {
                    match $root {
                        Some(r) => {
                            if let Some(scan) = scans.get_mut(&r) {
                                scan.sites.push((bid, $pos));
                            }
                        }
                        None => global_sites.push((bid, $pos)),
                    }
                };
            }
            macro_rules! escape {
                ($binder:expr, $pos:expr) => {
                    match provenance.get(&$binder) {
                        Some(FieldBinderProvenance::Unique { root, field }) => {
                            if let Some(scan) = scans.get_mut(root) {
                                scan.escapes.push((bid, $pos, *field));
                            }
                        }
                        Some(FieldBinderProvenance::RootOnly { root }) => poison!(*root),
                        _ => poison_all = true,
                    }
                };
            }
            match instr {
                Instr::RecordFieldLoad { record, dest, .. } => {
                    if let Some(&root) = base_local(*record).and_then(|rl| alias_of.get(&rl)) {
                        site!(Some(root), Some(idx));
                    }
                    // A member slot overwritten by a field load is not a
                    // construction shape this pass models.
                    if let Some(&root) = base_local(*dest).and_then(|dl| alias_of.get(&dl)) {
                        poison!(root);
                    }
                }
                Instr::RecordInit { fields, dest, .. } => {
                    for (_, p) in fields {
                        if let Some(l) = base_local(*p) {
                            if let Some(&root) = alias_of.get(&l) {
                                poison!(root);
                            } else if field_binders.contains(&l) {
                                // A binder packed into a fresh aggregate is an
                                // owning sink — an escape event.
                                escape!(l, idx);
                            }
                        }
                    }
                    if let Some(&root) = base_local(*dest).and_then(|dl| alias_of.get(&dl)) {
                        // Construction into the root slot (call-free init).
                        site!(Some(root), Some(idx));
                    }
                }
                Instr::ClosureEnvInit { fields, dest, .. } => {
                    for field in fields
                        .iter()
                        .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
                    {
                        if let Some(l) = base_local(field.src) {
                            if let Some(&root) = alias_of.get(&l) {
                                poison!(root);
                            } else if field_binders.contains(&l) {
                                escape!(l, idx);
                            }
                        }
                    }
                    if let Some(&root) = base_local(*dest).and_then(|dl| alias_of.get(&dl)) {
                        site!(Some(root), Some(idx));
                    }
                }
                Instr::RecordCloneInplace { src, dest, .. } => {
                    if let Some(l) = base_local(*src) {
                        if let Some(&root) = alias_of.get(&l) {
                            // A deep clone borrows the source's fields; the
                            // source keeps sole ownership of its originals.
                            site!(Some(root), Some(idx));
                        } else if field_binders.contains(&l) {
                            site!(binder_root(l), Some(idx));
                        }
                    }
                    if let Some(&root) = base_local(*dest).and_then(|dl| alias_of.get(&dl)) {
                        poison!(root);
                    }
                }
                Instr::Move { dest, src } => {
                    let sl = base_local(*src).filter(|_| matches!(src, Place::Local(_)));
                    let dl = base_local(*dest).filter(|_| matches!(dest, Place::Local(_)));
                    let src_member = sl.and_then(|l| alias_of.get(&l).copied());
                    let dest_member = dl.and_then(|l| alias_of.get(&l).copied());
                    let src_binder = sl.filter(|l| field_binders.contains(l));
                    let dest_binder = dl.filter(|l| field_binders.contains(l));
                    if let Some(r) = src_member {
                        if dest_member == Some(r) {
                            // Whole-value alias hand-off inside the set (the
                            // singleton-set gate refuses these roots anyway).
                            site!(Some(r), Some(idx));
                        } else {
                            // The whole record moves out — every field goes
                            // with it; nothing is left to discharge.
                            poison!(r);
                        }
                    } else if let Some(b) = src_binder {
                        if dest_binder.is_some() {
                            site!(binder_root(b), Some(idx));
                        } else {
                            // Moved into a non-member, non-binder place
                            // (ReturnSlot, an unrelated local): the escape.
                            escape!(b, idx);
                        }
                    } else if let Some(r) = dest_member {
                        // Construction / initialization write into the root
                        // slot from a non-member source.
                        site!(Some(r), Some(idx));
                    }
                    // A non-binder source moved INTO a binder slot is a reuse
                    // write; provenance pass 0 already forced it Ambiguous.
                }
                Instr::Drop { place, .. } => {
                    if let Some(l) = base_local(*place) {
                        if let Some(&root) = alias_of.get(&l) {
                            poison!(root);
                        } else if field_binders.contains(&l) {
                            // An inline release of a binder (an extracted
                            // field with its own drop, or a spliced
                            // read-temp release): a second release path this
                            // pass must not reason past.
                            match binder_root(l) {
                                Some(r) => poison!(r),
                                None => poison_all = true,
                            }
                        }
                    }
                }
                Instr::FieldDropInPlace { base, .. }
                | Instr::RecordFieldDrop { record: base, .. }
                | Instr::RecordFieldStore { record: base, .. } => {
                    // Field-granular writes/releases against a member carry
                    // overwrite semantics this pass does not model.
                    if let Some(l) = base_local(*base) {
                        if let Some(&root) = alias_of.get(&l) {
                            poison!(root);
                        }
                    }
                    if let Instr::RecordFieldStore { src, .. } = instr {
                        if let Some(l) = base_local(*src) {
                            if field_binders.contains(&l) {
                                // Binder stored into another aggregate's
                                // field slot — an owning sink.
                                escape!(l, idx);
                            }
                        }
                    }
                }
                other => {
                    let (reads, writes) = crate::dataflow::instr_reads_writes(other);
                    for p in reads {
                        if let Some(l) = base_local(p) {
                            if let Some(&root) = alias_of.get(&l) {
                                // Any unmodelled read of the record itself.
                                poison!(root);
                            } else if field_binders.contains(&l) {
                                if binder_read_is_borrow_safe_instr(other, l) {
                                    site!(binder_root(l), Some(idx));
                                } else {
                                    escape!(l, idx);
                                }
                            }
                        }
                    }
                    for p in writes {
                        if let Some(l) = base_local(p) {
                            if let Some(&root) = alias_of.get(&l) {
                                // The record slot overwritten by an
                                // unmodelled producer.
                                poison!(root);
                            }
                            // Binder reuse writes: provenance pass 0 already
                            // forced the binder Ambiguous.
                        }
                    }
                }
            }
        }
        for p in terminator_source_places(&block.terminator, suspend_kinds.get(&bid)) {
            let Some(l) = base_local(p) else { continue };
            if let Some(&root) = alias_of.get(&l) {
                // The whole record read by a terminator (returned, sent,
                // passed to a call): refuse.
                if let Some(scan) = scans.get_mut(&root) {
                    scan.poisoned = true;
                }
            } else if field_binders.contains(&l) {
                if binder_read_is_borrow_safe_terminator(
                    &block.terminator,
                    suspend_kinds.get(&bid),
                    l,
                ) {
                    match binder_root(l) {
                        Some(r) => {
                            if let Some(scan) = scans.get_mut(&r) {
                                scan.sites.push((bid, None));
                            }
                        }
                        None => global_sites.push((bid, None)),
                    }
                } else {
                    // A terminator escape has no post-escape insertion
                    // point; refuse the discharge (leak-as-before).
                    match binder_root(l) {
                        Some(r) => {
                            if let Some(scan) = scans.get_mut(&r) {
                                scan.poisoned = true;
                            }
                        }
                        None => poison_all = true,
                    }
                }
            }
        }
    }
    if poison_all {
        return;
    }

    let mut roots: Vec<u32> = scans.keys().copied().collect();
    roots.sort_unstable();
    let mut insertions: Vec<(u32, usize, Vec<Instr>)> = Vec::new();
    for root in roots {
        let scan = &scans[&root];
        if scan.poisoned || member_count.get(&root).copied().unwrap_or(0) != 1 {
            continue;
        }
        let &[(esc_block, esc_idx, esc_field)] = &scan.escapes[..] else {
            continue;
        };
        if field_binders
            .iter()
            .any(|b| binder_root(*b) == Some(root) && owned_binding_bases.contains(b))
        {
            continue;
        }
        let reach = blocks_reachable_from(blocks, esc_block);
        if reach.contains(&esc_block) {
            continue;
        }
        let in_region = |&(sb, si): &(u32, Option<usize>)| -> bool {
            if sb == esc_block {
                si.is_none_or(|i| i > esc_idx)
            } else {
                reach.contains(&sb)
            }
        };
        if scan.sites.iter().any(in_region) || global_sites.iter().any(in_region) {
            continue;
        }
        let record_ty = &root_record_ty[&root];
        let siblings: Vec<Instr> = owned_field_list(record_ty)
            .into_iter()
            .filter(|(idx, _)| *idx != esc_field)
            .filter(|(_, ty)| field_dischargeable(ty))
            .map(|(idx, ty)| Instr::FieldDropInPlace {
                base: Place::Local(root),
                field: crate::model::FieldAddr::Record(FieldOffset(idx)),
                ty,
            })
            .collect();
        if siblings.is_empty() {
            continue;
        }
        insertions.push((esc_block, esc_idx + 1, siblings));
    }
    // The one-hop scan above sees only a binder loaded DIRECTLY off a whole-value
    // alias member, so a ≥2-hop escape (`let mid = o.mid; let leaf = mid.leaf;
    // return leaf`) is invisible to it — yet the composite-drop provers DO
    // exclude the owner for it (via `close_alias_binders_forward`). Walk the
    // recorded alias chain — record AND tuple roots (#2383) — plus the #2387
    // match-bound byte-copy hop chain, and discharge the non-escaped siblings at
    // every level so the widened exclusion never outruns its compensation.
    let candidate_roots: HashSet<u32> = root_record_ty
        .keys()
        .chain(root_tuple_ty.keys())
        .copied()
        .collect();
    let match_hop_alias_seeds = alias_projection_chain_owner_seeds(alias_chain, &candidate_roots);
    let dest_is_byte_copy_aggregate = |local: u32| -> bool {
        local_is_byte_copy_aggregate(local, local_tys, record_field_orders, enum_layouts)
    };
    let mut discharge_alias_chain = alias_chain.to_vec();
    discharge_alias_chain.extend(descend_match_bound_hop_alias_chain(
        blocks,
        &match_hop_alias_seeds,
        alias_chain,
        &dest_is_byte_copy_aggregate,
    ));
    insertions.extend(compute_escaped_chain_sibling_drops(
        blocks,
        suspend_kinds,
        &root_record_ty,
        &root_tuple_ty,
        &alias_of,
        &tuple_alias_of,
        &discharge_alias_chain,
        local_tys,
        owned_field_list,
        owned_tuple_field_list,
        field_dischargeable,
    ));
    if insertions.is_empty() {
        return;
    }
    let mut by_block: HashMap<u32, Vec<(usize, Vec<Instr>)>> = HashMap::new();
    for (bid, at, ops) in insertions {
        by_block.entry(bid).or_default().push((at, ops));
    }
    for block in blocks.iter_mut() {
        let Some(mut ins) = by_block.remove(&block.id) else {
            continue;
        };
        // Descending index order so an earlier splice does not shift a later
        // (lower-index) one; each packet is spliced in reverse so its ops
        // land in field order.
        ins.sort_by_key(|entry| std::cmp::Reverse(entry.0));
        for (at, ops) in ins {
            let at = at.min(block.instructions.len());
            for op in ops.into_iter().rev() {
                block.instructions.insert(at, op);
                shift_instr_spans_on_insert(
                    instr_spans,
                    block.id,
                    u32::try_from(at).unwrap_or(u32::MAX),
                );
            }
        }
    }
}
/// Multi-hop sibling discharge for the ESCAPED deep-alias chain
/// (`let mid = o.mid; let leaf = mid.leaf; return leaf` and its tuple twin
/// `let mid = o.0; let leaf = mid.0; leaf`), the ≥2-hop companion to the
/// one-hop scan in [`apply_escaped_record_sibling_field_drops`].
///
/// `derive_owned_record_drop_allowed` and `derive_tuple_composite_drop_allowed`
/// fold the recorded deep aliases into their exclusion via
/// `close_alias_binders_forward`, so when a ≥2-hop alias escapes into an owning
/// sink they suppress the OWNER root's whole composite drop — otherwise the
/// owner would free a subtree the escapee already handed to the caller (the
/// #2375 double-free). The one-hop scan cannot see a ≥2-hop alias (its
/// field-binder scan reaches only a binder loaded DIRECTLY off a whole-value
/// alias member), so the widened exclusion removed the composite drop but
/// nothing discharged the non-escaped siblings ALONG the chain — the outer `c`
/// and the intermediate `mid.x` leaked unconditionally (the P0 regression),
/// and the tuple path leaked every chain sibling the same way (#2383,
/// ~4 strings/call on the nested-tuple return shape).
///
/// This walk mirrors the exclusion's reach: from the escapee up its immediate-
/// parent chain to the owning root, it emits one `FieldDropInPlace` per owned
/// field that does NOT lead to the next (escaping) hop, addressed through the
/// still-live byte-copy alias local at each level (`mid.x` through `mid`, `o.c`
/// through `o`; `mid.1` / `o.1` on the tuple chain). Each node's address kind
/// follows its own type — `FieldAddr::Tuple` for a tuple node,
/// `FieldAddr::Record` otherwise — so a mixed record/tuple chain discharges
/// each level through the matching selector. Exactly-once invariant: the
/// escaped field at each level is never discharged (the escapee owns that
/// subtree), every other owned field is discharged exactly once through its
/// level's alias slot.
///
/// Fail-closed, coupled to the provers' exclusion so the two never disagree:
/// exactly ONE chain alias may escape, at a single INSTRUCTION whose escape
/// trigger (a `Move` to a non-member/non-carrier slot, a `RecordInit` field, a
/// `RecordFieldStore` source) is a strict subset of the provers' exclusion
/// triggers; the chain must resolve cleanly to a single candidate root through
/// ≥2 byte-copy hops (a one-hop escape is the scan above's job); the escape
/// block must not be reachable from itself (no loop — the inline-composite
/// discharges have no null-store to make a re-run idempotent); and NO node of
/// the chain may be read after the escape point. Any use of a chain alias this
/// walk cannot model bails the whole pass (leak-as-before, never a double-free).
#[allow(
    clippy::too_many_arguments,
    reason = "each argument is a distinct caller-owned input the walk needs — \
              the MIR, the suspend table, the record/tuple candidate-root and \
              whole-value alias maps, the recorded chain, the local type table, \
              and the three type-shape predicates; bundling them into a struct \
              would only relocate the same fields at the single call site"
)]
#[allow(
    clippy::too_many_lines,
    reason = "one linear pipeline — carrier closure, escape scan, chain walk, \
              after-escape liveness guard, per-level discharge — whose \
              fail-closed ordering the soundness argument depends on; splitting \
              it would scatter the shared carrier/escape state"
)]
#[allow(
    clippy::similar_names,
    reason = "`escapee` (the escaping alias) and `escapes` (the collected escape \
              events) are the domain terms; renaming either obscures the walk"
)]
fn compute_escaped_chain_sibling_drops(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    root_record_ty: &HashMap<u32, ResolvedTy>,
    root_tuple_ty: &HashMap<u32, ResolvedTy>,
    alias_of: &HashMap<u32, u32>,
    tuple_alias_of: &HashMap<u32, u32>,
    alias_chain: &[(u32, u32, u32)],
    local_tys: &[ResolvedTy],
    owned_field_list: &dyn Fn(&ResolvedTy) -> Vec<(u32, ResolvedTy)>,
    owned_tuple_field_list: &dyn Fn(&ResolvedTy) -> Vec<(u32, ResolvedTy)>,
    field_dischargeable: &dyn Fn(&ResolvedTy) -> bool,
) -> Vec<(u32, usize, Vec<Instr>)> {
    // Immediate-parent map: alias_local -> (parent_local, field ordinal it reads).
    let parent_of: HashMap<u32, (u32, u32)> = alias_chain
        .iter()
        .copied()
        .map(|(alias, parent, field)| (alias, (parent, field)))
        .collect();
    if parent_of.is_empty() {
        return Vec::new();
    }

    // Forward whole-value-`Move` closure of the chain alias binding locals: every
    // slot a chain alias value flows into (`let l2 = leaf`) carries the same
    // escapee identity, so a later escape of the copy is still attributed to the
    // recorded alias — and, symmetrically, a `Move` INTO a carrier is a benign
    // hand-off, never an escape (so the escape scan below can never disagree with
    // the prover's `field_binders`-benign move rule).
    let mut carrier_of: HashMap<u32, u32> = parent_of.keys().map(|&a| (a, a)).collect();
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        if matches!(src, Place::Local(_)) && matches!(dest, Place::Local(_)) {
                            if let Some(&alias) = carrier_of.get(&sl) {
                                if let std::collections::hash_map::Entry::Vacant(slot) =
                                    carrier_of.entry(dl)
                                {
                                    slot.insert(alias);
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Escape scan. A chain carrier read into an owning sink is an escape; an
    // interior descent (`RecordFieldLoad`/`TupleFieldLoad` reading the carrier —
    // the next hop of the chain, or the consumed-match destructure) and a
    // benign whole-value hand-off into another carrier/alias member are not.
    // Any owning use this walk cannot classify bails the whole pass.
    let mut escapes: Vec<(u32, u32, usize)> = Vec::new();
    for block in blocks {
        for (idx, instr) in block.instructions.iter().enumerate() {
            match instr {
                // Interior descent read: feeds the next hop, never escapes.
                Instr::RecordFieldLoad { .. } | Instr::TupleFieldLoad { .. } => {}
                Instr::Move { dest, src } => {
                    if let Some(sl) = base_local(*src).filter(|_| matches!(src, Place::Local(_))) {
                        if let Some(&escapee) = carrier_of.get(&sl) {
                            let dest_local =
                                base_local(*dest).filter(|_| matches!(dest, Place::Local(_)));
                            let benign = dest_local.is_some_and(|dl| {
                                carrier_of.contains_key(&dl)
                                    || alias_of.contains_key(&dl)
                                    || tuple_alias_of.contains_key(&dl)
                            });
                            if !benign {
                                escapes.push((escapee, block.id, idx));
                            }
                        }
                    }
                }
                Instr::RecordInit { fields, .. } => {
                    for (_, place) in fields {
                        if let Some(l) = base_local(*place) {
                            if let Some(&escapee) = carrier_of.get(&l) {
                                escapes.push((escapee, block.id, idx));
                            }
                        }
                    }
                }
                Instr::ClosureEnvInit { fields, .. } => {
                    for field in fields
                        .iter()
                        .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
                    {
                        if let Some(l) = base_local(field.src) {
                            if let Some(&escapee) = carrier_of.get(&l) {
                                escapes.push((escapee, block.id, idx));
                            }
                        }
                    }
                }
                Instr::RecordFieldStore { src, .. } => {
                    if let Some(l) = base_local(*src) {
                        if let Some(&escapee) = carrier_of.get(&l) {
                            escapes.push((escapee, block.id, idx));
                        }
                    }
                }
                other => {
                    let (reads, _) = crate::dataflow::instr_reads_writes(other);
                    for place in reads {
                        if let Some(l) = base_local(place) {
                            if carrier_of.contains_key(&l)
                                && !binder_read_is_borrow_safe_instr(other, l)
                            {
                                // An owning use of a chain alias this walk does
                                // not model: fail closed (leak-as-before).
                                return Vec::new();
                            }
                        }
                    }
                }
            }
        }
        for place in terminator_source_places(&block.terminator, suspend_kinds.get(&block.id)) {
            if let Some(l) = base_local(place) {
                if carrier_of.contains_key(&l)
                    && !binder_read_is_borrow_safe_terminator(
                        &block.terminator,
                        suspend_kinds.get(&block.id),
                        l,
                    )
                {
                    // A terminator escape has no post-escape insertion point.
                    return Vec::new();
                }
            }
        }
    }

    // Exactly one chain alias may escape, at a single instruction.
    let &[(escapee, esc_block, esc_idx)] = &escapes[..] else {
        return Vec::new();
    };

    // Walk the escapee's immediate-parent chain to its candidate root, recording
    // `(node_local, field-that-leads-to-the-next-hop)` at each level. Requires ≥2
    // byte-copy hops and a clean termination at a candidate record or tuple root.
    let mut chain_nodes: Vec<(u32, u32)> = Vec::new();
    let mut cursor = escapee;
    let mut reached_root = false;
    for _ in 0..=parent_of.len() {
        let Some(&(parent, field)) = parent_of.get(&cursor) else {
            break;
        };
        chain_nodes.push((parent, field));
        if root_record_ty.contains_key(&parent) || root_tuple_ty.contains_key(&parent) {
            reached_root = true;
            break;
        }
        cursor = parent;
    }
    if !reached_root || chain_nodes.len() < 2 {
        return Vec::new();
    }

    // No node of the chain (root, intermediate aliases, escapee carriers) may be
    // read after the escape point — discharging a sibling before a live read
    // would free a slot the read still observes. A self-reachable escape block is
    // a loop the inline-composite discharges cannot make idempotent.
    let node_locals: HashSet<u32> = chain_nodes
        .iter()
        .map(|&(node, _)| node)
        .chain(carrier_of.keys().copied())
        .collect();
    let reach = blocks_reachable_from(blocks, esc_block);
    if reach.contains(&esc_block) {
        return Vec::new();
    }
    let in_region = |block_id: u32, position: Option<usize>| -> bool {
        if block_id == esc_block {
            position.is_none_or(|i| i > esc_idx)
        } else {
            reach.contains(&block_id)
        }
    };
    for block in blocks {
        for (idx, instr) in block.instructions.iter().enumerate() {
            if block.id == esc_block && idx == esc_idx {
                // The escape instruction itself reads the escapee; that read is
                // AT the escape, not after it.
                continue;
            }
            let reads_node = instr_source_places(instr)
                .into_iter()
                .filter_map(base_local)
                .any(|l| node_locals.contains(&l));
            if reads_node && in_region(block.id, Some(idx)) {
                return Vec::new();
            }
        }
        let term_reads_node =
            terminator_source_places(&block.terminator, suspend_kinds.get(&block.id))
                .into_iter()
                .filter_map(base_local)
                .any(|l| node_locals.contains(&l));
        if term_reads_node && in_region(block.id, None) {
            return Vec::new();
        }
    }

    // Emit the per-level sibling discharges: at each chain node, every owned
    // field except the one that leads to the next (escaping) hop. The address
    // selector follows the NODE's own type — `FieldAddr::Tuple` on a tuple
    // node, `FieldAddr::Record` otherwise — so mixed record/tuple chains
    // discharge each level through the matching selector; a node shape neither
    // list recognizes contributes no discharges (leak-as-before at that level).
    let mut siblings: Vec<Instr> = Vec::new();
    for &(node_local, escaped_field) in &chain_nodes {
        let Some(node_ty) = local_tys.get(node_local as usize) else {
            continue;
        };
        let node_is_tuple = matches!(node_ty, ResolvedTy::Tuple(_));
        let owned_fields = if node_is_tuple {
            owned_tuple_field_list(node_ty)
        } else {
            owned_field_list(node_ty)
        };
        for (field_idx, field_ty) in owned_fields {
            if field_idx == escaped_field || !field_dischargeable(&field_ty) {
                continue;
            }
            let field = if node_is_tuple {
                crate::model::FieldAddr::Tuple(field_idx)
            } else {
                crate::model::FieldAddr::Record(FieldOffset(field_idx))
            };
            siblings.push(Instr::FieldDropInPlace {
                base: Place::Local(node_local),
                field,
                ty: field_ty,
            });
        }
    }
    if siblings.is_empty() {
        return Vec::new();
    }
    vec![(esc_block, esc_idx + 1, siblings)]
}
/// W5.020 — fail-closed sole-owner derivation for **heap-owning enum
/// composite** bindings (`Result<T, string>`, `Option<string>`, any user
/// `enum` whose active variant owns heap). Returns the subset of
/// `owned_locals` whose composite binding still owns its active variant's
/// heap payload at scope exit and therefore earns a tag-aware
/// `DropKind::EnumInPlace` drop.
///
/// ## Why this is separate from `derive_cow_sole_owner`
///
/// `derive_cow_sole_owner` excludes any binding whose base local is read as
/// a source operand. A matched enum composite is ALWAYS read — `match a {…}`
/// copies the scrutinee whole-value into a fresh local and then reads its
/// tag/payload via interior projections — so that derivation can never admit
/// a matched composite (it would always leak). This derivation is the
/// inverse: the composite is the *owner*, and the question is whether its
/// payload **escapes** that ownership, not whether it is read.
///
/// ## The ownership model
///
/// The enum is a single tagged-union struct stored inline in the binding's
/// alloca. The active variant's owned payload (e.g. a `string` handle) is
/// the only heap owner. The M-COW spine emits NO retain on share, so:
/// - A whole-value `Move` of the composite (the `let a = …; match a` scrutinee
///   copy, or a `let b = a` rebind) byte-copies the struct, aliasing the same
///   payload pointer into a transient/rebind local with no retain. That copy
///   never independently drops (it is not in `owned_locals`, or is the rebind
///   tail we follow), so dropping the *original* owner exactly once is correct.
/// - A `match` destructure binder (`Move { dest, src: MachineVariant{..} }`)
///   is a non-owning alias of the payload (already excluded from its own drop
///   by `derive_cow_sole_owner`'s projection-alias taint). If such a binder
///   merely reads the payload transiently, the composite still owns it and
///   must drop it. If the binder **escapes** (is read into an owning sink —
///   returned, stored into another aggregate, sent, or passed as an owning
///   call argument), then the payload's single owner has moved out and the
///   composite must NOT drop (the escapee owns it now).
///
/// ## The fail-closed rule
///
/// A composite binding `b` is admitted IFF:
///   1. `b`'s base local is not itself read as a source EXCEPT as the `src`
///      of a whole-value `Move` (the scrutinee copy / rebind hand-off), AND
///   2. no interior-payload binder transitively derived from `b`'s alias set
///      escapes into an owning sink.
///
/// Anything the prover cannot positively clear is excluded — it leaks (as
/// before W5.020) but never double-frees. The escape classifier is the same
/// exhaustive `instr_source_places` / `terminator_source_places` machinery
/// `derive_cow_sole_owner` uses, so a future alias-producing instruction
/// auto-excludes rather than silently re-opening a double-free.
#[allow(
    clippy::too_many_lines,
    reason = "the body is four sequential single-purpose passes — candidate \
              collection, whole-value alias propagation, payload-binder \
              seeding, and the escape scan — each shallow and independently \
              commented; splitting them would scatter the shared fixpoint \
              state and obscure the fail-closed ordering the passes depend on"
)]
#[allow(
    clippy::too_many_arguments,
    reason = "the drop-allow derivations take the function's MIR (blocks, \
              suspend_kinds), the binding registries, and the layout lookups \
              (record_field_orders + enum_layouts) the unified heap-ownership \
              authority needs; bundling them into a struct would only relocate \
              the same fields"
)]
pub(super) fn derive_enum_composite_drop_allowed(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    binding_scope: &HashMap<BindingId, ScopeId>,
    transient_local_scopes: &HashMap<u32, ScopeId>,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
) -> HashSet<BindingId> {
    // A local carries a heap-owning value (string/Bytes/owning aggregate or a
    // nested heap-owning enum) iff its registered type says so. Bitcopy payload
    // binders (an `i64`/`bool` extracted from an `Ok(i64)` arm) carry no owner,
    // so their "escape" is harmless and must not exclude the composite drop —
    // that over-exclusion would leak every `Result<i64, string>` whose Ok arm
    // returns the i64 (the dominant real case). Record-aware through the single
    // `ty_owns_heap` authority so a binder of nested-record type whose field
    // owns heap is correctly recognised (DIV-1).
    let local_is_heap_owning = |local: u32| -> bool {
        local_tys
            .get(local as usize)
            .is_some_and(|ty| crate::model::ty_owns_heap_mir(ty, record_field_orders, enum_layouts))
    };
    // The candidate composite locals: base locals of heap-owning enum
    // composite bindings.
    let mut candidate_local_to_binding: HashMap<u32, BindingId> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !ty_is_heap_owning_enum_composite(ty, record_field_orders, enum_layouts) {
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
    if candidate_local_to_binding.is_empty() {
        return HashSet::new();
    }

    // Alias set: each candidate's local plus every local reachable from it
    // through forward-propagated whole-value `Move { dest: Local, src: Local }`
    // copies (the scrutinee copy and rebind chain). These all byte-alias the
    // same payload pointer with no retain. The propagation is monotone: a slot
    // reachable from two distinct roots is evicted, not oscillated (#1942).
    let alias_of =
        propagate_whole_value_alias_roots(blocks, candidate_local_to_binding.keys().copied());

    // Local→ScopeId helper for the propagation scope-equality check.
    // Built from the public `binding_locals` × `binding_scope` view: each
    // HIR binding has a base `Place` (its MIR slot) and a registered
    // declaring scope. A local with no entry here is a synthetic MIR
    // temp (call-arg slot, intermediate copy register) that does not
    // outlive its surrounding statement and so can safely participate
    // in payload-binder chains without inflating the surviving-binding
    // set.
    let mut local_scope: HashMap<u32, ScopeId> = binding_locals
        .iter()
        .filter_map(|(binding, place)| {
            let local = base_local(*place)?;
            let scope = binding_scope.get(binding).copied()?;
            Some((local, scope))
        })
        .collect();
    local_scope.extend(
        transient_local_scopes
            .iter()
            .map(|(local, scope)| (*local, *scope)),
    );

    // Payload-binder set: destinations of `Move { dest, src: interior
    // projection of an alias-set local }` — the match/while-let destructure
    // binders. Each entry remembers the SOURCE binder's declaring scope so
    // the onward-propagation step (next loop) can reject moves that hand
    // the buffer to a binding in a DIFFERENT scope — which is precisely the
    // outer/surviving-local escape shape (`var carry; while { match opt {
    // Some(item) => { carry = item; ... } } }` — `carry` lives past the
    // back-edge that would drop the payload, so admitting EnumInPlace would
    // free the buffer while `carry` still aliases it). MIR temps with no
    // scope mapping inherit the source binder's scope on propagation; HIR
    // bindings whose scope does not match are filtered out and the source
    // escape scan below treats the move as an unbound-destination escape.
    let mut payload_binders: HashMap<u32, Option<ScopeId>> = HashMap::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Move { dest, src } = instr {
                if place_is_interior_projection(*src) {
                    if let Some(sl) = base_local(*src) {
                        if alias_of.contains_key(&sl) {
                            if let Some(dl) = base_local(*dest) {
                                // Only a binder that actually receives a
                                // heap-owning payload can carry the buffer out
                                // of the composite. A bitcopy payload (an i64
                                // from an `Ok(i64)` arm) is harmless to alias
                                // out — tracking it would over-exclude the
                                // composite drop and leak.
                                if local_is_heap_owning(dl) {
                                    // The binder's real declaring scope
                                    // (`None` for a synthetic transient with no
                                    // HIR scope — e.g. the nested-constructor
                                    // predicate's payload local). Carried so
                                    // onward propagation can apply the
                                    // same-scope discipline; a `None` source
                                    // propagates freely (it never survives a
                                    // back-edge).
                                    payload_binders
                                        .entry(dl)
                                        .or_insert_with(|| local_scope.get(&dl).copied());
                                }
                            }
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
                        let Some(&src_scope) = payload_binders.get(&sl) else {
                            continue;
                        };
                        if payload_binders.contains_key(&dl) {
                            continue;
                        }
                        // Only a heap-owning destination can carry the payload
                        // buffer onward. A bitcopy destination — most notably
                        // the i64 discriminant a nested-constructor predicate
                        // loads via `Move { dest, src: EnumTag(binder) }` — holds
                        // no heap reference, so tainting it as a payload binder
                        // is wrong: its later bitcopy reads (the `IntCmp` tag
                        // dispatch) would be misread as payload escapes and
                        // exclude the parent composite from its `EnumInPlace`
                        // drop (the W5.020 nested-payload leak). Mirrors the
                        // `local_is_heap_owning` guard the seeding loop applies.
                        if !local_is_heap_owning(dl) {
                            continue;
                        }
                        // Same-scope discipline. A `Move` into a HIR binding
                        // is only a benign onward hand-off when the binding
                        // is declared in the SAME scope as the originating
                        // destructure binder — i.e. the binding closes (and
                        // its drop fires) before the surrounding loop's
                        // back-edge re-enters and overwrites the source
                        // composite's slot. A different-scope binding lives
                        // past that back-edge and the EnumInPlace would
                        // free its buffer while it still aliased the
                        // pointer (use-after-free). MIR temps (no entry in
                        // `local_scope`) have no own lifetime longer than
                        // the statement that produced them, so they inherit
                        // the source scope and propagate freely. A `None`
                        // SOURCE scope is a synthetic transient (the nested
                        // predicate's payload local) that likewise never
                        // survives a back-edge, so its hand-off to an inner
                        // same-arm binder propagates freely too.
                        let propagate = match (src_scope, local_scope.get(&dl).copied()) {
                            (None, _) | (_, None) => true,
                            (Some(s), Some(d)) => s == d,
                        };
                        if propagate {
                            // Carry `dl`'s own scope onward when it has one, so
                            // a further hand-off out of `dl` is judged against
                            // `dl`'s real lifetime; fall back to the source's
                            // scope for MIR temps.
                            payload_binders.insert(dl, local_scope.get(&dl).copied().or(src_scope));
                            changed = true;
                        }
                    }
                }
                // Nested-record/tuple field binders. When the active payload is
                // itself a record/tuple (`Some(r)` where `r: Row`), a heap-owning
                // field loaded out of it (`r.name`) is part of the payload the
                // composite still owns. Seed the loaded field as a payload binder
                // so its ONWARD escape (Move into a surviving outer binding, store,
                // return, owning call) still excludes the composite — while the
                // interior LOAD itself is exempted from the escape scan below.
                // The loaded dest inherits the SOURCE binder's scope so the
                // same-scope discipline above governs any further hand-off out of
                // it (a field cloned into an outer-scope `carry` is a payload
                // escape, not a benign in-arm read). Only heap-owning fields are
                // seeded (a bitcopy field carries no owner). Mirrors the
                // owned-field-binder seeding in `derive_owned_record_drop_allowed`.
                if let Instr::RecordFieldLoad { record, dest, .. }
                | Instr::TupleFieldLoad {
                    tuple: record,
                    dest,
                    ..
                } = instr
                {
                    if let (Some(sl), Some(dl)) = (base_local(*record), base_local(*dest)) {
                        if let Some(&src_scope) = payload_binders.get(&sl) {
                            if local_is_heap_owning(dl) && !payload_binders.contains_key(&dl) {
                                payload_binders
                                    .insert(dl, local_scope.get(&dl).copied().or(src_scope));
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Escape scan. A composite root is excluded if:
    //   (a) any alias-set member is read into an OWNING sink (a source operand
    //       that is NOT the whole-value `Move` hand-off already folded into the
    //       alias set, and NOT a tag/interior projection read for the match) —
    //       i.e. the whole composite escaped, OR
    //   (b) any payload binder derived from this root is read as a source into
    //       an owning sink (the payload escaped out of the composite).
    //
    // For (a): the only non-escaping whole-value read of a composite local is
    // a `Move { dest: Local, src: Local(member) }` whose dest is itself an
    // alias-set member (already followed). Any other read of the *whole* local
    // (a call arg, a field store, a return) escapes the composite. Interior
    // projection reads (tag/payload) do not escape the composite — they feed
    // the payload-binder scan instead.
    let mut excluded_roots: HashSet<u32> = HashSet::new();
    let note_alias_escape = |local: u32, excluded: &mut HashSet<u32>| {
        if let Some(&root) = alias_of.get(&local) {
            excluded.insert(root);
        }
    };
    for block in blocks {
        for instr in &block.instructions {
            // COPY-IN element store (`hew_vec_push_owned` / `hew_vec_set_owned`)
            // DEEP-CLONES its element operand: an owned enum-with-collection
            // value pushed WHOLE by value is cloned into the Vec slot and the
            // SOURCE keeps its `EnumInPlace` drop (#1722 COPY-IN retain). This
            // exempts ONLY the whole-composite alias escape below; a pushed
            // PAYLOAD BINDER (a matched inner payload) still excludes its
            // composite via the payload-escape scan, because that discharge path
            // double-frees if relaxed. The MOVE variant (`hew_vec_push_owned_move`)
            // is deliberately NOT exempt — it consumes its source.
            let copy_in_elem_store = matches!(instr, Instr::CallRuntimeAbi(call)
                if crate::runtime_symbols::callee_ownership_contract(call.symbol())
                    .is_vec_copy_in_element_store());
            // Direct prover-exclusion rule for the no-temp field-addressed
            // drop, mirroring `derive_owned_record_drop_allowed` /
            // `derive_tuple_composite_drop_allowed`: a `FieldDropInPlace`
            // whose base is an alias-set member or a payload binder
            // discharges part of the composite's payload through a byte-alias
            // of ITS storage (`match opt { Some(row) => match row { Row { a,
            // b: _ } => … } }` frees `row.b` through the payload binder while
            // the composite still owns the payload). The blanket scan below
            // exempts the op (an interior discharge is not a payload READ
            // escaping into an owning sink), so without this rule the
            // composite would stay admitted and its `EnumInPlace` walk would
            // re-free the discharged field's leaves — a double-free.
            // Exclusion (a leak of the payload remainder) is the fail-closed
            // direction; attribution uses `note_payload_escape`'s every-root
            // coarsening, matching this prover's posture.
            if let Instr::FieldDropInPlace { base, .. } = instr {
                if let Some(l) = base_local(*base) {
                    if alias_of.contains_key(&l) {
                        note_alias_escape(l, &mut excluded_roots);
                    }
                    if payload_binders.contains_key(&l) {
                        note_payload_escape(
                            &payload_binders,
                            l,
                            &alias_of,
                            blocks,
                            &mut excluded_roots,
                        );
                    }
                }
            }
            // Move escapes. A `Move` is the one instruction whose `dest`
            // discriminates a benign hand-off from a real escape, so it needs
            // its own analysis rather than the blanket source scan below.
            if let Instr::Move { dest, src } = instr {
                let src_local = base_local(*src);
                let dest_local = base_local(*dest);
                // (a) Whole-composite escape: an alias-set member read as a
                //     *whole* `Place::Local` source whose dest is NOT another
                //     alias-set member (the only benign whole-value read is the
                //     scrutinee-copy / rebind hand-off we already folded into
                //     the alias set).
                if let Some(sl) = src_local {
                    let src_is_member = alias_of.contains_key(&sl)
                        && matches!(src, Place::Local(_) | Place::ReturnSlot);
                    let dest_is_member = dest_local.is_some_and(|dl| {
                        alias_of.contains_key(&dl) && matches!(dest, Place::Local(_))
                    });
                    if src_is_member && !dest_is_member {
                        note_alias_escape(sl, &mut excluded_roots);
                    }
                }
                // (b) Payload-binder escape: a heap-owning destructure binder
                //     (or a value transitively copied from one) moved into an
                //     owning sink — `ReturnSlot`, an interior aggregate
                //     projection (stored into another struct/variant), or a
                //     plain local that is NOT itself a tracked binder (the
                //     value left the binder set into general storage). A
                //     binder→binder copy is the benign onward hand-off the
                //     forward-propagation already tracks.
                if let Some(sl) = src_local {
                    // A tag read (`Move { dest, src: EnumTag(binder) }`) copies
                    // only the bitcopy discriminant ordinal out of the binder —
                    // no heap payload escapes — so it is NOT a payload escape.
                    // This is what the nested-constructor predicate lowering
                    // emits to dispatch on an inner payload's variant tag; left
                    // unexempted it wrongly excludes the parent composite from
                    // its `EnumInPlace` drop and leaks the inner payload (W5.020
                    // nested-payload leak).
                    if payload_binders.contains_key(&sl) && !place_is_tag_read(*src) {
                        let benign_handoff = dest_local
                            .is_some_and(|dl| payload_binders.contains_key(&dl))
                            && matches!(dest, Place::Local(_));
                        if !benign_handoff {
                            note_payload_escape(
                                &payload_binders,
                                sl,
                                &alias_of,
                                blocks,
                                &mut excluded_roots,
                            );
                        }
                    }
                }
            }
            // Owning-sink reads (other than Move, handled above): any source
            // operand that is a whole alias-set member or a payload binder is
            // conservatively an escape. `print`/`println` lower as
            // `Terminator::Call` (handled with the borrow exemption below), not
            // as instructions, so an instruction that reads a payload binder is
            // always an owning use here (a runtime call that consumes it, an
            // aggregate construction, an arithmetic op on a non-string — the
            // last cannot occur for a heap binder). Fail-closed: exclude.
            //
            // EXEMPT the interior projection LOADS (`RecordFieldLoad` /
            // `TupleFieldLoad`) and the interior `RecordFieldDrop`: when the
            // active payload is itself a nested record/tuple (`Some(r)` where
            // `r: Row`), loading one of its fields (`r.name`) is an INTERIOR read
            // that does not transfer ownership of the payload out of the
            // composite — exactly as the owned-record escape scan
            // (`derive_owned_record_drop_allowed`) already exempts them. Without
            // this, `match opt { Some(r) => r.name.to_upper() }` over a
            // freshly-cloned `Option<Row>` (from `rows.get(i)` → `hew_vec_get_clone`,
            // a sole owner) wrongly excluded the composite from its `EnumInPlace`
            // drop and leaked the whole `Some(Row)` payload (the #54 interior-alias
            // clone-out: the parent composite is the single owner, `r` is itself
            // interior-alias-tainted out of its own `RecordInPlace`, so the
            // `EnumInPlace` is the ONE balancing drop — no double-free). The loaded
            // field is seeded as a payload binder in the loop above, so if it
            // ESCAPES onward (Move/store/return/owning call) the composite is still
            // excluded — fail-closed.
            //
            // `FieldDropInPlace` is the same interior shape: it is a skipped
            // field's extraction AND release in ONE op, so it is not a
            // payload READ escaping into an owning sink. Its
            // composite-suppression semantics are the DIRECT exclusion rule
            // at the top of this loop (a base that is an alias member or a
            // payload binder excludes the composite — the discharge freed
            // payload leaves the `EnumInPlace` walk would otherwise re-free),
            // not this blanket scan.
            if !matches!(
                instr,
                Instr::Move { .. }
                    | Instr::Drop { .. }
                    | Instr::RecordFieldLoad { .. }
                    | Instr::TupleFieldLoad { .. }
                    | Instr::RecordFieldDrop { .. }
                    | Instr::FieldDropInPlace { .. }
            ) {
                for p in instr_source_places(instr) {
                    if let Some(l) = base_local(p) {
                        if !copy_in_elem_store
                            && alias_of.contains_key(&l)
                            && matches!(p, Place::Local(_) | Place::ReturnSlot)
                        {
                            note_alias_escape(l, &mut excluded_roots);
                        }
                        // A payload binder read as the BORROWED receiver of a
                        // collection/bytes runtime op (`b.len()` lowers
                        // `hew_bytes_len` as `Instr::CallRuntimeAbi` with the
                        // binder as arg[0]) is a transient interior borrow the
                        // composite survives — the same arg[0] exemption the
                        // record/tuple provers already apply through
                        // `binder_read_is_borrow_safe_instr` (#2429). Every
                        // other read stays a fail-closed owning escape.
                        if payload_binders.contains_key(&l)
                            && !place_is_tag_read(p)
                            && !binder_read_is_borrow_safe_instr(instr, l)
                        {
                            note_payload_escape(
                                &payload_binders,
                                l,
                                &alias_of,
                                blocks,
                                &mut excluded_roots,
                            );
                        }
                    }
                }
            }
        }
        // COPY-IN element store as a terminator (`xs.push(src)` lowers to
        // `Terminator::Call { callee: hew_vec_push_owned, .. }`): a WHOLE
        // enum-with-collection source is deep-cloned, not consumed, so it retains
        // its `EnumInPlace` drop. Exempt ONLY the whole-composite alias escape; a
        // pushed PAYLOAD BINDER still excludes its composite via the payload scan
        // below (relaxing that discharge double-frees).
        let copy_in_elem_store = matches!(
            &block.terminator,
            Terminator::Call { callee, .. }
                if crate::runtime_symbols::callee_ownership_contract(callee)
                    .is_vec_copy_in_element_store()
        );
        // Terminator reads. A return value / send / ask payload escapes; a
        // branch cond is bitcopy and harmless. A `print`/`println` call
        // BORROWS its string argument (it does not take ownership), so a
        // payload binder passed there is a transient read, NOT an escape —
        // the same borrow-only-sink exemption W5.011's
        // `retained_string_terminator_drop_safe` encodes.
        for p in terminator_source_places(&block.terminator, suspend_kinds.get(&block.id)) {
            if let Some(l) = base_local(p) {
                if !copy_in_elem_store
                    && alias_of.contains_key(&l)
                    && matches!(p, Place::Local(_) | Place::ReturnSlot)
                {
                    // A whole composite passed to a borrowing print sink is
                    // not how composites are consumed, but apply the same
                    // borrow exemption for symmetry; any non-print read of a
                    // whole composite escapes.
                    if !retained_string_terminator_drop_safe(
                        &block.terminator,
                        suspend_kinds.get(&block.id),
                        l,
                    ) {
                        note_alias_escape(l, &mut excluded_roots);
                    }
                }
                // `binder_read_is_borrow_safe_terminator` subsumes the string
                // borrow/print exemption and extends it to the collection- and
                // bytes-receiver-borrow contracts, so a `Result<bytes, _>`
                // payload binder read by `hew_bytes_to_string(b)` (a
                // `Terminator::Call` receiver borrow) no longer excludes its
                // composite (#2429). Anything not provably borrow-safe stays a
                // fail-closed payload escape.
                if payload_binders.contains_key(&l)
                    && !place_is_tag_read(p)
                    && !binder_read_is_borrow_safe_terminator(
                        &block.terminator,
                        suspend_kinds.get(&block.id),
                        l,
                    )
                {
                    note_payload_escape(
                        &payload_binders,
                        l,
                        &alias_of,
                        blocks,
                        &mut excluded_roots,
                    );
                }
            }
        }
    }

    let mut allowed = HashSet::new();
    for (&local, &binding) in &candidate_local_to_binding {
        if !excluded_roots.contains(&local) {
            allowed.insert(binding);
        }
    }
    allowed
}
/// Value-class capstone — fail-closed sole-owner derivation for owned-aggregate
/// **record** bindings passed/returned by value (RC-4 / RC-6 / G12). Returns
/// the subset of `owned_locals` whose record binding still owns its heap fields
/// at scope exit and therefore earns a tag-aware `DropKind::RecordInPlace` drop.
///
/// ## Why this mirrors `derive_enum_composite_drop_allowed`, not the simpler
/// dataflow `exit_states` allow-set
///
/// A returned record is moved whole-value into the `ReturnSlot` via
/// `Instr::Move { dest: ReturnSlot, src: Local }` at a *tail* position — the
/// dataflow's `MirStatement::Use { intent: Consume }` is NOT emitted for that
/// implicit tail/if-arm move, so the `exit_states`-only allow-set (used for
/// owned Vecs) would keep the binding and the `RecordInPlace` drop would run on
/// the return path AFTER the record escaped — a double-free of its heap fields.
/// The enum-composite escape-scan is the correct model: the record is the
/// *owner*; the question is whether it (or one of its owned fields) **escapes**.
///
/// ## The ownership model (records are simpler than enum composites)
///
/// The record is an inline struct in the binding's alloca; its owned fields
/// (string / bytes / Vec / nested owned record-or-enum) are the heap owners. The
/// M-COW spine emits no retain on share, so:
/// - A whole-value `Move` of the record (`let b = a` rebind, or the if-arm tail
///   that flows to the return) byte-copies the struct, aliasing every field
///   pointer with no retain. That alias never independently drops, so dropping
///   the original owner exactly once is correct — UNLESS the alias is what
///   escapes (returned), in which case the *escapee* owns the fields now.
/// - A `RecordFieldLoad` reads one field out. Reading a `BitCopy` field is
///   harmless. Reading an OWNED field shares its pointer with no retain; if that
///   loaded field then escapes into an owning sink, the record must NOT drop it.
///
/// ## The fail-closed rule
///
/// A record binding `b` is admitted IFF:
///   1. `b`'s base local (and its whole-value alias set) is never read as a
///      *whole-value* source into an owning sink (return, call arg, field store,
///      send) EXCEPT the benign whole-value `Move` hand-off folded into the
///      alias set, AND
///   2. no owned field loaded from `b`'s alias set escapes into an owning sink.
///
/// Anything the prover cannot positively clear is excluded — it leaks (never
/// double-frees). A `RecordFieldLoad` is an interior read (it does NOT escape
/// the whole record); it only seeds an owned-field binder for rule 2.
#[allow(
    clippy::too_many_lines,
    reason = "four sequential single-purpose passes (candidate collection, \
              whole-value alias propagation, owned-field-binder seeding, escape \
              scan) sharing fixpoint state, mirroring derive_enum_composite_\
              drop_allowed; splitting scatters the fail-closed ordering"
)]
#[allow(
    clippy::too_many_arguments,
    reason = "the function's MIR + binding registries + the candidate predicate \
              + the layout lookups (record_field_orders + enum_layouts) the \
              unified heap-ownership authority needs; a struct would only \
              relocate the same fields"
)]
pub(super) fn derive_owned_record_drop_allowed(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    is_owned_record: &dyn Fn(&ResolvedTy) -> bool,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
    alias_field_binders: &[(u32, u32)],
) -> HashSet<BindingId> {
    // A local carries a heap-owning value iff its registered type says so. Used
    // to decide whether a `RecordFieldLoad` dest is an owned-field binder (a
    // BitCopy field load is harmless to alias out).
    //
    // The single `ty_owns_heap` authority is record-aware: a bare nested
    // user-record type (`Inner { label: string, n: i64 }`) whose field owns
    // heap answers `true` because the authority consults `record_field_orders`.
    // A nested-record field loaded out of the base record and then ESCAPED into
    // an owning sink (the functional-update result's `RecordInit`, a
    // `return b.inner`, …) is marked so the base root is excluded from its
    // composite `RecordInPlace` drop — otherwise the base frees the nested
    // record's heap leaves while the escapee still owns them (use-after-free /
    // double-free). Routing through the unified authority subsumed the former
    // `|| is_owned_record(ty)` workaround the record-blind walker needed (DIV-1).
    let local_is_heap_owning = |local: u32| -> bool {
        local_tys
            .get(local as usize)
            .is_some_and(|ty| crate::model::ty_owns_heap_mir(ty, record_field_orders, enum_layouts))
    };

    // Candidate record locals: base locals of owned-aggregate-record bindings.
    let mut candidate_local_to_binding: HashMap<u32, BindingId> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !is_owned_record(ty) {
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
    if candidate_local_to_binding.is_empty() {
        return HashSet::new();
    }

    // Whole-value alias set: each candidate plus every local reachable through
    // forward-propagated whole-value `Move { dest: Local, src: Local }` copies.
    // Monotone: a slot reachable from two distinct roots is evicted (#1942).
    let alias_of =
        propagate_whole_value_alias_roots(blocks, candidate_local_to_binding.keys().copied());

    // Owned-field-binder set: destinations of `RecordFieldLoad { record: alias-
    // set member }` whose loaded field is itself heap-owning, closed forward
    // through whole-value Move so a binder copied onward is still tracked.
    let mut field_binders = collect_record_field_binders(blocks, &alias_of, &local_is_heap_owning);
    // Per-binder escape attribution (#2212): where the load scan + alias map
    // prove which root a binder came from, its escape excludes exactly that
    // root; unprovable provenance keeps the blanket every-root exclusion.
    let mut binder_provenance =
        attribute_field_binder_provenance(blocks, &alias_of, &field_binders);

    // Fold the recorded byte-copy interior-alias binders (the `let mid = o.mid;
    // let leaf = mid.leaf` projection chain) into the field-binder set,
    // attributed to the OWNER root their carried provenance chains to. The
    // whole-value alias map + the `RecordFieldLoad` scan above only reach a ONE-
    // hop alias (`mid` reads the record root directly); a deeper alias (`leaf`
    // reads `mid`, not the root) is invisible to them, so its ESCAPE would leave
    // the owner admitted to free a subtree the escapee handed to the caller (the
    // #2375 double-free). Recording it here excludes exactly the owner when the
    // deep alias escapes, and — because a `RecordFieldLoad` reading the alias is
    // interior (exempt) — leaves the owner admitted when the alias is only read
    // interiorly (the consumed-match path). `or_insert` never overrides the
    // precise `Unique { root, field }` a one-hop binder already earned (the
    // sibling-discharge emitter needs that field).
    let folded_aliases = close_alias_binders_forward(blocks, alias_field_binders);
    for (&alias_local, &owner_local) in &folded_aliases {
        if !candidate_local_to_binding.contains_key(&owner_local) {
            continue;
        }
        field_binders.insert(alias_local);
        binder_provenance
            .entry(alias_local)
            .or_insert(FieldBinderProvenance::RootOnly { root: owner_local });
    }
    let retained_bytes_field_seeds: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| block.instructions.windows(2))
        .filter_map(|pair| {
            let dest = bytes_interior_producer_dest(&pair[0], local_tys)?;
            matches!(pair[1], Instr::BytesRetain { value } if value == dest)
                .then(|| base_local(dest))
                .flatten()
        })
        .collect();
    let retained_bytes_field_aliases: HashSet<u32> =
        propagate_whole_value_alias_roots(blocks, retained_bytes_field_seeds.iter().copied())
            .into_keys()
            .collect();
    field_binders.retain(|local| !retained_bytes_field_aliases.contains(local));
    binder_provenance.retain(|local, _| !retained_bytes_field_aliases.contains(local));

    // #2384 — match-bound hop aliases: a chain hop bound out of a `match`
    // destructure (a byte-copy aggregate loaded off the scrutinee COPY of a
    // recorded alias) is still an alias of the OWNER root's storage. The
    // `Move`-only closure above cannot see through the destructure's field
    // load, so such a binder's ESCAPE left the owner admitted to re-free the
    // subtree the escapee handed to the caller (the `free_cstring` sentinel
    // double-free). Fold the gated field-load descent into the ESCAPE arms
    // only — deliberately NOT into `field_binders`, whose
    // `release_owner_bases` Defect-1 blanket below would newly exclude owners
    // in today-clean NON-escaping match shapes (the binder is an owned
    // binding, so the blanket would always trip and leak the whole tree).
    let candidate_owned_aliases: HashMap<u32, u32> = folded_aliases
        .iter()
        .filter(|(_, owner)| candidate_local_to_binding.contains_key(owner))
        .map(|(&alias, &owner)| (alias, owner))
        .collect();
    let dest_is_byte_copy_aggregate = |local: u32| -> bool {
        local_is_byte_copy_aggregate(local, local_tys, record_field_orders, enum_layouts)
    };
    let mut match_hop_binders = descend_match_bound_hop_aliases(
        blocks,
        &candidate_owned_aliases,
        &dest_is_byte_copy_aggregate,
    );
    match_hop_binders.retain(|binder, _| !field_binders.contains(binder));
    for (&binder, &owner_local) in &match_hop_binders {
        binder_provenance
            .entry(binder)
            .or_insert(FieldBinderProvenance::RootOnly { root: owner_local });
    }
    let is_escape_binder =
        |l: u32| field_binders.contains(&l) || match_hop_binders.contains_key(&l);

    let mut excluded_roots: HashSet<u32> = HashSet::new();

    // Interior-alias exclusion (hard double-free): a record binding bound from an
    // ALIASING container getter — `let a = xs[i]` lowers to `hew_vec_get_owned`,
    // which returns an interior pointer into the still-live `Vec<Boxed>`'s element
    // slot (NOT a fresh owner; `hew-runtime/src/vec.rs`) — does NOT solely own
    // its heap fields. The
    // parent Vec's `hew_vec_free_owned` runs the per-element record drop thunk and
    // frees each element's owned fields; admitting the alias `a` to its OWN
    // `RecordInPlace` would free the same buffer a second time. Over-exclude every
    // interior-alias-tainted candidate up front (fail-closed) so the borrow-aware
    // field-read exemption below can never re-admit an aliased element to drop.
    // This is the record analogue of the taint the COLLECTION sole-owner provers
    // already apply (`compute_collection_interior_alias_taint`).
    let interior_alias_tainted = compute_collection_interior_alias_taint(blocks);
    for &local in candidate_local_to_binding.keys() {
        if interior_alias_tainted.contains(&local) {
            excluded_roots.insert(local);
        }
    }

    // Defect 1 (hard double-free), record analogue of the tuple seed: an owned
    // field extracted out of the record into a binding with its OWN release path
    // (`let g = rec.gen`, where `g` is dropped standalone via `owned_locals`, or
    // `for n in rec.gen` where a loop-scope generator binding consumes it). The
    // extracted handle is released exactly once through that binder; the record's
    // `RecordInPlace` member-drop must NOT free the same aliased pointer a second
    // time. The escape scan below exempts a `Drop` of the binder and the for-in
    // consuming `Move`, so without this seed the record stays admitted and the
    // ctx is double-freed. Over-exclude the WHOLE record (fail-closed: leak a
    // sibling, never double-free). `release_owner_bases` = base locals of every
    // `owned_locals` binding PLUS every local that is the place of an inline
    // `Instr::Drop { drop_fn: Some(_) }` already emitted into the finalized MIR
    // (the for-in iterator / loop-scope / break-continue release).
    //
    // The spliced `hew_string_drop` for a `string` `*FieldLoad` dest
    // (`apply_nested_fresh_string_temp_drops`, the #54 clone-out balance) releases
    // a FRESH, codegen-cloned (`retain_string_field_load` → `hew_string_clone`)
    // refcount of the field — NOT the record's original `+1`. Counting it as a
    // `release_owner_base` would treat a benign borrowing READ of a record field
    // (`println(user.name)`, `user.name.len()`, a cloned record's `q.a.len()`) as
    // a field-ownership EXTRACTION and wrongly drop the whole record's
    // `RecordInPlace` — the #54 regression that leaked the record's own field
    // buffer (verified: 4 leaks / 128 bytes on a plain owned record; 128 nodes on
    // the generic-record clone fixture once the control's masking floor is
    // removed). These read-temps NEVER seed `release_owner_bases`: the record (or
    // clone) still solely owns its original `+1` and keeps its sole-owner
    // `RecordInPlace`. A NON-string field extracted into an owning binding (a
    // generator handle `let g = pair.0`, an owned nested record `let inner =
    // b.inner`) — which moves the ORIGINAL owner out and DOES need the record
    // excluded — is not a `string_field_load_producer_dest` and still seeds.
    let mut exempt_read_temp_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| string_field_load_producer_dest(instr, local_tys))
        .filter_map(base_local)
        .collect();
    exempt_read_temp_locals.extend(retained_bytes_field_aliases);
    exempt_read_temp_locals.extend(
        propagate_whole_value_alias_roots(blocks, exempt_read_temp_locals.iter().copied())
            .into_keys(),
    );
    let mut release_owner_bases: HashSet<u32> = owned_locals
        .iter()
        .filter_map(|(binding, _, _)| binding_locals.get(binding).and_then(|p| base_local(*p)))
        .filter(|local| !exempt_read_temp_locals.contains(local))
        .collect();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Drop {
                place,
                drop_fn: Some(_),
                ..
            } = instr
            {
                if let Some(l) = base_local(*place) {
                    if exempt_read_temp_locals.contains(&l) {
                        continue;
                    }
                    release_owner_bases.insert(l);
                }
            }
        }
    }
    for &binder in &field_binders {
        if release_owner_bases.contains(&binder) {
            for &root in alias_of.values() {
                excluded_roots.insert(root);
            }
            break;
        }
    }

    // Escape scan. Exclude a record root if any whole-value alias member, or any
    // owned-field binder derived from it, is read into an owning sink. A
    // `RecordFieldLoad` reading the record is an INTERIOR read (it feeds the
    // field-binder pass, never escapes the whole record), so it is exempt from
    // the whole-value escape check below.
    let note_alias_escape = |local: u32, excluded: &mut HashSet<u32>| {
        if let Some(&root) = alias_of.get(&local) {
            excluded.insert(root);
        }
    };
    // A field binder escaping means its record no longer solely owns that
    // field. When `attribute_field_binder_provenance` proves which root the
    // binder came from, exclude exactly that root — its non-escaped owned
    // sibling fields are `apply_escaped_record_sibling_field_drops`'
    // discharge obligation (#2212). A binder whose provenance is unprovable
    // excludes every record root (over-exclusion leaks; never double-frees),
    // mirroring `note_payload_escape`.
    let note_field_escape = |binder: u32, excluded: &mut HashSet<u32>| match binder_provenance
        .get(&binder)
    {
        Some(
            FieldBinderProvenance::Unique { root, .. } | FieldBinderProvenance::RootOnly { root },
        ) => {
            excluded.insert(*root);
        }
        _ => {
            for &root in alias_of.values() {
                excluded.insert(root);
            }
        }
    };
    for block in blocks {
        for instr in &block.instructions {
            // COPY-IN element store (`hew_vec_push_owned` / `hew_vec_set_owned`)
            // DEEP-CLONES its element operand: an owned record pushed WHOLE by
            // value is cloned into the Vec slot and the SOURCE keeps sole
            // ownership of its own heap fields, so it retains its scope-exit
            // `RecordInPlace` drop (#1722 `container-ingress-ownership-is-per-
            // container` COPY-IN retain, the record analogue of the exemption
            // `derive_local_collection_drop_allowed` already applies for the Vec
            // handle). This exempts ONLY the whole-record alias escape below; a
            // pushed owned FIELD BINDER (`xs.push(r.field)`) still excludes its
            // composite via the field-escape scan, because that discharge path
            // double-frees if relaxed (`vec_copy_in_tail_split...`). The MOVE
            // variant (`hew_vec_push_owned_move`) is deliberately NOT exempt — it
            // consumes its source, which must then be excluded.
            let copy_in_elem_store = matches!(instr, Instr::CallRuntimeAbi(call)
                if crate::runtime_symbols::callee_ownership_contract(call.symbol())
                    .is_vec_copy_in_element_store());
            // Direct prover-exclusion rule for the no-temp field-addressed
            // drop: a `FieldDropInPlace` whose base is an alias-set member is
            // by construction BOTH the field extraction AND that field's
            // release — the combined role `field_binders ∩
            // release_owner_bases` plays for the load+drop leaf path. It
            // mints no load dest and no `Drop` place, so with bitcopy-only
            // sibling binders it seeds NEITHER set; without this rule the
            // composite would stay admitted and its `RecordInPlace` would
            // re-walk the freed field's leaves (double-free — inline
            // composites carry no null-store to short-circuit it). Exclude
            // the root directly; the remaining owned siblings are the
            // emitter's discharge obligation.
            //
            // A base that is a field BINDER (an extracted member alias —
            // `let inner = outer.field; match inner { … }`) discharges a
            // field of the OUTER root's storage through the binder's
            // byte-alias: the null-store (when the field shape has one)
            // lands in the binder's slot, never the root's, so the root's
            // composite walk would re-free the discharged field's leaves.
            // Resolve the binder through its provenance and exclude the root
            // it was loaded from (every record root when unprovable) — the
            // promise the blanket-scan exemption below relies on.
            if let Instr::FieldDropInPlace { base, .. } = instr {
                if let Some(l) = base_local(*base) {
                    if alias_of.contains_key(&l) {
                        note_alias_escape(l, &mut excluded_roots);
                    }
                    if is_escape_binder(l) {
                        note_field_escape(l, &mut excluded_roots);
                    }
                }
            }
            // A `Move` discriminates a benign whole-value hand-off (dest is
            // another alias member) from a real whole-record escape.
            if let Instr::Move { dest, src } = instr {
                let src_local = base_local(*src);
                let dest_local = base_local(*dest);
                if let Some(sl) = src_local {
                    let src_is_member = alias_of.contains_key(&sl)
                        && matches!(src, Place::Local(_) | Place::ReturnSlot);
                    let dest_is_member = dest_local.is_some_and(|dl| {
                        alias_of.contains_key(&dl) && matches!(dest, Place::Local(_))
                    });
                    // ReturnSlot dest is never an alias member, so a
                    // Move { dest: ReturnSlot, src: member } excludes the root.
                    if src_is_member && !dest_is_member {
                        note_alias_escape(sl, &mut excluded_roots);
                    }
                    if is_escape_binder(sl) {
                        let benign = dest_local.is_some_and(is_escape_binder)
                            && matches!(dest, Place::Local(_));
                        if !benign {
                            note_field_escape(sl, &mut excluded_roots);
                        }
                    }
                }
            }
            // Non-Move owning-sink reads. A `RecordFieldLoad` reading the record
            // base is interior (exempt); every other instruction reading a whole
            // alias member or a field binder is an escape. Fail-closed.
            //
            // Exception: a field binder read ONLY as the borrowed receiver
            // (arg[0]) of a collection-borrow call (`hew_vec_len` / `xs[i]` /
            // `m.get(k)` lowered as `Instr::CallRuntimeAbi`) is an interior
            // borrow, not an escape — `binder_read_is_borrow_safe_instr` clears
            // it, mirroring the borrowing-string exemption and the
            // collection-LOCAL scan's arg[0] rule (one receiver-borrow authority,
            // DI-017).
            if !matches!(
                instr,
                Instr::Move { .. }
                    | Instr::Drop { .. }
                    | Instr::RecordFieldLoad { .. }
                    // `RecordFieldDrop` is an interior operation (GEP+drop on one
                    // field slot) and does not transfer ownership of the whole
                    // record out of the scope, so it must not count as an escape
                    // of the record root. It is emitted by functional-update
                    // lowering to release overridden fields; the surrounding record
                    // binding is still live (and should receive its composite drop).
                    | Instr::RecordFieldDrop { .. }
                    // `FieldDropInPlace` is the same interior field-op shape (uses
                    // base, no dest, no alias); its composite-suppression semantics
                    // are the DIRECT exclusion rule at the top of this loop, not
                    // this blanket owning-sink scan (which would also misread a
                    // base that is a field BINDER as a whole-set field escape).
                    | Instr::FieldDropInPlace { .. }
                    // `RecordCloneInplace` (`let q = clone p`) reads `src` (`p`) as
                    // a DEEP copy: it `memcpy`s then deep-clones each owned field
                    // into `dest`, leaving `src` in sole ownership of its own
                    // original buffers (`lower_record_clone_inplace_instr`,
                    // `hew-codegen-rs/src/llvm.rs`). So reading the record as a clone
                    // source is NOT an ownership escape — `p` must keep its
                    // `RecordInPlace` drop, exactly as an interior `RecordFieldLoad`
                    // does. The fresh `dest` is a write, not a source-read, and
                    // earns its OWN independent `RecordInPlace` (the deep clone is a
                    // disjoint owner). Without this exemption the clone source leaks
                    // its original field buffers (the #54 regression, masked at the
                    // tip because the no-clone control leaked the identical floor).
                    | Instr::RecordCloneInplace { .. }
            ) {
                for p in instr_source_places(instr) {
                    if let Some(l) = base_local(p) {
                        if !copy_in_elem_store
                            && alias_of.contains_key(&l)
                            && matches!(p, Place::Local(_) | Place::ReturnSlot)
                        {
                            note_alias_escape(l, &mut excluded_roots);
                        }
                        if is_escape_binder(l) && !binder_read_is_borrow_safe_instr(instr, l) {
                            note_field_escape(l, &mut excluded_roots);
                        }
                    }
                }
            }
        }
        // COPY-IN element store as a terminator (`xs.push(src)` lowers to
        // `Terminator::Call { callee: hew_vec_push_owned, args: [xs, src] }`):
        // a WHOLE owned record source is deep-cloned, not consumed, so it retains
        // its `RecordInPlace` drop and the borrowed receiver is not a candidate
        // record. Exempt ONLY the whole-record alias escape below — a pushed
        // FIELD BINDER (`xs.push(r.field)`) still excludes its composite via the
        // field-escape scan (relaxing that discharge double-frees). This closes
        // the copy-in `.push(src)` leak of `src`'s owned collection field.
        let copy_in_elem_store = matches!(
            &block.terminator,
            Terminator::Call { callee, .. }
                if crate::runtime_symbols::callee_ownership_contract(callee)
                    .is_vec_copy_in_element_store()
        );
        // Terminator reads. A return / send / ask of a whole record or an owned
        // field binder escapes. `print`/`println` borrows its string arg, and a
        // collection-borrow call (`.len()` / `.get(i)`) borrows its receiver, so
        // a field binder read only as such a borrowed arg is a transient read,
        // not an escape (`binder_read_is_borrow_safe_terminator`).
        for p in terminator_source_places(&block.terminator, suspend_kinds.get(&block.id)) {
            if let Some(l) = base_local(p) {
                if !copy_in_elem_store
                    && alias_of.contains_key(&l)
                    && matches!(p, Place::Local(_) | Place::ReturnSlot)
                    && !binder_read_is_borrow_safe_terminator(
                        &block.terminator,
                        suspend_kinds.get(&block.id),
                        l,
                    )
                {
                    note_alias_escape(l, &mut excluded_roots);
                }
                if is_escape_binder(l)
                    && !binder_read_is_borrow_safe_terminator(
                        &block.terminator,
                        suspend_kinds.get(&block.id),
                        l,
                    )
                {
                    note_field_escape(l, &mut excluded_roots);
                }
            }
        }
    }

    let mut allowed = HashSet::new();
    for (&local, &binding) in &candidate_local_to_binding {
        if !excluded_roots.contains(&local) {
            allowed.insert(binding);
        }
    }
    allowed
}
/// Fail-closed sole-owner derivation for **local `HashMap` / `HashSet`** handle
/// bindings. Returns the subset of `owned_locals` whose collection handle is
/// proven to still be solely owned by this scope at every exit, and therefore
/// earns a `DropKind::CowHeap` scope-exit drop (`hew_hashmap_free_layout` /
/// `hew_hashset_free_layout`).
///
/// WHY a dedicated escape-scan and not the existing authorities:
///   - `derive_cow_sole_owner` excludes ANY binding read as a source operand.
///     Every useful map op (`m.insert(..)`, `m.get(..)`) reads the handle as
///     the call receiver (arg[0]), so that scan would exclude every non-trivial
///     map and the leak would persist. It cannot be reused.
///   - A dataflow-only gate (the `owned_vec_drop_allowed` shape) is unsafe here:
///     a handle moved into an actor's initial state record (`spawn A(f: m)`
///     lowers to `RecordInit` → `SpawnActor`) is `AliasedIntoAggregate`, which
///     `enumerate_exits`/`drops_for_exit` treats as Live (the drop fires) — so a
///     spawn-moved map would be freed here AND again by the actor's synthesised
///     `state_drop_fn`: a double free. The escape-scan is essential because it
///     removes the escaper from the LIFO entirely, before the per-exit Live
///     filter ever sees it.
///
/// The structure mirrors `derive_owned_record_drop_allowed` but is simpler: a
/// collection handle is a LEAF (a single pointer), so there is no owned-field
/// binder pass and no interior `RecordFieldLoad` read to exempt. The one
/// interior (non-escape) read is the call RECEIVER (arg[0]) of a
/// receiver-borrowing collection op:
/// those calls borrow the handle in place and never free it, so arg[0] is a
/// transient read, not an ownership escape. Every other read — a `Move` to a
/// non-member slot / `ReturnSlot`, a `RecordInit` / `SpawnActor` / closure-env
/// capture, an actor `Send` / `Ask`, a return — is an escape and excludes the
/// root. A handle the prover does not positively clear LEAKS (as before this
/// fix); it never double-frees. The default for any binding the prover did not
/// clear is exclusion, so an un-enumerated future alias/escape producer cannot
/// re-open a double-free.
///
/// SUBSTRATE INVARIANT (why one unconditional free per cleared binding is sound
/// — current M-COW spine). A collection handle is `ValueClass::CowValue` by
/// type, but every *share* of the handle lowers as a MOVE, not a refcounted
/// retain:
///   - `let m2 = m;` lowers to a bare `Move { dest, src }` (pointer bitcopy, NO
///     retain) and the move-checker CONSUMES the source — a later use of `m` is a
///     hard `E_MIR_CHECK` "used after an owned value move";
///   - pass-by-value / `spawn A(f: m)` byte-copies the handle bits into the
///     callee / actor state (`deep_copy_state` is `ptr::copy_nonoverlapping`, a
///     shallow memcpy, NOT a deep clone) and likewise consumes the source;
///   - `hew_hashmap_free_layout` / `hew_hashset_free_layout` is an UNCONDITIONAL
///     dealloc (`drop_in_place` + `dealloc`; set: inner-map free + `libc::free`),
///     NOT a refcount decrement — the handle carries no refcount.
///
/// The M-COW spine emits NO retain on share (the invariant repeated through this
/// file; `model.rs`: "Until the M-COW spine emits retain-on-share"). Net: at
/// every program point EXACTLY ONE live binding owns a given allocation, so this
/// scan frees that one owner once — or excludes it when ownership escaped — and
/// never double-frees. The only would-be second owner, surface `.clone()`, is
/// fail-closed at HIR until M-COW P2 (`hew-hir/src/lower.rs`).
///
/// REVISIT TRIGGER. When M-COW grows retain-on-share for collection handles, a
/// share stops consuming its source and TWO live bindings can own one
/// allocation. This allow-set, the alias-root exclusion below, and the
/// unconditional `*_free_layout` in `drop_kind_for` must then change in lockstep
/// with the sibling string / record / tuple drop paths (which encode the same
/// move-only assumption): the free must become refcount-aware and each retained
/// binding must release once. Until then refcounted COW is deliberately NOT
/// implemented here, and assigned copies are deliberately NOT converted to
/// no-drop (that would leak, never double-free).
///
/// LESSONS: `drop-allowset-from-value-flow`, `boundary-fail-closed`,
/// `container-ingress-ownership-is-per-container`, `cleanup-all-exits`,
/// `raii-null-after-move`.
#[allow(
    clippy::too_many_lines,
    reason = "three sequential single-purpose passes (candidate collection, \
              whole-value alias propagation, escape scan) sharing fixpoint \
              state, mirroring derive_owned_record_drop_allowed; splitting \
              scatters the fail-closed ordering the escape scan depends on"
)]
pub(super) fn derive_local_collection_drop_allowed(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    proven_borrow_call_args: &HashMap<u32, HashSet<usize>>,
    ty_filter: impl Fn(&ResolvedTy) -> bool,
) -> HashSet<BindingId> {
    // Candidate collection locals: base locals of owned handle bindings of
    // the caller-selected type class (HashMap/HashSet handles, or
    // closure-pair Vec handles — the escape model is identical: the handle
    // is the owner, receiver-borrowing collection ops are interior reads).
    let mut candidate_local_to_binding: HashMap<u32, BindingId> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !ty_filter(ty) {
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
    if candidate_local_to_binding.is_empty() {
        return HashSet::new();
    }

    // Whole-value alias set: each candidate plus every local reachable through
    // forward-propagated whole-value `Move { dest: Local, src: Local }` copies
    // (a rebind `let m2 = m;` hands the same handle to a new slot). The
    // constructor temp → binding-slot `Move` does NOT extend the set: its src is
    // the constructor temp, which is not a candidate, so it never seeds a root.
    // Monotone: a slot reachable from two distinct roots (two match arms each
    // moving a fresh handle into the shared result slot) is evicted, not
    // oscillated — the #1942 compile hang. The evicted slot reads as a non-member
    // dest in the escape scan below, so a `Move` into it EXCLUDES the source's
    // root (fail-closed: the ambiguous group leaks, never double-frees).
    let alias_of =
        propagate_whole_value_alias_roots(blocks, candidate_local_to_binding.keys().copied());

    let mut excluded_roots: HashSet<u32> = HashSet::new();
    let note_escape = |local: u32, excluded: &mut HashSet<u32>| {
        if let Some(&root) = alias_of.get(&local) {
            excluded.insert(root);
        }
    };

    for block in blocks {
        for instr in &block.instructions {
            // A `Move` discriminates a benign whole-value hand-off (dest is
            // another alias member — already folded into the alias set) from a
            // real escape (dest is a non-member local or the `ReturnSlot`, which
            // is never an alias member).
            if let Instr::Move { dest, src } = instr {
                if let Some(sl) = base_local(*src) {
                    let src_is_member = alias_of.contains_key(&sl)
                        && matches!(src, Place::Local(_) | Place::ReturnSlot);
                    let dest_is_member = base_local(*dest).is_some_and(|dl| {
                        alias_of.contains_key(&dl) && matches!(dest, Place::Local(_))
                    });
                    if src_is_member && !dest_is_member {
                        note_escape(sl, &mut excluded_roots);
                    }
                }
                continue;
            }
            // A `Drop` of the handle is its own release path (the for-in /
            // loop-scope / break-continue inline release already emitted into the
            // finalized MIR), never an escape — exempt so the scope-exit drop is
            // not double-counted against an inline release.
            if matches!(instr, Instr::Drop { .. }) {
                continue;
            }
            // A receiver-borrowing Vec op lowered as an INSTRUCTION (the
            // `xs[i]` bounds-check + getter path emits `hew_vec_len` /
            // `hew_vec_get_ptr` as `Instr::CallRuntimeAbi`, not a
            // terminator) borrows the handle as arg 0 — an interior read,
            // not an escape. Scan only the by-value operand tail, mirroring
            // the terminator-side receiver-borrow rule below.
            if let Instr::CallRuntimeAbi(call) = instr {
                // `hew_vec_push_owned` / `hew_vec_set_owned` DEEP-CLONE their
                // element operand (COPY-IN): neither the borrowed receiver nor the
                // cloned element source escapes the candidate, so the source keeps
                // its own scope-exit release (#1722 — `container-ingress-ownership-
                // is-per-container` COPY-IN retain). Skip the whole call.
                let contract = crate::runtime_symbols::callee_ownership_contract(call.symbol());
                if contract.is_vec_copy_in_element_store() {
                    continue;
                }
                if contract.borrows_vec_receiver() {
                    for p in call.args().iter().skip(1) {
                        if let Some(l) = base_local(*p) {
                            if alias_of.contains_key(&l)
                                && matches!(p, Place::Local(_) | Place::ReturnSlot)
                            {
                                note_escape(l, &mut excluded_roots);
                            }
                        }
                    }
                    continue;
                }
            }
            // The synthetic `for x in vec` cursor `record_init VecIter { vec, idx }`
            // BORROWS the source handle in its `vec` field — the loop only READS
            // it (`len()` / `get(i)`) and the cursor never frees what it borrows
            // (see `vec_iter_record_init_vec_source`). For a place source this is
            // a CowShare: the source binding stays the sole owner and keeps its
            // own scope-exit drop. So the `vec`-field source is NOT an escape;
            // skip it here. The actual owner-vs-borrower decision for the cursor
            // itself is made by `derive_vec_iter_drop_allowed` (a `Capture`-source
            // cursor borrows and does not drop; an rvalue / consumed-source cursor
            // owns and drops). A consumed place source (`v.into_iter()`) is still
            // removed by the dataflow `Consumed` filter, so exempting the read
            // here never re-admits a genuinely moved-out source.
            let vec_iter_borrow_src = vec_iter_record_init_vec_source(instr);
            // Every other instruction reading an alias member is an owning-sink
            // escape (stored into a record field, captured into a closure env,
            // moved into an aggregate via `RecordInit` / `SpawnActor`, …).
            // Fail-closed: a leaf handle has no benign interior instruction read.
            for p in instr_source_places(instr) {
                if vec_iter_borrow_src == Some(p) {
                    continue;
                }
                if let Some(l) = base_local(p) {
                    if alias_of.contains_key(&l) && matches!(p, Place::Local(_) | Place::ReturnSlot)
                    {
                        note_escape(l, &mut excluded_roots);
                    }
                }
            }
        }
        // Terminator reads. A receiver-borrowing collection op
        // (`hew_hashmap_insert_layout`, `hew_hashset_contains_layout`, …) reads
        // the handle as arg[0] but only borrows it — that is an interior read,
        // not an escape — so scan only arg[1..] (the by-value key / element
        // operands, which genuinely flow elsewhere). Every other terminator
        // (a different `Call`, an actor `Send` / `Ask`, a return moving the
        // handle to the `ReturnSlot`, …) is scanned in full: a member read there
        // is an escape.
        match &block.terminator {
            // COPY-IN element store (`hew_vec_push_owned` / `hew_vec_set_owned`):
            // the element operand is deep-cloned into the slot, so no operand
            // escapes a collection candidate — the source retains sole ownership
            // and keeps its own scope-exit drop (#1722 COPY-IN retain). Scan
            // nothing (must precede the receiver-borrow arm, which would still
            // scan the by-value element operand as an escape).
            Terminator::Call { callee, .. }
                if crate::runtime_symbols::callee_ownership_contract(callee)
                    .is_vec_copy_in_element_store() => {}
            // A direct user-function call borrows only the argument positions
            // proven by the module parameter-body summary. Scan every other
            // position as an escape, preserving the fail-closed behavior for a
            // helper that returns, stores, sends, or otherwise forwards the Vec.
            Terminator::Call { args, .. } if proven_borrow_call_args.contains_key(&block.id) => {
                let borrowed = &proven_borrow_call_args[&block.id];
                for (index, p) in args.iter().enumerate() {
                    if borrowed.contains(&index) {
                        continue;
                    }
                    if let Some(l) = base_local(*p) {
                        if alias_of.contains_key(&l)
                            && matches!(p, Place::Local(_) | Place::ReturnSlot)
                        {
                            note_escape(l, &mut excluded_roots);
                        }
                    }
                }
            }
            Terminator::Call { callee, args, .. }
                if {
                    let contract = crate::runtime_symbols::callee_ownership_contract(callee);
                    contract.borrows_vec_receiver() || contract.borrows_collection_receiver()
                } =>
            {
                for p in args.iter().skip(1) {
                    if let Some(l) = base_local(*p) {
                        if alias_of.contains_key(&l)
                            && matches!(p, Place::Local(_) | Place::ReturnSlot)
                        {
                            note_escape(l, &mut excluded_roots);
                        }
                    }
                }
            }
            other => {
                for p in terminator_source_places(other, suspend_kinds.get(&block.id)) {
                    if let Some(l) = base_local(p) {
                        if alias_of.contains_key(&l)
                            && matches!(p, Place::Local(_) | Place::ReturnSlot)
                        {
                            note_escape(l, &mut excluded_roots);
                        }
                    }
                }
            }
        }
    }

    // Interior-alias INGRESS exclusion (fail-closed). The escape scan above
    // catches a handle aliased OUT of its slot (read as an owning sink); it
    // does NOT catch a candidate whose slot received an interior pointer of a
    // still-live aggregate in the first place. Such a handle is never a sole
    // owner — the parent aggregate's own drop path owns the release — so a
    // scope-exit free here double-frees (the csv `Table::get`
    // `let r = data.get(row)` UAF, where `r` is `hew_vec_get_ptr(data, row)`,
    // a borrow of `data`'s element slot). `compute_collection_interior_alias_
    // taint` seeds the record/tuple/closure-env/actor-state field loads and
    // enum/machine payload destructures shared with the string/bytes provers,
    // PLUS the owned-element vector getters (`hew_vec_get_ptr` /
    // `hew_vec_get_owned`) those leaf provers do not need, and propagates the
    // taint forward through whole-value `Move`. Over-tainting only
    // over-excludes (leak), never double-frees.
    let interior_alias = compute_collection_interior_alias_taint(blocks);

    let mut allowed = HashSet::new();
    for (&local, &binding) in &candidate_local_to_binding {
        // A candidate holding an interior pointer of a still-live aggregate is
        // a borrow, not a sole owner — exclude it before any admit (see the
        // `interior_alias` rationale above).
        if interior_alias.contains(&local) {
            continue;
        }
        // Resolve the candidate to its whole-value alias ROOT before testing
        // exclusion. Escapes are recorded by ROOT (`note_escape` inserts
        // `alias_of[local]`), so a candidate that is itself an alias member —
        // `let b = a;` makes `b`'s root `a`, and `b` is never its own root — is
        // NEVER in `excluded_roots` under its own local even when its root `a`
        // escaped. Testing the candidate local directly would therefore admit
        // `b` for a producer-side drop of a handle an aggregate / the caller
        // already owns (`spawn A(f: a)` / `return a`): a double free. Resolving
        // to the root excludes the WHOLE alias group when any member escapes
        // (fail-closed). A candidate that is its own root resolves to itself, so
        // the common non-aliased case is unchanged.
        //
        // CONSERVATIVE / belt-and-suspenders under the current move-only
        // substrate (see SUBSTRATE INVARIANT above): a whole-value share consumes
        // its source, so the moved-from member is already dropped as a candidate
        // (dataflow `Consumed`) before this scan runs — front-end-lowered MIR
        // cannot present two live aliased candidates, and this root-resolution
        // therefore changes no real program's admit/exclude outcome. It is
        // load-bearing only for a directly-constructed multi-candidate CFG (the
        // `..._excludes_aliased_member_when_root_escapes` unit test) and as
        // fail-closed defense should a future lowering admit an un-consumed alias
        // member. It becomes semantically load-bearing the day retain-on-share
        // lands (see REVISIT TRIGGER above).
        let root = alias_of.get(&local).copied().unwrap_or(local);
        if !excluded_roots.contains(&root) {
            allowed.insert(binding);
        }
    }
    allowed
}
/// Fail-closed retain/drop derivation for **local `bytes`** bindings. Returns
/// both the scope-exit drop allow-set and the explicit MIR retain markers that
/// mint additional owners. Codegen consumes only those markers; it never
/// independently infers bytes retains from a type-shaped LLVM value.
///
/// Structure mirrors `derive_local_collection_drop_allowed` (the default-deny
/// escape-scan precedent): candidate collection, whole-value alias propagation
/// through `Move`, then an escape scan where any read of an alias member that
/// is not positively classified as a borrow EXCLUDES the whole alias group.
/// A binding the prover does not positively clear LEAKS (as before this fix);
/// it never double-frees. Two bytes-specific differences from the collection
/// scanner:
///
/// 1. **Projection-alias taint exclusion.** A `bytes` value is a by-value
///    `BytesTriple` struct; loading it out of a still-live aggregate
///    (`RecordFieldLoad` / `TupleFieldLoad` / `ClosureEnvFieldLoad` /
///    `ActorStateFieldLoad`, or a `Move` from an enum/machine variant
///    projection) byte-copies the triple with NO refcount bump — the parent
///    aggregate still owns the same buffer. A producer whose result becomes an
///    owner is marked with `BytesRetain` and removed from the taint in lockstep;
///    a borrow-only transient stays tainted and unretained.
///
/// 2. **Bytes runtime ops are instruction-level.** Collection ops lower as
///    `Terminator::Call`; the bytes surface ops (`len`/`index`/`slice`/`push`)
///    lower as `Instr::CallRuntimeAbi`, and `to_string` reaches MIR as a
///    `Terminator::Call`. The bytes-receiver contract is therefore applied at
///    BOTH scan sites; either way only arg[0] is exempt and arg[1..] are scanned.
///
/// A bytes value consumed by an actor `Terminator::Send` / `Ask` (the mailbox
/// `memcpy` hand-off — the receive side / actor state owns the buffer from
/// then on) is excluded twice over: the send terminator reads the place (an
/// escape under this scan), and the move checker marks the binding `Consumed`,
/// which the caller's dataflow filter removes as the belt-and-suspenders net.
///
/// STAGE S1 INVARIANT: every genuine bytes co-owner mint has one explicit
/// `BytesRetain` immediately before/after the share, and every admitted owner
/// releases once. Calls borrow by value; actor sends remain consuming hand-offs.
/// The marker and allow-set are produced together here, so a codegen-local type
/// check cannot over-retain borrow-only temporaries.
///
/// LESSONS: `drop-allowset-from-value-flow`, `boundary-fail-closed`,
/// `cleanup-all-exits`, `raii-null-after-move`.
#[allow(
    clippy::too_many_lines,
    reason = "three sequential single-purpose passes (candidate collection, \
              whole-value alias propagation, escape scan) sharing fixpoint \
              state, mirroring derive_local_collection_drop_allowed; splitting \
              scatters the fail-closed ordering the escape scan depends on"
)]
pub(super) fn derive_local_bytes_drop_allowed(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    borrowed_param_locals: &HashSet<u32>,
) -> BytesDropDerivation {
    // Candidate bytes locals: base locals of owned `bytes` bindings.
    let mut candidate_local_to_binding: HashMap<u32, BindingId> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !matches!(ty, ResolvedTy::Bytes) {
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
    let candidate_locals: HashSet<u32> = candidate_local_to_binding.keys().copied().collect();

    // Interior producers start as raw aliases. They become retained fresh owners
    // only when their result is bound to an owned bytes local, dropped inline, or
    // sent to an owning sink/return. Borrow-only receiver temps have none of
    // those uses and therefore remain unretained.
    let mut producer_sites: HashMap<u32, (u32, usize, Place)> = HashMap::new();
    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            if let Some(dest) = bytes_interior_producer_dest(instr, local_tys) {
                if let Some(local) = base_local(dest) {
                    producer_sites.insert(local, (block.id, instr_index, dest));
                }
            }
        }
    }
    let producer_alias_of =
        propagate_whole_value_alias_roots(blocks, producer_sites.keys().copied());
    let mut retained_producer_roots: HashSet<u32> = candidate_locals
        .iter()
        .filter_map(|local| producer_alias_of.get(local).copied())
        .collect();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Move { dest, src } = instr {
                if matches!(dest, Place::ReturnSlot) {
                    if let Some(root) =
                        base_local(*src).and_then(|local| producer_alias_of.get(&local).copied())
                    {
                        retained_producer_roots.insert(root);
                    }
                }
                continue;
            }
            if let Instr::CallRuntimeAbi(call) = instr {
                for (arg_index, place) in call.args().iter().enumerate() {
                    let Some(root) =
                        base_local(*place).and_then(|local| producer_alias_of.get(&local).copied())
                    else {
                        continue;
                    };
                    if !bytes_runtime_arg_is_borrow(call, arg_index) {
                        retained_producer_roots.insert(root);
                    }
                }
                continue;
            }
            // An inline drop of a projection temp is the existing destructive
            // release of the parent's original reference (match wildcard /
            // field-discard path), not a new co-owner. Retaining it would turn
            // the drop into a no-op and leak the consumed parent's field.
            if matches!(instr, Instr::Drop { .. }) {
                continue;
            }
            for place in instr_source_places(instr) {
                if let Some(root) =
                    base_local(place).and_then(|local| producer_alias_of.get(&local).copied())
                {
                    retained_producer_roots.insert(root);
                }
            }
        }
        if !matches!(block.terminator, Terminator::Call { .. }) {
            for place in terminator_source_places(&block.terminator, suspend_kinds.get(&block.id)) {
                if let Some(root) =
                    base_local(place).and_then(|local| producer_alias_of.get(&local).copied())
                {
                    retained_producer_roots.insert(root);
                }
            }
        }
    }
    let retained_producer_aliases: HashSet<u32> = producer_alias_of
        .iter()
        .filter_map(|(&local, &root)| retained_producer_roots.contains(&root).then_some(local))
        .collect();

    // Projection-alias taint: a candidate whose local aliases interior storage
    // of a still-live aggregate must never drop (the parent's drop path owns
    // the release). Conservative empty exemption set — over-tainting only
    // over-excludes (leak, never double-free). Uses the collection-aware taint
    // so a `bytes` triple borrowed out of a `Vec<bytes>` slot
    // (`hew_vec_get_ptr` / `hew_vec_get_owned`) is excluded too, not only the
    // record/tuple/closure-env/actor-state field-load aliases.
    let mut tainted = compute_collection_interior_alias_taint(blocks);
    tainted.retain(|local| !retained_producer_aliases.contains(local));

    // Whole-value alias set: each candidate plus every local reachable through
    // forward-propagated whole-value `Move { dest: Local, src: Local }` copies.
    // Monotone: a slot reachable from two distinct roots is evicted (#1942).
    let alias_of =
        propagate_whole_value_alias_roots(blocks, candidate_local_to_binding.keys().copied());
    let borrowed_alias_of =
        propagate_whole_value_alias_roots(blocks, borrowed_param_locals.iter().copied());

    let mut excluded_roots: HashSet<u32> = HashSet::new();
    let mut pending_share_sites: Vec<(u32, usize, Place, Vec<BindingId>)> = Vec::new();
    let note_escape = |local: u32, excluded: &mut HashSet<u32>| {
        if let Some(&root) = alias_of.get(&local) {
            excluded.insert(root);
        }
    };
    let scan_places = |places: &[Place],
                       alias_of: &HashMap<u32, u32>,
                       excluded: &mut HashSet<u32>| {
        for p in places {
            if let Some(l) = base_local(*p) {
                if alias_of.contains_key(&l) && matches!(p, Place::Local(_) | Place::ReturnSlot) {
                    if let Some(&root) = alias_of.get(&l) {
                        excluded.insert(root);
                    }
                }
            }
        }
    };

    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            // A `Move` discriminates a benign whole-value hand-off (dest is
            // another alias member — already folded into the alias set) from a
            // real escape (dest is a non-member local or the `ReturnSlot`).
            if let Instr::Move { dest, src } = instr {
                if let Some(sl) = base_local(*src) {
                    let src_is_member = alias_of.contains_key(&sl)
                        && matches!(src, Place::Local(_) | Place::ReturnSlot);
                    let dest_is_member = base_local(*dest).is_some_and(|dl| {
                        alias_of.contains_key(&dl) && matches!(dest, Place::Local(_))
                    });
                    if src_is_member && !dest_is_member {
                        note_escape(sl, &mut excluded_roots);
                    }
                }
                continue;
            }
            // A `Drop` of the triple is its own release path (an inline
            // per-iteration / destructure-discard release already emitted into
            // the finalized MIR), never an escape.
            if matches!(instr, Instr::Drop { .. }) {
                continue;
            }
            // `hew_bytes_append` reads BOTH operands as borrows: it copies the
            // source region (arg[1..=3]) into the receiver (arg[0]) and never
            // takes the source's reference, so neither binding escapes. Scan
            // nothing — both keep their scope-exit drop (a more precise
            // classification than the Vec append precedent, which over-excludes
            // arg[1] for a conservative leak).
            if let Instr::CallRuntimeAbi(call) = instr {
                let contract = crate::runtime_symbols::callee_ownership_contract(call.symbol());
                if contract.borrows_all_bytes_args() {
                    continue;
                }
            }
            let share_places = bytes_share_sink_places(instr);
            if !share_places.is_empty() {
                for place in share_places {
                    let Some(local) = base_local(place) else {
                        continue;
                    };
                    if let Some(&root) = alias_of.get(&local) {
                        if let Some(&binding) = candidate_local_to_binding.get(&root) {
                            pending_share_sites.push((block.id, instr_index, place, vec![binding]));
                        }
                    } else if borrowed_alias_of.contains_key(&local)
                        && !matches!(instr, Instr::ActorStateFieldStore { .. })
                    {
                        pending_share_sites.push((block.id, instr_index, place, Vec::new()));
                    }
                }
                continue;
            }
            // A receiver-borrowing bytes runtime op reads the triple as arg[0]
            // but only borrows it; scan
            // only arg[1..]. Every other instruction reading an alias member is
            // an owning-sink escape (stored into a record field, captured into
            // a closure env, moved into an aggregate, sent, …). Fail-closed: a
            // bytes triple has no other benign interior instruction read.
            if let Instr::CallRuntimeAbi(call) = instr {
                if crate::runtime_symbols::callee_ownership_contract(call.symbol())
                    .borrows_bytes_receiver()
                {
                    scan_places(&call.args()[1..], &alias_of, &mut excluded_roots);
                    continue;
                }
            }
            for p in instr_source_places(instr) {
                if let Some(l) = base_local(p) {
                    if alias_of.contains_key(&l) && matches!(p, Place::Local(_) | Place::ReturnSlot)
                    {
                        note_escape(l, &mut excluded_roots);
                    }
                }
            }
        }
        // Terminator reads. The receiver-borrow exemption applies to a
        // `Terminator::Call` on a listed bytes op (`hew_bytes_to_string`
        // reaches MIR in this shape); every other terminator (an actor
        // `Send` / `Ask`, a different `Call`, a return moving the triple to
        // the `ReturnSlot`, …) is scanned in full: a member read there is an
        // escape.
        match &block.terminator {
            // Hew's by-value heap parameters are borrows. A call never mints a
            // co-owner and never consumes the caller's bytes reference.
            Terminator::Call { .. } => {}
            other => {
                for p in terminator_source_places(other, suspend_kinds.get(&block.id)) {
                    if let Some(l) = base_local(p) {
                        if alias_of.contains_key(&l)
                            && matches!(p, Place::Local(_) | Place::ReturnSlot)
                        {
                            note_escape(l, &mut excluded_roots);
                        }
                    }
                }
            }
        }
    }

    let mut allowed = HashSet::new();
    for (&local, &binding) in &candidate_local_to_binding {
        // Projection-tainted candidates alias a still-live parent aggregate's
        // buffer; the parent's drop owns the release (see the function doc).
        if tainted.contains(&local) {
            continue;
        }
        // Resolve to the whole-value alias ROOT before testing exclusion, so a
        // candidate that is itself an alias member is excluded when any group
        // member escaped (same fail-closed root resolution as the collection
        // prover; see the rationale on `derive_local_collection_drop_allowed`).
        let root = alias_of.get(&local).copied().unwrap_or(local);
        if !excluded_roots.contains(&root) {
            allowed.insert(binding);
        }
    }
    let mut retain_sites = Vec::new();
    for (&producer_local, &(block, instr_index, dest)) in &producer_sites {
        if retained_producer_roots.contains(&producer_local) {
            retain_sites.push(BytesRetainSite {
                block,
                instr_index,
                placement: BytesRetainPlacement::After,
                value: dest,
                required_bindings: Vec::new(),
            });
        }
    }
    for (block, instr_index, value, required_bindings) in pending_share_sites {
        retain_sites.push(BytesRetainSite {
            block,
            instr_index,
            placement: BytesRetainPlacement::Before,
            value,
            required_bindings,
        });
    }

    // A by-value bytes parameter is a borrow from the caller. Returning it
    // duplicates that live external owner, so retain immediately before the
    // Move into ReturnSlot. Ordinary calls remain borrow-only and emit nothing.
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
                retain_sites.push(BytesRetainSite {
                    block: block.id,
                    instr_index,
                    placement: BytesRetainPlacement::Before,
                    value: *src,
                    required_bindings: Vec::new(),
                });
            }
        }
    }

    // A `CowShare` local-to-local copy (`let b = a;`) keeps both bindings live:
    // both `a` and `b` are distinct `bytes` triples pointing at ONE buffer that
    // the byte-copy `Move` shared with NO refcount bump. A fresh reference must
    // be minted so each end that reaches a drop (locally, or by handing its
    // reference to a caller/sink) balances against the buffer's rc. The mint
    // splits on whether the SOURCE is itself an owned-candidate bytes local:
    //
    // * Source IS a candidate (both ends are owned locals that drop at scope
    //   exit): retain once, gated on BOTH bindings surviving to a drop.
    //
    // * Source is NOT a candidate — a by-value `bytes` PARAMETER (a borrow; the
    //   caller retains ownership and drops the original) or an owned binding
    //   already CONSUMED/RETURNED (its single reference handed off to a caller /
    //   sink, so it is no longer in `owned_locals`). In both, `dest` is a fresh
    //   co-owner that MUST retain to balance its own scope-exit drop against the
    //   surviving external owner; dropping `dest` unretained double-frees that
    //   shared buffer (the caller's borrow, or the returned/sent handle). Gate
    //   solely on `dest` reaching a drop (`required_bindings = [dest]`): if
    //   `dest` itself escapes it is not a candidate here and the return / sink
    //   retain path mints its reference instead. Restricted to a Move whose
    //   source base is a NAMED binding slot (`binding_local_bases`) so an
    //   initialiser move (`let a = <fresh producer temp>`) or a projection temp
    //   — neither a `let b = a` co-own share — cannot mint a spurious owner.
    //   LESSONS: raii-null-after-move (S171: a by-value heap param is a borrow).
    let binding_local_bases: HashSet<u32> = binding_locals
        .values()
        .filter_map(|place| base_local(*place))
        .collect();
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
                // Source is itself an owned bytes candidate (or its alias): both
                // ends are locals that drop at scope exit, so retain once, gated
                // on BOTH bindings surviving to a drop.
                let Some(&src_binding) = candidate_local_to_binding.get(&root) else {
                    continue;
                };
                if src_binding == dest_binding {
                    continue;
                }
                retain_sites.push(BytesRetainSite {
                    block: block.id,
                    instr_index,
                    placement: BytesRetainPlacement::Before,
                    value: *src,
                    required_bindings: vec![src_binding, dest_binding],
                });
            } else {
                // Source is NOT an owned candidate. Fail-closed: only mint for a
                // genuine bytes co-own share — the source must be a live `bytes`
                // value that is either a by-value parameter borrow
                // (`borrowed_alias_of`) or a named binding slot
                // (`binding_local_bases`, i.e. an owned binding consumed/returned
                // before scope exit). An initialiser move (`let a = <fresh
                // producer temp>`) or a projection temp is neither, so it keeps
                // the pre-fix behaviour (no mint), which never double-frees.
                if !bytes_place_is_typed(*src, local_tys)
                    || !(binding_local_bases.contains(&src_local)
                        || borrowed_alias_of.contains_key(&src_local))
                {
                    continue;
                }
                retain_sites.push(BytesRetainSite {
                    block: block.id,
                    instr_index,
                    placement: BytesRetainPlacement::Before,
                    value: *src,
                    required_bindings: vec![dest_binding],
                });
            }
        }
    }

    BytesDropDerivation {
        allowed,
        retain_sites,
    }
}
/// W5.021 — fail-closed sole-owner derivation for owned-aggregate **tuple**
/// bindings (the tuple/record-of-owned-handles drop spine). Returns the subset
/// of `owned_locals` whose tuple binding still owns its heap members at scope
/// exit and therefore earns a `DropKind::TupleInPlace` drop.
///
/// This is the tuple analogue of [`derive_owned_record_drop_allowed`]: a tuple
/// is an inline struct in its binding's alloca; its owned members (pointer
/// handles like `Stream`/`Sink`, or heap leaves `string`/`bytes`/`Vec`/nested
/// owned record/enum/tuple) are the heap owners. The escape model is identical
/// — the tuple is the *owner* and the question is whether it (or one of its
/// owned members) escapes:
/// - A whole-value `Move` of the tuple (`let b = a` rebind, or a tail/if-arm
///   that flows to the return) byte-copies the struct with no retain; that
///   alias never independently drops, so dropping the original owner exactly
///   once is correct — UNLESS the alias is what escapes (returned), in which
///   case the escapee owns the members now.
/// - A `TupleFieldLoad` reads one element out. Reading a `BitCopy` element is
///   harmless. Reading an OWNED element (a pointer handle or a heap leaf)
///   shares its owner with no retain; if that loaded element then escapes into
///   an owning sink (e.g. a `let sink = __tuple_N.0` destructure binder that
///   itself owns and drops the handle), the tuple must NOT drop it. This is the
///   canonical `__tuple_N` destructure-temp case (DEFECT #3): the element
///   binders own the handles, so the whole-tuple temp is excluded.
///
/// Fail-closed: a tuple binding is admitted IFF its whole-value alias set is
/// never read into an owning sink (except the benign `Move` hand-off) AND no
/// owned element loaded from it escapes. Anything the prover cannot positively
/// clear is excluded — it leaks, never double-frees. A `TupleFieldLoad` reading
/// the tuple is an INTERIOR read (it seeds an element binder for the escape
/// rule, it does not escape the whole tuple). LESSONS:
/// drop-allowset-from-value-flow, raii-null-after-move, cleanup-all-exits.
#[allow(
    clippy::too_many_lines,
    reason = "four sequential single-purpose passes (candidate collection, \
              whole-value alias propagation, owned-element-binder seeding, \
              escape scan) sharing fixpoint state, mirroring derive_owned_\
              record_drop_allowed; splitting scatters the fail-closed ordering"
)]
#[allow(
    clippy::too_many_arguments,
    reason = "the function's MIR + binding registries + the layout lookups + the \
              carried interior-alias binder pairs the composite prover reads; a \
              struct would only relocate the same fields, mirroring \
              derive_owned_record_drop_allowed"
)]
pub(super) fn derive_tuple_composite_drop_allowed(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
    alias_field_binders: &[(u32, u32)],
) -> HashSet<BindingId> {
    // A local carries a heap-owning value iff its registered type says so. Used
    // to decide whether a `TupleFieldLoad` dest is an owned-element binder (a
    // BitCopy element load is harmless to alias out). Record-aware through the
    // single `ty_owns_heap` authority (DIV-1).
    let local_is_heap_owning = |local: u32| -> bool {
        local_tys
            .get(local as usize)
            .is_some_and(|ty| crate::model::ty_owns_heap_mir(ty, record_field_orders, enum_layouts))
    };

    // Candidate tuple locals: base locals of owned-tuple bindings (a `Tuple`
    // type carrying at least one heap-owning element).
    let mut candidate_local_to_binding: HashMap<u32, BindingId> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !ty_is_heap_owning_tuple(ty, record_field_orders, enum_layouts) {
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
    if candidate_local_to_binding.is_empty() {
        return HashSet::new();
    }

    // Whole-value alias set: each candidate plus every local reachable through
    // forward-propagated whole-value `Move { dest: Local, src: Local }` copies.
    // Monotone: a slot reachable from two distinct roots is evicted (#1942).
    let alias_of =
        propagate_whole_value_alias_roots(blocks, candidate_local_to_binding.keys().copied());

    // Owned-element-binder set: destinations of `TupleFieldLoad { tuple: alias-
    // set member }` whose loaded element is itself heap-owning. Propagate
    // forward through whole-value Move so a binder copied onward is still
    // tracked.
    let mut elem_binders: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::TupleFieldLoad { tuple, dest, .. } = instr {
                if let Some(sl) = base_local(*tuple) {
                    if alias_of.contains_key(&sl) {
                        if let Some(dl) = base_local(*dest) {
                            if local_is_heap_owning(dl) {
                                elem_binders.insert(dl);
                            }
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
                        if elem_binders.contains(&sl) && elem_binders.insert(dl) {
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

    // Fold the recorded byte-copy interior-alias binders (the `let mid = o.0;
    // let leaf = mid.0` projection chain) into the element-binder set. The
    // `TupleFieldLoad` scan above only reaches a ONE-hop alias (`mid` reads the
    // tuple root directly); a deeper alias (`leaf` reads `mid`, not the root) is
    // invisible to it, so its ESCAPE would leave the owner admitted to free a
    // subtree the escapee handed to the caller (the tuple twin of the #2375
    // double-free). Recording it here makes `note_elem_escape` exclude the tuple
    // roots when the deep alias escapes; a `TupleFieldLoad` reading the alias is
    // interior (exempt), so the owner stays admitted on the consumed-match path.
    let folded_aliases = close_alias_binders_forward(blocks, alias_field_binders);
    for (&alias_local, &owner_local) in &folded_aliases {
        if candidate_local_to_binding.contains_key(&owner_local) {
            elem_binders.insert(alias_local);
        }
    }
    let retained_bytes_element_seeds: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| block.instructions.windows(2))
        .filter_map(|pair| {
            let Instr::TupleFieldLoad { dest, .. } = pair[0] else {
                return None;
            };
            (bytes_place_is_typed(dest, local_tys)
                && matches!(pair[1], Instr::BytesRetain { value } if value == dest))
            .then(|| base_local(dest))
            .flatten()
        })
        .collect();
    let retained_bytes_element_aliases: HashSet<u32> =
        propagate_whole_value_alias_roots(blocks, retained_bytes_element_seeds.iter().copied())
            .into_keys()
            .collect();
    elem_binders.retain(|local| !retained_bytes_element_aliases.contains(local));

    // #2384 (tuple twin) — match-bound hop aliases: an element bound out of a
    // `match` destructure of a chain alias (`let leaf = match mid { (l, _) =>
    // l }`) byte-copies the owner root's storage; the `Move`-only closure
    // above cannot see through the destructure's `TupleFieldLoad`, so such a
    // binder's escape left the owner admitted to re-free the subtree the
    // caller now holds. Folded into the ESCAPE arms only — never into
    // `elem_binders`, whose `release_owner_bases` Defect-1 blanket below would
    // newly exclude owners in today-clean non-escaping match shapes (the
    // binder is an owned binding, so the blanket would always trip).
    let candidate_owned_aliases: HashMap<u32, u32> = folded_aliases
        .iter()
        .filter(|(_, owner)| candidate_local_to_binding.contains_key(owner))
        .map(|(&alias, &owner)| (alias, owner))
        .collect();
    let dest_is_byte_copy_aggregate = |local: u32| -> bool {
        local_is_byte_copy_aggregate(local, local_tys, record_field_orders, enum_layouts)
    };
    let mut match_hop_binders = descend_match_bound_hop_aliases(
        blocks,
        &candidate_owned_aliases,
        &dest_is_byte_copy_aggregate,
    );
    match_hop_binders.retain(|binder, _| !elem_binders.contains(binder));
    let is_escape_binder = |l: u32| elem_binders.contains(&l) || match_hop_binders.contains_key(&l);

    let mut excluded_roots: HashSet<u32> = HashSet::new();

    // Interior-alias exclusion (hard double-free), tuple analogue of the record
    // seed: a tuple binding bound from an ALIASING container getter
    // (`let p = xs[i]` where `xs: Vec<(Vec<i64>, i64)>` → `hew_vec_get_owned`
    // returns an interior pointer into the still-live Vec element slot) does NOT
    // solely own its members. The parent Vec's `hew_vec_free_owned` frees each
    // element's owned members; admitting the alias to its own `TupleInPlace` would
    // double-free. Over-exclude every interior-alias-tainted candidate up front
    // (fail-closed) so the borrow-aware member-read exemption below cannot
    // re-admit an aliased element to drop (`compute_collection_interior_alias_taint`).
    let retained_bytes_owner_bases: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| block.instructions.windows(2))
        .filter_map(|pair| {
            let Instr::TupleFieldLoad { tuple, dest, .. } = pair[0] else {
                return None;
            };
            (bytes_place_is_typed(dest, local_tys)
                && matches!(pair[1], Instr::BytesRetain { value } if value == dest))
            .then(|| base_local(tuple))
            .flatten()
        })
        .collect();
    let mut interior_alias_tainted = compute_collection_interior_alias_taint(blocks);
    interior_alias_tainted.retain(|local| !retained_bytes_owner_bases.contains(local));
    for &local in candidate_local_to_binding.keys() {
        if interior_alias_tainted.contains(&local) {
            excluded_roots.insert(local);
        }
    }

    // Defect 1 (hard double-free): an owned element extracted out of the tuple
    // into a binding that carries its OWN release path. `let g = pair.0` (g a
    // `Generator`/`CancellationToken`/owned handle) loads element 0 into `g`'s
    // slot. The extracted handle is then released exactly once through `g`'s own
    // path — EITHER its standalone scope-exit drop (`g` lands in `owned_locals`,
    // so the AffineResource arm in `build_lifo_drops` fires `hew_gen_coro_destroy(g)`),
    // OR a for-in consume (`for n in g` moves `g` into the loop iterator binding,
    // whose per-loop-exit `hew_gen_coro_destroy` releases it; that iterator binding is in
    // `extracted_consumer_bases`). If the tuple is ALSO admitted to
    // `TupleInPlace`, its member-drop thunk frees the SAME aliased ctx pointer a
    // second time — the codegen null-store nulls the consuming binding's slot but
    // never the tuple's field-0 slot, so the runtime sees a non-null (dangling)
    // pointer and double-frees (verified: two `hew_gen_coro_destroy` call sites on one
    // ctx). The extracted binder is the sole owner now, so the tuple must NOT
    // drop that element. Over-exclude the WHOLE tuple (fail-closed: leak a
    // sibling, never double-free) — symmetric to the `__tuple_N` destructure-temp
    // exclusion. The `Vec` path is immune because a for-in consume removes the
    // Vec from the spine via `owned_vec_drop_allowed`; an owned handle's
    // equivalent is this consumer-binder seed. The escape scan below already
    // excludes the tuple when an extracted binder is read into an owning sink
    // (returned / sent / `close()`d); this seed adds the two
    // exempt-from-that-scan release paths (standalone `Drop`, for-in consume).
    let mut exempt_read_temp_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| string_field_load_producer_dest(instr, local_tys))
        .filter_map(base_local)
        .collect();
    exempt_read_temp_locals.extend(retained_bytes_element_aliases);
    exempt_read_temp_locals.extend(
        propagate_whole_value_alias_roots(blocks, exempt_read_temp_locals.iter().copied())
            .into_keys(),
    );
    let mut release_owner_bases: HashSet<u32> = owned_locals
        .iter()
        .filter_map(|(binding, _, _)| binding_locals.get(binding).and_then(|p| base_local(*p)))
        .filter(|local| !exempt_read_temp_locals.contains(local))
        .collect();
    // Plus any local that is the place of an inline generator/owned-handle
    // release `Drop` already emitted into the finalized MIR — the for-in iterator
    // binding (`for n in t.0`) is released by an inline `Instr::Drop { drop_fn:
    // Some("hew_gen_coro_destroy") }` from `emit_scope_generator_drops` at loop-scope
    // close. That binding is drained out of `scope_generator_bindings` once its
    // scope drop is emitted, so an `owned_locals` snapshot misses it; reading the
    // emitted Drop places recovers every consumer regardless of which mechanism
    // (owned_locals standalone drop, loop-scope drop, or break/continue edge)
    // released it.
    //
    // EXCEPT the string read-temps (#54, the record prover's exemption mirrored
    // for tuples — #2383): the spliced `hew_string_drop` for a `string`
    // `TupleFieldLoad` dest (`apply_nested_fresh_string_temp_drops`) releases a
    // FRESH, codegen-cloned (`retain_string_field_load` → `hew_string_clone`)
    // refcount of the element — NOT the tuple's original `+1`. Counting it as a
    // `release_owner_base` treats a benign borrowing READ of a tuple element
    // (`l.0.len()`, `println(pair.1)`) as an element-ownership EXTRACTION and
    // wrongly drops the whole tuple's `TupleInPlace` — the caller-side half of
    // the #2383 leak (a returned pair whose elements were only read leaked both
    // strings every call). A NON-string element extracted into an owning
    // binding (a generator handle `let g = pair.0`) is not a
    // `string_field_load_producer_dest` and still seeds.
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Drop {
                place,
                drop_fn: Some(_),
                ..
            } = instr
            {
                if let Some(l) = base_local(*place) {
                    if exempt_read_temp_locals.contains(&l) {
                        continue;
                    }
                    release_owner_bases.insert(l);
                }
            }
        }
    }
    for &binder in &elem_binders {
        if release_owner_bases.contains(&binder) {
            for &root in alias_of.values() {
                excluded_roots.insert(root);
            }
            break;
        }
    }

    // Escape scan. Exclude a tuple root if any whole-value alias member, or any
    // owned-element binder derived from it, is read into an owning sink. A
    // `TupleFieldLoad` reading the tuple is an INTERIOR read (it feeds the
    // element-binder pass, never escapes the whole tuple), so it is exempt from
    // the whole-value escape check.
    let note_alias_escape = |local: u32, excluded: &mut HashSet<u32>| {
        if let Some(&root) = alias_of.get(&local) {
            excluded.insert(root);
        }
    };
    // An element binder escaping means the tuple no longer solely owns that
    // element. We cannot cheaply attribute a binder to one tuple root, so —
    // fail-closed — exclude every tuple root when any element binder escapes
    // (over-exclusion leaks; never double-frees), mirroring `note_field_escape`.
    //
    // W5.021 Finding 4: this is also the safety net for a projection-receiver
    // consume — `pair.0.close()` loads element 0 out of an OWNED tuple via a
    // `TupleFieldLoad` and consumes it. The projection is not a binding, so the
    // consume-intent move-mark (`mark_binding_moved`) cannot fire on it; instead
    // the loaded element becomes an element binder and its escape into the close
    // call excludes the WHOLE tuple here. That leaks the un-closed sibling rather
    // than double-freeing the closed one — the fail-closed outcome. (For the
    // `stream.pipe` pair specifically the shared backing is reclaimed when one
    // half closes, so `leaks` observes none; the invariant proven is no
    // double-free.) Precise per-element attribution is a v0.5.1 follow-on.
    let note_elem_escape = |excluded: &mut HashSet<u32>| {
        for &root in alias_of.values() {
            excluded.insert(root);
        }
    };
    for block in blocks {
        for instr in &block.instructions {
            // Direct prover-exclusion rule for the no-temp field-addressed
            // drop, the tuple twin of `derive_owned_record_drop_allowed`'s: a
            // `FieldDropInPlace` whose base is an alias-set member is BOTH
            // the element extraction AND its release, mints no load dest and
            // no `Drop` place, and so — with bitcopy-only sibling binders —
            // seeds neither `elem_binders` nor `release_owner_bases`. Without
            // this rule the tuple's `TupleInPlace` would re-walk the freed
            // element's leaves (double-free; no null-store on inline
            // composites). Exclude the root directly.
            //
            // A base that is an ELEMENT BINDER (an extracted member alias —
            // `let inner = t.0; match inner { … }`) discharges part of the
            // OUTER root's storage through the binder's byte-alias — the
            // root's composite walk would re-free the discharged leaves.
            // The tuple prover carries no binder-provenance attribution
            // (deliberately blanket — "cannot cheaply attribute"), so the
            // exclusion is `note_elem_escape`'s every-root coarsening:
            // over-exclusion leaks, never double-frees. This rule is the
            // pairing the blanket-scan `FieldDropInPlace` exemption below
            // depends on — exempting the op there WITHOUT resolving binder
            // bases here would trade the old over-exclusion leak for a
            // composite re-walk double-free.
            if let Instr::FieldDropInPlace { base, .. } = instr {
                if let Some(l) = base_local(*base) {
                    if alias_of.contains_key(&l) {
                        note_alias_escape(l, &mut excluded_roots);
                    }
                    if is_escape_binder(l) {
                        note_elem_escape(&mut excluded_roots);
                    }
                }
            }
            // A `Move` discriminates a benign whole-value hand-off (dest is
            // another alias member) from a real whole-tuple escape.
            if let Instr::Move { dest, src } = instr {
                let src_local = base_local(*src);
                let dest_local = base_local(*dest);
                if let Some(sl) = src_local {
                    let src_is_member = alias_of.contains_key(&sl)
                        && matches!(src, Place::Local(_) | Place::ReturnSlot);
                    let dest_is_member = dest_local.is_some_and(|dl| {
                        alias_of.contains_key(&dl) && matches!(dest, Place::Local(_))
                    });
                    // ReturnSlot dest is never an alias member, so a
                    // Move { dest: ReturnSlot, src: member } excludes the root.
                    if src_is_member && !dest_is_member {
                        note_alias_escape(sl, &mut excluded_roots);
                    }
                    if is_escape_binder(sl) {
                        let benign = dest_local.is_some_and(is_escape_binder)
                            && matches!(dest, Place::Local(_));
                        if !benign {
                            note_elem_escape(&mut excluded_roots);
                        }
                    }
                }
            }
            // Non-Move owning-sink reads. A `TupleFieldLoad` reading the tuple
            // base is interior (exempt); every other instruction reading a
            // whole alias member or an element binder is an escape. Fail-closed.
            //
            // Exception: an element binder read ONLY as the borrowed receiver
            // (arg[0]) of a collection-borrow call (`p.0.len()` / `p.0[i]`
            // lowered as `Instr::CallRuntimeAbi`) is an interior borrow, not an
            // escape — `binder_read_is_borrow_safe_instr` clears it (the tuple
            // analogue of the record field-binder exemption, DI-017).
            if !matches!(
                instr,
                Instr::Move { .. }
                    | Instr::Drop { .. }
                    | Instr::TupleFieldLoad { .. }
                    // `FieldDropInPlace` is an interior field op (uses base, no
                    // dest, no alias); its composite-suppression semantics are
                    // the DIRECT exclusion rule at the top of this loop — which
                    // resolves an alias-member OR element-binder base — not
                    // this blanket owning-sink scan (which would misread a
                    // binder base as an element escape and blanket-exclude
                    // every tuple root, including roots whose storage the op
                    // never touches). Mirrors the record and enum provers'
                    // exemption arms.
                    | Instr::FieldDropInPlace { .. }
            ) {
                for p in instr_source_places(instr) {
                    if let Some(l) = base_local(p) {
                        if alias_of.contains_key(&l)
                            && matches!(p, Place::Local(_) | Place::ReturnSlot)
                        {
                            note_alias_escape(l, &mut excluded_roots);
                        }
                        if is_escape_binder(l) && !binder_read_is_borrow_safe_instr(instr, l) {
                            note_elem_escape(&mut excluded_roots);
                        }
                    }
                }
            }
        }
        // Terminator reads. A return / send / ask of a whole tuple or an owned
        // element binder escapes. `print`/`println` borrows its string arg, and a
        // collection-borrow call (`.len()` / `.get(i)`) borrows its receiver, so
        // an element binder read only as such a borrowed arg is a transient read,
        // not an escape (`binder_read_is_borrow_safe_terminator`).
        for p in terminator_source_places(&block.terminator, suspend_kinds.get(&block.id)) {
            if let Some(l) = base_local(p) {
                if alias_of.contains_key(&l)
                    && matches!(p, Place::Local(_) | Place::ReturnSlot)
                    && !binder_read_is_borrow_safe_terminator(
                        &block.terminator,
                        suspend_kinds.get(&block.id),
                        l,
                    )
                {
                    note_alias_escape(l, &mut excluded_roots);
                }
                if is_escape_binder(l)
                    && !binder_read_is_borrow_safe_terminator(
                        &block.terminator,
                        suspend_kinds.get(&block.id),
                        l,
                    )
                {
                    note_elem_escape(&mut excluded_roots);
                }
            }
        }
    }

    readmit_retained_bytes_tuple_roots(
        blocks,
        suspend_kinds,
        &alias_of,
        &retained_bytes_owner_bases,
        &mut excluded_roots,
    );

    let mut allowed = HashSet::new();
    for (&local, &binding) in &candidate_local_to_binding {
        if !excluded_roots.contains(&local) {
            allowed.insert(binding);
        }
    }
    allowed
}
/// True if reading this `Place` as an aggregate member source (a construct
/// element, a rebind src, a return-slot move src) is an ownership hand-off the
/// value-flow pass should follow — i.e. it names a standalone owning slot whose
/// value flows into the returned aggregate, not an interior alias of a still-live
/// parent.
///
/// W5.021 (defect #1, revision 2) — the seed / `add_member` / construct
/// decomposition originally gated on `matches!(place, Place::Local(_))`, which is
/// an asymmetry with the map-back step: the map-back resolves a member's local
/// via [`base_local`], which accepts the owned handle places
/// (`DuplexHandle`/`SendHalf`/`RecvHalf`/`LambdaActorHandle`/`ActorHandle`).
/// Those handle members are registered in `binding_locals` directly as their
/// handle Place (`lower.rs` Bind arm: handle-typed `value_place` IS the binding's
/// slot, no second `Local`), so they surface as `TupleConstruct` / `RecordInit`
/// elements as their handle Place — and the `Place::Local(_)` gate silently
/// dropped them on the floor, leaving a returned handle tuple double-dropped by
/// the callee.
///
/// This predicate restores symmetry with `base_local`: accept `Local` *and* the
/// five owned handle places (each a standalone, resource-owning slot whose `Move`
/// hands off ownership), while CONTINUING TO REJECT the interior-projection
/// places (`EnumVariant`/`MachineVariant`/`MachineTag`/`EnumTag`),
/// which alias a still-live parent aggregate's storage — adding one of those to
/// `flows_to_return` would exclude a parent-owned payload from drop and LEAK the
/// parent's buffer. `ReturnSlot` is the aggregate's destination, never a member
/// source, so it is rejected here too.
///
/// Mirrors the [`place_is_interior_projection`] classifier's hand-off-vs-interior
/// split, inverted: that one over-taints handle places (fail-closed for ITS
/// purpose — over-tainting only over-excludes); this one accepts handle places
/// (fail-closed for OUR purpose — accepting an owned hand-off slot is exactly what
/// keeps a returned handle from being double-dropped, and the worst case of
/// accepting too much is over-exclusion → leak, never double-free). Exhaustive
/// with no wildcard so a future `Place` variant must be classified deliberately.
#[must_use]
fn place_is_owned_handoff_member(place: Place) -> bool {
    match place {
        // Standalone owning slots: a `Move`/construct-element read here is an
        // ownership hand-off into the returned aggregate.
        Place::Local(_)
        | Place::DuplexHandle(_)
        | Place::LambdaActorHandle(_)
        | Place::ActorHandle(_)
        | Place::SendHalf(_)
        | Place::RecvHalf(_) => true,
        // Interior-projection places alias a still-live parent's storage; the
        // return-slot is the destination, not a member source. Following these
        // would over-exclude a parent-owned value and leak it.
        Place::ReturnSlot
        | Place::EnumVariant { .. }
        | Place::MachineVariant { .. }
        | Place::MachineTag(_)
        | Place::EnumTag(_) => false,
    }
}
fn retained_string_values_before(block: &BasicBlock, instr_index: usize) -> HashSet<Place> {
    block.instructions[..instr_index]
        .iter()
        .rev()
        .take_while(|instr| {
            matches!(
                instr,
                Instr::BytesRetain { .. } | Instr::StringRetain { .. }
            )
        })
        .filter_map(|instr| match instr {
            Instr::StringRetain { value } => Some(*value),
            _ => None,
        })
        .collect()
}
/// W5.021 (defect #1) — fail-closed value-flow derivation of the owned member
/// bindings that a function HANDS to its caller through a returned aggregate,
/// and therefore must NOT also drop at its own scope exit.
///
/// A composite return — `(a, b)` / `R { f: a, g: b }`, reached directly, by
/// name, or through any control-flow tail — byte-copies each constituent into
/// the returned aggregate struct with no retain (the M-COW spine emits no
/// retain on share), then moves the whole aggregate to the `ReturnSlot`. The
/// caller now owns those members; if the callee also dropped them at scope exit
/// it would `close` / free a buffer the caller still holds (the Finding-1 hard
/// double-free: an unguarded `Box::from_raw` twice — see the codegen
/// Stream/Sink drop arm).
///
/// The previous revision excluded these members by walking the SYNTACTIC return
/// expression (`mark_returned_binding_moved`): it matched only `BindingRef` /
/// `TupleLiteral` / `StructInit` / `Block`, so any aggregate reached through a
/// `let pair = (s, r); pair` rebind or an `if` / `match` / `scope` / `loop`
/// tail fell through and the members stayed drop-eligible → double-free. That is
/// structurally fail-OPEN: every return grammar the enumeration does not list
/// re-opens the hole.
///
/// This derivation inverts to value-flow, the same alias/construct basis
/// [`derive_tuple_composite_drop_allowed`] and [`derive_owned_record_drop_allowed`]
/// already use, so a syntactic shape cannot fall behind the grammar:
///   1. Seed the `flows_to_return` set with every owned hand-off slot moved
///      whole-value into the `ReturnSlot` (`Move { dest: ReturnSlot, src }`,
///      where `src` is a `Local` or one of the owned handle places — see
///      [`place_is_owned_handoff_member`]).
///   2. Fixpoint two monotone rules that add SOURCE locals to the set:
///      - whole-value back-prop: `Move { dest: Local(d in set), src }` adds
///        `src`'s local (the aggregate flowed onward through a rebind/temp);
///      - construct decomposition: a `TupleConstruct` / `RecordInit` whose dest
///        is in the set adds each element/field source local — including owned
///        handle-place members — (so a returned aggregate's members, and
///        recursively a nested aggregate's members, enter the set);
///      - variant-payload decomposition: a `Move { dest:
///        Place::{Machine,Enum}Variant { local in set, .. }, src }` adds `src`'s
///        local (an enum/Result variant constructor stores its owned payload
///        through an interior-projection dest, not a `TupleConstruct` /
///        `RecordInit`, so the returned `Ok(handle)` member enters the set).
///   3. Map back: every `owned_locals` binding whose backing local (resolved by
///      [`base_local`], which also resolves the handle places) is in
///      `flows_to_return` is a returned member and is excluded from drop.
///
/// Owned handle members (`DuplexHandle`/`SendHalf`/`RecvHalf`/`LambdaActorHandle`/
/// `ActorHandle`) are registered in `binding_locals` directly as their handle
/// Place, so they appear as construct elements as that handle Place; the seed,
/// `add_member`, and decomposition all admit them via
/// [`place_is_owned_handoff_member`] so a returned handle tuple is not
/// double-dropped by the callee. Interior-projection places
/// (`EnumVariant`/`MachineVariant`/`GenState`/`MachineTag`/`EnumTag`) are still
/// rejected — they alias a live parent and following them would leak the parent.
///
/// Fail-closed: the set only grows, so the worst case is over-exclusion of a
/// member that did not actually escape — that LEAKS, it never double-frees. The
/// intermediate temps the fixpoint also collects (a construct dest, a rebind
/// alias) are not `owned_locals` bindings, so excluding them is a no-op. The
/// aggregate binding ITSELF (the `pair` local) is governed by the per-aggregate
/// `derive_*_drop_allowed` escape scans, which already exclude a returned
/// aggregate; this pass is the complementary half that reaches the scalar member
/// handles those scans do not own.
///
/// KNOWN over-exclusion (branch-divergent member sets): the value-flow set is
/// flow-INSENSITIVE, so when distinct control-flow tails construct the returned
/// aggregate from DIFFERENT members — e.g. `if c { (s1, r2) } else { (s2, r1) }`,
/// both `TupleConstruct`s flowing to the same `ReturnSlot` — every member of
/// every tail (`s1`, `r2`, `s2`, `r1`) enters `flows_to_return` and is excluded
/// from drop in BOTH bodies. On either branch only two of the four actually
/// escape; the other two are excluded anyway and LEAK. This is the deliberate
/// fail-closed direction (over-exclusion → leak, never double-free); precise
/// per-branch member attribution is a follow-on slice if a fixture ever needs it.
/// LESSONS: drop-allowset-from-value-flow, raii-null-after-move, cleanup-all-exits.
pub(super) fn derive_returned_aggregate_member_bindings(
    blocks: &[BasicBlock],
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
) -> HashSet<BindingId> {
    // Seed: every owned hand-off slot (Local or handle place) whole-value
    // moved into the ReturnSlot.
    let mut flows_to_return: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Move { dest, src } = instr {
                if matches!(dest, Place::ReturnSlot) {
                    if let Some(sl) = base_local(*src) {
                        if place_is_owned_handoff_member(*src) {
                            flows_to_return.insert(sl);
                        }
                    }
                }
            }
        }
    }
    if flows_to_return.is_empty() {
        return HashSet::new();
    }

    // Fixpoint: grow the set backward along whole-value Moves and downward
    // through aggregate constructors whose dest already flows to the return.
    // Add an owned hand-off source (Local or handle place) to the set,
    // reporting whether it grew. Interior-projection places are rejected.
    let add_member = |place: &Place, set: &mut HashSet<u32>| -> bool {
        match base_local(*place) {
            Some(local) if place_is_owned_handoff_member(*place) => set.insert(local),
            _ => false,
        }
    };
    loop {
        let mut changed = false;
        for block in blocks {
            for (instr_index, instr) in block.instructions.iter().enumerate() {
                match instr {
                    // Whole-value rebind/temp: `Move { dest: in-set, src }`
                    // means `src` flowed onward into a local that reaches the
                    // ReturnSlot, so `src` reaches it too.
                    Instr::Move { dest, src }
                        if matches!(dest, Place::Local(_))
                            && base_local(*dest)
                                .is_some_and(|dl| flows_to_return.contains(&dl)) =>
                    {
                        changed |= add_member(src, &mut flows_to_return);
                    }
                    // Aggregate construction whose dest reaches the return: each
                    // element source is a member handed to the caller.
                    Instr::TupleConstruct { elements, dest }
                        if base_local(*dest).is_some_and(|dl| flows_to_return.contains(&dl)) =>
                    {
                        let retained = retained_string_values_before(block, instr_index);
                        for elem in elements {
                            if !retained.contains(elem) {
                                changed |= add_member(elem, &mut flows_to_return);
                            }
                        }
                    }
                    // Record construction whose dest reaches the return: each
                    // field source is a member handed to the caller.
                    Instr::RecordInit { fields, dest, .. }
                        if base_local(*dest).is_some_and(|dl| flows_to_return.contains(&dl)) =>
                    {
                        let retained = retained_string_values_before(block, instr_index);
                        for (_offset, field) in fields {
                            if !retained.contains(field) {
                                changed |= add_member(field, &mut flows_to_return);
                            }
                        }
                    }
                    Instr::ClosureEnvInit { fields, dest, .. }
                        if base_local(*dest).is_some_and(|dl| flows_to_return.contains(&dl)) =>
                    {
                        for field in fields
                            .iter()
                            .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
                        {
                            changed |= add_member(&field.src, &mut flows_to_return);
                        }
                    }
                    // Enum / machine variant payload store whose carrier
                    // aggregate reaches the return: the stored source is an
                    // owned member handed to the caller through that variant.
                    //
                    // A variant constructor (`Ok(s)`, a user enum variant with
                    // an owned payload) does NOT lower to `TupleConstruct` /
                    // `RecordInit`; it lowers to a tag store plus a
                    // `Move { dest: Place::{Machine,Enum}Variant { local, .. },
                    // src }` payload store into the aggregate's backing local.
                    // That dest is an interior projection (not `Place::Local`),
                    // so the whole-value back-prop arm above never sees it —
                    // without this arm the returned variant's payload (the
                    // `stream$try_to_file` Sink moved into `Result::Ok`) stays
                    // drop-eligible and is freed by the callee before the caller
                    // receives it (the Bug-B double-free).
                    Instr::Move { dest, src }
                        if matches!(
                            dest,
                            Place::MachineVariant { .. } | Place::EnumVariant { .. }
                        ) && base_local(*dest)
                            .is_some_and(|dl| flows_to_return.contains(&dl)) =>
                    {
                        changed |= add_member(src, &mut flows_to_return);
                    }
                    _ => {}
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Map member locals back to their owned bindings.
    let mut returned_members = HashSet::new();
    for (binding, _name, _ty) in owned_locals {
        if let Some(place) = binding_locals.get(binding) {
            if let Some(local) = base_local(*place) {
                if flows_to_return.contains(&local) {
                    returned_members.insert(*binding);
                }
            }
        }
    }
    returned_members
}
/// Fail-closed value-flow derivation of the owned-handle member bindings whose
/// handle is moved into a LOCALLY-constructed aggregate and then extracted-and-
/// consumed back out of that aggregate by a downstream release-consumer — and
/// which therefore must NOT also drop at their own scope exit (they would
/// double-free the ctx the consumer already releases).
///
/// This is the local-aggregate analogue of
/// [`derive_returned_aggregate_member_bindings`]. That one excludes a member
/// handed to the CALLER through the `ReturnSlot`; this one excludes a member
/// handed to a downstream CONSUMER through a field of an in-scope aggregate.
///
/// The defect this closes (W3.053): a `Generator`/owned-handle binding `g` is
/// moved (`BitCopy`, no retain — the M-COW spine emits no retain on aggregate
/// construction) into a local tuple/record, e.g. `let packed = (g, 99)`. The
/// generator field is then consumed out of the aggregate by a for-in iterator
/// binding (`for n in packed.0`) or a `let` extraction binding (`let x =
/// packed.0; for n in x`). That consumer takes ownership of the SAME ctx pointer
/// and releases it exactly once via its own inline `hew_gen_coro_destroy` (loop-scope or
/// break/continue-edge drop). `g`'s standalone scope-exit drop then frees the
/// SAME ctx a second time → use-after-free. The aggregate's own in-place member
/// spine is already excluded by [`derive_tuple_composite_drop_allowed`] /
/// [`derive_owned_record_drop_allowed`] (the extracted-binder ∩ release-owner
/// seed); this pass is the complementary half that reaches the SOURCE binding
/// the aggregate-spine scans do not own.
///
/// The discriminator that keeps this from over-excluding the no-consume shape
/// (`let repacked = (g, 99); println(repacked.1)`, where `g`'s standalone drop
/// IS the correct sole free) is FIELD PRECISION: a source binding is excluded
/// only when the SPECIFIC field its handle was placed into is later extracted
/// into a release-consumer DISTINCT from the source binding itself. A field that
/// is never extracted (or a sibling field extracted instead) leaves the source
/// binding's own drop as the sole owner.
///
/// Algorithm (three monotone forward passes over an immutable instruction
/// stream, sharing fixpoint state):
///   1. `agg_field_source[(agg_local, field)] = src_local`: every heap-owning
///      element/field placed into an aggregate by `TupleConstruct` / `RecordInit`.
///      Propagated forward through whole-value `Move` so a constructed aggregate
///      copied into its binding slot (and onward) carries its field sources.
///   2. `extracted_carrier[carrier_local] = src_local`: every `TupleFieldLoad` /
///      `RecordFieldLoad` whose `(aggregate, field)` has a recorded source —
///      the loaded local now carries that source's ctx. Propagated forward
///      through whole-value `Move` so the for-in iterator binding (one `Move`
///      past the field load) and any rebind still carry it.
///   3. A source binding is excluded when one of its extracted carriers is in
///      `release_owner_bases` (a local released by an inline `Drop { drop_fn:
///      Some(_) }` — the for-in / extraction consumer) AND that carrier local is
///      not the source local itself (so `g`'s own standalone drop never
///      self-excludes).
///
/// Fail-closed: the sets only grow and the field-precise match never admits a
/// binding the value flow does not positively connect to a distinct consumer, so
/// the worst direction is failing to exclude (a genuine double-free stays — but
/// every such shape is what this pass is built to catch and the probe suite
/// pins) rather than over-excluding a live owner. Where field precision cannot
/// attribute a carrier to one source (it cannot here — each carrier records its
/// single originating source), no coarsening is needed.
/// LESSONS: drop-allowset-from-value-flow, raii-null-after-move, cleanup-all-exits.
#[allow(
    clippy::too_many_lines,
    reason = "three sequential single-purpose forward passes (aggregate field \
              sources, extracted carriers, consumer match) sharing fixpoint \
              state, mirroring derive_tuple_composite_drop_allowed; splitting \
              scatters the fail-closed ordering"
)]
pub(super) fn derive_consumed_local_aggregate_member_bindings(
    blocks: &[BasicBlock],
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
) -> HashSet<BindingId> {
    // Record-aware through the single `ty_owns_heap` authority (DIV-1).
    let local_is_heap_owning = |local: u32| -> bool {
        local_tys
            .get(local as usize)
            .is_some_and(|ty| crate::model::ty_owns_heap_mir(ty, record_field_orders, enum_layouts))
    };

    // Release-consumer locals: the base local of every inline release `Drop`
    // already emitted into the finalized MIR (`drop_fn: Some(_)` — the for-in
    // iterator binding's loop-scope/break/continue `hew_gen_coro_destroy`, the standalone
    // owned-handle drop, etc.). This mirrors the `release_owner_bases` seed in
    // `derive_tuple_composite_drop_allowed`.
    let mut release_owner_bases: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Drop {
                place,
                drop_fn: Some(_),
                ..
            } = instr
            {
                if let Some(l) = base_local(*place) {
                    release_owner_bases.insert(l);
                }
            }
        }
    }
    if release_owner_bases.is_empty() {
        return HashSet::new();
    }

    // Pass 1: aggregate field sources, propagated through whole-value Move.
    let mut agg_field_source: HashMap<(u32, u32), u32> = HashMap::new();
    for block in blocks {
        for instr in &block.instructions {
            match instr {
                Instr::TupleConstruct { elements, dest } => {
                    let Some(dl) = base_local(*dest) else {
                        continue;
                    };
                    for (i, elem) in elements.iter().enumerate() {
                        if let Some(el) = base_local(*elem) {
                            if place_is_owned_handoff_member(*elem) && local_is_heap_owning(el) {
                                let field =
                                    u32::try_from(i).expect("tuple element index exceeds u32::MAX");
                                agg_field_source.insert((dl, field), el);
                            }
                        }
                    }
                }
                Instr::RecordInit { fields, dest, .. } => {
                    let Some(dl) = base_local(*dest) else {
                        continue;
                    };
                    for (offset, field) in fields {
                        if let Some(fl) = base_local(*field) {
                            if place_is_owned_handoff_member(*field) && local_is_heap_owning(fl) {
                                agg_field_source.insert((dl, offset.0), fl);
                            }
                        }
                    }
                }
                Instr::ClosureEnvInit { fields, dest, .. } => {
                    let Some(dl) = base_local(*dest) else {
                        continue;
                    };
                    for field in fields
                        .iter()
                        .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
                    {
                        if let Some(fl) = base_local(field.src) {
                            if place_is_owned_handoff_member(field.src) && local_is_heap_owning(fl)
                            {
                                agg_field_source.insert((dl, field.field_offset.0), fl);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }
    if agg_field_source.is_empty() {
        return HashSet::new();
    }
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if matches!(src, Place::Local(_)) && matches!(dest, Place::Local(_)) {
                        if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                            let carried: Vec<(u32, u32)> = agg_field_source
                                .iter()
                                .filter(|((al, _), _)| *al == sl)
                                .map(|((_, f), s)| (*f, *s))
                                .collect();
                            for (f, s) in carried {
                                if agg_field_source.insert((dl, f), s).is_none() {
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Pass 2: extracted carriers — field loads whose (aggregate, field) has a
    // recorded source — propagated forward through whole-value Move.
    let mut extracted_carrier: HashMap<u32, u32> = HashMap::new();
    for block in blocks {
        for instr in &block.instructions {
            match instr {
                Instr::TupleFieldLoad {
                    tuple,
                    field_index,
                    dest,
                } => {
                    if string_field_load_producer_dest(instr, local_tys) == Some(*dest) {
                        continue;
                    }
                    if let (Some(tl), Some(dl)) = (base_local(*tuple), base_local(*dest)) {
                        if let Some(&src) = agg_field_source.get(&(tl, *field_index)) {
                            extracted_carrier.insert(dl, src);
                        }
                    }
                }
                Instr::RecordFieldLoad {
                    record,
                    field_offset,
                    dest,
                } => {
                    if string_field_load_producer_dest(instr, local_tys) == Some(*dest) {
                        continue;
                    }
                    if let (Some(rl), Some(dl)) = (base_local(*record), base_local(*dest)) {
                        if let Some(&src) = agg_field_source.get(&(rl, field_offset.0)) {
                            extracted_carrier.insert(dl, src);
                        }
                    }
                }
                _ => {}
            }
        }
    }
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if matches!(src, Place::Local(_)) && matches!(dest, Place::Local(_)) {
                        if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                            if let Some(&s) = extracted_carrier.get(&sl) {
                                if extracted_carrier.insert(dl, s).is_none() {
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Pass 3: a source local is excluded when one of its extracted carriers is a
    // distinct release-consumer.
    let mut consumed_source_locals: HashSet<u32> = HashSet::new();
    for (&carrier, &src) in &extracted_carrier {
        if carrier != src && release_owner_bases.contains(&carrier) {
            consumed_source_locals.insert(src);
        }
    }

    let mut consumed_members = HashSet::new();
    for (binding, _name, _ty) in owned_locals {
        if let Some(place) = binding_locals.get(binding) {
            if let Some(local) = base_local(*place) {
                if consumed_source_locals.contains(&local) {
                    consumed_members.insert(*binding);
                }
            }
        }
    }
    consumed_members
}
/// Fail-closed value-flow derivation of the owned-HANDLE-LEAF bindings whose
/// handle is moved into an ACTOR INITIAL-STATE record consumed by a
/// `SpawnActor`, and which therefore must NOT also drop at their own scope
/// exit — the actor's synthesised `state_drop_fn` is the single free site
/// (Stream→`hew_stream_close` / Sink→`hew_sink_close`, the `IoHandle` state
/// field arm).
///
/// This is the spawn-consumed analogue of the two siblings above:
/// [`derive_returned_aggregate_member_bindings`] excludes a member handed to
/// the CALLER through the `ReturnSlot`; [`derive_consumed_local_aggregate_member_bindings`]
/// excludes a member handed to a downstream CONSUMER through a local aggregate.
/// Here the single owner is the SPAWNED ACTOR: `spawn A(sink: sink)` lowers to
/// `RecordInit` (the actor initial-state record) → `Instr::SpawnActor`, whose
/// `state` place IS that record. The M-COW spine byte-copies the handle into
/// the record with no retain, so the source binding and the actor state alias
/// one runtime context; exactly-once demands the source's standalone drop be
/// removed (this set) so only `state_drop_fn` frees it.
///
/// A handle-leaf source binding joins the set iff ALL hold:
///   - its handle local is a field source of a `RecordInit` whose dest is the
///     `state` place of an `Instr::SpawnActor`;
///   - its handle local carries EXACTLY that one owning ingress — it does not
///     also flow (transitively through whole-value `Move`) into the
///     `ReturnSlot`, a non-spawn aggregate construct, or an inline release
///     `Drop`. Any second owning path leaves the binding OUT (fail-closed).
///
/// Direction (fail-closed): a binding the derivation does not positively clear
/// keeps its standalone drop, and the W3.053 gate keeps refusing the shape (a
/// compile error, never a UAF). Adding a binding here removes exactly one free
/// path; the worst mis-classification is a leak (the `state_drop_fn` is not the
/// owner), never a double-free. Both the elaborator (`build_lifo_drops`) and
/// the detector's `source_excluded` consume this SAME set so their exactly-once
/// accounting stays in lock-step.
///
/// LESSONS: drop-allowset-from-value-flow, raii-null-after-move,
/// cleanup-all-exits, boundary-fail-closed.
#[allow(
    clippy::too_many_lines,
    reason = "one fail-closed value-flow derivation: spawn-state collection, \
              origin propagation through Move, spawn-state field attribution, \
              and second-owner disqualification must share the same origin map; \
              splitting scatters the exactly-once accounting (mirrors the two \
              sibling derivations above)"
)]
pub(super) fn derive_spawn_consumed_handle_bindings(
    blocks: &[BasicBlock],
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
) -> HashSet<BindingId> {
    // The `state` record local of every `SpawnActor` — the actor initial-state
    // aggregate the handle is moved into.
    let mut spawn_state_locals: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::SpawnActor {
                state: Some(state), ..
            } = instr
            {
                if let Some(l) = base_local(*state) {
                    spawn_state_locals.insert(l);
                }
            }
        }
    }
    if spawn_state_locals.is_empty() {
        return HashSet::new();
    }

    let is_handle_leaf_local = |local: u32| -> bool {
        local_tys
            .get(local as usize)
            .is_some_and(ty_is_owned_handle_leaf)
    };

    // Origin propagation: every owned handle-leaf binding local carries itself;
    // grown forward through whole-value `Move` so a handle copied into a temp
    // before the state `RecordInit` still carries its source origin. A slot that
    // would carry two distinct origins is AMBIGUOUS and attributed to neither
    // (fail-closed — an ambiguous carrier is never used to admit a binding).
    let mut carries: HashMap<u32, u32> = HashMap::new();
    let mut ambiguous: HashSet<u32> = HashSet::new();
    for place in binding_locals.values() {
        if let Some(local) = base_local(*place) {
            if is_handle_leaf_local(local) && place_is_owned_handoff_member(*place) {
                carries.insert(local, local);
            }
        }
    }
    if carries.is_empty() {
        return HashSet::new();
    }
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if matches!(src, Place::Local(_)) && matches!(dest, Place::Local(_)) {
                        if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                            if ambiguous.contains(&dl) {
                                continue;
                            }
                            if let Some(&origin) = carries.get(&sl) {
                                match carries.get(&dl).copied() {
                                    Some(existing) if existing != origin => {
                                        carries.remove(&dl);
                                        ambiguous.insert(dl);
                                        changed = true;
                                    }
                                    Some(_) => {}
                                    None => {
                                        carries.insert(dl, origin);
                                        changed = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    let carried_origin = |place: Place| -> Option<u32> {
        base_local(place).and_then(|l| {
            if ambiguous.contains(&l) {
                None
            } else {
                carries.get(&l).copied()
            }
        })
    };

    // Origins whose handle is a field source of a spawn-state `RecordInit`.
    let mut spawn_consumed_origins: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::RecordInit { fields, dest, .. } = instr {
                let Some(dl) = base_local(*dest) else {
                    continue;
                };
                if !spawn_state_locals.contains(&dl) {
                    continue;
                }
                for (_offset, field) in fields {
                    if place_is_owned_handoff_member(*field) {
                        if let Some(origin) = carried_origin(*field) {
                            spawn_consumed_origins.insert(origin);
                        }
                    }
                }
            }
        }
    }
    if spawn_consumed_origins.is_empty() {
        return HashSet::new();
    }

    // Fail-closed disqualification: an origin that ALSO reaches a second owning
    // sink — the `ReturnSlot`, a NON-spawn aggregate construct, or an inline
    // release `Drop` — has more than one candidate free path, so leave it
    // refused (the standalone drop stays and the gate fires). Move-once already
    // forbids most of these, but the scan is the belt to that discipline.
    let mut disqualified: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            match instr {
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src,
                } => {
                    if let Some(o) = carried_origin(*src) {
                        disqualified.insert(o);
                    }
                }
                Instr::RecordInit { fields, dest, .. } => {
                    if base_local(*dest).is_some_and(|d| spawn_state_locals.contains(&d)) {
                        continue;
                    }
                    for (_offset, field) in fields {
                        if let Some(o) = carried_origin(*field) {
                            disqualified.insert(o);
                        }
                    }
                }
                Instr::TupleConstruct { elements, .. } => {
                    for elem in elements {
                        if let Some(o) = carried_origin(*elem) {
                            disqualified.insert(o);
                        }
                    }
                }
                Instr::ClosureEnvInit { fields, .. } => {
                    for field in fields
                        .iter()
                        .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
                    {
                        if let Some(o) = carried_origin(field.src) {
                            disqualified.insert(o);
                        }
                    }
                }
                Instr::Drop {
                    place,
                    drop_fn: Some(_),
                    ..
                } => {
                    if let Some(o) = carried_origin(*place) {
                        disqualified.insert(o);
                    }
                }
                _ => {}
            }
        }
    }

    let mut result = HashSet::new();
    for (binding, _name, ty) in owned_locals {
        if !ty_is_owned_handle_leaf(ty) {
            continue;
        }
        let Some(place) = binding_locals.get(binding) else {
            continue;
        };
        if !place_is_owned_handoff_member(*place) {
            continue;
        }
        let Some(local) = base_local(*place) else {
            continue;
        };
        if spawn_consumed_origins.contains(&local) && !disqualified.contains(&local) {
            result.insert(*binding);
        }
    }
    result
}
/// W3.053 catch-all FAIL-CLOSED gate for the combinatorial owned-handle
/// aggregate-extraction double-free class.
///
/// Invariant enforced: **no owned handle may reach codegen with more than one
/// live free path for the same runtime context.** The M-COW spine emits NO
/// retain when a handle is placed into an aggregate (`TupleConstruct` /
/// `RecordInit` byte-copy the ctx pointer), so the moment an owned-handle source
/// binding's handle is moved into a tuple/record there are TWO aliases of the
/// one runtime context: the source binding's own slot and the aggregate field.
/// That is only safe if EXACTLY ONE of them frees the context. The precise
/// value-flow analyses prove the safe cases and remove one side's drop:
///   - the aggregate's in-place member drop is removed by
///     [`derive_tuple_composite_drop_allowed`] / [`derive_owned_record_drop_allowed`]
///     when the field is extracted into a release-consumer;
///   - the SOURCE binding's standalone drop is removed by
///     [`derive_consumed_local_aggregate_member_bindings`] (extracted into a
///     downstream consumer) or [`derive_returned_aggregate_member_bindings`]
///     (handed to the caller through the `ReturnSlot`).
///
/// The combinatorial hole is everything those proofs do NOT cover —
/// re-aggregation (`let b = (a.0, 2); for n in b.0`), nested aggregates
/// (`((g, 1), 2)`), multi-hop chains, tuple→record re-wraps, and the
/// never-extracted-but-still-double-freed shape (`let r = (g, 99);
/// println(r.1)`, where the tuple's member drop AND the source drop both fire).
/// Each of those leaves the source binding STILL drop-eligible while the
/// aggregate side also frees the context → use-after-free (exit 139 under
/// `MallocScribble`). Rather than chase each sibling shape with another exclusion
/// hop, this gate refuses every such binding.
///
/// Why this is a finite catch-all (not shape-by-shape): the detector does not
/// enumerate shapes. Its seed is the structural fact "this owned-handle source
/// binding's handle was placed into an aggregate field" — and it propagates that
/// taint transitively through whole-value `Move` so a handle that flows into a
/// tuple via any number of rebinds is still seen. A tainted binding is REFUSED
/// unless the precise exclusion analysis already PROVED it freed exactly once
/// (`binding ∈ excluded`). The safe set is finite and proven; everything else is
/// refused. A new aggregate-extraction grammar cannot evade the gate: if it ends
/// up aliasing a handle into an aggregate, the seed fires; if a future exact-once
/// proof clears it, it joins `excluded` and the gate goes silent for it.
///
/// Over-refusal direction (fail-closed): the worst case is refusing a shape a
/// future exact-once proof would accept — a compile error, never a UAF. The
/// proven KEEP cases stay green because their source binding is in `excluded`:
///   - single-hop `let packed = (g, 99); for n in packed.0` →
///     `derive_consumed_local_aggregate_member_bindings`;
///   - return-tuple `let g = pair.0` / `pipe()`-style `(Sink, Stream)` →
///     `derive_returned_aggregate_member_bindings`.
///
/// LESSONS: boundary-fail-closed, raii-null-after-move, drop-allowset-from-value-flow,
/// cleanup-all-exits.
#[allow(
    clippy::too_many_lines,
    clippy::too_many_arguments,
    reason = "one IR-grounded free-count model: ctx-origin propagation + the three \
              drop-source tallies (inline consumer drops, source LIFO drops, \
              aggregate member drops) must share the same origin map; splitting \
              them scatters the exactly-once accounting. The suspend_kinds \
              side-table is threaded alongside the blocks so the bare-Suspend \
              escape-poison can recover a collapsed carrier's moved-out payload"
)]
#[must_use]
pub(super) fn detect_unproven_aggregate_handle_double_free(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
    source_excluded: &HashSet<BindingId>,
    composite_drop_allowed: &HashSet<BindingId>,
) -> Vec<MirCheck> {
    // Record-aware through the single `ty_owns_heap` authority (DIV-1).
    let local_is_heap_owning = |local: u32| -> bool {
        local_tys
            .get(local as usize)
            .is_some_and(|ty| crate::model::ty_owns_heap_mir(ty, record_field_orders, enum_layouts))
    };

    // ── ctx-origin propagation ────────────────────────────────────────────
    // `carries[local]` = the set of owned-handle source-origin locals whose
    // runtime context is (transitively) reachable from `local` — whether `local`
    // IS a handle slot or an aggregate that contains handles, at any nesting
    // depth. Seeded at each owned-handle source binding (carries itself) and
    // grown by a monotone fixpoint:
    //   - `Move { dest, src }`            : dest carries src's origins.
    //   - `TupleConstruct`/`RecordInit`   : dest carries the union of its
    //                                       element/field locals' origins (so a
    //                                       nested aggregate carries the inner
    //                                       handle's origin).
    //   - `TupleFieldLoad`/`RecordFieldLoad`: dest carries the aggregate's
    //                                       origins (over-approx: a field load
    //                                       may extract any contained handle —
    //                                       fail-closed, refuses rather than
    //                                       under-counts).
    // Over-approximation only ever ADDS origins to a carrier, which can only
    // raise a free count → refuse. It never hides a real double-free.
    //
    // An ORIGIN is an owned-HANDLE LEAF only — a `Generator`/`AsyncGenerator`,
    // a `CancellationToken`, or a `Resource`-marker builtin handle
    // (Stream/Sink/Duplex/SendHalf/RecvHalf/LambdaActorHandle). Each owns a
    // single runtime context whose ONLY release path is its handle drop;
    // aliasing it into an aggregate with no retain creates the two-free hazard
    // this gate guards. The NON-OWNING actor-pid leaves
    // (`Pid`/`LocalPid`/`RemotePid`) are deliberately EXCLUDED by
    // `ty_is_owned_handle_leaf`: a pid has no drop glue (its drop is a codegen
    // no-op) and is a copyable reference, so it can never double-free and is
    // never an origin. Plain CoW VALUE leaves (`String`/`Bytes`/`Vec`/`HashMap`/
    // `HashSet`) are deliberately EXCLUDED — their exactly-once is already proven
    // by `derive_cow_sole_owner` / `owned_vec_drop_allowed` (refcount / sole-
    // owner), and a string aliased into a tuple (`let _t = (s, i)`) is a correct,
    // common pattern those analyses admit. An aggregate binding (`(Gen, i64)` /
    // `Holder { gen, .. }`) is never an origin: its handle members are the
    // origins, accounted via the member-drop tally below.
    let mut carries: HashMap<u32, HashSet<u32>> = HashMap::new();
    // SEED from `binding_locals`, the COMPLETE binding→local map — not the
    // `owned_locals` ledger, which the elaborator prunes at loop scope. Every
    // binding whose slot is an owned-handle handoff member (a `let g = gen()`
    // source, a `for n in …` iterator, a consuming param) is a potential
    // double-free origin and seeds itself. Recovering the pruned loop-body
    // bindings is what closes the loop-carried-aggregate edge: their inline
    // back-edge + consumer drops then tally against a seeded origin instead of
    // silently early-returning. `handle_bindings` (deduped by local) is the set
    // the findings loop reports over.
    let mut handle_bindings: Vec<(BindingId, u32)> = Vec::new();
    let mut seeded_locals: HashSet<u32> = HashSet::new();
    for (binding, place) in binding_locals {
        let Some(local) = base_local(*place) else {
            continue;
        };
        let is_handle_leaf = local_tys
            .get(local as usize)
            .is_some_and(ty_is_owned_handle_leaf);
        if is_handle_leaf && place_is_owned_handoff_member(*place) {
            carries.entry(local).or_default().insert(local);
            if seeded_locals.insert(local) {
                handle_bindings.push((*binding, local));
            }
        }
    }
    if carries.is_empty() {
        return Vec::new();
    }
    let synthetic_stream_cursor_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .filter_map(|statement| {
            let MirStatement::Bind { binding, name, .. } = statement else {
                return None;
            };
            name.starts_with(FOR_ITER_CURSOR_NAME_PREFIX)
                .then(|| {
                    binding_locals
                        .get(binding)
                        .and_then(|place| base_local(*place))
                })
                .flatten()
        })
        .collect();
    let merge = |from: &HashSet<u32>, into: &mut HashSet<u32>| -> bool {
        let mut changed = false;
        for &o in from {
            changed |= into.insert(o);
        }
        changed
    };
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                match instr {
                    Instr::Move { dest, src } => {
                        if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                            if let Some(src_origins) = carries.get(&sl).cloned() {
                                changed |= merge(&src_origins, carries.entry(dl).or_default());
                            }
                        }
                    }
                    Instr::TupleConstruct { elements, dest } => {
                        if let Some(dl) = base_local(*dest) {
                            let mut acc: HashSet<u32> = HashSet::new();
                            for elem in elements {
                                if let Some(el) = base_local(*elem) {
                                    if let Some(o) = carries.get(&el) {
                                        for &x in o {
                                            acc.insert(x);
                                        }
                                    }
                                }
                            }
                            changed |= merge(&acc, carries.entry(dl).or_default());
                        }
                    }
                    Instr::RecordInit { fields, dest, .. } => {
                        if let Some(dl) = base_local(*dest) {
                            let mut acc: HashSet<u32> = HashSet::new();
                            for (_offset, field) in fields {
                                if let Some(fl) = base_local(*field) {
                                    if let Some(o) = carries.get(&fl) {
                                        for &x in o {
                                            acc.insert(x);
                                        }
                                    }
                                }
                            }
                            changed |= merge(&acc, carries.entry(dl).or_default());
                        }
                    }
                    Instr::ClosureEnvInit { fields, dest, .. } => {
                        if let Some(dl) = base_local(*dest) {
                            let mut acc: HashSet<u32> = HashSet::new();
                            for field in fields.iter().filter(|field| {
                                field.ownership == ClosureEnvFieldOwnership::OwnsMoved
                            }) {
                                if let Some(fl) = base_local(field.src) {
                                    if let Some(o) = carries.get(&fl) {
                                        for &x in o {
                                            acc.insert(x);
                                        }
                                    }
                                }
                            }
                            changed |= merge(&acc, carries.entry(dl).or_default());
                        }
                    }
                    Instr::TupleFieldLoad { tuple, dest, .. } => {
                        if let (Some(tl), Some(dl)) = (base_local(*tuple), base_local(*dest)) {
                            if let Some(o) = carries.get(&tl).cloned() {
                                changed |= merge(&o, carries.entry(dl).or_default());
                            }
                        }
                    }
                    Instr::RecordFieldLoad { record, dest, .. } => {
                        if let (Some(rl), Some(dl)) = (base_local(*record), base_local(*dest)) {
                            if let Some(o) = carries.get(&rl).cloned() {
                                changed |= merge(&o, carries.entry(dl).or_default());
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        if !changed {
            break;
        }
    }

    // ── returned-origin set (clean move-out, caller-owned) ────────────────
    // An origin whose carrier is written into `Place::ReturnSlot` is handed to
    // the caller by value — the caller's frame now owns its single drop, so it
    // is NOT a double-free in THIS function. Computed from the SAME `carries`
    // fixpoint that drives the tally, so an origin is "returned" only when a
    // TRACKED ownership-transfer path (a `Move`, a `Tuple`/`Record` construct,
    // a field load) carries it to the return slot — e.g. `Ok(g)` / `(s, r)`
    // returned by value. Crucially, the `hew_vec_push_ptr`-style container
    // aliasing the fixpoint deliberately does NOT model is therefore never
    // marked returned: a handle pushed into a vec that is itself returned keeps
    // its escape poison and is still refused, so this exemption can only ever
    // rescue a genuine by-value move-out, never mask the collection-push hazard.
    let mut returned_origins: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Move {
                dest: Place::ReturnSlot,
                src,
            } = instr
            {
                if let Some(sl) = base_local(*src) {
                    if let Some(o) = carries.get(&sl) {
                        returned_origins.extend(o.iter().copied());
                    }
                }
            }
        }
    }
    // Three drop sources free a context; counting them per origin reconstructs
    // the LLVM `hew_gen_coro_destroy` (etc.) site count the elaborator + codegen emit:
    //
    //   1. inline release `Drop { drop_fn: Some(_) }` already in the stream
    //      (the for-in / extraction consumer's free) — frees every origin the
    //      dropped local carries.
    //   2. the SOURCE binding's standalone LIFO drop — emitted iff the binding is
    //      NOT in `source_excluded` (the elaborator's exclusion sets) — frees its
    //      own origin once.
    //   3. each owned-AGGREGATE binding's in-place member drop — emitted iff the
    //      aggregate IS in `composite_drop_allowed` — frees every origin its
    //      handle members carry.
    //
    // An origin freed by ≥2 of these reaches codegen with more than one live free
    // path → refuse. (`source_excluded` ∩ `composite_drop_allowed` is exactly the
    // proven exactly-once bookkeeping: the proven KEEP cases land on a count of 1.)
    let mut free_count: HashMap<u32, u32> = HashMap::new();
    let bump = |origins: &HashSet<u32>, fc: &mut HashMap<u32, u32>| {
        for &o in origins {
            *fc.entry(o).or_insert(0) += 1;
        }
    };

    // Source 1: inline consumer drops.
    let mut inline_drop_sites: HashMap<u32, Vec<(u32, usize)>> = HashMap::new();
    for block in blocks {
        for (index, instr) in block.instructions.iter().enumerate() {
            if let Instr::Drop {
                place,
                drop_fn: Some(_),
                ..
            } = instr
            {
                if let Some(l) = base_local(*place) {
                    if let Some(o) = carries.get(&l).cloned() {
                        for origin in &o {
                            inline_drop_sites
                                .entry(*origin)
                                .or_default()
                                .push((block.id, index));
                        }
                        bump(&o, &mut free_count);
                    }
                }
            }
        }
    }

    // Sources 2 + 3: source LIFO drops and aggregate member drops.
    let mut non_inline_freed: HashSet<u32> = HashSet::new();
    for (binding, _name, ty) in owned_locals {
        let Some(place) = binding_locals.get(binding) else {
            continue;
        };
        let Some(local) = base_local(*place) else {
            continue;
        };
        let Some(origins) = carries.get(&local).cloned() else {
            continue;
        };
        // A handle leaf is an origin (its own standalone drop frees its ctx); an
        // aggregate is anything heap-owning that is NOT a handle leaf (a tuple or
        // record carrying handle members — its in-place member drop frees those
        // members' ctx). CoW value leaves (String/Vec/Bytes) never carry a handle
        // origin, so `carries.get(&local)` is empty for them and they fall out
        // above — the gate ignores them entirely.
        let is_handle = ty_is_owned_handle_leaf(ty) && place_is_owned_handoff_member(*place);
        let is_aggregate = !is_handle && local_is_heap_owning(local);
        if is_handle && !source_excluded.contains(binding) {
            // Source's own standalone drop frees its origin once.
            let mut self_origin = HashSet::new();
            self_origin.insert(local);
            non_inline_freed.insert(local);
            bump(&self_origin, &mut free_count);
        }
        if is_aggregate && composite_drop_allowed.contains(binding) {
            // Aggregate member drop frees every origin its handle members carry.
            non_inline_freed.extend(origins.iter().copied());
            bump(&origins, &mut free_count);
        }
    }

    // A synthetic for-await cursor deliberately carries one cloned inline close
    // on each exit edge. Coalesce those sites only when no source/aggregate drop
    // also frees the origin and no close can reach another while the SAME
    // runtime handle remains in the slot. Loop re-entry may reach a later close
    // only after a `Move` installs a fresh cursor; sequential duplicate closes
    // without that reinitialization remain refused.
    let blocks_by_id: HashMap<u32, &BasicBlock> =
        blocks.iter().map(|block| (block.id, block)).collect();
    let can_reach_same_value = |from: (u32, usize), target: (u32, usize), local: u32| -> bool {
        let reinitializes = |instructions: &[Instr]| {
            instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::Move { dest, .. } if base_local(*dest) == Some(local)
                )
            })
        };
        let Some(from_block) = blocks_by_id.get(&from.0) else {
            return false;
        };
        if from.0 == target.0
            && from.1 < target.1
            && !reinitializes(&from_block.instructions[from.1 + 1..target.1])
        {
            return true;
        }
        if reinitializes(&from_block.instructions[from.1 + 1..]) {
            return false;
        }
        let mut seen = HashSet::new();
        let mut pending = from_block.successors();
        while let Some(id) = pending.pop() {
            if !seen.insert(id) {
                continue;
            }
            let Some(block) = blocks_by_id.get(&id) else {
                continue;
            };
            if id == target.0 {
                if !reinitializes(&block.instructions[..target.1]) {
                    return true;
                }
                continue;
            }
            if reinitializes(&block.instructions) {
                continue;
            }
            pending.extend(block.successors());
        }
        false
    };
    let path_exclusive_inline_frees: HashSet<u32> = synthetic_stream_cursor_locals
        .iter()
        .copied()
        .filter(|origin| {
            if non_inline_freed.contains(origin) {
                return false;
            }
            let Some(sites) = inline_drop_sites.get(origin) else {
                return false;
            };
            let unique_sites: HashSet<(u32, usize)> = sites.iter().copied().collect();
            if sites.len() < 2 || unique_sites.len() != sites.len() {
                return false;
            }
            let sites: Vec<(u32, usize)> = unique_sites.into_iter().collect();
            for (index, &left) in sites.iter().enumerate() {
                for &right in &sites[index + 1..] {
                    if can_reach_same_value(left, right, *origin)
                        || can_reach_same_value(right, left, *origin)
                    {
                        return false;
                    }
                }
            }
            true
        })
        .collect();

    // ── escape poisoning ──────────────────────────────────────────────────
    // The per-origin tally above models the drops the elaborator emits or
    // reconstructs, but it only sees drops. A carrier read by an operation that
    // ALIASES THE HANDLE OUT of this function's tracked dataflow — moved into a
    // `Vec`/`HashMap`/aggregate field, captured by a closure, spawned into a
    // task/actor, erased into a `dyn` box, sent as an actor-message payload, or
    // passed by value to a call (a user fn, or a runtime collection-push helper
    // such as `hew_vec_push_ptr`, both lowered as `Terminator::Call`) — reaches
    // a second, untracked free path (the container's / callee's drop) on top of
    // its own source drop. That is precisely the collection-push and
    // cross-call re-aggregation hazard the instruction fixpoint cannot model,
    // so we fail closed: every origin such an escape carries is poisoned and
    // refused. Borrowing reads (`.next()`, identity compare, cancellation
    // check, lock acquire/release, field LOADs, the actor-pid of a send/ask)
    // keep the handle's sole-owner drop intact and are NOT escapes.
    let mut poisoned: HashSet<u32> = HashSet::new();
    let poison = |place: Place, poisoned: &mut HashSet<u32>| {
        let Some(local) = base_local(place) else {
            return;
        };
        // Only an escaped place whose STATIC TYPE can actually carry a handle
        // can alias one out. A copy-on-write sibling extracted from a mixed
        // aggregate (e.g. the `Vec<i64>` field of a `(Generator, Vec<i64>, …)`
        // tuple, whose origin set the fail-closed field-load over-approximates
        // to include the generator) is provably handle-free, so reading it must
        // NOT poison the generator origin. This refinement only ever removes
        // false-positive poisons — it never adds one — so it cannot mask a real
        // double-free.
        let may_carry = local_tys
            .get(local as usize)
            .is_some_and(|t| crate::model::ty_may_carry_owned_handle(t, enum_layouts));
        if !may_carry {
            return;
        }
        if let Some(origins) = carries.get(&local) {
            poisoned.extend(origins.iter().copied());
        }
    };
    for block in blocks {
        for instr in &block.instructions {
            for place in instr_escape_places(instr) {
                poison(place, &mut poisoned);
            }
        }
        for place in
            terminator_escape_places(&block.terminator, suspend_kinds.get(&block.id), local_tys)
        {
            poison(place, &mut poisoned);
        }
    }

    // Name/type metadata for the diagnostic, recovered from the `Bind`
    // statement stream (which retains the loop-scoped bindings the
    // `owned_locals` ledger prunes), with `owned_locals` as a fallback.
    let mut bind_info: HashMap<BindingId, (String, ResolvedTy)> = HashMap::new();
    for block in blocks {
        for stmt in &block.statements {
            if let MirStatement::Bind {
                binding, name, ty, ..
            } = stmt
            {
                bind_info
                    .entry(*binding)
                    .or_insert_with(|| (name.clone(), ty.clone()));
            }
        }
    }
    for (binding, name, ty) in owned_locals {
        bind_info
            .entry(*binding)
            .or_insert_with(|| (name.clone(), ty.clone()));
    }

    // Refuse every owned-HANDLE-leaf binding whose origin is freed more than
    // once OR aliased out past the tracked dataflow. Report against the
    // user-named handle binding.
    let mut findings = Vec::new();
    let mut refused: HashSet<BindingId> = HashSet::new();
    for (binding, local) in &handle_bindings {
        let fc = free_count.get(local).copied().unwrap_or(0);
        let over_freed = fc > 1 && !path_exclusive_inline_frees.contains(local);
        let escaped = poisoned.contains(local);
        let returned = returned_origins.contains(local);
        // Refuse when the origin is freed more than once on tracked paths, OR
        // when it is aliased OUT of tracked dataflow (`escaped`) while ALSO
        // retaining an independent in-frame free (`fc >= 1`) and NOT being
        // cleanly moved out to the caller (`!returned`). A pure consuming move
        // with no residual in-frame drop (`fc == 0`: `handle.close()`,
        // `hew_stream_pipe(s, d)`) and a borrow-then-return (`is_valid(s)`
        // followed by `Ok(s)`) are both the proven exactly-once shapes the gate
        // must preserve.
        if !(over_freed || (escaped && fc >= 1 && !returned)) {
            continue;
        }
        if !refused.insert(*binding) {
            continue;
        }
        let (name, ty) = bind_info.get(binding).cloned().unwrap_or_else(|| {
            (
                format!("local{local}"),
                local_tys
                    .get(*local as usize)
                    .cloned()
                    .unwrap_or(ResolvedTy::Unit),
            )
        });
        findings.push(MirCheck::OwnedHandleAggregateDoubleFree {
            binding: *binding,
            name,
            handle_ty: render_owned_handle_ty(&ty),
            overwrite: false,
            owner: AggregateOwner::Record,
        });
    }
    findings
}
/// RAII-1 fail-closed gate: refuse any unsupported aggregate operation on a
/// `#[resource] #[opaque]` handle FIELD — both PROJECTING it OUT of its owning
/// record (`let d = h.dq`, `h.dq.close()`, `f(h.dq)`; a `RecordFieldLoad`) and
/// OVERWRITING it in place (`h.dq = src`; a `RecordFieldStore`).
///
/// A field-bearing record with an opaque-resource field is admitted to the
/// owned-aggregate set and freed by the recursive `__hew_record_drop_inplace_<R>`
/// thunk, which runs the field's user `close(self)` exactly once on every exit
/// path. Two user operations defeat that exactly-once contract, both because the
/// handle is a pointer-width value the M-COW spine copies with NO null-after-move
/// on the source slot:
///   * EXTRACTION (`let d = h.dq`): the record's thunk AND the extracted handle's
///     consumer / scope-exit drop both free the one runtime context — a
///     double-free (abort under `MallocScribble`).
///   * OVERWRITE (`h.dq = src`): the store raw-overwrites the slot, so the OLD
///     handle is lost without its `close` (a leak) and `src` is byte-copied in
///     with no move/null discipline (a second owner that double-frees at its own
///     drop).
///
/// Until overwrite-release + source-slot null-after-move lands (RAII-2), the
/// compiler refuses both rather than emit the leak / double-free.
///
/// Narrow by construction: keyed on the opaque-resource type being the LOADED
/// field (`RecordFieldLoad.dest`) or the STORED value (`RecordFieldStore.src`,
/// which for a well-typed store IS the field type) — the W3.029-admitted set
/// carried in `resource_opaque_close` — so a plain `#[opaque]` handle with no
/// close (`json.Value`) and every non-resource field access are untouched.
/// `RecordInit` construction, whole-record move, and the codegen-synthesised
/// drop thunk never produce these field-load/store instrs, so the auto-drop spine
/// never trips this gate — only a user-written extraction or reassignment of the
/// resource leaf does. Reuses the W3.053 aggregate diagnostic
/// (`OwnedHandleAggregateExtractionUnsupported`, with `overwrite` selecting the
/// wording): the failure mode and the fail-closed rationale are identical.
///
/// LESSONS: boundary-fail-closed, raii-null-after-move; sibling of the
/// builtin-handle W3.053 gate [`detect_unproven_aggregate_handle_double_free`].
pub(super) fn detect_opaque_resource_field_misuse(
    blocks: &[BasicBlock],
    local_tys: &[ResolvedTy],
    binding_locals: &HashMap<BindingId, Place>,
    opaque_resource_names: &HashSet<String>,
) -> Vec<MirCheck> {
    if opaque_resource_names.is_empty() {
        return Vec::new();
    }
    let is_user_opaque_resource = |ty: &ResolvedTy| -> bool {
        // The registry (`resource_opaque_close`) is the authoritative
        // opaque-resource set — already filtered to `#[opaque]` ∩
        // `ResourceMarker::Resource` ∩ user-`close`. A MIR local's `is_opaque`
        // flag is NOT reliably propagated (the field-load dest arrives as
        // `is_opaque: false` even for a `#[opaque]` type), so match on the
        // resolved type NAME against the registry, not the flag: within a
        // compilation a name resolves to exactly one type, so a `Named` whose
        // name is in the registry IS that opaque resource. Full-name match with
        // a short-name fallback bridges any module-prefix asymmetry; the only
        // residual (a cross-module short-name twin) over-refuses — a compile
        // error, never a double-free (boundary-fail-closed).
        matches!(
            ty,
            ResolvedTy::Named { name, .. }
                if opaque_resource_names.contains(name.as_str())
                    || opaque_resource_names.contains(short_name(name))
        )
    };
    // local → the user binding it carries (for the diagnostic name): the
    // extracted dest for a load, the mutated record for a store.
    let mut local_to_binding: HashMap<u32, BindingId> = HashMap::new();
    for (binding, place) in binding_locals {
        if let Some(local) = base_local(*place) {
            local_to_binding.entry(local).or_insert(*binding);
        }
    }
    let mut bind_names: HashMap<BindingId, String> = HashMap::new();
    for block in blocks {
        for stmt in &block.statements {
            if let MirStatement::Bind { binding, name, .. } = stmt {
                bind_names.entry(*binding).or_insert_with(|| name.clone());
            }
        }
    }
    // Resolve a stable binding id + human name for `local`, falling back to the
    // rendered handle type when the local carries no user binding (a temporary).
    let name_for = |local: u32, ty: &ResolvedTy| -> (BindingId, String) {
        let binding = local_to_binding
            .get(&local)
            .copied()
            .unwrap_or(BindingId(local));
        let name = local_to_binding
            .get(&local)
            .and_then(|b| bind_names.get(b))
            .cloned()
            .unwrap_or_else(|| render_owned_handle_ty(ty));
        (binding, name)
    };
    let mut findings = Vec::new();
    let mut seen_load: HashSet<u32> = HashSet::new();
    let mut seen_store: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            match instr {
                // Projecting the resource leaf OUT of the record.
                Instr::RecordFieldLoad { dest, .. } => {
                    let Some(dl) = base_local(*dest) else {
                        continue;
                    };
                    let Some(ty) = local_tys.get(dl as usize) else {
                        continue;
                    };
                    if !is_user_opaque_resource(ty) || !seen_load.insert(dl) {
                        continue;
                    }
                    let (binding, name) = name_for(dl, ty);
                    findings.push(MirCheck::OwnedHandleAggregateDoubleFree {
                        binding,
                        name,
                        handle_ty: render_owned_handle_ty(ty),
                        overwrite: false,
                        owner: AggregateOwner::Record,
                    });
                }
                // Overwriting the resource leaf IN PLACE within the record.
                // The stored value's type IS the field type for a well-typed
                // `h.dq = src` (nominal: `src` must be the field's opaque
                // resource). Name the violation by the MUTATED record — the
                // aggregate whose old field handle is dropped on the floor.
                Instr::RecordFieldStore { record, src, .. } => {
                    let Some(sl) = base_local(*src) else {
                        continue;
                    };
                    let Some(ty) = local_tys.get(sl as usize) else {
                        continue;
                    };
                    if !is_user_opaque_resource(ty) || !seen_store.insert(sl) {
                        continue;
                    }
                    let name_local = base_local(*record).unwrap_or(sl);
                    let (binding, name) = name_for(name_local, ty);
                    findings.push(MirCheck::OwnedHandleAggregateDoubleFree {
                        binding,
                        name,
                        handle_ty: render_owned_handle_ty(ty),
                        overwrite: true,
                        owner: AggregateOwner::Record,
                    });
                }
                _ => {}
            }
        }
    }
    findings
}
/// True when overwriting an actor-state field of this classified kind would
/// silently leak the previous handle — the exact #2654 hazard.
///
/// The gated kinds are those whose scope-exit / actor-shutdown drop runs a real
/// close AND whose `ActorStateFieldStore` has NO release-before-store today:
///
///   - [`StateFieldCloneKind::Resource`] — a user `#[resource] #[opaque]` handle
///     whose `close(self)` runs once in `__hew_state_drop_<A>`; the store's
///     `lower_actor_state_field_store` no-release arm groups it with the other
///     opaque kinds and emits a bare `store` (the leak this gate fences).
///   - [`StateFieldCloneKind::IoHandle`] with a pointer-backed handle whose drop
///     actually frees/closes (`Stream`→`hew_stream_close`, `Sink`→`hew_sink_close`,
///     `Generator`→`hew_gen_coro_destroy`, `CancellationToken`→
///     `hew_cancel_token_release`). Same no-release store arm; the descriptor /
///     coroutine frame / refcount of the OLD handle leaks on overwrite.
///
/// NOT gated (no leak on overwrite, so no refusal):
///
///   - kinds with an existing release-before-store — `String`/`Bytes`/`Vec`/
///     `HashMap`/`HashSet` go through `emit_state_field_old_value_release`'s
///     pointer-inequality guard, which releases the old payload before the store;
///   - `IoHandle::Connection` — its state-level drop is a no-op (the fd is torn
///     down by the runtime's actor-teardown, not the state drop), so overwriting
///     the slot leaks nothing;
///   - no-close `OpaqueHandle` (e.g. `json.Value`) and `BitCopy` — no owned
///     resource to leak.
fn actor_state_kind_leaks_on_overwrite(kind: &crate::state_clone::StateFieldCloneKind) -> bool {
    use crate::state_clone::{IoHandleKind, StateFieldCloneKind};
    matches!(
        kind,
        StateFieldCloneKind::Resource { .. }
            | StateFieldCloneKind::IoHandle {
                kind: IoHandleKind::Stream
                    | IoHandleKind::Sink
                    | IoHandleKind::Generator
                    | IoHandleKind::CancellationToken,
            }
    )
}
/// #2654 fail-closed gate: refuse an in-place overwrite of an actor-state field
/// (`self.dq = src`, lowering to `Instr::ActorStateFieldStore`) whose classified
/// kind leaks the previous handle on overwrite.
///
/// This is the actor-state sibling of the record `RecordFieldStore` overwrite arm
/// in [`detect_opaque_resource_field_misuse`]: the SAME exactly-once-close
/// invariant, the SAME fail-closed posture (LESSONS boundary-fail-closed,
/// raii-null-after-move, lifecycle-symmetry). Codegen's actor-state store has no
/// release-before-store for these kinds (`lower_actor_state_field_store`
/// no-release arm; `emit_overwrite_neutralize_leaves` null-the-slot arms), so the
/// old handle's `close` never runs (leak) while the actor's shutdown drop
/// (`__hew_state_drop_<A>`) double-owns the freshly-stored handle. A safe
/// release-before-store is deferred to RAII-2 (retain-to-compare + source-slot
/// null-after-move); until then the operation is refused, exactly as the
/// structurally identical record store already is on HEAD.
///
/// Reuses the authoritative per-field classification
/// ([`ActorLayout::state_field_clone_kinds`]) — the same vector the drop-body
/// synthesis and clone/drop registration consume — so the gate cannot drift from
/// the exact kinds whose drop it fences.
///
/// Overwrite only: actor-state resource *extraction* (`let x = self.dq`, giving
/// `x` independent drop authority) is a distinct latent concern, out of scope for
/// #2654 — this gate never inspects `ActorStateFieldLoad`.
pub(super) fn detect_actor_state_resource_overwrite(
    blocks: &[BasicBlock],
    state_field_clone_kinds: &[crate::state_clone::StateFieldCloneKind],
    state_field_names: &[String],
    state_field_tys: &[ResolvedTy],
) -> Vec<MirCheck> {
    if state_field_clone_kinds.is_empty() {
        return Vec::new();
    }
    let mut findings = Vec::new();
    let mut seen: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            let Instr::ActorStateFieldStore { field_offset, .. } = instr else {
                continue;
            };
            let idx = field_offset.0;
            let Some(kind) = state_field_clone_kinds.get(idx as usize) else {
                continue;
            };
            if !actor_state_kind_leaks_on_overwrite(kind) || !seen.insert(idx) {
                continue;
            }
            // Name the violation by the mutated state field; fall back to the
            // rendered handle type when the layout carries no name (mirrors the
            // record gate's `name_for`).
            let name = state_field_names
                .get(idx as usize)
                .filter(|n| !n.is_empty())
                .cloned()
                .unwrap_or_else(|| {
                    state_field_tys
                        .get(idx as usize)
                        .map_or_else(|| format!("field{idx}"), render_owned_handle_ty)
                });
            let handle_ty = state_field_tys
                .get(idx as usize)
                .map_or_else(|| name.clone(), render_owned_handle_ty);
            findings.push(MirCheck::OwnedHandleAggregateDoubleFree {
                // No source-level `BindingId` for an actor-state field; the
                // field offset is a stable synthetic id (dedup is by offset
                // above, and `check_to_diagnostic` ignores the binding).
                binding: BindingId(idx),
                name,
                handle_ty,
                overwrite: true,
                owner: AggregateOwner::ActorState,
            });
        }
    }
    findings
}
/// CAP-08 fail-closed gate: refuse an explicit CONSUMING close on an owned
/// builtin-handle held in ACTOR STATE (`sink.close()` on the bare state field —
/// an `Instr::ActorStateFieldLoad` whose `dest` becomes the receiver of a
/// `consumes_receiver` runtime call, e.g. `hew_sink_close` / `hew_stream_close`).
///
/// The handle is owned by the actor's synthesised `state_drop_fn`, which closes
/// it EXACTLY ONCE at teardown (Stream→`hew_stream_close` / Sink→`hew_sink_close`;
/// the runtime close is an UNGUARDED `Box::from_raw`). A handler that also closes
/// it frees the one runtime context twice → a double-free (verified: exit 139
/// under `MallocScribble`; two `hew_sink_close` sites in the emitted IR). This is
/// the consume/extraction sibling of [`detect_actor_state_resource_overwrite`]
/// (the store/overwrite gate) — the SAME exactly-once-close invariant, the SAME
/// fail-closed posture, and it mirrors the `#[resource]` handle posture
/// (`detect_opaque_resource_field_misuse` refuses `h.dq.close()`): a resource
/// handle in actor state is closed only by teardown.
///
/// The consuming close is a `Terminator::Call` whose `builtin` family reports
/// `consumes_receiver()`; the receiver is `args[0]`, traced back (through
/// whole-value `Move`) to the `ActorStateFieldLoad` dest. Reuses the same
/// authoritative per-field classification the overwrite gate consumes
/// (`ActorLayout::state_field_clone_kinds` → `actor_state_kind_leaks_on_overwrite`)
/// so the two gates fence exactly the same close-bearing handle set.
///
/// Direction: refuse rather than emit the double-free (over-refusal is a compile
/// error, never a UAF). To signal EOF, close a sink owned as a LOCAL, or let the
/// actor's teardown close the state-held half.
///
/// LESSONS: boundary-fail-closed, raii-null-after-move, cleanup-all-exits,
/// lifecycle-symmetry.
pub(super) fn detect_actor_state_handle_consume(
    blocks: &[BasicBlock],
    state_field_clone_kinds: &[crate::state_clone::StateFieldCloneKind],
    state_field_names: &[String],
    state_field_tys: &[ResolvedTy],
) -> Vec<MirCheck> {
    if state_field_clone_kinds.is_empty() {
        return Vec::new();
    }
    // Locals carrying a close-bearing handle loaded out of an actor state field,
    // mapped to their field offset; grown forward through whole-value `Move`.
    let mut handle_field_local: HashMap<u32, u32> = HashMap::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::ActorStateFieldLoad {
                field_offset, dest, ..
            } = instr
            {
                let idx = field_offset.0;
                let Some(kind) = state_field_clone_kinds.get(idx as usize) else {
                    continue;
                };
                if !actor_state_kind_leaks_on_overwrite(kind) {
                    continue;
                }
                if let Some(dl) = base_local(*dest) {
                    handle_field_local.insert(dl, idx);
                }
            }
        }
    }
    if handle_field_local.is_empty() {
        return Vec::new();
    }
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        if let Some(&idx) = handle_field_local.get(&sl) {
                            if handle_field_local.insert(dl, idx).is_none() {
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    let mut findings = Vec::new();
    let mut seen: HashSet<u32> = HashSet::new();
    for block in blocks {
        let Terminator::Call {
            builtin: Some(family),
            args,
            ..
        } = &block.terminator
        else {
            continue;
        };
        if !family.consumes_receiver() {
            continue;
        }
        let Some(receiver) = args.first() else {
            continue;
        };
        let Some(rl) = base_local(*receiver) else {
            continue;
        };
        let Some(&idx) = handle_field_local.get(&rl) else {
            continue;
        };
        if !seen.insert(idx) {
            continue;
        }
        let name = state_field_names
            .get(idx as usize)
            .filter(|n| !n.is_empty())
            .cloned()
            .unwrap_or_else(|| {
                state_field_tys
                    .get(idx as usize)
                    .map_or_else(|| format!("field{idx}"), render_owned_handle_ty)
            });
        let handle_ty = state_field_tys
            .get(idx as usize)
            .map_or_else(|| name.clone(), render_owned_handle_ty);
        findings.push(MirCheck::OwnedHandleAggregateDoubleFree {
            binding: BindingId(idx),
            name,
            handle_ty,
            overwrite: false,
            owner: AggregateOwner::ActorState,
        });
    }
    findings
}
#[cfg(test)]
mod owned_record_drop_derivation {
    //! Direct structural tests for `derive_owned_record_drop_allowed` — the
    //! value-class-capstone fail-closed sole-owner gate for owned-aggregate
    //! records passed/returned by value. These poke the derivation with
    //! synthetic MIR blocks: a returned record (escape) must be EXCLUDED so its
    //! `RecordInPlace` drop never double-frees the escapee's fields, while a
    //! field-read-only record must be ADMITTED so its heap fields are freed.
    //!
    //! The headline `one_arm_consume_*` pair pins the audit-#5 reconciliation:
    //! a record consumed (returned) on one branch but live on another is gated
    //! by this per-exit escape analysis, NOT by a path-insensitive global
    //! `owned_locals` removal.
    use super::*;

    /// An owned record named "Rec"; everything else is not a record candidate.
    fn rec_ty() -> ResolvedTy {
        ResolvedTy::named_user("Rec", vec![])
    }

    fn vec_string_ty() -> ResolvedTy {
        ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String])
    }

    fn is_rec(ty: &ResolvedTy) -> bool {
        matches!(ty, ResolvedTy::Named { name, .. } if name == "Rec")
    }

    fn is_vec_handle(ty: &ResolvedTy) -> bool {
        matches!(
            ty,
            ResolvedTy::Named {
                builtin: Some(BuiltinType::Vec),
                ..
            }
        )
    }

    fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![],
            instructions,
            terminator,
        }
    }

    fn derive(
        blocks: &[BasicBlock],
        owned: &[(BindingId, String, ResolvedTy)],
        binding_locals: &HashMap<BindingId, Place>,
        local_tys: &[ResolvedTy],
    ) -> HashSet<BindingId> {
        // `Rec` carries a heap-owning `string` field so the unified
        // `ty_owns_heap` authority (record-aware) classifies a `Rec`-typed
        // field-load binder as heap-owning — the verdict the candidate
        // predicate `is_rec` selects on, kept in agreement now that the
        // record-blindness workaround is gone (DIV-1).
        let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
        record_field_orders.insert(
            "Rec".to_string(),
            vec![("label".to_string(), ResolvedTy::String)],
        );
        derive_owned_record_drop_allowed(
            blocks,
            &HashMap::new(),
            owned,
            binding_locals,
            local_tys,
            &is_rec,
            &record_field_orders,
            &[],
            &[],
        )
    }

    /// A record local that is never read (no construction-site escape, no field
    /// read) is its own sole owner and must be admitted for `RecordInPlace`.
    #[test]
    fn untouched_record_local_is_admitted() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        let local_tys = vec![rec_ty()];

        let allowed = derive(
            &[block(0, vec![], Terminator::Return)],
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            allowed.contains(&b),
            "an untouched owned record is its own sole owner and must be admitted"
        );
    }

    /// A record read via `RecordFieldLoad` of a `BitCopy` field stays the sole
    /// owner — the field read is an interior read, not an escape — and must be
    /// admitted (so its other owned fields are freed at scope exit).
    #[test]
    fn record_bitcopy_field_read_is_admitted() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        // local 1 receives the loaded BitCopy field (i64).
        let local_tys = vec![rec_ty(), ResolvedTy::I64];
        let instrs = vec![Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(1),
            dest: Place::Local(1),
        }];

        let allowed = derive(
            &[block(0, instrs, Terminator::Return)],
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            allowed.contains(&b),
            "a record whose only use is reading a BitCopy field stays the sole \
             owner and must be admitted; got {allowed:?}"
        );
    }

    /// A record moved whole-value into the `ReturnSlot` (returned) has escaped:
    /// the caller owns its fields now, so it must be EXCLUDED — dropping it
    /// would double-free the returned value's heap fields.
    #[test]
    fn returned_record_is_excluded() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        let local_tys = vec![rec_ty()];
        let instrs = vec![Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(0),
        }];

        let allowed = derive(
            &[block(0, instrs, Terminator::Return)],
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            !allowed.contains(&b),
            "a returned record escaped to the caller and must be excluded from \
             the scope-exit drop (else double-free); got {allowed:?}"
        );
    }

    /// An owned (string) field loaded out of the record and moved into the
    /// `ReturnSlot` means the field escaped: the record must be EXCLUDED so it
    /// does not double-free the returned field.
    #[test]
    fn escaped_owned_field_excludes_record() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        // local 1 receives the loaded owned (string) field.
        let local_tys = vec![rec_ty(), ResolvedTy::String];
        let instrs = vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ];

        let allowed = derive(
            &[block(0, instrs, Terminator::Return)],
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            !allowed.contains(&b),
            "an owned field loaded out and returned escaped the record; the \
             record must be excluded to avoid double-freeing it; got {allowed:?}"
        );
    }

    /// A `FieldDropInPlace` addressing the candidate root is BOTH the field
    /// extraction and its release, yet it mints no load dest and no `Drop`
    /// place — with bitcopy-only sibling binders it seeds neither
    /// `field_binders` nor `release_owner_bases`, so only the direct
    /// prover-exclusion rule suppresses the composite. Without it the
    /// `RecordInPlace` drop would re-walk the freed field's leaves
    /// (double-free; inline composites carry no null-store).
    #[test]
    fn field_drop_in_place_on_root_excludes_record() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        let local_tys = vec![rec_ty()];
        let instrs = vec![Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: crate::model::FieldAddr::Record(FieldOffset(0)),
            ty: ResolvedTy::String,
        }];

        let allowed = derive(
            &[block(0, instrs, Terminator::Return)],
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            !allowed.contains(&b),
            "a record root addressed by FieldDropInPlace already discharged \
             that field's release; admitting its composite drop would re-walk \
             the freed field (double-free); got {allowed:?}"
        );
    }

    /// A `FieldDropInPlace` whose base is a field BINDER — the extracted
    /// member alias `let inner = outer.field; match inner { Inner { a, b: _ }
    /// => … }` — discharges a field of the OUTER root's storage through the
    /// binder's byte-copy (the null-store lands in the binder's slot, never
    /// the root's). The direct rule must resolve the binder through its
    /// provenance and exclude exactly the root it was loaded from, while a
    /// sibling root the binder never touched stays admitted. Red-before: the
    /// rule consulted `alias_of` only, the loaded-from root stayed admitted,
    /// and its `RecordInPlace` re-walked the freed field — the reproduced
    /// Guard-Malloc double-free.
    #[test]
    fn field_drop_in_place_on_field_binder_excludes_loaded_root_only() {
        let outer = BindingId(1);
        let other = BindingId(2);
        let owned = vec![
            (outer, "outer".to_string(), rec_ty()),
            (other, "other".to_string(), rec_ty()),
        ];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(outer, Place::Local(0));
        binding_locals.insert(other, Place::Local(1));
        // local 2: the extracted heap-owning member binder (`let inner =
        // outer.field`); local 3: the match scrutinee copy of the binder.
        let local_tys = vec![rec_ty(), rec_ty(), rec_ty(), rec_ty()];
        let instrs = vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(2),
            },
            Instr::Move {
                dest: Place::Local(3),
                src: Place::Local(2),
            },
            Instr::FieldDropInPlace {
                base: Place::Local(3),
                field: crate::model::FieldAddr::Record(FieldOffset(1)),
                ty: ResolvedTy::String,
            },
        ];

        let allowed = derive(
            &[block(0, instrs, Terminator::Return)],
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            !allowed.contains(&outer),
            "a FieldDropInPlace against a field binder freed part of the \
             loaded-from root's storage through the binder's byte-alias; the \
             root must be excluded or its composite walk re-frees it \
             (double-free); got {allowed:?}"
        );
        assert!(
            allowed.contains(&other),
            "the binder's provenance names the loaded-from root uniquely; a \
             sibling root it never touched keeps its composite drop \
             (precision pin); got {allowed:?}"
        );
    }

    /// `hew_vec_push_owned` / `hew_vec_set_owned` copy their element into the
    /// destination Vec. The collection-local prover exempts that element operand,
    /// while composite binder scans still treat the tail operand as an escape.
    #[test]
    fn vec_copy_in_tail_split_keeps_local_candidate_but_excludes_composite_binder() {
        let callee = "hew_vec_push_owned";
        let call_builtin = hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(callee);

        let record = BindingId(10);
        let record_owned = vec![(record, "r".to_string(), rec_ty())];
        let record_binding_locals: HashMap<BindingId, Place> =
            [(record, Place::Local(0))].into_iter().collect();
        let record_local_tys = vec![rec_ty(), vec_string_ty(), vec_string_ty()];
        let record_blocks = vec![block(
            0,
            vec![Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            }],
            Terminator::Call {
                callee: callee.to_string(),
                builtin: call_builtin,
                args: vec![Place::Local(2), Place::Local(1)],
                dest: None,
                next: 1,
            },
        )];

        let record_allowed = derive(
            &record_blocks,
            &record_owned,
            &record_binding_locals,
            &record_local_tys,
        );
        assert!(
            !record_allowed.contains(&record),
            "a composite field binder used as the copy-in element operand is a \
             tail read and must exclude the composite; allowed: {record_allowed:?}"
        );

        let elem = BindingId(11);
        let collection_owned = vec![(elem, "elem".to_string(), vec_string_ty())];
        let collection_binding_locals: HashMap<BindingId, Place> =
            [(elem, Place::Local(1))].into_iter().collect();
        let collection_blocks = vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: callee.to_string(),
                builtin: call_builtin,
                args: vec![Place::Local(2), Place::Local(1)],
                dest: None,
                next: 1,
            },
        }];

        let collection_allowed = derive_local_collection_drop_allowed(
            &collection_blocks,
            &HashMap::new(),
            &collection_owned,
            &collection_binding_locals,
            &HashMap::new(),
            is_vec_handle,
        );
        assert!(
            collection_allowed.contains(&elem),
            "a local collection candidate used as the copy-in element operand is \
             borrowed for clone and must keep its drop admission; allowed: \
             {collection_allowed:?}"
        );
    }

    /// One-arm-consume (audit #5): a record consumed (returned) on one branch
    /// and field-read on the other. The whole-record escape on the consume arm
    /// excludes the binding fail-closed (over-exclusion leaks on the live arm,
    /// never double-frees on the consume arm). This pins that the gate is the
    /// per-exit escape analysis — not the path-insensitive global removal that
    /// would silently produce the same exclusion but for the wrong reason and
    /// could not be tightened to per-arm precision later.
    #[test]
    fn one_arm_consume_record_is_excluded_fail_closed() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        let local_tys = vec![rec_ty(), ResolvedTy::I64];
        // bb0: branch to bb1 (consume) / bb2 (live).
        let bb0 = block(
            0,
            vec![],
            Terminator::Branch {
                cond: Place::Local(1),
                then_target: 1,
                else_target: 2,
            },
        );
        // bb1: return the record (consume / escape).
        let bb1 = block(
            1,
            vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(0),
            }],
            Terminator::Return,
        );
        // bb2: only read a BitCopy field (live; would be dropped here).
        let bb2 = block(
            2,
            vec![Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(1),
                dest: Place::Local(1),
            }],
            Terminator::Return,
        );

        let allowed = derive(&[bb0, bb1, bb2], &owned, &binding_locals, &local_tys);
        assert!(
            !allowed.contains(&b),
            "a record consumed on one arm must be excluded fail-closed (the \
             escape on the consume arm wins); got {allowed:?}"
        );
    }

    /// Companion to the one-arm-consume case: a record that is field-read on
    /// BOTH arms (never escapes) must be ADMITTED — the escape analysis does not
    /// over-exclude a record merely because it is branched on.
    #[test]
    fn record_live_on_both_arms_is_admitted() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        let local_tys = vec![rec_ty(), ResolvedTy::I64];
        let bb0 = block(
            0,
            vec![],
            Terminator::Branch {
                cond: Place::Local(1),
                then_target: 1,
                else_target: 2,
            },
        );
        let field_read = |dest| Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(1),
            dest,
        };
        let bb1 = block(1, vec![field_read(Place::Local(1))], Terminator::Return);
        let bb2 = block(2, vec![field_read(Place::Local(1))], Terminator::Return);

        let allowed = derive(&[bb0, bb1, bb2], &owned, &binding_locals, &local_tys);
        assert!(
            allowed.contains(&b),
            "a record field-read on both arms never escapes and must be admitted \
             so its heap fields are freed; got {allowed:?}"
        );
    }

    /// #2212 attribution: a field-binder escape provably traced to ONE root
    /// excludes exactly that root — an unrelated record candidate in the same
    /// function keeps its composite drop (pre-attribution the blanket
    /// exclusion leaked every record's fields on any field escape).
    #[test]
    fn attributed_field_escape_keeps_unrelated_root_admitted() {
        let escaping = BindingId(1);
        let unrelated = BindingId(2);
        let owned = vec![
            (escaping, "a".to_string(), rec_ty()),
            (unrelated, "b".to_string(), rec_ty()),
        ];
        let binding_locals: HashMap<BindingId, Place> =
            [(escaping, Place::Local(0)), (unrelated, Place::Local(1))]
                .into_iter()
                .collect();
        // local 2 receives the loaded owned (string) field of local 0.
        let local_tys = vec![rec_ty(), rec_ty(), ResolvedTy::String];
        let instrs = vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(2),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(2),
            },
        ];

        let allowed = derive(
            &[block(0, instrs, Terminator::Return)],
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            !allowed.contains(&escaping),
            "the escaped field's root must stay excluded; got {allowed:?}"
        );
        assert!(
            allowed.contains(&unrelated),
            "a record no binder of which escaped must keep its composite drop \
             under per-root attribution; got {allowed:?}"
        );
    }

    /// #2212 fail-closed boundary: a binder loaded from TWO different roots
    /// has ambiguous provenance — its escape must exclude EVERY record root
    /// (the pre-attribution blanket), never guess one.
    #[test]
    fn ambiguous_binder_escape_excludes_every_root() {
        let first = BindingId(1);
        let second = BindingId(2);
        let owned = vec![
            (first, "a".to_string(), rec_ty()),
            (second, "b".to_string(), rec_ty()),
        ];
        let binding_locals: HashMap<BindingId, Place> =
            [(first, Place::Local(0)), (second, Place::Local(1))]
                .into_iter()
                .collect();
        let local_tys = vec![rec_ty(), rec_ty(), ResolvedTy::String];
        let instrs = vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(2),
            },
            Instr::RecordFieldLoad {
                record: Place::Local(1),
                field_offset: FieldOffset(0),
                dest: Place::Local(2),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(2),
            },
        ];

        let allowed = derive(
            &[block(0, instrs, Terminator::Return)],
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            !allowed.contains(&first) && !allowed.contains(&second),
            "a binder loaded from two roots is unattributable; its escape must \
             exclude both roots fail-closed; got {allowed:?}"
        );
    }

    /// #2212 attribution through a reused binder slot: an instruction write
    /// into the binder that is not a member load or binder move (a rebind
    /// from an unrelated local) forces `Ambiguous`, so the escape falls back
    /// to the blanket every-root exclusion.
    #[test]
    fn reused_binder_escape_falls_back_to_blanket_exclusion() {
        let root = BindingId(1);
        let bystander = BindingId(2);
        let owned = vec![
            (root, "a".to_string(), rec_ty()),
            (bystander, "b".to_string(), rec_ty()),
        ];
        let binding_locals: HashMap<BindingId, Place> =
            [(root, Place::Local(0)), (bystander, Place::Local(1))]
                .into_iter()
                .collect();
        // local 2: the binder; local 3: an unrelated string overwriting it.
        let local_tys = vec![rec_ty(), rec_ty(), ResolvedTy::String, ResolvedTy::String];
        let instrs = vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(2),
            },
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(3),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(2),
            },
        ];

        let allowed = derive(
            &[block(0, instrs, Terminator::Return)],
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            !allowed.contains(&root) && !allowed.contains(&bystander),
            "a binder overwritten by a non-member source is unattributable; \
             its escape must exclude every root fail-closed; got {allowed:?}"
        );
    }
}
#[cfg(test)]
mod escaped_sibling_field_discharge {
    //! Structural tests for `apply_escaped_record_sibling_field_drops` — the
    //! #2212 sibling-discharge emitter. The positive shape (one attributed
    //! instruction escape, record untouched afterwards) splices one
    //! `FieldDropInPlace` per dischargeable owned sibling right after the
    //! escape; every fail-closed refusal condition must leave the blocks
    //! untouched (leak-as-before, never a double-free).
    use super::*;

    fn rec_ty() -> ResolvedTy {
        ResolvedTy::named_user("Rec", vec![])
    }

    fn outer_ty() -> ResolvedTy {
        ResolvedTy::named_user("Outer", vec![])
    }

    fn mid_ty() -> ResolvedTy {
        ResolvedTy::named_user("Mid", vec![])
    }

    fn leaf_ty() -> ResolvedTy {
        ResolvedTy::named_user("Leaf", vec![])
    }

    fn is_rec(ty: &ResolvedTy) -> bool {
        matches!(ty, ResolvedTy::Named { name, .. } if name == "Rec")
    }

    fn is_chain_rec(ty: &ResolvedTy) -> bool {
        matches!(ty, ResolvedTy::Named { name, .. } if matches!(name.as_str(), "Outer" | "Mid" | "Leaf"))
    }

    /// `Rec { inner: string, tag: string }` — field 0 escapes in the test
    /// shapes, field 1 is the dischargeable sibling.
    fn owned_fields(ty: &ResolvedTy) -> Vec<(u32, ResolvedTy)> {
        if is_rec(ty) {
            vec![(0, ResolvedTy::String), (1, ResolvedTy::String)]
        } else {
            Vec::new()
        }
    }

    fn chain_owned_fields(ty: &ResolvedTy) -> Vec<(u32, ResolvedTy)> {
        let ResolvedTy::Named { name, .. } = ty else {
            return Vec::new();
        };
        match name.as_str() {
            "Outer" => vec![(0, mid_ty()), (1, ResolvedTy::String)],
            "Mid" => vec![(0, leaf_ty()), (1, ResolvedTy::String)],
            "Leaf" => vec![(0, ResolvedTy::String), (1, ResolvedTy::String)],
            _ => Vec::new(),
        }
    }

    fn dischargeable(ty: &ResolvedTy) -> bool {
        matches!(ty, ResolvedTy::String)
    }

    /// Tuple owned-element list: every `string` element of a tuple node.
    fn owned_tuple_fields(ty: &ResolvedTy) -> Vec<(u32, ResolvedTy)> {
        let ResolvedTy::Tuple(items) = ty else {
            return Vec::new();
        };
        items
            .iter()
            .enumerate()
            .filter(|(_, item)| matches!(item, ResolvedTy::String))
            .filter_map(|(idx, item)| u32::try_from(idx).ok().map(|i| (i, item.clone())))
            .collect()
    }

    fn field_orders() -> HashMap<String, Vec<(String, ResolvedTy)>> {
        let mut orders = HashMap::new();
        orders.insert(
            "Rec".to_string(),
            vec![
                ("inner".to_string(), ResolvedTy::String),
                ("tag".to_string(), ResolvedTy::String),
            ],
        );
        orders.insert(
            "Leaf".to_string(),
            vec![
                ("s".to_string(), ResolvedTy::String),
                ("t".to_string(), ResolvedTy::String),
            ],
        );
        orders.insert(
            "Mid".to_string(),
            vec![
                ("leaf".to_string(), leaf_ty()),
                ("x".to_string(), ResolvedTy::String),
            ],
        );
        orders.insert(
            "Outer".to_string(),
            vec![
                ("mid".to_string(), mid_ty()),
                ("c".to_string(), ResolvedTy::String),
            ],
        );
        orders
    }

    fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![],
            instructions,
            terminator,
        }
    }

    fn apply(
        blocks: &mut [BasicBlock],
        owned: &[(BindingId, String, ResolvedTy)],
        binding_locals: &HashMap<BindingId, Place>,
        local_tys: &[ResolvedTy],
    ) {
        apply_with(
            blocks,
            owned,
            binding_locals,
            local_tys,
            &[],
            &is_rec,
            &owned_fields,
        );
    }

    fn apply_with(
        blocks: &mut [BasicBlock],
        owned: &[(BindingId, String, ResolvedTy)],
        binding_locals: &HashMap<BindingId, Place>,
        local_tys: &[ResolvedTy],
        alias_chain: &[(u32, u32, u32)],
        is_owned_record: &dyn Fn(&ResolvedTy) -> bool,
        owned_field_list: &dyn Fn(&ResolvedTy) -> Vec<(u32, ResolvedTy)>,
    ) {
        let mut instr_spans = BTreeMap::new();
        apply_escaped_record_sibling_field_drops(
            blocks,
            &HashMap::new(),
            owned,
            binding_locals,
            local_tys,
            &field_orders(),
            &[],
            alias_chain,
            is_owned_record,
            owned_field_list,
            &owned_tuple_fields,
            &dischargeable,
            &mut instr_spans,
        );
    }

    fn match_hop_record_blocks(terminator: Terminator) -> Vec<BasicBlock> {
        vec![block(
            0,
            vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(2),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(3),
                },
                Instr::Move {
                    dest: Place::Local(4),
                    src: Place::Local(3),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(4),
                },
            ],
            terminator,
        )]
    }

    fn apply_match_hop(blocks: &mut [BasicBlock], local_tys: &[ResolvedTy]) {
        let root = BindingId(1);
        let owned = vec![(root, "o".to_string(), outer_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(root, Place::Local(0))].into_iter().collect();
        let alias_chain = vec![(1, 0, 0)];
        apply_with(
            blocks,
            &owned,
            &binding_locals,
            local_tys,
            &alias_chain,
            &is_chain_rec,
            &chain_owned_fields,
        );
    }

    /// The #2212 shape: one field loaded out and returned, record untouched
    /// afterwards → the owned sibling gets its in-place discharge spliced
    /// directly after the escape instruction.
    #[test]
    fn single_attributed_escape_discharges_owned_sibling() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(b, Place::Local(0))].into_iter().collect();
        let local_tys = vec![rec_ty(), ResolvedTy::String];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            Terminator::Return,
        )];

        apply(&mut blocks, &owned, &binding_locals, &local_tys);
        assert_eq!(
            blocks[0].instructions.len(),
            3,
            "exactly one sibling discharge must be spliced; got {:?}",
            blocks[0].instructions
        );
        assert_eq!(
            blocks[0].instructions[2],
            Instr::FieldDropInPlace {
                base: Place::Local(0),
                field: crate::model::FieldAddr::Record(FieldOffset(1)),
                ty: ResolvedTy::String,
            },
            "the discharge must address the NON-escaped sibling (field 1) on \
             the root local, typed at the field"
        );
    }

    /// The escaped field itself must never be discharged: when field 1 is the
    /// escapee, the spliced set contains ONLY field 0 — a discharge of the
    /// escaped slot would free the buffer the escapee now owns.
    #[test]
    fn escaped_field_is_never_in_the_discharge_set() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(b, Place::Local(0))].into_iter().collect();
        let local_tys = vec![rec_ty(), ResolvedTy::String];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(1),
                    dest: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            Terminator::Return,
        )];

        apply(&mut blocks, &owned, &binding_locals, &local_tys);
        let ops: Vec<_> = blocks[0]
            .instructions
            .iter()
            .filter(|i| matches!(i, Instr::FieldDropInPlace { .. }))
            .collect();
        assert_eq!(
            ops,
            vec![&Instr::FieldDropInPlace {
                base: Place::Local(0),
                field: crate::model::FieldAddr::Record(FieldOffset(0)),
                ty: ResolvedTy::String,
            }],
            "only the non-escaped sibling (field 0) may be discharged — a \
             discharge of escaped field 1 double-frees the escapee"
        );
    }

    /// A read of the record after the escape (a later field load) refuses the
    /// discharge — freeing the sibling earlier would be a use-after-free at
    /// that read.
    #[test]
    fn record_read_after_escape_refuses_discharge() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(b, Place::Local(0))].into_iter().collect();
        let local_tys = vec![rec_ty(), ResolvedTy::String, ResolvedTy::String];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(1),
                    dest: Place::Local(2),
                },
            ],
            Terminator::Return,
        )];

        apply(&mut blocks, &owned, &binding_locals, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "a record field read after the escape must refuse the discharge; \
             got {:?}",
            blocks[0].instructions
        );
    }

    /// A second escape event refuses the discharge — a per-escape splice
    /// would run twice on one path.
    #[test]
    fn two_escape_events_refuse_discharge() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(b, Place::Local(0))].into_iter().collect();
        let local_tys = vec![rec_ty(), ResolvedTy::String, ResolvedTy::String];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(1),
                    dest: Place::Local(2),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(2),
                },
            ],
            Terminator::Return,
        )];

        apply(&mut blocks, &owned, &binding_locals, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "two escape events on one root must refuse the discharge; got {:?}",
            blocks[0].instructions
        );
    }

    /// An escape inside a loop (its block reachable from itself) refuses the
    /// discharge — the splice would re-run per iteration.
    #[test]
    fn escape_in_cycle_refuses_discharge() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(b, Place::Local(0))].into_iter().collect();
        let local_tys = vec![rec_ty(), ResolvedTy::String];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            Terminator::Goto { target: 0 },
        )];

        apply(&mut blocks, &owned, &binding_locals, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "an escape whose block is self-reachable must refuse the \
             discharge; got {:?}",
            blocks[0].instructions
        );
    }

    /// A binder that is itself an `owned_locals` base (`let g = r.field`) has
    /// its own release path; the discharge must refuse rather than race it.
    #[test]
    fn extracted_owned_binding_refuses_discharge() {
        let root = BindingId(1);
        let extracted = BindingId(2);
        let owned = vec![
            (root, "r".to_string(), rec_ty()),
            (extracted, "g".to_string(), ResolvedTy::String),
        ];
        let binding_locals: HashMap<BindingId, Place> =
            [(root, Place::Local(0)), (extracted, Place::Local(1))]
                .into_iter()
                .collect();
        let local_tys = vec![rec_ty(), ResolvedTy::String];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            Terminator::Return,
        )];

        apply(&mut blocks, &owned, &binding_locals, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "a binder that is an owned binding's base has its own release \
             path; the discharge must refuse; got {:?}",
            blocks[0].instructions
        );
    }

    /// A whole-value copy of the record (`let b2 = b`) refuses the discharge
    /// — the copies byte-share field pointers and the pass frees through the
    /// root slot only.
    #[test]
    fn whole_value_alias_copy_refuses_discharge() {
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(b, Place::Local(0))].into_iter().collect();
        let local_tys = vec![rec_ty(), ResolvedTy::String, rec_ty()];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(0),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(2),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            Terminator::Return,
        )];

        apply(&mut blocks, &owned, &binding_locals, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "a whole-value alias copy of the record must refuse the \
             discharge; got {:?}",
            blocks[0].instructions
        );
    }

    /// An owned sibling whose shape the field-drop contract does not cover
    /// keeps its leak while the coverable sibling is still discharged —
    /// partial discharge is strictly better and never double-frees.
    #[test]
    fn uncoverable_sibling_keeps_leak_while_coverable_discharges() {
        fn three_fields(ty: &ResolvedTy) -> Vec<(u32, ResolvedTy)> {
            if is_rec(ty) {
                vec![
                    (0, ResolvedTy::String),
                    (1, ResolvedTy::String),
                    (
                        2,
                        ResolvedTy::named_builtin(
                            "Vec",
                            BuiltinType::Vec,
                            vec![ResolvedTy::String],
                        ),
                    ),
                ]
            } else {
                Vec::new()
            }
        }
        let b = BindingId(1);
        let owned = vec![(b, "r".to_string(), rec_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(b, Place::Local(0))].into_iter().collect();
        let local_tys = vec![rec_ty(), ResolvedTy::String];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            Terminator::Return,
        )];

        let mut instr_spans = BTreeMap::new();
        apply_escaped_record_sibling_field_drops(
            &mut blocks,
            &HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &field_orders(),
            &[],
            &[],
            &is_rec,
            &three_fields,
            &owned_tuple_fields,
            &dischargeable,
            &mut instr_spans,
        );
        let ops: Vec<_> = blocks[0]
            .instructions
            .iter()
            .filter(|i| matches!(i, Instr::FieldDropInPlace { .. }))
            .collect();
        assert_eq!(
            ops,
            vec![&Instr::FieldDropInPlace {
                base: Place::Local(0),
                field: crate::model::FieldAddr::Record(FieldOffset(1)),
                ty: ResolvedTy::String,
            }],
            "the string sibling is discharged; the uncovered Vec sibling \
             keeps its leak (fail-closed partial discharge)"
        );
    }

    /// #2387: an intermediate chain hop bound through `match` preserves the
    /// immediate parent relation (`leaf -> match-scrutinee-mid -> outer`) so
    /// the escaped-chain compensator releases only the non-escaped siblings at
    /// both levels.
    #[test]
    fn match_bound_hop_chain_discharges_record_siblings() {
        let local_tys = vec![outer_ty(), mid_ty(), mid_ty(), leaf_ty(), leaf_ty()];
        let mut blocks = match_hop_record_blocks(Terminator::Return);

        apply_match_hop(&mut blocks, &local_tys);
        let ops: Vec<_> = blocks[0]
            .instructions
            .iter()
            .filter(|i| matches!(i, Instr::FieldDropInPlace { .. }))
            .collect();
        assert_eq!(
            ops,
            vec![
                &Instr::FieldDropInPlace {
                    base: Place::Local(2),
                    field: crate::model::FieldAddr::Record(FieldOffset(1)),
                    ty: ResolvedTy::String,
                },
                &Instr::FieldDropInPlace {
                    base: Place::Local(0),
                    field: crate::model::FieldAddr::Record(FieldOffset(1)),
                    ty: ResolvedTy::String,
                },
            ],
            "the match-bound escape must discharge mid.x through the scrutinee \
             alias and outer.c through the root, never the escaped leaf"
        );
    }

    /// Non-escaping match-bound destructures keep the leak-safety posture:
    /// sibling discharges are only compensation for a proven owning escape.
    #[test]
    fn non_escaping_match_bound_hop_emits_no_sibling_discharge() {
        let local_tys = vec![
            outer_ty(),
            mid_ty(),
            mid_ty(),
            leaf_ty(),
            ResolvedTy::String,
        ];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(2),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(3),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(3),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(4),
                },
            ],
            Terminator::Return,
        )];

        apply_match_hop(&mut blocks, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "a match-bound hop that never escapes must not add sibling drops; \
             got {:?}",
            blocks[0].instructions
        );
    }

    /// A read of any chain node after the escape refuses the discharge: freeing
    /// a sibling before that read would be a use-after-free through the alias.
    #[test]
    fn match_bound_hop_post_escape_read_refuses_discharge() {
        let local_tys = vec![
            outer_ty(),
            mid_ty(),
            mid_ty(),
            leaf_ty(),
            leaf_ty(),
            ResolvedTy::String,
        ];
        let mut blocks = match_hop_record_blocks(Terminator::Return);
        blocks[0].instructions.push(Instr::RecordFieldLoad {
            record: Place::Local(2),
            field_offset: FieldOffset(1),
            dest: Place::Local(5),
        });

        apply_match_hop(&mut blocks, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "a chain-node read after the escape must keep the fail-closed leak; \
             got {:?}",
            blocks[0].instructions
        );
    }

    /// Retained string fields are not byte-copy aggregate aliases. Returning a
    /// string loaded through the match-bound path must not be attributed as an
    /// escaped aggregate chain.
    #[test]
    fn match_bound_retained_string_load_is_not_attributed() {
        let local_tys = vec![
            outer_ty(),
            mid_ty(),
            mid_ty(),
            ResolvedTy::String,
            ResolvedTy::String,
        ];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(2),
                    field_offset: FieldOffset(1),
                    dest: Place::Local(3),
                },
                Instr::Move {
                    dest: Place::Local(4),
                    src: Place::Local(3),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(4),
                },
            ],
            Terminator::Return,
        )];

        apply_match_hop(&mut blocks, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "a retained string field load is a fresh owner, not a byte-copy \
             aggregate alias; got {:?}",
            blocks[0].instructions
        );
    }

    /// Handle-transfer fields are also not byte-copy aggregate aliases. The
    /// helper must keep `local_is_byte_copy_aggregate` as the gate and leave the
    /// pre-existing fail-closed posture unchanged.
    #[test]
    fn match_bound_handle_transfer_load_is_not_attributed() {
        let vec_ty = ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String]);
        let local_tys = vec![outer_ty(), mid_ty(), mid_ty(), vec_ty.clone(), vec_ty];
        let mut blocks = vec![block(
            0,
            vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(2),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(3),
                },
                Instr::Move {
                    dest: Place::Local(4),
                    src: Place::Local(3),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(4),
                },
            ],
            Terminator::Return,
        )];

        apply_match_hop(&mut blocks, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "a transferred handle field must not be treated as a match-hop \
             byte-copy aggregate alias; got {:?}",
            blocks[0].instructions
        );
    }

    /// The self-reachable-block bail-out remains load-bearing for match-hop
    /// chains: inline-composite sibling drops are not idempotent in loops.
    #[test]
    fn match_bound_hop_escape_in_cycle_refuses_discharge() {
        let local_tys = vec![outer_ty(), mid_ty(), mid_ty(), leaf_ty(), leaf_ty()];
        let mut blocks = match_hop_record_blocks(Terminator::Goto { target: 0 });

        apply_match_hop(&mut blocks, &local_tys);
        assert!(
            !blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
            "a match-hop escape whose block is self-reachable must refuse the \
             discharge; got {:?}",
            blocks[0].instructions
        );
    }
}
#[cfg(test)]
mod tuple_composite_field_drop_exclusion {
    //! Direct structural pins for `derive_tuple_composite_drop_allowed`'s
    //! `FieldDropInPlace` exclusion rule — the tuple twin of the record
    //! prover's. The op mints no load dest and no `Drop` place, so with
    //! bitcopy-only sibling binders it is invisible to the
    //! `elem_binders ∩ release_owner_bases` intersection; the direct rule is
    //! what keeps the `TupleInPlace` drop from re-walking the freed element.
    use super::*;

    fn pair_ty() -> ResolvedTy {
        ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64])
    }

    fn derive(instrs: Vec<Instr>) -> (BindingId, HashSet<BindingId>) {
        let b = BindingId(1);
        let owned = vec![(b, "p".to_string(), pair_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        let local_tys = vec![pair_ty()];
        let allowed = derive_tuple_composite_drop_allowed(
            &[BasicBlock {
                id: 0,
                statements: vec![],
                instructions: instrs,
                terminator: Terminator::Return,
            }],
            &HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &HashMap::new(),
            &[],
            &[],
        );
        (b, allowed)
    }

    /// Control: an untouched owned tuple is its own sole owner and admitted.
    #[test]
    fn untouched_tuple_is_admitted() {
        let (b, allowed) = derive(vec![]);
        assert!(
            allowed.contains(&b),
            "an untouched owned tuple must keep its TupleInPlace drop"
        );
    }

    /// A `FieldDropInPlace` addressing the tuple root excludes it — the op
    /// already discharged that element's release.
    #[test]
    fn field_drop_in_place_on_root_excludes_tuple() {
        let (b, allowed) = derive(vec![Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: crate::model::FieldAddr::Tuple(0),
            ty: ResolvedTy::String,
        }]);
        assert!(
            !allowed.contains(&b),
            "a tuple root addressed by FieldDropInPlace must be excluded from \
             its composite drop (else the freed element is re-walked); got \
             {allowed:?}"
        );
    }

    /// A `FieldDropInPlace` whose base is an ELEMENT BINDER (`let inner =
    /// t.0; match inner { … }`) discharges part of the outer root's storage
    /// through the binder's byte-copy; the direct rule must exclude the root.
    /// This pins the exemption/direct-rule PAIRING: the blanket owning-sink
    /// scan now exempts `FieldDropInPlace` (so a binder base no longer
    /// misreads as an element escape), and this test fails if that exemption
    /// ever lands without the direct rule resolving binder bases — the shape
    /// that would trade the old over-exclusion leak for a composite re-walk
    /// double-free.
    #[test]
    fn field_drop_in_place_on_elem_binder_excludes_tuple() {
        let b = BindingId(1);
        let owned = vec![(b, "p".to_string(), pair_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        // local 1: the extracted heap-owning element binder; local 2: the
        // match scrutinee copy of the binder.
        let local_tys = vec![pair_ty(), ResolvedTy::String, ResolvedTy::String];
        let allowed = derive_tuple_composite_drop_allowed(
            &[BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![
                    Instr::TupleFieldLoad {
                        tuple: Place::Local(0),
                        field_index: 0,
                        dest: Place::Local(1),
                    },
                    Instr::Move {
                        dest: Place::Local(2),
                        src: Place::Local(1),
                    },
                    Instr::FieldDropInPlace {
                        base: Place::Local(2),
                        field: crate::model::FieldAddr::Tuple(1),
                        ty: ResolvedTy::String,
                    },
                ],
                terminator: Terminator::Return,
            }],
            &HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &HashMap::new(),
            &[],
            &[],
        );
        assert!(
            !allowed.contains(&b),
            "a FieldDropInPlace against an element binder freed part of the \
             tuple root's storage through the binder's byte-alias; the root \
             must be excluded (leak-not-double-free); got {allowed:?}"
        );
    }
}
#[cfg(test)]
mod enum_composite_field_drop_exemption {
    //! Pins for the enum composite prover's `FieldDropInPlace` handling: the
    //! blanket-scan exemption (the op is an interior discharge, not a payload
    //! READ into an owning sink) paired with the DIRECT exclusion rule (a
    //! base that is an alias member or a payload binder frees payload leaves
    //! through a byte-alias of the composite's storage, so the composite must
    //! be excluded — its `EnumInPlace` walk would re-free them; the
    //! empirically reproduced two-step nested destructure `match opt {
    //! Some(row) => match row { Row { a, b: _ } => … } }` aborted under
    //! Guard-Malloc while the composite stayed admitted). The differential
    //! control proves a genuine owning-sink read of the same binder still
    //! excludes the composite.
    use super::*;

    fn opt_ty() -> ResolvedTy {
        ResolvedTy::named_user("Opt", vec![])
    }

    fn row_ty() -> ResolvedTy {
        ResolvedTy::named_user("Row", vec![])
    }

    fn derive(instrs: Vec<Instr>) -> (BindingId, HashSet<BindingId>) {
        let b = BindingId(1);
        let owned = vec![(b, "o".to_string(), opt_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(b, Place::Local(0));
        // local 0: the Opt composite; local 1: the Row payload binder;
        // local 2: a general-storage sink for the differential control.
        let local_tys = vec![opt_ty(), row_ty(), ResolvedTy::Tuple(vec![row_ty()])];
        let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
        record_field_orders.insert(
            "Row".to_string(),
            vec![
                ("inner".to_string(), ResolvedTy::String),
                ("tag".to_string(), ResolvedTy::String),
            ],
        );
        let enum_layouts = vec![crate::model::EnumLayout {
            name: "Opt".to_string(),
            tag_width: 1,
            variants: vec![
                crate::model::MachineVariantLayout {
                    name: "Some".to_string(),
                    field_tys: vec![row_ty()],
                    field_names: vec![],
                },
                crate::model::MachineVariantLayout {
                    name: "None".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
            ],
            is_indirect: false,
        }];
        let allowed = derive_enum_composite_drop_allowed(
            &[BasicBlock {
                id: 0,
                statements: vec![],
                instructions: instrs,
                terminator: Terminator::Return,
            }],
            &HashMap::new(),
            &owned,
            &binding_locals,
            &HashMap::new(),
            &HashMap::new(),
            &local_tys,
            &record_field_orders,
            &enum_layouts,
        );
        (b, allowed)
    }

    /// `Some(r)` destructure: the payload binder receives the interior
    /// projection of the composite.
    fn payload_destructure() -> Instr {
        Instr::Move {
            dest: Place::Local(1),
            src: Place::EnumVariant {
                local: 0,
                variant_idx: 0,
                field_idx: 0,
            },
        }
    }

    /// A `FieldDropInPlace` discharging one skipped field of the payload
    /// binder frees payload leaves through the binder's byte-alias of the
    /// composite's storage — the composite must be EXCLUDED, or its
    /// `EnumInPlace` walk re-frees the discharged field (the reproduced
    /// nested-destructure double-free: Guard-Malloc SIGSEGV on the second
    /// iteration while the composite stayed admitted). Exclusion leaks the
    /// payload remainder instead — the fail-closed direction. This is the
    /// direct rule's pin; the blanket-scan exemption alone left the
    /// composite admitted.
    #[test]
    fn field_drop_on_payload_binder_excludes_composite() {
        let (b, allowed) = derive(vec![
            payload_destructure(),
            Instr::FieldDropInPlace {
                base: Place::Local(1),
                field: crate::model::FieldAddr::Record(FieldOffset(1)),
                ty: ResolvedTy::String,
            },
        ]);
        assert!(
            !allowed.contains(&b),
            "a FieldDropInPlace against the payload binder discharged payload \
             leaves the composite's EnumInPlace walk would re-free; the \
             composite must be excluded (leak-not-double-free); got {allowed:?}"
        );
    }

    /// Differential control: a genuine owning-sink read of the same payload
    /// binder (an aggregate construction) still excludes the composite —
    /// the exemption admits exactly the interior discharge op, nothing wider.
    #[test]
    fn owning_sink_read_of_payload_binder_still_excludes_composite() {
        let (b, allowed) = derive(vec![
            payload_destructure(),
            Instr::TupleConstruct {
                elements: vec![Place::Local(1)],
                dest: Place::Local(2),
            },
        ]);
        assert!(
            !allowed.contains(&b),
            "a payload binder read into an owning sink escaped the composite; \
             it must be excluded (fail-closed); got {allowed:?}"
        );
    }
}
#[cfg(test)]
mod witness_verifier_composite_traversal {
    //! W5.007a fix — the MIR witness-operand verifier must descend EVERY
    //! composite `ResolvedTy` constructor so an out-of-scope `TypeParam`
    //! nested under a `Borrow` or `TraitObject` is caught and fails closed
    //! with `MirCheck::WitnessOperandUnresolved` (A622 / DI-019). The pre-fix
    //! `collect_undeclared_type_params` fell through `_ => {}` for those two
    //! composites, so an undeclared parameter underneath them BYPASSED the
    //! declared-parameter check. These tests FAIL on the pre-fix tree.

    use super::*;

    /// Run the witness-operand verifier over a single `WitnessSizeOf` whose
    /// operand is `ty`, with `declared` as the enclosing function's binders.
    /// Returns the `MirCheck`s the verifier produced.
    fn verify_witness(ty: ResolvedTy, declared: &[&str]) -> Vec<MirCheck> {
        let builder = Builder::default();
        let block = BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::WitnessSizeOf {
                dest: Place::Local(0),
                ty,
            }],
            terminator: Terminator::Return,
        };
        let func = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: "origin".to_string(),
            type_params: declared.iter().map(|s| (*s).to_string()).collect(),
            is_generator: false,
            intrinsic_id: None,
            params: Vec::new(),
            return_ty: ResolvedTy::Unit,
            body: HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: ResolvedTy::Unit,
                span: 0..0,
            },
            span: 0..0,
        };
        check_function(&builder, std::slice::from_ref(&block), &func).checks
    }

    fn has_witness_unresolved(checks: &[MirCheck]) -> bool {
        checks
            .iter()
            .any(|c| matches!(c, MirCheck::WitnessOperandUnresolved { .. }))
    }

    /// `&U` with `U` out of scope: the verifier must descend the borrow
    /// pointee and reject.
    #[test]
    fn out_of_scope_type_param_under_borrow_fails_closed() {
        let ty = ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::TypeParam {
                name: "U".to_string(),
            }),
        };
        let checks = verify_witness(ty, &[]);
        assert!(
            has_witness_unresolved(&checks),
            "an undeclared TypeParam under a borrow must fail closed; got: {checks:?}"
        );
    }

    /// A trait-object operand carrying an undeclared parameter in a trait
    /// argument must be rejected.
    #[test]
    fn out_of_scope_type_param_in_trait_object_arg_fails_closed() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![hew_types::ResolvedTraitBound {
                trait_name: "Into".to_string(),
                args: vec![ResolvedTy::TypeParam {
                    name: "U".to_string(),
                }],
                assoc_bindings: vec![],
            }],
        };
        let checks = verify_witness(ty, &[]);
        assert!(
            has_witness_unresolved(&checks),
            "an undeclared TypeParam in a trait-object arg must fail closed; got: {checks:?}"
        );
    }

    /// A trait-object operand carrying an undeclared parameter in an
    /// associated-type binding must be rejected.
    #[test]
    fn out_of_scope_type_param_in_trait_object_assoc_binding_fails_closed() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![hew_types::ResolvedTraitBound {
                trait_name: "Iterator".to_string(),
                args: vec![],
                assoc_bindings: vec![(
                    "Item".to_string(),
                    ResolvedTy::TypeParam {
                        name: "U".to_string(),
                    },
                )],
            }],
        };
        let checks = verify_witness(ty, &[]);
        assert!(
            has_witness_unresolved(&checks),
            "an undeclared TypeParam in a trait-object assoc binding must fail closed; got: {checks:?}"
        );
    }

    /// A DECLARED parameter under a borrow is admitted — the descent does not
    /// over-reject in-scope parameters (the abstract-origin happy path).
    #[test]
    fn declared_type_param_under_borrow_is_admitted() {
        let ty = ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::TypeParam {
                name: "T".to_string(),
            }),
        };
        let checks = verify_witness(ty, &["T"]);
        assert!(
            !has_witness_unresolved(&checks),
            "a declared TypeParam under a borrow must be admitted; got: {checks:?}"
        );
    }
}
#[cfg(test)]
mod w3053_aggregate_handle_double_free_gate {
    //! Direct structural tests for `detect_unproven_aggregate_handle_double_free`
    //! — the W3.053 catch-all fail-closed gate. These poke the detector with
    //! synthetic MIR because the owned-handle aggregate-extraction shapes (and
    //! the cross-handle-type `CancellationToken` / record forms) either cannot be
    //! built through the minimal test pipeline or have no buildable surface
    //! syntax that reaches MIR. Asserting on the gate's findings directly is the
    //! authoritative cross-handle-type proof.
    use super::*;

    fn generator_ty() -> ResolvedTy {
        ResolvedTy::named_builtin(
            "Generator",
            BuiltinType::Generator,
            vec![ResolvedTy::I64, ResolvedTy::Unit],
        )
    }

    fn block(instructions: Vec<Instr>) -> BasicBlock {
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions,
            terminator: Terminator::Return,
        }
    }

    fn is_refused(findings: &[MirCheck], binding: BindingId) -> bool {
        findings.iter().any(|c| {
            matches!(
                c,
                MirCheck::OwnedHandleAggregateDoubleFree { binding: b, .. } if *b == binding
            )
        })
    }

    /// Shape A (re-aggregation): a generator handle aliased into tuple `a`, the
    /// field extracted and re-aggregated into tuple `b`, then `b.0` consumed by an
    /// inline release `Drop`. The handle's standalone source drop is NOT excluded
    /// (the single-hop exclusion misses the re-aggregation), so the context is
    /// freed twice → must be REFUSED.
    #[test]
    fn reaggregated_generator_handle_is_refused() {
        // locals: 1=g(handle) 3=a-tuple 4=a-binding 5=a.0 7=b-tuple 8=b-binding
        //         9=b.0 10=for-iter
        let g = BindingId(1);
        let a_bind = BindingId(2);
        let b_bind = BindingId(3);
        let tuple_ty = ResolvedTy::Tuple(vec![generator_ty(), ResolvedTy::I64]);
        let instrs = vec![
            Instr::TupleConstruct {
                elements: vec![Place::Local(1), Place::Local(2)],
                dest: Place::Local(3),
            },
            Instr::Move {
                dest: Place::Local(4),
                src: Place::Local(3),
            },
            Instr::TupleFieldLoad {
                tuple: Place::Local(4),
                field_index: 0,
                dest: Place::Local(5),
            },
            Instr::TupleConstruct {
                elements: vec![Place::Local(5), Place::Local(6)],
                dest: Place::Local(7),
            },
            Instr::Move {
                dest: Place::Local(8),
                src: Place::Local(7),
            },
            Instr::TupleFieldLoad {
                tuple: Place::Local(8),
                field_index: 0,
                dest: Place::Local(9),
            },
            Instr::Move {
                dest: Place::Local(10),
                src: Place::Local(9),
            },
            // The for-iter consumer's inline release.
            Instr::Drop {
                place: Place::Local(10),
                ty: generator_ty(),
                drop_fn: Some(crate::model::DropFnSpec::Release("hew_gen_coro_destroy")),
            },
        ];
        let owned = vec![
            (g, "g".to_string(), generator_ty()),
            (a_bind, "a".to_string(), tuple_ty.clone()),
            (b_bind, "b".to_string(), tuple_ty),
        ];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(g, Place::Local(1));
        binding_locals.insert(a_bind, Place::Local(4));
        binding_locals.insert(b_bind, Place::Local(8));
        // local types: index = local id.
        let mut local_tys = vec![ResolvedTy::I64; 11];
        local_tys[1] = generator_ty();
        local_tys[3] = ResolvedTy::Tuple(vec![generator_ty(), ResolvedTy::I64]);
        local_tys[4] = ResolvedTy::Tuple(vec![generator_ty(), ResolvedTy::I64]);
        local_tys[5] = generator_ty();
        local_tys[7] = ResolvedTy::Tuple(vec![generator_ty(), ResolvedTy::I64]);
        local_tys[8] = ResolvedTy::Tuple(vec![generator_ty(), ResolvedTy::I64]);
        local_tys[9] = generator_ty();
        local_tys[10] = generator_ty();

        // The exclusion analysis does NOT cover the re-aggregation, so `g` is not
        // in `source_excluded`; the consumer drops the handle once, the source
        // drops it again.
        let source_excluded = HashSet::new();
        let composite_drop_allowed = HashSet::new();
        let findings = detect_unproven_aggregate_handle_double_free(
            &[block(instrs)],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &std::collections::HashMap::new(),
            &[],
            &source_excluded,
            &composite_drop_allowed,
        );
        assert!(
            is_refused(&findings, g),
            "re-aggregated generator handle freed twice must be refused; got {findings:?}"
        );
    }

    /// Cross-handle-type coverage: a `CancellationToken` (distinct `ResolvedTy`
    /// variant, not a `Named` builtin) aliased into a tuple and freed by both the
    /// tuple member drop AND its own source drop is the same double-free class —
    /// the gate is type-agnostic via `ty_contains_heap_owning`, so it must REFUSE.
    #[test]
    fn cancellation_token_double_freed_via_aggregate_is_refused() {
        let tok = BindingId(1);
        let pair = BindingId(2);
        let tuple_ty = ResolvedTy::Tuple(vec![ResolvedTy::CancellationToken, ResolvedTy::I64]);
        // tok(local 1) into tuple(local 3) bound to `pair`(local 4); no extraction.
        let instrs = vec![
            Instr::TupleConstruct {
                elements: vec![Place::Local(1), Place::Local(2)],
                dest: Place::Local(3),
            },
            Instr::Move {
                dest: Place::Local(4),
                src: Place::Local(3),
            },
        ];
        let owned = vec![
            (tok, "tok".to_string(), ResolvedTy::CancellationToken),
            (pair, "pair".to_string(), tuple_ty.clone()),
        ];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(tok, Place::Local(1));
        binding_locals.insert(pair, Place::Local(4));
        let mut local_tys = vec![ResolvedTy::I64; 5];
        local_tys[1] = ResolvedTy::CancellationToken;
        local_tys[3] = tuple_ty.clone();
        local_tys[4] = tuple_ty;
        // No extraction consumer: `tok`'s own source drop fires (not excluded) AND
        // the `pair` tuple's member drop fires (admitted in composite_drop_allowed)
        // — two frees of the one token context.
        let source_excluded = HashSet::new();
        let mut composite_drop_allowed = HashSet::new();
        composite_drop_allowed.insert(pair);
        let findings = detect_unproven_aggregate_handle_double_free(
            &[block(instrs)],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &std::collections::HashMap::new(),
            &[],
            &source_excluded,
            &composite_drop_allowed,
        );
        assert!(
            is_refused(&findings, tok),
            "a CancellationToken freed by both the tuple member drop and its own \
             source drop must be refused; got {findings:?}"
        );
    }

    /// Proven KEEP (single-hop extraction): a generator extracted out of a tuple
    /// into a consumer, where the SOURCE binding's drop is excluded
    /// (`source_excluded`) and the tuple member drop is suppressed (NOT in
    /// `composite_drop_allowed`) — exactly one free. The gate must NOT refuse it.
    #[test]
    fn single_hop_extraction_proven_exact_once_is_not_refused() {
        let g = BindingId(1);
        let packed = BindingId(2);
        let tuple_ty = ResolvedTy::Tuple(vec![generator_ty(), ResolvedTy::I64]);
        let instrs = vec![
            Instr::TupleConstruct {
                elements: vec![Place::Local(1), Place::Local(2)],
                dest: Place::Local(3),
            },
            Instr::Move {
                dest: Place::Local(4),
                src: Place::Local(3),
            },
            Instr::TupleFieldLoad {
                tuple: Place::Local(4),
                field_index: 0,
                dest: Place::Local(5),
            },
            Instr::Move {
                dest: Place::Local(6),
                src: Place::Local(5),
            },
            Instr::Drop {
                place: Place::Local(6),
                ty: generator_ty(),
                drop_fn: Some(crate::model::DropFnSpec::Release("hew_gen_coro_destroy")),
            },
        ];
        let owned = vec![
            (g, "g".to_string(), generator_ty()),
            (packed, "packed".to_string(), tuple_ty.clone()),
        ];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(g, Place::Local(1));
        binding_locals.insert(packed, Place::Local(4));
        let mut local_tys = vec![ResolvedTy::I64; 7];
        local_tys[1] = generator_ty();
        local_tys[3] = tuple_ty.clone();
        local_tys[4] = tuple_ty;
        local_tys[5] = generator_ty();
        local_tys[6] = generator_ty();
        // Proven exactly-once: `g`'s source drop excluded; `packed` member drop
        // suppressed. Only the consumer (local 6) frees.
        let mut source_excluded = HashSet::new();
        source_excluded.insert(g);
        let composite_drop_allowed = HashSet::new();
        let findings = detect_unproven_aggregate_handle_double_free(
            &[block(instrs)],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &std::collections::HashMap::new(),
            &[],
            &source_excluded,
            &composite_drop_allowed,
        );
        assert!(
            !is_refused(&findings, g),
            "single-hop extraction proven freed exactly once must NOT be refused; \
             got {findings:?}"
        );
    }

    fn stream_ty() -> ResolvedTy {
        ResolvedTy::named_builtin("Stream", BuiltinType::Stream, vec![ResolvedTy::I64])
    }

    fn stream_close() -> Instr {
        Instr::Drop {
            place: Place::Local(1),
            ty: stream_ty(),
            drop_fn: Some(crate::model::DropFnSpec::Runtime(
                hew_types::runtime_call::RuntimeDropDescriptor::StreamClose,
            )),
        }
    }

    fn synthetic_cursor_findings(blocks: &[BasicBlock]) -> Vec<MirCheck> {
        let cursor = BindingId(1);
        let mut binding_locals = HashMap::new();
        binding_locals.insert(cursor, Place::Local(1));
        detect_unproven_aggregate_handle_double_free(
            blocks,
            &HashMap::new(),
            &[],
            &binding_locals,
            &[ResolvedTy::Bool, stream_ty()],
            &HashMap::new(),
            &[],
            &HashSet::new(),
            &HashSet::new(),
        )
    }

    #[test]
    fn mutually_exclusive_stream_cursor_closes_are_not_refused() {
        let cursor = BindingId(1);
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![MirStatement::Bind {
                    binding: cursor,
                    name: format!("{FOR_ITER_CURSOR_NAME_PREFIX}1"),
                    site: SiteId(0),
                    ty: stream_ty(),
                }],
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
                instructions: vec![stream_close()],
                terminator: Terminator::Return,
            },
            BasicBlock {
                id: 2,
                statements: vec![],
                instructions: vec![stream_close()],
                terminator: Terminator::Return,
            },
        ];
        let findings = synthetic_cursor_findings(&blocks);
        assert!(
            !is_refused(&findings, cursor),
            "cloned closes on mutually-exclusive cursor exits are exactly-once; got {findings:?}"
        );
    }

    #[test]
    fn reinitialized_stream_cursor_closes_are_not_refused() {
        let cursor = BindingId(1);
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![MirStatement::Bind {
                    binding: cursor,
                    name: format!("{FOR_ITER_CURSOR_NAME_PREFIX}1"),
                    site: SiteId(0),
                    ty: stream_ty(),
                }],
                instructions: vec![stream_close()],
                terminator: Terminator::Goto { target: 1 },
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![Instr::Move {
                    dest: Place::Local(1),
                    src: Place::Local(2),
                }],
                terminator: Terminator::Goto { target: 2 },
            },
            BasicBlock {
                id: 2,
                statements: vec![],
                instructions: vec![stream_close()],
                terminator: Terminator::Return,
            },
        ];
        let findings = synthetic_cursor_findings(&blocks);
        assert!(
            !is_refused(&findings, cursor),
            "a loop re-entry installs a fresh cursor before the later close; got {findings:?}"
        );
    }

    #[test]
    fn sequential_stream_cursor_closes_remain_refused() {
        let cursor = BindingId(1);
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![MirStatement::Bind {
                    binding: cursor,
                    name: format!("{FOR_ITER_CURSOR_NAME_PREFIX}1"),
                    site: SiteId(0),
                    ty: stream_ty(),
                }],
                instructions: vec![stream_close()],
                terminator: Terminator::Goto { target: 1 },
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![stream_close()],
                terminator: Terminator::Return,
            },
        ];
        let findings = synthetic_cursor_findings(&blocks);
        assert!(
            is_refused(&findings, cursor),
            "two cursor closes on one CFG path must remain refused; got {findings:?}"
        );
    }

    fn localpid_ty() -> ResolvedTy {
        ResolvedTy::named_builtin("LocalPid", BuiltinType::LocalPid, vec![ResolvedTy::I64])
    }

    /// A block whose terminator is a `Terminator::Call` passing `args` by value
    /// to `callee` — the by-value-call escape shape the borrow-arg gate fix
    /// narrows.
    fn call_block(callee: &str, args: Vec<Place>) -> BasicBlock {
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: callee.to_string(),
                // Mirror the producer lift: hand-built escape-gate MIR
                // carries the typed family exactly as the real lowering
                // does, so family-keyed borrow classification is exercised.
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(callee),
                args,
                dest: None,
                next: 0,
            },
        }
    }

    /// REFUSE (regression guard): an OWNING handle leaf (a generator) passed by
    /// value to an ordinary user-function call still escapes — the callee can
    /// alias it into a second, untracked free path on top of the source drop, so
    /// the gate must keep failing closed. The borrow-arg fix must NOT relax this.
    #[test]
    fn owning_handle_arg_to_user_call_is_refused() {
        let g = BindingId(1);
        let owned = vec![(g, "g".to_string(), generator_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(g, Place::Local(1));
        let mut local_tys = vec![ResolvedTy::I64; 2];
        local_tys[1] = generator_ty();
        let findings = detect_unproven_aggregate_handle_double_free(
            &[call_block("use_gen", vec![Place::Local(1)])],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &std::collections::HashMap::new(),
            &[],
            &HashSet::new(),
            &HashSet::new(),
        );
        assert!(
            is_refused(&findings, g),
            "an owning generator handle passed by value to a user call must stay \
             refused (the escape gate's default); got {findings:?}"
        );
    }

    /// KEEP (borrow-arg fix): a NON-OWNING actor-pid leaf (`LocalPid`, no `close`
    /// ABI — its drop frees nothing) passed by value to an ordinary call is a
    /// borrow/consume that can never alias a second free, so it must NOT be
    /// refused. This is the `fn use_pid(p: LocalPid<_>)` over-rejection the fix
    /// removes.
    #[test]
    fn nonowning_pid_arg_to_user_call_is_not_refused() {
        let p = BindingId(1);
        let owned = vec![(p, "p".to_string(), localpid_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(p, Place::Local(1));
        let mut local_tys = vec![ResolvedTy::I64; 2];
        local_tys[1] = localpid_ty();
        let findings = detect_unproven_aggregate_handle_double_free(
            &[call_block("use_pid", vec![Place::Local(1)])],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &std::collections::HashMap::new(),
            &[],
            &HashSet::new(),
            &HashSet::new(),
        );
        assert!(
            !is_refused(&findings, p),
            "a non-owning LocalPid passed by value to a call frees nothing and \
             must NOT be refused; got {findings:?}"
        );
    }

    /// KEEP (borrow-arg fix, allowlist): the ratified active-mode
    /// `conn.attach(handler)` lowers to a `hew_tcp_attach_local(conn, handler)`
    /// call whose `LocalPid` handler the runtime registers as a non-owning
    /// by-value snapshot. The borrowing-ABI allowlist plus the non-owning-leaf
    /// rule both exempt the handler arg, so the pid handle must NOT be refused.
    #[test]
    fn attach_local_pid_handler_arg_is_not_refused() {
        let h = BindingId(1);
        let owned = vec![(h, "handler".to_string(), localpid_ty())];
        let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
        binding_locals.insert(h, Place::Local(1));
        let mut local_tys = vec![ResolvedTy::I64; 2];
        local_tys[1] = localpid_ty();
        let findings = detect_unproven_aggregate_handle_double_free(
            &[call_block("hew_tcp_attach_local", vec![Place::Local(1)])],
            &std::collections::HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &std::collections::HashMap::new(),
            &[],
            &HashSet::new(),
            &HashSet::new(),
        );
        assert!(
            !is_refused(&findings, h),
            "the borrowed LocalPid handler of `conn.attach` must NOT be refused; \
             got {findings:?}"
        );
    }
}
#[cfg(test)]
mod spawn_consumed_handle_exclusion {
    //! Direct structural tests for `derive_spawn_consumed_handle_bindings` and
    //! its load-bearing effect on the W3.053 gate. A Sink/Stream half moved into
    //! an actor initial-state record consumed by `SpawnActor` is owned by the
    //! actor's synthesised `state_drop_fn`, so its source binding's standalone
    //! drop is removed and the gate must admit it. The negative control disables
    //! the derivation (empty `source_excluded`) and confirms the gate then
    //! REFUSES — proving the exclusion is not a no-op (LESSONS
    //! drop-allowset-from-value-flow: include a negative control).
    use super::*;

    fn is_refused(findings: &[MirCheck], binding: BindingId) -> bool {
        findings.iter().any(|c| {
            matches!(
                c,
                MirCheck::OwnedHandleAggregateDoubleFree { binding: b, .. } if *b == binding
            )
        })
    }

    fn sink_ty() -> ResolvedTy {
        ResolvedTy::named_builtin("Sink", BuiltinType::Sink, vec![ResolvedTy::String])
    }

    fn writer_state_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Writer".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }
    }

    /// sink(local 1) → state record(local 2) via `RecordInit`, consumed by
    /// `SpawnActor` (handle local 3). The canonical `spawn Writer(sink: sink)`
    /// shape.
    fn spawn_blocks() -> Vec<BasicBlock> {
        vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::RecordInit {
                    ty: writer_state_ty(),
                    fields: vec![(FieldOffset(0), Place::Local(1))],
                    dest: Place::Local(2),
                },
                Instr::SpawnActor {
                    actor_name: "Writer".to_string(),
                    state: Some(Place::Local(2)),
                    init_args: vec![],
                    dest: Place::ActorHandle(3),
                    max_heap_bytes: None,
                    cycle_capable: false,
                    mailbox_capacity: None,
                    overflow_policy: None,
                },
            ],
            terminator: Terminator::Return,
        }]
    }

    #[allow(
        clippy::type_complexity,
        reason = "test fixture returns the four detector inputs as a tuple"
    )]
    fn setup() -> (
        BindingId,
        HashMap<BindingId, Place>,
        Vec<(BindingId, String, ResolvedTy)>,
        Vec<ResolvedTy>,
    ) {
        let sink = BindingId(1);
        let mut binding_locals = HashMap::new();
        binding_locals.insert(sink, Place::Local(1));
        let owned = vec![(sink, "sink".to_string(), sink_ty())];
        let mut local_tys = vec![ResolvedTy::I64; 4];
        local_tys[1] = sink_ty();
        local_tys[2] = writer_state_ty();
        (sink, binding_locals, owned, local_tys)
    }

    #[test]
    fn sink_into_spawn_state_is_derived_as_excluded() {
        let (sink, binding_locals, owned, local_tys) = setup();
        let excluded = derive_spawn_consumed_handle_bindings(
            &spawn_blocks(),
            &owned,
            &binding_locals,
            &local_tys,
        );
        assert!(
            excluded.contains(&sink),
            "a Sink half moved into an actor initial-state record consumed by \
             SpawnActor must be derived as spawn-consumed; got {excluded:?}"
        );
    }

    #[test]
    fn spawn_consumed_sink_admitted_with_exclusion_refused_without() {
        let (sink, binding_locals, owned, local_tys) = setup();
        let blocks = spawn_blocks();
        // Negative control: derivation disabled (empty source_excluded) → the
        // source's standalone drop is counted and the SpawnActor escape poisons
        // the origin, so the gate REFUSES.
        let refused_without = detect_unproven_aggregate_handle_double_free(
            &blocks,
            &HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &HashMap::new(),
            &[],
            &HashSet::new(),
            &HashSet::new(),
        );
        assert!(
            is_refused(&refused_without, sink),
            "without the spawn-consumed exclusion the gate must refuse the moved \
             handle (negative control); got {refused_without:?}"
        );
        // With the derivation feeding `source_excluded` → exactly one free (the
        // actor state_drop_fn), so the gate ADMITS.
        let excluded =
            derive_spawn_consumed_handle_bindings(&blocks, &owned, &binding_locals, &local_tys);
        let findings = detect_unproven_aggregate_handle_double_free(
            &blocks,
            &HashMap::new(),
            &owned,
            &binding_locals,
            &local_tys,
            &HashMap::new(),
            &[],
            &excluded,
            &HashSet::new(),
        );
        assert!(
            !is_refused(&findings, sink),
            "with the spawn-consumed exclusion the gate must admit the moved \
             handle; got {findings:?}"
        );
    }

    #[test]
    fn sink_also_returned_is_not_excluded() {
        // A handle flowing BOTH into a spawn-state record AND the ReturnSlot has
        // two candidate owners → left refused fail-closed (not derived).
        let (sink, binding_locals, owned, local_tys) = setup();
        let mut blocks = spawn_blocks();
        blocks[0].instructions.push(Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(1),
        });
        let excluded =
            derive_spawn_consumed_handle_bindings(&blocks, &owned, &binding_locals, &local_tys);
        assert!(
            !excluded.contains(&sink),
            "a handle also moved to the ReturnSlot must NOT be spawn-consumed \
             excluded (fail-closed); got {excluded:?}"
        );
    }
}
#[cfg(test)]
mod generic_record_owned_aggregate_admission {
    //! Slice 1 — the value-class admit authority `is_owned_aggregate_record_ty`
    //! must accept a generic record INSTANTIATION keyed on its `hew_hir::mangle`d
    //! name (`Pair$$i64$string`) once its substituted-field layout is registered,
    //! and stay fail-closed (W3.029) for any instantiation whose mangled layout
    //! is absent or whose substituted fields do not all classify. The key change
    //! is added-around the gate, never relaxing it: the unregistered/unclassifiable
    //! shapes still return `false` here and reject downstream.
    use super::*;

    /// A `Builder` whose `record_field_orders` carries exactly the supplied
    /// (mangled or bare) record keys + their substituted field types.
    fn builder_with_field_orders(orders: Vec<(&str, Vec<(&str, ResolvedTy)>)>) -> Builder {
        let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
        for (key, fields) in orders {
            record_field_orders.insert(
                key.to_string(),
                fields
                    .into_iter()
                    .map(|(fname, fty)| (fname.to_string(), fty))
                    .collect(),
            );
        }
        Builder {
            record_field_orders,
            ..Builder::default()
        }
    }

    fn pair_ty(args: Vec<ResolvedTy>) -> ResolvedTy {
        ResolvedTy::named_user("Pair", args)
    }

    /// `Pair<i64, string>` whose mangled layout `Pair$$i64$string` is registered
    /// with substituted fields `[i64, string]` is admitted: it owns the `string`
    /// field, every field classifies, so codegen can synthesise its drop thunk.
    #[test]
    fn generic_instantiation_with_registered_mangled_layout_is_admitted() {
        let key = hew_hir::mangle("Pair", &[ResolvedTy::I64, ResolvedTy::String]);
        let builder = builder_with_field_orders(vec![(
            key.as_str(),
            vec![("first", ResolvedTy::I64), ("second", ResolvedTy::String)],
        )]);
        assert!(
            builder
                .is_owned_aggregate_record_ty(&pair_ty(vec![ResolvedTy::I64, ResolvedTy::String])),
            "Pair<i64,string> with a registered mangled layout owning a string \
             field must be admitted as an owned-aggregate record"
        );
    }

    /// `Pair<i64, i64>` is all-`BitCopy`: it has no owned field, so the
    /// owned-aggregate authority returns `false` (it is classified by
    /// `ValueClass::of_ty` upstream, not via the drop spine — no drop thunk).
    #[test]
    fn generic_instantiation_all_bitcopy_is_not_owned_aggregate() {
        let key = hew_hir::mangle("Pair", &[ResolvedTy::I64, ResolvedTy::I64]);
        let builder = builder_with_field_orders(vec![(
            key.as_str(),
            vec![("first", ResolvedTy::I64), ("second", ResolvedTy::I64)],
        )]);
        assert!(
            !builder.is_owned_aggregate_record_ty(&pair_ty(vec![ResolvedTy::I64, ResolvedTy::I64])),
            "an all-BitCopy generic instantiation has no owned field and must not \
             be an owned-aggregate record"
        );
    }

    /// Negative control / fail-closed: a generic instantiation whose mangled
    /// layout is NOT registered (the producer never monomorphised it, or a key
    /// mismatch) stays fail-closed — the authority cannot resolve the layout, so
    /// it returns `false` and the W3.029 reject fires downstream.
    #[test]
    fn generic_instantiation_without_registered_layout_fails_closed() {
        // Register only the i64/string layout; ask about i64/bytes.
        let key = hew_hir::mangle("Pair", &[ResolvedTy::I64, ResolvedTy::String]);
        let builder = builder_with_field_orders(vec![(
            key.as_str(),
            vec![("first", ResolvedTy::I64), ("second", ResolvedTy::String)],
        )]);
        assert!(
            !builder
                .is_owned_aggregate_record_ty(&pair_ty(vec![ResolvedTy::I64, ResolvedTy::Bytes])),
            "an unregistered generic instantiation must stay fail-closed (W3.029), \
             never silently admitted"
        );
    }

    /// The bare-name monomorphic path is unchanged: a registered bare record
    /// owning a string field is still admitted (no regression from routing the
    /// key through `user_record_layout_key`).
    #[test]
    fn bare_name_monomorphic_owned_record_still_admitted() {
        let builder = builder_with_field_orders(vec![(
            "PairIS",
            vec![("first", ResolvedTy::I64), ("second", ResolvedTy::String)],
        )]);
        assert!(
            builder.is_owned_aggregate_record_ty(&ResolvedTy::named_user("PairIS", vec![])),
            "the bare-name monomorphic owned record must remain admitted"
        );
    }

    /// P1-1 — fail closed at the value-class gate, not late at codegen. A generic
    /// instantiation whose substituted field is an `#[opaque]` handle
    /// (`json.Value`) classifies as `OpaqueHandle`, whose CLONE direction has no
    /// dup runtime helper. The admit authority must REJECT it at the W3.029
    /// value-class gate — NOT admit it as `CowValue` and let codegen fail closed
    /// late during clone-synthesis. The opaque field is registered with
    /// `is_opaque: true`, so the classifier routes it to `OpaqueHandle` by type
    /// identity (no `opaque_handle_names` entry required).
    #[test]
    fn generic_instantiation_with_opaque_field_rejected_at_value_class_gate() {
        let opaque = ResolvedTy::named_opaque("json.Value", vec![]);
        // Register under the SHORT-arg-normalised key the admit authority now
        // computes (`Pair$$Value$i64`, matching codegen), so the layout RESOLVES
        // and the rejection is genuinely the clone-helper-supported gate (P1-1),
        // not a key miss. The substituted field stays the opaque handle.
        let key = hew_hir::mangle(
            "Pair",
            &[ResolvedTy::named_user("Value", vec![]), ResolvedTy::I64],
        );
        let builder = builder_with_field_orders(vec![(
            key.as_str(),
            vec![("first", opaque.clone()), ("second", ResolvedTy::I64)],
        )]);
        // Sanity: the layout resolves under the authority's computed key — so a
        // pass below is the clone-support gate firing, never a lookup miss.
        assert!(
            builder
                .owned_aggregate_record_field_kinds_for_key(&key)
                .is_ok_and(|kinds| kinds.is_none()),
            "the opaque-bearing layout resolves but the clone-support gate must \
             reject it (None), not a key miss"
        );
        assert!(
            !builder.is_owned_aggregate_record_ty(&pair_ty(vec![opaque, ResolvedTy::I64])),
            "Pair<json.Value, i64> carries an OpaqueHandle field whose clone \
             direction has no helper; it must fail closed at the W3.029 \
             value-class gate, NOT admit as CowValue and refuse late at codegen"
        );
    }

    /// Companion to the opaque-reject test: the supported drop-matrix shapes are
    /// NOT over-rejected by the clone-helper-supported gate. A nested-Vec owned
    /// field (`Pair<string, Vec<i64>>`) — the deepest supported owned shape in
    /// the corpus — still admits.
    #[test]
    fn generic_instantiation_with_supported_owned_fields_still_admitted() {
        let vec_i64 =
            ResolvedTy::named_builtin("Vec", hew_types::BuiltinType::Vec, vec![ResolvedTy::I64]);
        let key = hew_hir::mangle("Pair", &[ResolvedTy::String, vec_i64.clone()]);
        let builder = builder_with_field_orders(vec![(
            key.as_str(),
            vec![("first", ResolvedTy::String), ("second", vec_i64.clone())],
        )]);
        assert!(
            builder.is_owned_aggregate_record_ty(&pair_ty(vec![ResolvedTy::String, vec_i64])),
            "Pair<string, Vec<i64>> carries only clone/drop-supported owned \
             fields and must still admit — the opaque gate must not over-reject"
        );
    }
}
#[cfg(test)]
mod plain_vec_drop_interior_alias_and_escape {
    //! Revision regression net for the plain-`Vec` scope-exit release
    //! (`fix/v050-plain-vec-local-drop`). The shipped
    //! `derive_local_collection_drop_allowed` escape scan caught a handle
    //! aliased OUT of its slot (read as an owning sink) but had NO interior-alias
    //! INGRESS exclusion: a candidate whose slot is itself a BORROW of a
    //! still-live parent — `let r = data.get(row)` lowering to
    //! `hew_vec_get_ptr(data, row)` → `Move` → `r` — was admitted for
    //! `hew_vec_free`, double-freeing the element the parent vector still owns
    //! (the csv `Table::get` UAF). These fixtures pin the prover's allow-set —
    //! the authority that drives the `hew_vec_free` drop plan — for the
    //! aggregate-escape and interior-alias shapes, NOT behaviourally.
    use super::*;

    fn vec_string_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::String],
            builtin: Some(BuiltinType::Vec),
            is_opaque: false,
        }
    }

    fn ty_is_vec_handle(ty: &ResolvedTy) -> bool {
        matches!(
            ty,
            ResolvedTy::Named {
                builtin: Some(BuiltinType::Vec),
                ..
            }
        )
    }

    fn tbl_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Tbl".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }
    }

    fn push_str(receiver: u32, value: u32, next: u32) -> Terminator {
        Terminator::Call {
            callee: "hew_vec_push_str".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("hew_vec_push_str"),
            args: vec![Place::Local(receiver), Place::Local(value)],
            dest: None,
            next,
        }
    }

    /// (a) push-then-record-return. A `Vec<string>` mutated via `push` (a
    /// receiver-borrow interior read, NOT an escape) and then moved into a
    /// record constructed at the return position. The handle escapes into the
    /// returned aggregate — the caller owns it now — so a producer-side
    /// `hew_vec_free` would double-free; the prover must exclude it.
    #[test]
    fn push_then_record_return_excludes_escaping_vec() {
        let hdrs = BindingId(401);
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: push_str(1, 4, 1),
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![Instr::RecordInit {
                    ty: tbl_ty(),
                    fields: vec![(FieldOffset(0), Place::Local(1))],
                    dest: Place::Local(6),
                }],
                terminator: Terminator::Return,
            },
        ];
        let owned_locals = vec![(hdrs, "hdrs".to_string(), vec_string_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(hdrs, Place::Local(1))].into_iter().collect();

        let allowed = derive_local_collection_drop_allowed(
            &blocks,
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &HashMap::new(),
            ty_is_vec_handle,
        );
        assert!(
            !allowed.contains(&hdrs),
            "a Vec pushed-to then moved into a returned record escapes; the \
             caller owns the buffer now and a producer-side hew_vec_free \
             double-frees. allowed: {allowed:?}"
        );
    }

    /// (b) empty-literal alias + push + aggregate (the double-drop). The `[]`
    /// desugar mints a synthetic `__hew_array_0` temp that the binding `hdrs`
    /// whole-value-aliases (`Move` chain `arr → t → hdrs`); `hdrs` is then
    /// pushed-to and moved into a returned record. BOTH alias-group members map
    /// to the same buffer, so admitting EITHER for `hew_vec_free` double-frees
    /// even before the escape is considered. The prover must clear NEITHER.
    #[test]
    fn empty_literal_alias_group_into_aggregate_excludes_both() {
        let arr = BindingId(411);
        let hdrs = BindingId(412);
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![
                    // arr (Local 1) → temp (Local 2) → hdrs (Local 3): the
                    // array-literal-desugar whole-value hand-off chain.
                    Instr::Move {
                        dest: Place::Local(2),
                        src: Place::Local(1),
                    },
                    Instr::Move {
                        dest: Place::Local(3),
                        src: Place::Local(2),
                    },
                ],
                terminator: push_str(3, 4, 1),
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![Instr::RecordInit {
                    ty: tbl_ty(),
                    fields: vec![(FieldOffset(0), Place::Local(3))],
                    dest: Place::Local(6),
                }],
                terminator: Terminator::Return,
            },
        ];
        let owned_locals = vec![
            (arr, "__hew_array_0".to_string(), vec_string_ty()),
            (hdrs, "hdrs".to_string(), vec_string_ty()),
        ];
        let binding_locals: HashMap<BindingId, Place> =
            [(arr, Place::Local(1)), (hdrs, Place::Local(3))]
                .into_iter()
                .collect();

        let allowed = derive_local_collection_drop_allowed(
            &blocks,
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &HashMap::new(),
            ty_is_vec_handle,
        );
        assert!(
            allowed.is_empty(),
            "neither the empty-literal temp nor its `hdrs` alias may earn a \
             free: they share one buffer (double-free) and that buffer escapes \
             into the returned record. allowed: {allowed:?}"
        );
    }

    /// (c) the csv `parse_impl` shape: a `Vec<string>` `headers` mutated by
    /// nested-loop pushes and moved into the returned `Table` at TWO return
    /// sites (an early `return Table{..}` and the tail `Table{..}`). The
    /// whole-function escape scan must exclude it at both — a handle that
    /// escapes on ANY exit can never earn a producer-side free.
    #[test]
    fn csv_parse_impl_two_return_sites_excludes_returned_vec() {
        let headers = BindingId(421);
        let blocks = vec![
            // Branch: early-return arm (block 1) vs body (block 2).
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Branch {
                    cond: Place::Local(9),
                    then_target: 1,
                    else_target: 2,
                },
            },
            // Early return: `return Table { hdrs: headers, .. }`.
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![Instr::RecordInit {
                    ty: tbl_ty(),
                    fields: vec![(FieldOffset(0), Place::Local(1))],
                    dest: Place::Local(6),
                }],
                terminator: Terminator::Return,
            },
            // Body: a loop-body push, then the tail `Table { hdrs: headers }`.
            BasicBlock {
                id: 2,
                statements: vec![],
                instructions: vec![],
                terminator: push_str(1, 4, 3),
            },
            BasicBlock {
                id: 3,
                statements: vec![],
                instructions: vec![Instr::RecordInit {
                    ty: tbl_ty(),
                    fields: vec![(FieldOffset(0), Place::Local(1))],
                    dest: Place::Local(7),
                }],
                terminator: Terminator::Return,
            },
        ];
        let owned_locals = vec![(headers, "headers".to_string(), vec_string_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(headers, Place::Local(1))].into_iter().collect();

        let allowed = derive_local_collection_drop_allowed(
            &blocks,
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &HashMap::new(),
            ty_is_vec_handle,
        );
        assert!(
            !allowed.contains(&headers),
            "`headers` escapes into the returned Table on both return paths; it \
             must not earn a producer-side free. allowed: {allowed:?}"
        );
    }

    /// (d) the interior-alias UAF the revision fixes (csv `Table::get`). A
    /// `Vec<string>` element loaded out of a `Vec<Vec<string>>` parent via the
    /// borrowing getter `hew_vec_get_ptr` (a `Terminator::Call`, the
    /// `data.get(row)` method-path lowering) is rebound (`let r = ..`) and only
    /// read (`r.len()`). The slot is a BORROW of the parent vector's element —
    /// the parent still owns the buffer — so a scope-exit `hew_vec_free(r)`
    /// double-frees. The prover must exclude `r`.
    #[test]
    fn interior_borrow_terminator_getter_excludes_aliased_element() {
        let r = BindingId(431);
        let blocks = vec![
            // parent = tbl.data (a record field load — an interior alias seed);
            // then r_tmp = hew_vec_get_ptr(parent, idx) (a borrow of the slot).
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(1),
                    dest: Place::Local(11),
                }],
                terminator: Terminator::Call {
                    callee: "hew_vec_get_ptr".to_string(),
                    builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                        "hew_vec_get_ptr",
                    ),
                    args: vec![Place::Local(11), Place::Local(1)],
                    dest: Some(Place::Local(12)),
                    next: 1,
                },
            },
            // r = r_tmp (rebind), then a receiver-borrow read `r.len()`.
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![Instr::Move {
                    dest: Place::Local(13),
                    src: Place::Local(12),
                }],
                terminator: Terminator::Call {
                    callee: "hew_vec_len".to_string(),
                    builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                        "hew_vec_len",
                    ),
                    args: vec![Place::Local(13)],
                    dest: Some(Place::Local(18)),
                    next: 2,
                },
            },
            BasicBlock {
                id: 2,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ];
        let owned_locals = vec![(r, "r".to_string(), vec_string_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(r, Place::Local(13))].into_iter().collect();

        let allowed = derive_local_collection_drop_allowed(
            &blocks,
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &HashMap::new(),
            ty_is_vec_handle,
        );
        assert!(
            !allowed.contains(&r),
            "`r = data.get(row)` is a borrow of the parent vector's element \
             slot (hew_vec_get_ptr); a producer-side hew_vec_free double-frees \
             the buffer the parent still owns. allowed: {allowed:?}"
        );
    }

    /// (d′) the same interior-alias UAF via the `xs[i]` INDEX-path lowering,
    /// where the borrowing getter is an `Instr::CallRuntimeAbi` rather than a
    /// `Terminator::Call`. Both lowering shapes must taint the result.
    #[test]
    fn interior_borrow_instr_getter_excludes_aliased_element() {
        let r = BindingId(441);
        let blocks = vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::CallRuntimeAbi(
                    crate::model::RuntimeCall::new(
                        "hew_vec_get_ptr",
                        vec![Place::Local(11), Place::Local(1)],
                        Some(Place::Local(12)),
                    )
                    .expect("hew_vec_get_ptr is an allowlisted runtime symbol"),
                ),
                Instr::Move {
                    dest: Place::Local(13),
                    src: Place::Local(12),
                },
            ],
            terminator: Terminator::Return,
        }];
        let owned_locals = vec![(r, "r".to_string(), vec_string_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(r, Place::Local(13))].into_iter().collect();

        let allowed = derive_local_collection_drop_allowed(
            &blocks,
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &HashMap::new(),
            ty_is_vec_handle,
        );
        assert!(
            !allowed.contains(&r),
            "the index-path (Instr) hew_vec_get_ptr borrow must be tainted too; \
             allowed: {allowed:?}"
        );
    }

    /// Positive control: a FRESH, non-escaping local `Vec<string>` (built in
    /// place, mutated via `push`, read via `len`, never moved out or aliased
    /// in) is the sole owner and MUST stay admitted — the interior-alias
    /// exclusion must not over-exclude and silently reintroduce the very leak
    /// the lane exists to close.
    #[test]
    fn fresh_non_escaping_local_vec_stays_admitted() {
        let xs = BindingId(451);
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: push_str(1, 4, 1),
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Call {
                    callee: "hew_vec_len".to_string(),
                    builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                        "hew_vec_len",
                    ),
                    args: vec![Place::Local(1)],
                    dest: Some(Place::Local(2)),
                    next: 2,
                },
            },
            BasicBlock {
                id: 2,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ];
        let owned_locals = vec![(xs, "xs".to_string(), vec_string_ty())];
        let binding_locals: HashMap<BindingId, Place> =
            [(xs, Place::Local(1))].into_iter().collect();

        let allowed = derive_local_collection_drop_allowed(
            &blocks,
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &HashMap::new(),
            ty_is_vec_handle,
        );
        assert!(
            allowed.contains(&xs),
            "a fresh sole-owner local Vec must stay admitted for its scope-exit \
             hew_vec_free (the leak the lane closes); allowed: {allowed:?}"
        );
    }
}
