#![allow(
    deprecated,
    reason = "temporary named identity reconstruction migration seam"
)]

mod aggregate_borrowed_ingress_clone;
mod builtin_handle_record_field_overwrite;
mod bytes_payload_handoff;
mod foundation;
mod opaque_resource_field_misuse;
mod retained_string_aliases;
mod shell_drop_safety;
#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    alias_projection_chain_owner_seeds, attribute_field_binder_provenance, base_local,
    binder_read_is_borrow_safe_instr, binder_read_is_borrow_safe_terminator, block_dominators,
    blocks_reachable_from, bytes_interior_producer_dest, bytes_place_is_typed,
    bytes_runtime_arg_is_borrow, bytes_share_sink_places, collect_record_field_binders,
    compute_collection_interior_alias_taint, descend_match_bound_hop_alias_chain,
    instr_source_places, local_is_byte_copy_aggregate, propagate_whole_value_alias_roots,
    render_owned_handle_ty, shift_instr_spans_on_insert, string_field_load_producer_dest,
    terminator_source_places, ty_is_heap_owning_tuple, user_record_layout_key,
    vec_iter_record_init_vec_source, AggregateOwner, BTreeMap, BasicBlock, BindingId,
    BytesDropDerivation, BytesRetainPlacement, BytesRetainSite, ClosureEnvFieldOwnership,
    FieldBinderProvenance, FieldOffset, HashMap, HashSet, Instr, MirCheck, MirStatement, Place,
    ResolvedTy, RootScan, StringRetainCondition, SuspendKind, Terminator,
};
pub(crate) use aggregate_borrowed_ingress_clone::string_or_bitcopy_tree;
use aggregate_borrowed_ingress_clone::{
    aggregate_borrowed_ingress_retain_clones_value, aggregate_borrowed_ingress_sink_clones_source,
};
pub(super) use builtin_handle_record_field_overwrite::detect_builtin_handle_record_field_overwrite;
use bytes_payload_handoff::provable_bytes_payload_handoff_sites;
#[cfg(test)]
use bytes_payload_handoff::BytesPayloadHandoff;
use foundation::{
    generator_env_snapshot_init_locals, initializes_generator_env_snapshot,
    local_ty_carries_drop_obligation,
};
pub(super) use opaque_resource_field_misuse::detect_opaque_resource_field_misuse;
use retained_string_aliases::uniquely_defined_retained_string_field_load_aliases;
pub(super) use shell_drop_safety::{
    direct_payload_has_registered_resource_record, enum_payloads_are_shell_drop_safe,
};

type FieldDropInsertions = Vec<(u32, usize, Vec<Instr>)>;

/// #2212 — discharge the non-escaped owned sibling fields of a record ONE of
/// whose fields escaped through a field binder.
///
/// When an owned-field binder loaded from a record root escapes (the escapee
/// owns that field now), the root's whole-record release must not walk the
/// escaped field, and every OTHER owned field of the record — still solely
/// owned by the record slot — leaked (#2212: one 64 B `tag` buffer per
/// frame at slope 1). This pass emits one `Instr::FieldDropInPlace` per
/// non-escaped owned sibling right after the escape instruction, or at the
/// front of a consuming call's unique continuation, where the value flow
/// proves that point is past the record's last use.
///
/// Runs post-seal, after `apply_nested_fresh_string_temp_drops` and before
/// `check_function` / drop elaboration, so the dataflow observes each
/// discharge as a read of the record local and codegen emits the release.
///
/// ## Fail-closed admission (ALL conditions required; any miss keeps
/// today's whole-record leak — never a double-free)
///
/// 1. The root is an `owned_locals` record with at least one projected owned
///    field and its whole-value alias set is the root alone (no `let b2 = b`
///    copies — copies byte-share field pointers and can diverge; the
///    discharge frees through the root slot only). The record need not admit
///    a whole-value `RecordInPlace` drop: mixed resource/COW records are the
///    field-granular case this pass must cover.
/// 2. Exactly ONE escape event exists across the root's binders. It is either
///    an instruction or a consuming call terminator whose continuation has
///    exactly that call as its predecessor, and the escaping binder's
///    provenance is `Unique { root, field }` — the value flow proves both the
///    root and WHICH field escaped. The escaped field is never discharged:
///    for a moved-out binder the escapee owns it. A proven retained `string`
///    clone is the exception: the escapee owns an independent share, so this
///    pass also discharges the record's original field.
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
/// keeps its leak. The emitted op's base is the root local, so the drop-plan
/// verifier's inline-composite pairing rule (`validate_field_drop_in_place`)
/// rejects any exit plan that still walks the root — admission and
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
    lifecycle_registry: &hew_hir::LifecycleRegistry,
    alias_chain: &[(u32, u32, u32)],
    aggregate_clone_sites: &HashSet<(u32, usize, Place)>,
    projection_borrow_cursor_inits: &HashSet<Place>,
    is_owned_record: &dyn Fn(&ResolvedTy) -> bool,
    owned_field_list: &dyn Fn(&ResolvedTy) -> Vec<(u32, ResolvedTy)>,
    owned_tuple_field_list: &dyn Fn(&ResolvedTy) -> Vec<(u32, ResolvedTy)>,
    field_dischargeable: &dyn Fn(&ResolvedTy) -> bool,
    leaf_field_drop: &dyn Fn(Place, u32, &ResolvedTy) -> Option<Instr>,
    instr_spans: &mut BTreeMap<(u32, u32), (u32, u32)>,
) {
    let owned_binding_bases: HashSet<u32> = owned_locals
        .iter()
        .filter_map(|(binding, _, _)| binding_locals.get(binding).and_then(|p| base_local(*p)))
        .collect();
    let neutralized_payload_slots: HashSet<Place> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::NeutralizePayloadSlot { place, .. } => Some(*place),
            _ => None,
        })
        .collect();
    let carrier_backed_payload_aliases: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::Move {
                dest: Place::Local(dest),
                src:
                    source @ (Place::MachineVariant { local: carrier, .. }
                    | Place::EnumVariant { local: carrier, .. }),
            } if owned_binding_bases.contains(carrier)
                && !neutralized_payload_slots.contains(source) =>
            {
                Some(*dest)
            }
            _ => None,
        })
        .collect();

    // Candidate roots: base locals of record bindings with projected owned
    // fields. This intentionally includes mixed resource/COW records which
    // cannot admit a whole-record RecordInPlace drop but whose individually
    // admissible siblings can still be discharged safely.
    let mut root_record_ty: HashMap<u32, ResolvedTy> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !is_owned_record(ty) && owned_field_list(ty).is_empty() {
            continue;
        }
        let Some(place) = binding_locals.get(binding) else {
            continue;
        };
        let Some(local) = base_local(*place) else {
            continue;
        };
        // A match payload binder backed by an owned, non-neutralized carrier is
        // a byte-copy view. The carrier's terminal enum drop still owns every
        // original field; sibling discharges from the binder would release the
        // same storage a second time (rC's `Option<Secret>` path).
        if carrier_backed_payload_aliases.contains(&local) {
            continue;
        }
        root_record_ty.insert(local, ty.clone());
    }
    // Tuple candidate roots (#2383): base locals of owned heap-owning TUPLE
    // bindings — the tuple composite prover's candidate set. The one-hop scan
    // below is record-only; the multi-hop chain compensator walks BOTH root
    // kinds, because a deep alias escaping from a tuple root leaves the
    // chain siblings unreleased exactly as it does from a record root, and
    // an escape without compensation leaks every chain sibling.
    let mut root_tuple_ty: HashMap<u32, ResolvedTy> = HashMap::new();
    for (binding, _name, ty) in owned_locals {
        if !ty_is_heap_owning_tuple(ty, record_field_orders, enum_layouts, lifecycle_registry) {
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
        local_tys.get(local as usize).is_some_and(|ty| {
            local_ty_carries_drop_obligation(
                ty,
                record_field_orders,
                enum_layouts,
                lifecycle_registry,
            )
        })
    };
    let retained_string_field_aliases =
        uniquely_defined_retained_string_field_load_aliases(blocks, local_tys);
    let all_field_binders = collect_record_field_binders(blocks, &alias_of, &local_is_heap_owning);
    let provenance = attribute_field_binder_provenance(blocks, &alias_of, &all_field_binders);
    // A join local with another, unrelated definition is not an ownership
    // hand-off. Stop the binder chain at its last uniquely-attributed source
    // so sibling cleanup can run on that predecessor before the values merge.
    let field_binders: HashSet<u32> = all_field_binders
        .into_iter()
        .filter(|local| {
            !matches!(
                provenance.get(local),
                Some(FieldBinderProvenance::Ambiguous) | None
            )
        })
        .collect();
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
    let (mut insertions, explicit_projection_roots) = explicit_projection_transfer_sibling_drops(
        blocks,
        &root_record_ty,
        &alias_of,
        owned_field_list,
        field_dischargeable,
        leaf_field_drop,
    );
    // Condition 3 — base locals of every owned binding (a binder in this set
    // is an extracted field with its own release path).
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
                                scan.escapes.push((bid, Some($pos), *field, $binder));
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
                Instr::TupleFieldLoad { tuple, dest, .. } => {
                    if let Some(local) = base_local(*tuple) {
                        if field_binders.contains(&local) {
                            site!(binder_root(local), Some(idx));
                        }
                    }
                    if let Some(&root) = base_local(*dest).and_then(|dl| alias_of.get(&dl)) {
                        poison!(root);
                    }
                }
                Instr::RecordInit { fields, dest, .. } => {
                    // A live-root field-projection cursor init BORROWS its
                    // `vec`-field binder (`vec_iter_projection_borrow_inits`):
                    // the root keeps sole ownership, so the read is a use
                    // site, never an escape event — the same borrow the
                    // cursor's ingress classification records, so admission
                    // and discharge cannot disagree.
                    let borrowed_cursor_vec_src = if projection_borrow_cursor_inits.contains(dest) {
                        vec_iter_record_init_vec_source(instr)
                    } else {
                        None
                    };
                    for (_, p) in fields {
                        if borrowed_cursor_vec_src == Some(*p) {
                            if let Some(l) = base_local(*p) {
                                if field_binders.contains(&l) {
                                    site!(binder_root(l), Some(idx));
                                }
                            }
                            continue;
                        }
                        if aggregate_borrowed_ingress_sink_clones_source(
                            block,
                            idx,
                            *p,
                            Some(aggregate_clone_sites),
                            local_tys,
                            record_field_orders,
                            enum_layouts,
                        ) {
                            continue;
                        }
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
                            if retained_string_field_aliases.contains(&l) {
                                // String field loads own a retained read-copy.
                                // Its balancing drop does not discharge the
                                // record's original field.
                                site!(binder_root(l), Some(idx));
                            } else {
                                // An inline release of any other binder is a
                                // second release path this pass must not race.
                                match binder_root(l) {
                                    Some(r) => poison!(r),
                                    None => poison_all = true,
                                }
                            }
                        }
                    }
                }
                Instr::FieldDropInPlace { base, .. }
                | Instr::RecordFieldDrop { record: base, .. } => {
                    // A separate field release is a second discharge path.
                    if let Some(l) = base_local(*base) {
                        if let Some(&root) = alias_of.get(&l) {
                            poison!(root);
                        }
                    }
                }
                Instr::RecordFieldStore { record, src, .. } => {
                    if let Some(member) = base_local(*record) {
                        if let Some(&root) = alias_of.get(&member) {
                            if member == root {
                                // Overwrite lowering has already discharged
                                // the old field value. The authoritative root
                                // still owns the replacement, so this is a
                                // modeled use rather than an escape.
                                site!(Some(root), Some(idx));
                            } else {
                                // Updating a byte-copy temporary can diverge
                                // it from the authoritative root slot.
                                poison!(root);
                            }
                        }
                    }
                    if aggregate_borrowed_ingress_sink_clones_source(
                        block,
                        idx,
                        *src,
                        Some(aggregate_clone_sites),
                        local_tys,
                        record_field_orders,
                        enum_layouts,
                    ) {
                        continue;
                    }
                    if let Some(l) = base_local(*src) {
                        if field_binders.contains(&l) {
                            // Binder stored into another aggregate's field
                            // slot — an owning sink.
                            escape!(l, idx);
                        }
                    }
                }
                Instr::StringRetain { value, .. }
                    if base_local(*value)
                        .and_then(|local| local_tys.get(local as usize))
                        .is_some_and(|ty| {
                            aggregate_borrowed_ingress_retain_clones_value(
                                instr,
                                *value,
                                ty,
                                record_field_orders,
                                enum_layouts,
                            )
                        }) => {}
                other => {
                    let (reads, writes, _) = crate::dataflow::instr_reads_writes(other);
                    for p in reads {
                        if aggregate_borrowed_ingress_sink_clones_source(
                            block,
                            idx,
                            p,
                            Some(aggregate_clone_sites),
                            local_tys,
                            record_field_orders,
                            enum_layouts,
                        ) {
                            continue;
                        }
                        if let Some(l) = base_local(p) {
                            if let Some(&root) = alias_of.get(&l) {
                                if binder_read_is_borrow_safe_instr(other, l) {
                                    site!(Some(root), Some(idx));
                                } else {
                                    // Any unmodelled read of the aggregate itself.
                                    poison!(root);
                                }
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
                } else if matches!(block.terminator, Terminator::Call { .. }) {
                    match provenance.get(&l) {
                        Some(FieldBinderProvenance::Unique { root, field }) => {
                            if let Some(scan) = scans.get_mut(root) {
                                scan.escapes.push((bid, None, *field, l));
                            }
                        }
                        Some(FieldBinderProvenance::RootOnly { root }) => {
                            if let Some(scan) = scans.get_mut(root) {
                                scan.poisoned = true;
                            }
                        }
                        _ => poison_all = true,
                    }
                } else {
                    // A non-call terminator escape has no normal continuation
                    // where sibling cleanup can run.
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
    for root in roots {
        if explicit_projection_roots.contains(&root) {
            continue;
        }
        let scan = &scans[&root];
        if scan.poisoned || member_count.get(&root).copied().unwrap_or(0) != 1 {
            continue;
        }
        let &[(esc_block, esc_idx, esc_field, esc_binder)] = &scan.escapes[..] else {
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
        let insertion = if let Some(esc_idx) = esc_idx {
            (esc_block, esc_idx + 1)
        } else {
            let Some(escape_block) = blocks.iter().find(|block| block.id == esc_block) else {
                continue;
            };
            let Terminator::Call { next, .. } = &escape_block.terminator else {
                continue;
            };
            let predecessors: HashSet<u32> = blocks
                .iter()
                .filter(|block| block.successors().contains(next))
                .map(|block| block.id)
                .collect();
            if predecessors != HashSet::from([esc_block])
                || !blocks.iter().any(|block| block.id == *next)
            {
                continue;
            }
            (*next, 0)
        };
        let in_region = |&(sb, si): &(u32, Option<usize>)| -> bool {
            match esc_idx {
                Some(esc_idx) if sb == esc_block => si.is_none_or(|i| i > esc_idx),
                None if sb == esc_block => false,
                _ => reach.contains(&sb),
            }
        };
        if scan.sites.iter().any(in_region) || global_sites.iter().any(in_region) {
            continue;
        }
        let record_ty = &root_record_ty[&root];
        let retained_clone_escape = retained_string_field_aliases.contains(&esc_binder);
        let siblings: Vec<Instr> = owned_field_list(record_ty)
            .into_iter()
            .filter(|(idx, _)| retained_clone_escape || *idx != esc_field)
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
        let (insert_block, insert_idx) = insertion;
        insertions.push((insert_block, insert_idx, siblings));
    }
    // The one-hop scan above sees only a binder loaded DIRECTLY off a whole-value
    // alias member, so a ≥2-hop escape (`let mid = o.mid; let leaf = mid.leaf;
    // return leaf`) is invisible to it — yet the escapee owns the subtree just
    // the same. Walk the recorded alias chain — record AND tuple roots
    // (#2383) — plus the #2387 match-bound byte-copy hop chain, and discharge
    // the non-escaped siblings at every level so the escape never outruns its
    // compensation.
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
        record_field_orders,
        enum_layouts,
        aggregate_clone_sites,
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

/// Discharge siblings after lowering has published an exact field-projection
/// transfer for the aggregate generation. Unlike the legacy escape scan, this
/// proof permits the extracted binder to keep its own release path: the
/// transfer event has already ended the parent owner, so only later reads of
/// the parent storage can make an immediate sibling release unsafe.
fn explicit_projection_transfer_sibling_drops(
    blocks: &[BasicBlock],
    root_record_ty: &HashMap<u32, ResolvedTy>,
    alias_of: &HashMap<u32, u32>,
    owned_field_list: &dyn Fn(&ResolvedTy) -> Vec<(u32, ResolvedTy)>,
    field_dischargeable: &dyn Fn(&ResolvedTy) -> bool,
    leaf_field_drop: &dyn Fn(Place, u32, &ResolvedTy) -> Option<Instr>,
) -> (FieldDropInsertions, HashSet<u32>) {
    use crate::model::OwnershipEvent;

    let mut candidates: HashMap<u32, Vec<(u32, usize, u32, u32)>> = HashMap::new();
    for block in blocks {
        for (index, pair) in block.instructions.windows(2).enumerate() {
            let [Instr::RecordFieldLoad {
                record,
                field_offset,
                ..
            }, Instr::OwnershipEvent(OwnershipEvent::Transfer {
                from,
                to: None,
                to_owner: None,
                ..
            })] = pair
            else {
                continue;
            };
            let Some(record_local) = base_local(*record) else {
                continue;
            };
            let root = alias_of.get(&record_local).copied().unwrap_or(record_local);
            if base_local(*from) == Some(record_local) {
                candidates.entry(root).or_default().push((
                    block.id,
                    index + 1,
                    field_offset.0,
                    record_local,
                ));
            }
        }
    }

    let mut insertions = Vec::new();
    let mut proven_roots = HashSet::new();
    for (root, transfers) in candidates {
        let [(block_id, transfer_index, escaped_field, record_local)] = transfers.as_slice() else {
            continue;
        };
        let root_members = alias_of
            .iter()
            .filter_map(|(local, candidate_root)| (*candidate_root == root).then_some(*local))
            .chain(std::iter::once(root))
            .collect::<HashSet<_>>();
        let reachable = blocks_reachable_from(blocks, *block_id);
        if reachable.contains(block_id) {
            continue;
        }
        let parent_read_after_transfer = blocks.iter().any(|block| {
            let in_region = if block.id == *block_id {
                Some(*transfer_index + 1)
            } else if reachable.contains(&block.id) {
                Some(0)
            } else {
                None
            };
            let Some(start) = in_region else {
                return false;
            };
            block.instructions[start.min(block.instructions.len())..]
                .iter()
                .flat_map(instr_source_places)
                .chain(terminator_source_places(&block.terminator, None))
                .filter_map(base_local)
                .any(|local| root_members.contains(&local))
        });
        if parent_read_after_transfer {
            continue;
        }
        proven_roots.insert(root);
        let Some(record_ty) = root_record_ty
            .get(record_local)
            .or_else(|| root_record_ty.get(&root))
        else {
            continue;
        };
        let siblings = owned_field_list(record_ty)
            .into_iter()
            .filter(|(field, _)| *field != *escaped_field)
            .filter_map(|(field, ty)| {
                leaf_field_drop(Place::Local(*record_local), field, &ty).or_else(|| {
                    field_dischargeable(&ty).then_some(Instr::FieldDropInPlace {
                        base: Place::Local(*record_local),
                        field: crate::model::FieldAddr::Record(FieldOffset(field)),
                        ty,
                    })
                })
            })
            .collect::<Vec<_>>();
        if !siblings.is_empty() {
            insertions.push((*block_id, *transfer_index + 1, siblings));
        }
    }
    (insertions, proven_roots)
}
/// Multi-hop sibling discharge for the ESCAPED deep-alias chain
/// (`let mid = o.mid; let leaf = mid.leaf; return leaf` and its tuple twin
/// `let mid = o.0; let leaf = mid.0; leaf`), the ≥2-hop companion to the
/// one-hop scan in [`apply_escaped_record_sibling_field_drops`].
///
/// When a ≥2-hop alias escapes into an owning sink the OWNER root's whole
/// composite drop must not walk the escaped subtree — otherwise the owner
/// would free a subtree the escapee already handed to the caller (the
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
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
    aggregate_clone_sites: &HashSet<(u32, usize, Place)>,
) -> FieldDropInsertions {
    // Immediate-parent map: alias_local -> (parent_local, field ordinal it reads).
    // A retained string field load is a fresh co-owner, so it terminates the
    // byte-alias chain rather than transferring the parent's original field.
    let retained_string_aliases =
        uniquely_defined_retained_string_field_load_aliases(blocks, local_tys);
    let parent_of: HashMap<u32, (u32, u32)> = alias_chain
        .iter()
        .filter(|(alias, _, _)| !retained_string_aliases.contains(alias))
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
                        if aggregate_borrowed_ingress_sink_clones_source(
                            block,
                            idx,
                            *place,
                            Some(aggregate_clone_sites),
                            local_tys,
                            record_field_orders,
                            enum_layouts,
                        ) {
                            continue;
                        }
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
                        if aggregate_borrowed_ingress_sink_clones_source(
                            block,
                            idx,
                            field.src,
                            Some(aggregate_clone_sites),
                            local_tys,
                            record_field_orders,
                            enum_layouts,
                        ) {
                            continue;
                        }
                        if let Some(l) = base_local(field.src) {
                            if let Some(&escapee) = carrier_of.get(&l) {
                                escapes.push((escapee, block.id, idx));
                            }
                        }
                    }
                }
                Instr::RecordFieldStore { src, .. } => {
                    if aggregate_borrowed_ingress_sink_clones_source(
                        block,
                        idx,
                        *src,
                        Some(aggregate_clone_sites),
                        local_tys,
                        record_field_orders,
                        enum_layouts,
                    ) {
                        continue;
                    }
                    if let Some(l) = base_local(*src) {
                        if let Some(&escapee) = carrier_of.get(&l) {
                            escapes.push((escapee, block.id, idx));
                        }
                    }
                }
                Instr::StringRetain { value, .. }
                    if base_local(*value)
                        .and_then(|local| local_tys.get(local as usize))
                        .is_some_and(|ty| {
                            aggregate_borrowed_ingress_retain_clones_value(
                                instr,
                                *value,
                                ty,
                                record_field_orders,
                                enum_layouts,
                            )
                        }) => {}
                other => {
                    let (reads, _, _) = crate::dataflow::instr_reads_writes(other);
                    for place in reads {
                        if aggregate_borrowed_ingress_sink_clones_source(
                            block,
                            idx,
                            place,
                            Some(aggregate_clone_sites),
                            local_tys,
                            record_field_orders,
                            enum_layouts,
                        ) {
                            continue;
                        }
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
/// Fail-closed retain derivation for **local `bytes`** bindings. Returns the
/// set of bindings proven to be sole owners (`allowed`) and the explicit MIR
/// retain markers that mint additional owners. `allowed` gates only which
/// retain markers are materialized and which hand-offs mark their source
/// consumed — mint-site decisions; exit plans derive from the owner replay.
/// Codegen consumes only the markers; it never independently infers bytes
/// retains from a type-shaped LLVM value.
///
/// Structure is the default-deny escape scan: candidate collection,
/// whole-value alias propagation through `Move`, then an escape scan where
/// any read of an alias member that is not positively classified as a borrow
/// EXCLUDES the whole alias group. A binding the scan does not positively
/// clear is never treated as a co-owner mint site; it never double-frees.
/// Two bytes-specific points:
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
              state; splitting \
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
    let binding_local_bases: HashSet<u32> = binding_locals
        .values()
        .filter_map(|place| base_local(*place))
        .collect();
    let payload_handoff_sites = provable_bytes_payload_handoff_sites(
        blocks,
        local_tys,
        &candidate_local_to_binding,
        &binding_local_bases,
    );

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
    for handoff in payload_handoff_sites.values() {
        tainted.remove(&handoff.dest_local);
    }

    // Whole-value alias set: each candidate plus every local reachable through
    // forward-propagated whole-value `Move { dest: Local, src: Local }` copies.
    // Monotone: a slot reachable from two distinct roots is evicted (#1942).
    let alias_of =
        propagate_whole_value_alias_roots(blocks, candidate_local_to_binding.keys().copied());
    let borrowed_alias_of =
        propagate_whole_value_alias_roots(blocks, borrowed_param_locals.iter().copied());

    let mut excluded_roots: HashSet<u32> = HashSet::new();
    let generator_env_inits = generator_env_snapshot_init_locals(blocks);
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
            if initializes_generator_env_snapshot(instr, &generator_env_inits) {
                continue;
            }
            if matches!(instr, Instr::InteriorMutationCommit { .. }) {
                // The marker updates the existing stack triple on the call's
                // normal edge. It is a borrow-compatible in-place write, not
                // an owning sink or alias escape.
                continue;
            }
            // A `Move` discriminates a benign whole-value hand-off (dest is
            // another alias member — already folded into the alias set) from a
            // real escape. A move into `ReturnSlot` is a path-local transfer:
            // keep the source in the cleanup candidate set so an earlier call's
            // unwind edge releases it; the ownership dataflow marks it consumed
            // on the successful return path.
            if let Instr::Move { dest, src } = instr {
                if let Some(sl) = base_local(*src) {
                    let src_is_member = alias_of.contains_key(&sl)
                        && matches!(src, Place::Local(_) | Place::ReturnSlot);
                    let dest_is_member = base_local(*dest).is_some_and(|dl| {
                        alias_of.contains_key(&dl) && matches!(dest, Place::Local(_))
                    });
                    if src_is_member && !dest_is_member && !matches!(dest, Place::ReturnSlot) {
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
        // member escaped (fail-closed root resolution).
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
    for (&(block, instr_index), handoff) in &payload_handoff_sites {
        retain_sites.push(BytesRetainSite {
            block,
            instr_index,
            placement: BytesRetainPlacement::Before,
            value: handoff.source,
            required_bindings: vec![handoff.dest_binding],
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
            if payload_handoff_sites.contains_key(&(block.id, instr_index)) {
                continue;
            }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct InstrSite {
    block: u32,
    index: usize,
}

fn whole_local_write_sites(blocks: &[BasicBlock], local: u32) -> Option<Vec<InstrSite>> {
    let mut sites = Vec::new();
    for block in blocks {
        for (index, instr) in block.instructions.iter().enumerate() {
            for place in crate::dataflow::instr_reads_writes(instr).1 {
                if base_local(place) == Some(local) {
                    if place != Place::Local(local) {
                        return None;
                    }
                    sites.push(InstrSite {
                        block: block.id,
                        index,
                    });
                }
            }
        }
        if crate::dataflow::terminator_write_places(&block.terminator)
            .into_iter()
            .any(|place| base_local(place) == Some(local))
        {
            return None;
        }
    }
    Some(sites)
}

fn instr_site_dominates(
    dominators: &HashMap<u32, HashSet<u32>>,
    def: InstrSite,
    use_site: InstrSite,
) -> bool {
    if def.block == use_site.block {
        return def.index < use_site.index;
    }
    dominators
        .get(&use_site.block)
        .is_some_and(|blocks| blocks.contains(&def.block))
}

fn single_dominating_local_generation(
    blocks: &[BasicBlock],
    dominators: &HashMap<u32, HashSet<u32>>,
    local: u32,
    def: InstrSite,
) -> bool {
    if whole_local_write_sites(blocks, local).as_deref() != Some(&[def]) {
        return false;
    }
    let Some(def_instr) = blocks
        .iter()
        .find(|block| block.id == def.block)
        .and_then(|block| block.instructions.get(def.index))
    else {
        return false;
    };
    if crate::dataflow::instr_reads_writes(def_instr)
        .0
        .into_iter()
        .any(|place| base_local(place) == Some(local))
    {
        return false;
    }
    blocks.iter().all(|block| {
        block.instructions.iter().enumerate().all(|(index, instr)| {
            let site = InstrSite {
                block: block.id,
                index,
            };
            !instr_references_local(instr, local)
                || site == def
                || instr_site_dominates(dominators, def, site)
        }) && (!terminator_references_local(&block.terminator, None, local)
            || instr_site_dominates(
                dominators,
                def,
                InstrSite {
                    block: block.id,
                    index: block.instructions.len(),
                },
            ))
    })
}

fn instr_references_local(instr: &Instr, local: u32) -> bool {
    let (reads, writes, _) = crate::dataflow::instr_reads_writes(instr);
    reads
        .into_iter()
        .chain(writes)
        .any(|place| base_local(place) == Some(local))
}

fn terminator_references_local(
    terminator: &Terminator,
    suspend_kind: Option<&SuspendKind>,
    local: u32,
) -> bool {
    terminator_source_places(terminator, suspend_kind)
        .into_iter()
        .chain(crate::dataflow::terminator_write_places(terminator))
        .any(|place| base_local(place) == Some(local))
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
mod returned_member_flow;
use returned_member_flow::compute_returned_flow_locals;

/// W5.021 (defect #1) — fail-closed value-flow derivation of the owned member
/// bindings that a function HANDS to its caller through a returned aggregate,
/// and therefore must NOT also drop at its own scope exit.
///
/// A composite return — `(a, b)` / `R { f: a, g: b }`, reached directly, by
/// name, or through any control-flow tail — byte-copies each constituent into
/// the returned aggregate struct, then moves the whole aggregate to the
/// `ReturnSlot`. Without a retain, the caller receives the member binding's
/// owner and the callee must relinquish it. An explicit retain instead mints
/// the caller's owner, so the member binding keeps its original exit
/// discharge.
///
/// The previous revision excluded these members by walking the SYNTACTIC return
/// expression (`mark_returned_binding_moved`): it matched only `BindingRef` /
/// `TupleLiteral` / `StructInit` / `Block`, so any aggregate reached through a
/// `let pair = (s, r); pair` rebind or an `if` / `match` / `scope` / `loop`
/// tail fell through and the members stayed drop-eligible → double-free. That is
/// structurally fail-OPEN: every return grammar the enumeration does not list
/// re-opens the hole.
///
/// This derivation inverts to value-flow, the same alias/construct basis the
/// sibling-discharge scan uses, so a syntactic shape cannot fall behind the
/// grammar:
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
///      `flows_to_return` is a returned member and is excluded from drop, except
///      a whole owner moved directly to `ReturnSlot`. That direct move is
///      represented precisely by ownership dataflow: it consumes the owner on
///      the successful return path while keeping it live for earlier unwind
///      exits, so blanket exclusion would leak those exits.
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
/// aggregate binding ITSELF (the `pair` local) is governed by its own owner's
/// replay, which ends the generation when the aggregate is returned; this pass
/// is the complementary half that reaches the scalar member handles that
/// replay does not own.
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
    let flows_to_return = compute_returned_flow_locals(blocks);
    let direct_return_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::Move {
                dest: Place::ReturnSlot,
                src,
            } => base_local(*src),
            _ => None,
        })
        .collect();
    // Map member locals back to their owned bindings.
    let mut returned_members = HashSet::new();
    for (binding, _name, _ty) in owned_locals {
        if let Some(place) = binding_locals.get(binding) {
            if let Some(local) = base_local(*place) {
                if flows_to_return.contains(&local) && !direct_return_locals.contains(&local) {
                    returned_members.insert(*binding);
                }
            }
        }
    }
    returned_members
}

#[cfg(test)]
mod returned_member_retain_scope;

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
///   - the aggregate's in-place member drop is suppressed at the extraction
///     (`AggregateProjectionNeutralize` nulls the moved-out member) when the
///     field is extracted into a release-consumer;
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
#[cfg(test)]
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
    // by `derive_cow_sole_owner` and the owner replay (refcount / sole-
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
#[cfg(test)]
mod escaped_sibling_field_discharge;
// Split into a sibling file (not inlined here) to stay under the
// `src/lower/` line-count ratchet (`hew-mir/tests/lower_module_size.rs`).
#[cfg(test)]
mod enum_composite_field_drop_exemption;
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
            declaration: hew_types::DefId::legacy_reconstruct_from_full_path("origin"),
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
                authority: (hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(callee))
                    .map(crate::CallAuthority::Runtime)
                    .unwrap_or_default(),
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
mod shell_drop_safety_payload_cap;
