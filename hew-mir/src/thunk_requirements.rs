//! One MIR-owned registry of clone/drop thunk-synthesis requirements.
//!
//! Codegen's `emit_state_clone_drop_synthesis` emits the
//! `__hew_{record,enum}_{clone,drop}_inplace_<key>` BODIES only for seeded
//! layout keys, while many emission sites merely DECLARE the helper they
//! call. Historically each reachability shape (owned-Vec elements, tuple
//! members, `dyn` concretes, closure captures, wire-codec fail paths, …) grew
//! its own raw-MIR or elaborated-MIR seed scan inside the codegen crate — the
//! producer fact ("this key needs a synthesised thunk body") was re-derived
//! far from the MIR that authored it.
//!
//! [`IrPipeline::thunk_synthesis_requirements`] is now the single producer:
//! one deduplicated, deterministically-ordered registry of record and enum
//! layout keys, derived entirely from pipeline-owned MIR facts (raw MIR,
//! elaborated drop plans, layouts, the dyn-vtable registry, supervisor
//! configs, checker clone seeds). It mirrors the `dyn_vtable_registry`
//! precedent in `model.rs`: producers author, the registry collects, codegen
//! consumes without re-scanning.
//!
//! The pin: a missed seed leaves a thunk declared-but-undefined, and LLVM
//! verify / link rejects the module loudly ("Global is external, but doesn't
//! have external or weak linkage"). There is no silent fallback path, so any
//! divergence between this registry and codegen's declaration sites fails
//! closed at build time.
//!
//! LESSONS: `codegen-abi-authority` (P0) — the requirement fact is authored
//! once here, next to the MIR that produces it; codegen must not re-derive
//! it. `exhaustive-traversal-and-lowering` (P0) — every declaring emission
//! site must have its reachability shape represented by a collector below.

use std::collections::HashSet;

use hew_types::{BuiltinType, ResolvedTy};

use crate::model::{
    find_enum_layout, find_record_layout_for_ty, is_indirect_enum, machine_enum_views, EnumLayout,
    IrPipeline, RawMirFunction, RecordLayout, SupervisorLayout,
};
use crate::{Instr, Place, StateFieldCloneKind, Terminator};

/// The deduplicated seed sets codegen's clone/drop synthesis pass must emit
/// bodies for. Ordering is deterministic (first-seen across a fixed collector
/// order), so emitted-module layout is reproducible across builds.
#[derive(Debug, Clone, Default)]
pub struct ThunkSynthesisRequirements {
    /// Record-layout registration keys needing a
    /// `__hew_record_{clone,drop}_inplace_<key>` body.
    pub record_seeds: Vec<String>,
    /// Enum-layout registration keys (machine projections included) needing a
    /// `__hew_enum_{clone,drop}_inplace_<key>` body.
    pub enum_seeds: Vec<String>,
}

/// Append each `src` key not already present in `dst`, preserving first-seen
/// order (the merge discipline every historical seed channel used).
fn merge_seeds(dst: &mut Vec<String>, src: Vec<String>) {
    for seed in src {
        if !dst.contains(&seed) {
            dst.push(seed);
        }
    }
}

impl IrPipeline {
    /// Build the thunk-synthesis registry from the finished pipeline.
    ///
    /// Must run after drop elaboration (`elaborated_mir` drop plans are an
    /// input) and after `attach_lowering_facts` (the checker-authored
    /// `user_clone_record_seeds` join the record channel). The collector
    /// order below is load-bearing for byte-identical emission: seeds feed
    /// `emit_state_clone_drop_synthesis` in registry order, so reordering
    /// collectors reorders synthesised bodies in the module.
    #[must_use]
    #[allow(
        clippy::too_many_lines,
        reason = "the ordered collector list is the auditable synthesis manifest"
    )]
    pub fn thunk_synthesis_requirements(&self) -> ThunkSynthesisRequirements {
        // Machines are enums at the value-classification layer: the seed
        // scans resolve machine names against the enum view. The pipeline's
        // own `enum_layouts` (registration, xnode codecs) stays untouched,
        // and the collectors that must agree with an enum-slice-only
        // consumer (xnode codec keys, closure-capture classification, the
        // elaborated `EnumInPlace` walk) take `self.enum_layouts` directly.
        let synthesis_enum_layouts: Vec<EnumLayout> = self
            .enum_layouts
            .iter()
            .cloned()
            .chain(machine_enum_views(&self.machine_layouts))
            .collect();

        let mut enum_seeds =
            collect_enum_inplace_drop_seeds(&self.elaborated_mir, &self.enum_layouts);
        let (mut record_seeds, vec_owned_enum_seeds) = collect_vec_owned_element_seeds(
            &self.raw_mir,
            &self.record_layouts,
            &synthesis_enum_layouts,
        );
        merge_seeds(&mut enum_seeds, vec_owned_enum_seeds);
        merge_seeds(
            &mut record_seeds,
            collect_record_inplace_drop_seeds(&self.elaborated_mir, &self.record_layouts),
        );
        let (tuple_member_record_seeds, tuple_member_enum_seeds) =
            collect_tuple_member_inplace_drop_seeds(
                &self.elaborated_mir,
                &self.record_layouts,
                &synthesis_enum_layouts,
            );
        merge_seeds(&mut record_seeds, tuple_member_record_seeds);
        merge_seeds(&mut enum_seeds, tuple_member_enum_seeds);
        let wire_codec_value_types = collect_wire_codec_value_types(&self.raw_mir);
        let (xnode_codec_record_seeds, xnode_codec_enum_seeds) = collect_xnode_codec_drop_seeds(
            &self.actor_layouts,
            &wire_codec_value_types,
            &self.record_layouts,
            &self.enum_layouts,
        );
        merge_seeds(&mut enum_seeds, xnode_codec_enum_seeds);
        merge_seeds(&mut record_seeds, xnode_codec_record_seeds);
        let (wire_vec_elem_record_seeds, wire_vec_elem_enum_seeds) =
            collect_wire_value_owned_vec_element_seeds(
                &wire_codec_value_types,
                &self.record_layouts,
                &synthesis_enum_layouts,
            );
        merge_seeds(&mut enum_seeds, wire_vec_elem_enum_seeds);
        merge_seeds(&mut record_seeds, wire_vec_elem_record_seeds);
        let (dyn_concrete_record_seeds, dyn_concrete_enum_seeds) = collect_dyn_concrete_drop_seeds(
            &self.dyn_vtable_registry,
            &self.record_layouts,
            &synthesis_enum_layouts,
            &self.lifecycle_registry,
        );
        merge_seeds(&mut enum_seeds, dyn_concrete_enum_seeds);
        merge_seeds(&mut record_seeds, dyn_concrete_record_seeds);
        merge_seeds(&mut record_seeds, self.user_clone_record_seeds.clone());
        merge_seeds(
            &mut record_seeds,
            collect_record_clone_inplace_seeds(&self.raw_mir, &self.record_layouts),
        );
        let (field_store_record_seeds, field_store_enum_seeds) =
            collect_record_field_store_overwrite_seeds(
                &self.raw_mir,
                &self.record_layouts,
                &synthesis_enum_layouts,
                &self.lifecycle_registry,
            );
        merge_seeds(&mut record_seeds, field_store_record_seeds);
        merge_seeds(&mut enum_seeds, field_store_enum_seeds);
        merge_seeds(
            &mut enum_seeds,
            collect_enum_clone_inplace_seeds(&self.raw_mir, &self.enum_layouts),
        );
        let (inline_inplace_record_seeds, inline_inplace_enum_seeds) =
            collect_inline_inplace_drop_seeds(
                &self.raw_mir,
                &self.record_layouts,
                &synthesis_enum_layouts,
            );
        merge_seeds(&mut record_seeds, inline_inplace_record_seeds);
        merge_seeds(&mut enum_seeds, inline_inplace_enum_seeds);
        merge_seeds(
            &mut record_seeds,
            collect_supervisor_config_drop_seeds(
                &self.supervisor_layouts,
                &self.record_layouts,
                &self.enum_layouts,
                &self.lifecycle_registry,
            ),
        );
        let (closure_capture_record_seeds, closure_capture_enum_seeds) =
            collect_closure_capture_drop_seeds(
                &self.raw_mir,
                &self.record_layouts,
                &self.enum_layouts,
                &self.lifecycle_registry,
            );
        merge_seeds(&mut record_seeds, closure_capture_record_seeds);
        merge_seeds(&mut enum_seeds, closure_capture_enum_seeds);
        let (generator_env_record_seeds, generator_env_enum_seeds) =
            collect_generator_env_clone_seeds(&self.raw_mir);
        merge_seeds(&mut record_seeds, generator_env_record_seeds);
        merge_seeds(&mut enum_seeds, generator_env_enum_seeds);

        ThunkSynthesisRequirements {
            record_seeds,
            enum_seeds,
        }
    }
}

/// W5.020 — gather the `enum_layouts` registration keys of every enum that an
/// elaborated function drops via `DropKind::EnumInPlace`. These seed the
/// in-place drop-helper synthesis so a free-function-returned heap-owning enum
/// (reachable from no actor/record) still gets a `__hew_enum_drop_inplace_*`
/// body. Resolving from `ElabDrop::ty` reuses the same `register_enum_layouts`
/// scheme the caller-side drop emission uses, so the seeded helper name and the
/// emitted call name agree.
fn collect_enum_inplace_drop_seeds(
    elaborated: &[crate::ElaboratedMirFunction],
    enum_layouts: &[EnumLayout],
) -> Vec<String> {
    let mut seeds: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for func in elaborated {
        for (_exit, plan) in &func.drop_plans {
            for drop in &plan.drops {
                if drop.kind != crate::DropKind::EnumInPlace {
                    continue;
                }
                let ResolvedTy::Named { name, args, .. } = &drop.ty else {
                    continue;
                };
                let key = enum_layout_key(name, args, enum_layouts);
                if let Some(key) = key {
                    if seen.insert(key.clone()) {
                        seeds.push(key);
                    }
                }
            }
        }
    }
    seeds
}

/// Value-class capstone — collect the record-layout keys of every record that
/// an elaborated function drops via `DropKind::RecordInPlace` (RC-4 / RC-6 /
/// G12: an owned aggregate record passed/returned by value, dropped at scope
/// exit). Its `__hew_record_{clone,drop}_inplace_<Record>` body is referenced
/// only from the per-function `RecordInPlace` drop call and — unlike a record
/// reachable from an actor/supervisor state field — is NOT discovered by
/// `collect_reachable_clone_targets`. Without this seed the helper would be
/// declared-but-never-defined and the drop call would fail closed at codegen
/// ("no synthesized helper") — correct but blocks the feature. Resolves the
/// SAME key `record_inplace_drop_name` resolves at the consumer (the bare name
/// of a monomorphic user record), so seed and use can never drift.
fn collect_record_inplace_drop_seeds(
    elaborated: &[crate::ElaboratedMirFunction],
    record_layouts: &[RecordLayout],
) -> Vec<String> {
    let mut seeds: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for func in elaborated {
        for (_exit, plan) in &func.drop_plans {
            for drop in &plan.drops {
                if drop.kind != crate::DropKind::RecordInPlace {
                    continue;
                }
                // RecordInPlace carries a user record — bare-name monomorphic
                // OR a generic INSTANTIATION (`Pair<i64, string>`, args present)
                // — OR a builtin owned-aggregate record (M-5: `CrashInfo`, keyed
                // by its bare name). `record_inplace_drop_name` is the consumer
                // authority for the helper name; this seed must register the SAME
                // key so the body is synthesised before the drop call resolves
                // it. A non-record ty here is a producer invariant violation the
                // consumer already rejects, so skip it (the consumer's
                // fail-closed arm is the diagnostic surface, not this seed).
                let ResolvedTy::Named { .. } = &drop.ty else {
                    continue;
                };
                // Confirm the record actually has a registered layout before
                // seeding — a seed for an unregistered key would itself fail
                // closed in the synthesis pass with a less specific message. For
                // The resolved full-owner registry key is shared with the drop
                // consumer; no leaf retry may redirect a seed to another module.
                let key = find_record_layout_for_ty(&drop.ty, record_layouts)
                    .map(|layout| layout.name.clone());
                if let Some(key) = key {
                    if seen.insert(key.clone()) {
                        seeds.push(key);
                    }
                }
            }
        }
    }
    seeds
}

/// Collect the record / enum / machine layout keys of every record or enum that
/// appears as a member of a `DropKind::TupleInPlace` drop's tuple type.
///
/// A heap-owning tuple's `__hew_tuple_drop_inplace_<key>` body runs a per-member
/// drop that, for a record/enum member, calls the member's
/// `__hew_record_drop_inplace_<R>` / `__hew_enum_drop_inplace_<E>` thunk. That
/// member thunk's BODY is synthesised only if its key is seeded — and a record
/// reachable ONLY as a tuple member (`(Boxed, i64)` where `Boxed { payload:
/// Vec<i64> }`) is discovered by no other seed pass (it is not a state field,
/// not an owned-Vec element, not a direct `RecordInPlace` local). Without this
/// seed the member thunk is declared-but-undefined and LLVM verify rejects the
/// module. This pass closes that gap for the tuple-member shape the unified
/// record-aware heap-ownership authority newly admits to a `TupleInPlace` drop.
///
/// Mirrors `collect_vec_owned_element_seeds`'s resolution: a `Named` member
/// resolving to a registered enum (or machine, via the enum view) seeds the
/// enum list; one resolving to a registered record seeds the record list.
/// Nested tuples/arrays are walked; primitive / builtin members never bear a
/// per-member thunk and are skipped. Returns `(record_seeds, enum_seeds)`.
#[allow(
    clippy::too_many_lines,
    reason = "moved verbatim from hew-codegen-rs/src/llvm.rs"
)]
fn collect_tuple_member_inplace_drop_seeds(
    elaborated: &[crate::ElaboratedMirFunction],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> (Vec<String>, Vec<String>) {
    let mut record_seeds: Vec<String> = Vec::new();
    let mut enum_seeds: Vec<String> = Vec::new();
    let mut rec_seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut enum_seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    #[allow(
        clippy::items_after_statements,
        reason = "moved verbatim from hew-codegen-rs/src/llvm.rs"
    )]
    fn consider(
        ty: &ResolvedTy,
        record_layouts: &[RecordLayout],
        enum_layouts: &[EnumLayout],
        rec_seen: &mut std::collections::HashSet<String>,
        enum_seen: &mut std::collections::HashSet<String>,
        record_seeds: &mut Vec<String>,
        enum_seeds: &mut Vec<String>,
    ) {
        match ty {
            // Nested aggregate members are themselves dropped per-element; walk
            // them so a `((Boxed, i64), i64)` reaches the inner record.
            ResolvedTy::Tuple(elems) => {
                for e in elems {
                    consider(
                        e,
                        record_layouts,
                        enum_layouts,
                        rec_seen,
                        enum_seen,
                        record_seeds,
                        enum_seeds,
                    );
                }
            }
            ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) => {
                consider(
                    inner,
                    record_layouts,
                    enum_layouts,
                    rec_seen,
                    enum_seen,
                    record_seeds,
                    enum_seeds,
                );
            }
            ResolvedTy::Named { name, args, .. } => {
                // Enum-first (machine views are folded into the enum slice by
                // the caller), mirroring owned_elem_thunk_key resolution order.
                let enum_key = enum_layout_key(name, args, enum_layouts);
                if let Some(key) = enum_key {
                    if enum_seen.insert(key.clone()) {
                        enum_seeds.push(key);
                    }
                    return;
                }
                let rec_key =
                    find_record_layout_for_ty(ty, record_layouts).map(|layout| layout.name.clone());
                if let Some(key) = rec_key {
                    if rec_seen.insert(key.clone()) {
                        record_seeds.push(key);
                    }
                }
            }
            _ => {}
        }
    }

    for func in elaborated {
        for (_exit, plan) in &func.drop_plans {
            for drop in &plan.drops {
                if drop.kind != crate::DropKind::TupleInPlace {
                    continue;
                }
                let ResolvedTy::Tuple(elems) = &drop.ty else {
                    continue;
                };
                for elem in elems {
                    consider(
                        elem,
                        record_layouts,
                        enum_layouts,
                        &mut rec_seen,
                        &mut enum_seen,
                        &mut record_seeds,
                        &mut enum_seeds,
                    );
                }
            }
        }
    }
    (record_seeds, enum_seeds)
}

/// Collect the config struct name of every supervisor whose config struct has
/// at least one OWNED field, so its `__hew_record_drop_inplace_<ConfigTy>` body
/// is synthesised. The supervisor-owned config buffer owns those inner fields;
/// the bootstrap registers this drop fn (`hew_supervisor_set_config_drop_fn`)
/// so teardown releases them before the flat free. A config struct used only as
/// a supervisor config param is reachable from no other seed.
fn collect_supervisor_config_drop_seeds(
    supervisor_layouts: &[SupervisorLayout],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> Vec<String> {
    let mut seeds: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for sup in supervisor_layouts {
        let Some(config_param) = &sup.config_param else {
            continue;
        };
        if !seen.insert(config_param.config_ty_name.clone()) {
            continue;
        }
        let Some(record) = record_layouts
            .iter()
            .find(|r| r.name == config_param.config_ty_name)
        else {
            continue;
        };
        // Seed only when at least one field is non-BitCopy (owned). An
        // all-scalar config has nothing to drop; the bootstrap then registers
        // no config_drop_fn.
        let has_owned = record.field_tys.iter().any(|ty| {
            let mut visited = std::collections::HashSet::new();
            crate::classify_state_field_with_lifecycle_registry(
                ty,
                record_layouts,
                enum_layouts,
                &[],
                lifecycle_registry,
                &mut visited,
            )
            .is_ok_and(|kind| !matches!(kind, StateFieldCloneKind::BitCopy { .. }))
        });
        if has_owned {
            seeds.push(config_param.config_ty_name.clone());
        }
    }
    seeds
}

/// Collect the monomorphised record layout key of every `RecordCloneInplace`
/// instruction in raw MIR so its `__hew_record_clone_inplace_<key>` /
/// `__hew_record_drop_inplace_<key>` thunk PAIR is synthesised by
/// `emit_state_clone_drop_synthesis`.
///
/// `clone <record>` lowers to `RecordCloneInplace { record_name, .. }`, where
/// `record_name` is the monomorphised layout key (the bare name for a
/// monomorphic record, the mangled `Pair$$i64$i64` for a generic instantiation —
/// see `user_record_layout_key` in hew-mir). A generic instantiation reaches
/// the synthesis pass through NO other seed:
///
///  * `user_clone_record_seeds` (the checker-side clone seed) carries the bare
///    declared name (`Pair`), which never matches the mangled layout key; and
///  * the drop twin (`collect_record_inplace_drop_seeds`) only fires for a
///    record with a non-trivial drop, so a generic record whose fields are all
///    Copy (`Pair<i64, i64>`) is cloned but never drop-seeded.
///
/// Without this seed the generic clone thunk is declared-but-undefined and LLVM
/// verify rejects the module. Seeding the clone site directly closes the gap;
/// because the synthesis loop emits the clone AND drop body together per key,
/// the thunk pair stays symmetric — there is no clone without its matching drop
/// (the leak / double-free hazard on unwind).
///
/// A seed is registered only when `record_name` names a registered layout, so
/// the key the synthesis pass emits a body under is exactly the key the clone
/// call resolves; a `record_name` with no registered layout is left unseeded
/// and fails closed loudly at LLVM verify rather than silently aliasing.
fn collect_record_clone_inplace_seeds(
    raw_mir: &[RawMirFunction],
    record_layouts: &[RecordLayout],
) -> Vec<String> {
    let mut seeds: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for func in raw_mir {
        for block in &func.blocks {
            for instr in &block.instructions {
                let record_name = match instr {
                    Instr::RecordCloneInplace { record_name, .. } => Some(record_name),
                    Instr::ValueSnapshotClone { plan, .. } => match plan.root() {
                        crate::StateFieldCloneKind::UserRecord { name } => Some(name),
                        _ => None,
                    },
                    _ => None,
                };
                let Some(record_name) = record_name else {
                    continue;
                };
                if !record_layouts.iter().any(|rl| rl.name == *record_name) {
                    continue;
                }
                if seen.insert(record_name.clone()) {
                    seeds.push(record_name.clone());
                }
            }
        }
    }
    seeds
}

/// Collect each registered inline-record or enum type overwritten through
/// [`Instr::RecordFieldStore`].
///
/// Codegen selects cleanup from the destination field type and releases an
/// inline aggregate through its record/enum overwrite helper before storing
/// the replacement. Generic layouts are excluded from the synthesis pass's
/// implicit direct-string seed scan, so the declaring store must seed the
/// registered monomorphised layout or LLVM verification sees an unbodied
/// internal declaration. Unknown layouts remain unseeded: codegen also
/// declines to declare a helper without the same registered-layout witness.
fn record_layout_for_ty<'a>(
    ty: &ResolvedTy,
    record_layouts: &'a [RecordLayout],
) -> Option<&'a RecordLayout> {
    let ResolvedTy::Named {
        is_opaque: false, ..
    } = ty
    else {
        return None;
    };
    find_record_layout_for_ty(ty, record_layouts)
}

fn enum_layout_key(name: &str, args: &[ResolvedTy], enum_layouts: &[EnumLayout]) -> Option<String> {
    find_enum_layout(name, args, enum_layouts).map(|layout| layout.name.clone())
}

fn collect_record_field_store_overwrite_seeds(
    raw_mir: &[RawMirFunction],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> (Vec<String>, Vec<String>) {
    let mut record_seeds = Vec::new();
    let mut record_seen = HashSet::new();
    let mut enum_seeds = Vec::new();
    let mut enum_seen = HashSet::new();
    for func in raw_mir {
        for block in &func.blocks {
            for instr in &block.instructions {
                let Instr::RecordFieldStore {
                    record: Place::Local(record),
                    field_offset,
                    ..
                } = instr
                else {
                    continue;
                };
                let Some(parent_ty) = func.locals.get(*record as usize) else {
                    continue;
                };
                let Some(parent_layout) = record_layout_for_ty(parent_ty, record_layouts) else {
                    continue;
                };
                let Some(field_ty) = parent_layout.field_tys.get(field_offset.0 as usize) else {
                    continue;
                };
                let ResolvedTy::Named {
                    name,
                    args,
                    is_opaque: false,
                    ..
                } = field_ty
                else {
                    continue;
                };
                if let Some(layout) = record_layout_for_ty(field_ty, record_layouts) {
                    let key = layout.name.clone();
                    if lifecycle_registry
                        .resource_record(&hew_types::DefId::new(&key))
                        .is_none()
                        && record_seen.insert(key.clone())
                    {
                        record_seeds.push(key);
                    }
                } else if let Some(key) = enum_layout_key(name, args, enum_layouts) {
                    if enum_seen.insert(key.clone()) {
                        enum_seeds.push(key);
                    }
                }
            }
        }
    }
    (record_seeds, enum_seeds)
}

/// Collect the monomorphised tagged-union layout key of every `EnumCloneInplace`
/// instruction in raw MIR so its `__hew_enum_clone_inplace_<key>` /
/// `__hew_enum_drop_inplace_<key>` thunk PAIR is synthesised by
/// `emit_state_clone_drop_synthesis`. The enum twin of
/// `collect_record_clone_inplace_seeds`.
///
/// `clone <enum>` lowers to `EnumCloneInplace { enum_name, .. }`, where
/// `enum_name` is the monomorphised layout key (the bare name for a monomorphic
/// enum, the mangled `Maybe$$i64` for a generic instantiation — see
/// `enum_clone_layout_key` in hew-mir). A generic enum whose every variant
/// payload is `BitCopy` (`Maybe<i64>`) is cloned but never earns an
/// `EnumInPlace` drop, so the drop-side seed (`collect_enum_inplace_drop_seeds`)
/// does NOT cover it; seeding the clone site directly closes that gap. Because
/// the synthesis loop emits the clone AND drop body together per key, the thunk
/// pair stays symmetric — there is no clone without its matching drop (the leak
/// / double-free hazard on unwind).
///
/// A seed is registered only when `enum_name` names a registered layout, so the
/// key the synthesis pass emits a body under is exactly the key the clone call
/// resolves; a `enum_name` with no registered layout is left unseeded and fails
/// closed loudly at LLVM verify rather than silently aliasing.
fn collect_enum_clone_inplace_seeds(
    raw_mir: &[RawMirFunction],
    enum_layouts: &[EnumLayout],
) -> Vec<String> {
    let mut seeds: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for func in raw_mir {
        for block in &func.blocks {
            for instr in &block.instructions {
                let enum_name = match instr {
                    Instr::EnumCloneInplace { enum_name, .. } => Some(enum_name),
                    Instr::ValueSnapshotClone { plan, .. } => match plan.root() {
                        crate::StateFieldCloneKind::Enum { name } => Some(name),
                        _ => None,
                    },
                    _ => None,
                };
                let Some(enum_name) = enum_name else {
                    continue;
                };
                if !enum_layouts.iter().any(|el| el.name == *enum_name) {
                    continue;
                }
                if seen.insert(enum_name.clone()) {
                    seeds.push(enum_name.clone());
                }
            }
        }
    }
    seeds
}

/// #2419 — collect the record/enum layout keys of every value CAPTURED by an
/// escaping (heap-boxed) closure so its `__hew_{record,enum}_drop_inplace_<key>`
/// body is synthesised before the env free thunk that CALLS it is emitted.
///
/// WHY: `MakeClosure { env_mode: HeapBox }` plants a per-closure env free thunk
/// (`get_or_emit_closure_env_free_thunk`) that drops each owned captured field
/// through `emit_field_drop_step`; a record/enum capture dispatches to
/// `get_or_declare_{record,enum}_drop_inplace`, which only DECLARES the helper
/// (internal linkage). The body is emitted by `emit_state_clone_drop_synthesis`
/// for seeded keys only — and a closure capture reaches no other seed channel:
/// the capture MOVES into the env (suppressing the caller binding's
/// `{Record,Enum}InPlace` drop-plan seed), and an all-BitCopy record never earns
/// a drop plan at all yet still classifies as `UserRecord` at the thunk site.
/// Without this seed the helper is declared-but-undefined and LLVM verify
/// rejects the module ("Global is external, but doesn't have external or weak
/// linkage").
///
/// Classifies each captured field via `classify_state_field_with_enum_layouts`
/// over the SAME layout slices the thunk site (`closure_env_capture_drop_kinds`)
/// consults, so the seeded key and the requested key cannot drift (the
/// dyn-concrete seed pass discipline). Kinds are walked recursively through
/// `Tuple`/`Array` so a record captured inside a tuple is also seeded; nested
/// record/enum FIELDS of a seeded record are drained by
/// `collect_reachable_clone_targets` itself. Only `HeapBox` closures are
/// considered: a stack-env capture stays owned by the caller binding and never
/// reaches the free-thunk drop, and seeding it could newly reject programs
/// whose captures the classifier cannot map. Classification failures stay
/// silent here — `closure_env_capture_drop_kinds` is the fail-closed authority
/// for unclassifiable captures. Returns `(record_seeds, enum_seeds)`.
fn collect_closure_capture_drop_seeds(
    raw_mir: &[RawMirFunction],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> (Vec<String>, Vec<String>) {
    let mut record_seeds: Vec<String> = Vec::new();
    let mut enum_seeds: Vec<String> = Vec::new();
    let mut rec_seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut enum_seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    #[allow(
        clippy::items_after_statements,
        reason = "moved verbatim from hew-codegen-rs/src/llvm.rs"
    )]
    fn add_kind_seeds(
        kind: &StateFieldCloneKind,
        rec_seen: &mut std::collections::HashSet<String>,
        enum_seen: &mut std::collections::HashSet<String>,
        record_seeds: &mut Vec<String>,
        enum_seeds: &mut Vec<String>,
    ) {
        match kind {
            StateFieldCloneKind::UserRecord { name } => {
                if rec_seen.insert(name.clone()) {
                    record_seeds.push(name.clone());
                }
            }
            StateFieldCloneKind::Enum { name } => {
                if enum_seen.insert(name.clone()) {
                    enum_seeds.push(name.clone());
                }
            }
            StateFieldCloneKind::Tuple { elems } => {
                for elem in elems {
                    add_kind_seeds(elem, rec_seen, enum_seen, record_seeds, enum_seeds);
                }
            }
            StateFieldCloneKind::Array { elem, .. } => {
                add_kind_seeds(elem, rec_seen, enum_seen, record_seeds, enum_seeds);
            }
            // String/Bytes/Vec/HashMap/HashSet/IoHandle/Opaque/ClosurePair/
            // Resource/BitCopy drop through runtime helpers or planted thunks,
            // not a per-type synthesised drop body. Vec/HashMap owned-element
            // record keys are seeded by `collect_vec_owned_element_seeds`
            // (the captured collection was a local, so its type is walked).
            _ => {}
        }
    }

    for func in raw_mir {
        for block in &func.blocks {
            for instr in &block.instructions {
                let Instr::MakeClosure {
                    env,
                    env_mode: crate::ClosureEnvMode::HeapBox,
                    ..
                } = instr
                else {
                    continue;
                };
                let Place::Local(local) = env else {
                    continue;
                };
                let Some(env_ty) = func.locals.get(*local as usize) else {
                    continue;
                };
                let ResolvedTy::Named { name: env_name, .. } = env_ty else {
                    continue;
                };
                let Some(env_layout) = record_layouts.iter().find(|rl| rl.name == *env_name) else {
                    continue;
                };
                for field_ty in &env_layout.field_tys {
                    let mut visited: std::collections::HashSet<String> =
                        std::collections::HashSet::new();
                    let Ok(kind) = crate::classify_state_field_with_lifecycle_registry(
                        field_ty,
                        record_layouts,
                        enum_layouts,
                        &[],
                        lifecycle_registry,
                        &mut visited,
                    ) else {
                        continue;
                    };
                    add_kind_seeds(
                        &kind,
                        &mut rec_seen,
                        &mut enum_seen,
                        &mut record_seeds,
                        &mut enum_seeds,
                    );
                }
            }
        }
    }
    (record_seeds, enum_seeds)
}

fn collect_generator_env_clone_seeds(raw_mir: &[RawMirFunction]) -> (Vec<String>, Vec<String>) {
    let mut record_seeds = Vec::new();
    let mut enum_seeds = Vec::new();
    let mut record_seen = HashSet::new();
    let mut enum_seen = HashSet::new();

    #[allow(
        clippy::items_after_statements,
        reason = "moved verbatim from hew-codegen-rs/src/llvm.rs"
    )]
    fn add_kind(
        kind: &StateFieldCloneKind,
        record_seeds: &mut Vec<String>,
        enum_seeds: &mut Vec<String>,
        record_seen: &mut HashSet<String>,
        enum_seen: &mut HashSet<String>,
    ) {
        match kind {
            StateFieldCloneKind::UserRecord { name } => {
                if record_seen.insert(name.clone()) {
                    record_seeds.push(name.clone());
                }
            }
            StateFieldCloneKind::Enum { name } => {
                if enum_seen.insert(name.clone()) {
                    enum_seeds.push(name.clone());
                }
            }
            StateFieldCloneKind::Tuple { elems } => {
                for elem in elems {
                    add_kind(elem, record_seeds, enum_seeds, record_seen, enum_seen);
                }
            }
            StateFieldCloneKind::Array { elem, .. }
            | StateFieldCloneKind::Vec { elem }
            | StateFieldCloneKind::HashSet { elem } => {
                add_kind(elem, record_seeds, enum_seeds, record_seen, enum_seen);
            }
            StateFieldCloneKind::HashMap { key, val } => {
                add_kind(key, record_seeds, enum_seeds, record_seen, enum_seen);
                add_kind(val, record_seeds, enum_seeds, record_seen, enum_seen);
            }
            _ => {}
        }
    }

    for function in raw_mir {
        for block in &function.blocks {
            let Terminator::MakeGenerator { env: Some(env), .. } = &block.terminator else {
                continue;
            };
            for field in &env.fields {
                if let crate::GeneratorEnvFieldPlan::Owned(plan)
                | crate::GeneratorEnvFieldPlan::OwnedMove(plan) = field
                {
                    add_kind(
                        plan.root(),
                        &mut record_seeds,
                        &mut enum_seeds,
                        &mut record_seen,
                        &mut enum_seen,
                    );
                }
            }
        }
    }
    (record_seeds, enum_seeds)
}

/// Collect the record/enum layout keys of every inline composite yield-value
/// release (`Instr::Drop { drop_fn: Some(DropFnSpec::InPlace(_)) }`) in raw
/// MIR so the `__hew_{record,enum}_drop_inplace_<key>` body is synthesised
/// before the drop call resolves it. Returns `(record_seeds, enum_seeds)`.
///
/// Belt-and-suspenders: every composite that flows through the pump /
/// for-await seam is ALSO referenced by its stream layout witness
/// (`owned_elem_layout_descriptor_ptr`) or generator out-slot drop thunk,
/// whose emission seeds the same bodies — but that coupling is structural,
/// not enforced. Seeding the drop sites directly keeps the inline release
/// resolvable even for a shape whose witness emission is skipped or
/// reordered, at the cost of one raw-MIR walk. Resolution mirrors
/// `collect_record_inplace_drop_seeds` / `collect_enum_inplace_drop_seeds`
/// through the same exact full-owner layout authority as the consumer, so seed
/// and use resolve one key.
fn collect_inline_inplace_drop_seeds(
    raw_mir: &[RawMirFunction],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> (Vec<String>, Vec<String>) {
    let mut record_seeds: Vec<String> = Vec::new();
    let mut enum_seeds: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<(bool, String)> = std::collections::HashSet::new();
    for func in raw_mir {
        for block in &func.blocks {
            for instr in &block.instructions {
                let Instr::Drop {
                    ty,
                    drop_fn: Some(crate::DropFnSpec::InPlace(kind)),
                    ..
                } = instr
                else {
                    continue;
                };
                let ResolvedTy::Named { name, args, .. } = ty else {
                    continue;
                };
                let (is_enum, key) = match kind {
                    crate::InPlaceReleaseKind::Record => (
                        false,
                        find_record_layout_for_ty(ty, record_layouts)
                            .map(|layout| layout.name.clone()),
                    ),
                    crate::InPlaceReleaseKind::Enum => {
                        (true, enum_layout_key(name, args, enum_layouts))
                    }
                    crate::InPlaceReleaseKind::AggregateRecursive => (false, None),
                };
                if let Some(key) = key {
                    if seen.insert((is_enum, key.clone())) {
                        if is_enum {
                            enum_seeds.push(key);
                        } else {
                            record_seeds.push(key);
                        }
                    }
                }
            }
        }
    }
    (record_seeds, enum_seeds)
}

/// D2 / W3.031 — collect the record/enum layout keys of every type that
/// appears as a `dyn Trait` CONCRETE so its
/// `__hew_{record,enum}_drop_inplace_<key>` body is synthesised before
/// `emit_dyn_trait_drop_in_place_fns` emits the vtable slot-0
/// `drop_in_place` thunk that CALLS it.
///
/// The slot-0 thunk runs the concrete value's structural drop by
/// dispatching to the per-type record/enum drop-in-place helper (the
/// runtime-erased counterpart to `DropKind::{Record,Enum}InPlace`). That
/// helper's BODY is emitted only by `emit_state_clone_drop_synthesis` for
/// reachable-or-seeded types. A concrete used ONLY behind `dyn` (e.g. a
/// `Widget` coerced to `dyn Show` and never stored in an actor/record
/// field) is not otherwise reached, so without this seed the slot-0 thunk
/// would call a declared-but-undefined helper and LLVM verify would reject
/// the module ("Global is external, but doesn't have external or weak
/// linkage").
///
/// Classifies enum-aware (so an enum concrete seeds the enum channel
/// instead of failing closed) and returns the SAME registry key
/// `classify_user_record`/`classify_enum` resolve — the exact key
/// `get_or_declare_{record,enum}_drop_inplace` mangle into the helper
/// symbol — so seed and use can never drift. Returns
/// `(record_seeds, enum_seeds)`. `BitCopy` concretes need no seed (slot-0
/// emits an empty body); any other classification is a bare owned-heap
/// concrete diagnosed at the slot-0 emission site (the fail-closed
/// authority), so this pass stays silent there to avoid masking it.
fn collect_dyn_concrete_drop_seeds(
    registry: &[crate::DynVtableInstance],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> (Vec<String>, Vec<String>) {
    let mut record_seeds: Vec<String> = Vec::new();
    let mut enum_seeds: Vec<String> = Vec::new();
    let mut seen_rec: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut seen_enum: std::collections::HashSet<String> = std::collections::HashSet::new();
    for inst in registry {
        let mut visited: std::collections::HashSet<String> = std::collections::HashSet::new();
        let Ok(kind) = crate::classify_state_field_with_lifecycle_registry(
            &inst.concrete_type,
            record_layouts,
            enum_layouts,
            &[],
            lifecycle_registry,
            &mut visited,
        ) else {
            continue;
        };
        match kind {
            StateFieldCloneKind::UserRecord { name } if seen_rec.insert(name.clone()) => {
                record_seeds.push(name);
            }
            StateFieldCloneKind::Enum { name } if seen_enum.insert(name.clone()) => {
                enum_seeds.push(name);
            }
            _ => {}
        }
    }
    (record_seeds, enum_seeds)
}

/// Collect the record- and enum-layout keys of every actor-handler message and
/// reply type that needs a `__hew_record_drop_inplace_*` or
/// `__hew_enum_drop_inplace_*` body emitted by the synthesis pass for the
/// cross-node codec path.
///
/// WHY: `emit_cbor_codec_thunks` (the serialize/deserialize emitter) calls
/// `get_or_declare_record_drop_inplace` (for record-typed
/// Msg/Reply) and `get_or_declare_enum_drop_inplace` (for enum-typed ones) in
/// the fail-path drop walk (`emit_de_drop_owned`).  Those calls only DECLARE the
/// helper; the BODY is emitted by `emit_state_clone_drop_synthesis`.  A record
/// or enum used only as a handler message/reply type and not reachable from any
/// actor state field will not appear in the existing seed collectors
/// (`collect_enum_inplace_drop_seeds` / `collect_record_inplace_drop_seeds`), so
/// without this seed its body is never emitted and LLVM verify rejects the
/// module with "Global is external, but doesn't have external or weak linkage".
/// WHEN-OBSOLETE: if `emit_de_drop_owned` is refactored to emit the body
/// in-place rather than relying on a separately-seeded synthesis pass.
/// WHAT: seed by walking actor handler `param_tys`/`return_ty` AND the value
/// types of every direct `.encode()` / `.decode()` call (`wire_codec_types`) —
/// the deserialize thunk's fail-path drop walk references the same inplace-drop
/// helper regardless of whether the codec was reached via an actor message or a
/// direct call.  The key resolution mirrors `xnode_registry_key` so the declared
/// name and the body name agree.  Returns `(record_seeds, enum_seeds)`.
fn collect_xnode_codec_drop_seeds(
    actor_layouts: &[crate::ActorLayout],
    wire_codec_types: &[ResolvedTy],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> (Vec<String>, Vec<String>) {
    let mut rec_seeds: Vec<String> = Vec::new();
    let mut enum_seeds: Vec<String> = Vec::new();
    let mut rec_seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut enum_seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    let try_add = |ty: &ResolvedTy,
                   rec_seeds: &mut Vec<String>,
                   enum_seeds: &mut Vec<String>,
                   rec_seen: &mut std::collections::HashSet<String>,
                   enum_seen: &mut std::collections::HashSet<String>| {
        let ResolvedTy::Named { name, args, .. } = ty else {
            return;
        };
        // The decoder resolves a single exact full-owner layout key. Preserve
        // its record-before-enum precedence, but never retry a same-leaf key.
        if let Some(rl) = find_record_layout_for_ty(ty, record_layouts) {
            if rec_seen.insert(rl.name.clone()) {
                rec_seeds.push(rl.name.clone());
            }
            return;
        }
        if let Some(el) = find_enum_layout(name, args, enum_layouts) {
            if enum_seen.insert(el.name.clone()) {
                enum_seeds.push(el.name.clone());
            }
        }
    };

    for actor in actor_layouts {
        for h in &actor.handlers {
            // Mirrors `emit_actor_codec_module_init`: only single-param
            // handlers get codec thunks, so only their msg types need
            // drop-seed bodies. Multi-arg handlers are not seeded (their
            // packed-args wire has no cross-node codec yet).
            if let [msg_ty] = h.param_tys.as_slice() {
                if !matches!(msg_ty, ResolvedTy::Named { name, .. }
                    if is_indirect_enum(name, enum_layouts))
                {
                    try_add(
                        msg_ty,
                        &mut rec_seeds,
                        &mut enum_seeds,
                        &mut rec_seen,
                        &mut enum_seen,
                    );
                }
            }
            try_add(
                &h.return_ty,
                &mut rec_seeds,
                &mut enum_seeds,
                &mut rec_seen,
                &mut enum_seen,
            );
        }
    }
    // Direct `.encode()` / `.decode()` value types need the same drop-seed
    // bodies: the deserialize thunk's fail-path walk references the inplace
    // drop helper for the reconstructed value.
    for ty in wire_codec_types {
        try_add(
            ty,
            &mut rec_seeds,
            &mut enum_seeds,
            &mut rec_seen,
            &mut enum_seen,
        );
    }
    (rec_seeds, enum_seeds)
}

/// Collect the distinct wire `value_ty`s referenced by every direct
/// `.encode()` / `.decode()` (`Instr::WireCodec`) call across the MIR. Feeds
/// `collect_xnode_codec_drop_seeds` and `emit_wire_codec_call_thunks` so a wire
/// type reached only through a direct codec call still gets its thunk bodies and
/// drop-seed helpers emitted.
fn collect_wire_codec_value_types(raw_mir: &[RawMirFunction]) -> Vec<ResolvedTy> {
    let mut out: Vec<ResolvedTy> = Vec::new();
    for func in raw_mir {
        for block in &func.blocks {
            for instr in &block.instructions {
                if let Instr::WireCodec { value_ty, .. } = instr {
                    if !out.contains(value_ty) {
                        out.push(value_ty.clone());
                    }
                }
            }
        }
    }
    out
}

/// Walk every raw-MIR function's local/param/return types and collect the
/// record / enum layout keys of every owned-element `Vec<T>` (W5.016) and every
/// heap-owning `HashMap<K, V>` value descriptor (G1 clone-on-get).
///
/// An owned-Vec element or owned `HashMap` value type's
/// `__hew_record/enum_{clone,drop}_inplace_<key>` helper body is NOT reachable
/// from any actor/record state field nor from a `DropKind::EnumInPlace` seed —
/// it is referenced only by the descriptor
/// (`owned_elem_layout_descriptor_ptr` / `hashmap_owned_value_layout_descriptor_ptr`).
/// Without seeding the body, the descriptor's thunk pointers would dangle at
/// link time (fail-closed at link, but blocks the feature). Returns
/// `(record_seeds, enum_seeds)` keyed the same way `collect_reachable_clone_targets`
/// / `collect_enum_inplace_drop_seeds` expect.
///
/// Element-shape resolution mirrors `owned_elem_thunk_key`: a `Named` element
/// resolving to a registered enum seeds the enum list; one resolving to a
/// registered record seeds the record list. Tuple / primitive / builtin
/// elements never produce a thunk-bearing descriptor, so they are skipped here.
#[allow(
    clippy::too_many_lines,
    reason = "moved verbatim from hew-codegen-rs/src/llvm.rs"
)]
fn collect_vec_owned_element_seeds(
    raw_mir: &[RawMirFunction],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> (Vec<String>, Vec<String>) {
    let mut record_seeds: Vec<String> = Vec::new();
    let mut enum_seeds: Vec<String> = Vec::new();
    let mut rec_seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut enum_seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut visited_ty: std::collections::HashSet<String> = std::collections::HashSet::new();

    let mut consider_elem =
        |elem: &ResolvedTy, record_seeds: &mut Vec<String>, enum_seeds: &mut Vec<String>| {
            let ResolvedTy::Named { name, args, .. } = elem else {
                return;
            };
            // Enum-first (mirrors owned_elem_thunk_key resolution order).
            let enum_key = enum_layout_key(name, args, enum_layouts);
            if let Some(key) = enum_key {
                if enum_seen.insert(key.clone()) {
                    enum_seeds.push(key);
                }
                return;
            }
            let rec_key =
                find_record_layout_for_ty(elem, record_layouts).map(|layout| layout.name.clone());
            if let Some(key) = rec_key {
                if rec_seen.insert(key.clone()) {
                    record_seeds.push(key);
                }
            }
        };

    // Recursively scan a type for `Vec<elem>` and `HashMap<K, V>` occurrences
    // (so a `Vec<Vec<owned>>`, an owned-Vec nested in a tuple/option arg, or a
    // map whose heap-owning V is not otherwise dropped is seeded).
    #[allow(
        clippy::items_after_statements,
        reason = "moved verbatim from hew-codegen-rs/src/llvm.rs"
    )]
    fn scan_ty(
        ty: &ResolvedTy,
        visited: &mut std::collections::HashSet<String>,
        on_vec_elem: &mut dyn FnMut(&ResolvedTy),
    ) {
        match ty {
            ResolvedTy::Named { name, args, .. } => {
                // Queue carriers (`Sender<T>`/`Receiver<T>`/`Stream<T>`)
                // share the owned-element layout witness with `Vec<T>`
                // (`channel_elem_layout_witness_ptr` →
                // `owned_elem_layout_descriptor_ptr` references the same
                // `__hew_{record,enum}_{clone,drop}_inplace_*` thunks), so
                // their element types must be seeded identically or the
                // witness's thunk pointers dangle at llvm-verify for any
                // heap-payload enum element reachable only through a
                // channel (string-bearing records were masked by the
                // direct-string record seed). The checker stamps their
                // `BuiltinType`, so user declarations with the same leaf do
                // not enter this builtin-only branch.
                if ty.is_builtin(BuiltinType::Vec)
                    || ty.is_builtin(BuiltinType::Sender)
                    || ty.is_builtin(BuiltinType::Receiver)
                    || ty.is_builtin(BuiltinType::Stream)
                {
                    if let Some(elem) = args.first() {
                        on_vec_elem(elem);
                    }
                } else if ty.is_builtin(BuiltinType::HashMap) {
                    // Seed BOTH the value (heap-owning V drop thunk) AND the key
                    // (managed-record-key drop thunk). A `string`-bearing record
                    // key is owned by the map and dropped via its per-record
                    // `__hew_record_drop_inplace_<R>` body; without seeding the
                    // key, that body would be declared-but-undefined and LLVM
                    // verify would reject the module.
                    if let Some(key) = args.first() {
                        on_vec_elem(key);
                    }
                    if let Some(value) = args.get(1) {
                        on_vec_elem(value);
                    }
                }
                // Guard against unbounded recursion through self-referential
                // generic args (e.g. a recursive enum reachable via args).
                let key = format!("{name}::{}", args.len());
                if !visited.insert(key.clone()) {
                    return;
                }
                for a in args {
                    scan_ty(a, visited, on_vec_elem);
                }
                visited.remove(&key);
            }
            ResolvedTy::Tuple(elems) => {
                for e in elems {
                    scan_ty(e, visited, on_vec_elem);
                }
            }
            ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) => {
                scan_ty(inner, visited, on_vec_elem);
            }
            _ => {}
        }
    }

    for func in raw_mir {
        let mut on_vec_elem = |elem: &ResolvedTy| {
            consider_elem(elem, &mut record_seeds, &mut enum_seeds);
        };
        for ty in func
            .locals
            .iter()
            .chain(func.params.iter())
            .chain(std::iter::once(&func.return_ty))
        {
            scan_ty(ty, &mut visited_ty, &mut on_vec_elem);
        }
    }

    (record_seeds, enum_seeds)
}

/// Seed the owned-Vec ELEMENT record/enum keys reachable through a wire type's
/// LAYOUT (its record fields / enum payloads) — the decode-only reachability
/// gap. A wire type materialised ONLY by the CBOR codec (`Batch.decode(bytes)`
/// with no Hew-side construction) has its `Vec<Item>` field type in no MIR local,
/// so [`collect_vec_owned_element_seeds`] (which scans MIR function types) never
/// sees the element. The decode-side owned descriptor
/// (`owned_elem_layout_descriptor_ptr`) references the element's
/// `__hew_{record,enum}_{clone,drop}_inplace_<key>` thunk, so without this seed
/// that thunk is declared-but-undefined and LLVM verify rejects the module (a
/// loud link-time failure, but this pass converts it to a supported feature).
///
/// Walks each wire value type's fields/payloads transitively (expanding records
/// and enums via their layouts, cycle-guarded on the layout name) and seeds every
/// `Vec<E>` element `E` that resolves to a registered record/enum — the same
/// resolution [`collect_vec_owned_element_seeds`] applies to MIR-local Vec types.
#[allow(
    clippy::too_many_lines,
    reason = "moved verbatim from hew-codegen-rs/src/llvm.rs"
)]
fn collect_wire_value_owned_vec_element_seeds(
    wire_types: &[ResolvedTy],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> (Vec<String>, Vec<String>) {
    let mut record_seeds: Vec<String> = Vec::new();
    let mut enum_seeds: Vec<String> = Vec::new();
    let mut rec_seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut enum_seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Resolve a `Vec` element to its registered record/enum key and seed it —
    // enum-first, mirroring `owned_elem_thunk_key` / `collect_vec_owned_element_seeds`.
    let mut consider_elem =
        |elem: &ResolvedTy, record_seeds: &mut Vec<String>, enum_seeds: &mut Vec<String>| {
            let ResolvedTy::Named { name, args, .. } = elem else {
                return;
            };
            let enum_key = enum_layout_key(name, args, enum_layouts);
            if let Some(key) = enum_key {
                if enum_seen.insert(key.clone()) {
                    enum_seeds.push(key);
                }
                return;
            }
            let rec_key =
                find_record_layout_for_ty(elem, record_layouts).map(|layout| layout.name.clone());
            if let Some(key) = rec_key {
                if rec_seen.insert(key.clone()) {
                    record_seeds.push(key);
                }
            }
        };

    // Worklist over the wire types' layouts: expand user records/enums into their
    // field/payload types, seed every `Vec<E>` element, and keep walking through
    // nested aggregates so a `Vec<Vec<owned>>` or a record-of-record-of-Vec is
    // reached. Cycle-guarded on the expanded layout name.
    let mut visited: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut stack: Vec<ResolvedTy> = wire_types.to_vec();
    while let Some(ty) = stack.pop() {
        match &ty {
            ResolvedTy::Named { name, args, .. } => {
                if ty.is_builtin(BuiltinType::Vec)
                    || ty.is_builtin(BuiltinType::Sender)
                    || ty.is_builtin(BuiltinType::Receiver)
                    || ty.is_builtin(BuiltinType::Stream)
                {
                    if let Some(elem) = args.first() {
                        consider_elem(elem, &mut record_seeds, &mut enum_seeds);
                        stack.push(elem.clone());
                    }
                    continue;
                }
                if ty.is_builtin(BuiltinType::HashMap) || ty.is_builtin(BuiltinType::HashSet) {
                    for a in args {
                        stack.push(a.clone());
                    }
                    continue;
                }
                if ty.is_builtin(BuiltinType::Option)
                    || ty.is_builtin(BuiltinType::Result)
                    || ty.is_builtin(BuiltinType::Range)
                {
                    for a in args {
                        stack.push(a.clone());
                    }
                    continue;
                }
                // A user record / enum: expand its fields / variant payloads once.
                if !visited.insert(name.clone()) {
                    continue;
                }
                if let Some(el) = find_enum_layout(name, args, enum_layouts) {
                    for v in &el.variants {
                        for ft in &v.field_tys {
                            stack.push(ft.clone());
                        }
                    }
                } else if let Some(rl) = find_record_layout_for_ty(&ty, record_layouts) {
                    for ft in &rl.field_tys {
                        stack.push(ft.clone());
                    }
                }
            }
            ResolvedTy::Tuple(elems) => {
                for e in elems {
                    stack.push(e.clone());
                }
            }
            ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) => {
                stack.push((**inner).clone());
            }
            _ => {}
        }
    }

    (record_seeds, enum_seeds)
}

#[cfg(test)]
mod record_field_store_overwrite_tests {
    use super::*;
    use crate::model::{
        BasicBlock, FieldOffset, FunctionCallConv, MachineVariantLayout, SourceOrigin,
    };

    fn store_function(name: &str, parent_ty: ResolvedTy, src_ty: ResolvedTy) -> RawMirFunction {
        RawMirFunction {
            name: name.to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![parent_ty, src_ty],
            local_names: vec![],
            local_scopes: vec![],
            local_decl_bytes: vec![],
            scope_table: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![Instr::RecordFieldStore {
                    record: Place::Local(0),
                    field_offset: FieldOffset(0),
                    src: Place::Local(1),
                }],
                terminator: Terminator::Return,
            }],
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),
            lambda_actor_user_param_locals: vec![],
            span: None,
            instr_spans: std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        }
    }

    #[test]
    fn synthetic_cursor_field_store_seeds_classed_monomorphised_helper() {
        let key = hew_hir::synthetic_cursor_layout_key(BuiltinType::VecIter, &[ResolvedTy::String])
            .expect("VecIter is a synthetic cursor");
        let user_key = hew_hir::mangle_layout_key("VecIter", &[ResolvedTy::String]);
        let vec_iter_string =
            ResolvedTy::named_builtin("VecIter", BuiltinType::VecIter, vec![ResolvedTy::String]);
        let layouts = vec![
            RecordLayout {
                name: "Holder".to_string(),
                field_tys: vec![vec_iter_string.clone()],
                field_names: vec!["iter".to_string()],
            },
            RecordLayout {
                name: user_key,
                field_tys: vec![ResolvedTy::I64],
                field_names: vec!["user_field".to_string()],
            },
            RecordLayout {
                name: key.clone(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec!["current".to_string()],
            },
        ];
        let funcs = vec![store_function(
            "store_vec_iter",
            ResolvedTy::named_user("Holder", vec![]),
            vec_iter_string,
        )];

        let (record_seeds, enum_seeds) = collect_record_field_store_overwrite_seeds(
            &funcs,
            &layouts,
            &[],
            &hew_hir::LifecycleRegistry::default(),
        );
        assert_eq!(record_seeds, [key]);
        assert!(enum_seeds.is_empty());
    }

    #[test]
    fn hashmap_cursor_field_store_rejects_same_leaf_user_layout() {
        let args = [ResolvedTy::String, ResolvedTy::I64];
        let key = hew_hir::synthetic_cursor_layout_key(BuiltinType::HashMapIter, &args)
            .expect("HashMapIter is a synthetic cursor");
        let user_key = hew_hir::mangle_layout_key("HashMapIter", &args);
        let cursor =
            ResolvedTy::named_builtin("HashMapIter", BuiltinType::HashMapIter, args.to_vec());
        let layouts = vec![
            RecordLayout {
                name: "Holder".to_string(),
                field_tys: vec![cursor.clone()],
                field_names: vec!["iter".to_string()],
            },
            // Put the colliding user layout before the synthetic row so a
            // leaf-based lookup would deterministically select the wrong one.
            RecordLayout {
                name: user_key,
                field_tys: vec![ResolvedTy::I64],
                field_names: vec!["user_field".to_string()],
            },
            RecordLayout {
                name: key.clone(),
                field_tys: vec![ResolvedTy::String, ResolvedTy::I64],
                field_names: vec!["key".to_string(), "value".to_string()],
            },
        ];
        let funcs = vec![store_function(
            "store_hashmap_iter",
            ResolvedTy::named_user("Holder", vec![]),
            cursor,
        )];

        let (record_seeds, enum_seeds) = collect_record_field_store_overwrite_seeds(
            &funcs,
            &layouts,
            &[],
            &hew_hir::LifecycleRegistry::default(),
        );
        assert_eq!(record_seeds, [key]);
        assert!(enum_seeds.is_empty());
    }

    #[test]
    fn generic_enum_field_store_seeds_monomorphised_helper() {
        let key = hew_hir::mangle_layout_key("Maybe", &[ResolvedTy::String]);
        let records = vec![RecordLayout {
            name: "Holder".to_string(),
            field_tys: vec![ResolvedTy::named_user("Maybe", vec![ResolvedTy::String])],
            field_names: vec!["value".to_string()],
        }];
        let enums = vec![EnumLayout {
            name: key.clone(),
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Some".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec!["value".to_string()],
            }],
            is_indirect: false,
        }];
        let funcs = vec![store_function(
            "store_maybe",
            ResolvedTy::named_user("Holder", vec![]),
            ResolvedTy::named_user("Maybe", vec![ResolvedTy::String]),
        )];

        let (record_seeds, enum_seeds) = collect_record_field_store_overwrite_seeds(
            &funcs,
            &records,
            &enums,
            &hew_hir::LifecycleRegistry::default(),
        );
        assert!(record_seeds.is_empty());
        assert_eq!(enum_seeds, [key]);
    }

    #[test]
    fn unknown_or_resource_record_field_store_mints_no_helper_seed() {
        let layouts = vec![
            RecordLayout {
                name: "UnknownHolder".to_string(),
                field_tys: vec![ResolvedTy::named_user("Missing", vec![])],
                field_names: vec!["value".to_string()],
            },
            RecordLayout {
                name: "ResourceHolder".to_string(),
                field_tys: vec![ResolvedTy::named_user("ResourceRecord", vec![])],
                field_names: vec!["value".to_string()],
            },
            RecordLayout {
                name: "ResourceRecord".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec!["value".to_string()],
            },
        ];
        let funcs = vec![
            store_function(
                "store_unknown",
                ResolvedTy::named_user("UnknownHolder", vec![]),
                ResolvedTy::named_user("Missing", vec![]),
            ),
            store_function(
                "store_resource",
                ResolvedTy::named_user("ResourceHolder", vec![]),
                ResolvedTy::named_user("ResourceRecord", vec![]),
            ),
        ];
        let mut type_classes = hew_hir::TypeClassTable::default();
        type_classes
            .admit_resource_record_lifecycle(hew_hir::ResourceRecordLifecycle {
                resource_declaration: hew_types::DefId::new("ResourceRecord"),
                close_declaration: hew_types::DefId::new("ResourceRecord::close"),
                close_symbol: "ResourceRecord::close".to_string(),
            })
            .unwrap();
        let (record_seeds, enum_seeds) = collect_record_field_store_overwrite_seeds(
            &funcs,
            &layouts,
            &[],
            type_classes.lifecycle_registry(),
        );
        assert!(record_seeds.is_empty());
        assert!(enum_seeds.is_empty());
    }
}
