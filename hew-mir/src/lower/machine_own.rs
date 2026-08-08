//! Scope-exit drop authority for a machine value held in a local.
//!
//! A `machine` value is a tagged union whose active state's payload may own a
//! `#[resource]` handle or a heap leaf. The step function releases the payload
//! of a state it transitions AWAY from, but the LAST state a machine local ends
//! its scope in was never released by anything: a machine type is
//! `ValueClass::Unknown`, so its binding fell through every drop class in
//! `build_lifo_drops` and leaked — silently, with no diagnostic, while the same
//! handle held in a bare local closed correctly.
//!
//! This module is the fail-closed sole-owner derivation that admits such a
//! binding to the tag-aware `DropKind::EnumInPlace` drop (machines are enums at
//! the value-classification layer — `machine_enum_views`, so the helper family
//! `__hew_enum_drop_inplace_<Machine>` is the same tag-dispatch authority a user
//! enum uses).
//!
//! ## Why this is not `derive_enum_composite_drop_allowed`
//!
//! The step round-trip is unique to machines. `m.step(e)` lowers to
//! `dest = call mc$$M$$__step(m, e)` followed by `m = move dest`: the machine is
//! handed to the step function BY VALUE and the new value is stored back. To
//! the generic escape scan that whole-value call-argument read is an escape, so
//! a machine binding is excluded before any payload reasoning starts. Rather
//! than punch a machine-shaped hole in the enum prover — whose exemptions are
//! load-bearing for every `Result`/`Option` in the corpus — machines get their
//! own derivation over the SAME exhaustive operand classifiers.
//!
//! ## The admission rule
//!
//! A machine binding `b` with base local `L` is admitted IFF every read of `L`
//! is one of exactly three non-escaping shapes:
//!
//! 1. the value argument of a call to `L`'s own machine step symbol whose
//!    result is stored straight back into `L` and read nowhere else — the step
//!    round-trip, which returns ownership;
//! 2. an `Instr::MachineStateName` read — codegen loads the tag and yields a
//!    static string; nothing leaves the value;
//! 3. a `Place::MachineTag(L)` read — a tag test, which touches no payload.
//!
//! Every other read excludes the binding. A `Place::MachineVariant` read (an
//! inline `match` moving a payload out), a return, a store into an aggregate, a
//! send, or an argument to any other function all mean the payload may have a
//! second owner, so the binding keeps the pre-existing leak posture rather than
//! risking a double-close.
//!
//! Admission additionally requires the payload leaves to be shell-drop-safe
//! (`enum_payloads_are_shell_drop_safe`) — the same conjunction the enum
//! composite arm applies, so a state that mixes a releasable leaf with one the
//! shell cannot release refuses as a whole rather than half-releasing.

#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    base_local, instr_source_places, machine_synth::mangle_machine_step, terminator_source_places,
    BasicBlock, BindingId, HashMap, HashSet, Instr, Place, ResolvedTy, SuspendKind, Terminator,
};

#[cfg(not(test))]
use super::composite_own::enum_payloads_are_shell_drop_safe;

/// Resolve `ty` to the registered machine layout name it names, if any.
///
/// Machine layouts are registered under the mangled instantiation key, the same
/// scheme `find_enum_layout` resolves against, so an admitted binding's
/// `EnumInPlace` drop and the seeded `__hew_enum_drop_inplace_*` body agree on
/// one symbol.
pub(super) fn machine_layout_name_for_ty(
    ty: &ResolvedTy,
    machine_layout_names: &HashSet<String>,
) -> Option<String> {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return None;
    };
    // `machine_layout_names` is keyed by the canonical machine instantiation
    // key (`mc$$Name$$`), the same key `find_enum_layout` matches the machine
    // enum view against, so the admitted binding's drop and the seeded
    // `__hew_enum_drop_inplace_*` body agree on one symbol.
    let key = hew_hir::machine_layout_key(name, args);
    machine_layout_names.get(&key).cloned()
}

/// Fail-closed sole-owner allow-set for machine-typed owned locals.
///
/// See the module docs for the admission rule. Returns the subset of
/// `owned_locals` whose machine binding still owns its active state's payload
/// at scope exit and therefore earns the tag-aware in-place drop.
#[allow(
    clippy::too_many_arguments,
    reason = "the derivation reads the function MIR, the binding registries, and \
              the four layout/classification authorities the payload-safety \
              conjunction needs; bundling them would only relocate the fields"
)]
pub(super) fn derive_machine_composite_drop_allowed(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    machine_layout_names: &HashSet<String>,
    enum_layouts: &[crate::model::EnumLayout],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    type_classes: &hew_hir::TypeClassTable,
    record_layouts: &[crate::model::RecordLayout],
    opaque_handle_names: &[String],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> HashSet<BindingId> {
    let mut admitted = HashSet::new();
    // Candidate roots: base local -> (binding, its own step symbol).
    let mut candidates: Vec<(BindingId, u32, String)> = Vec::new();
    for (binding, _name, ty) in owned_locals {
        let Some(layout_name) = machine_layout_name_for_ty(ty, machine_layout_names) else {
            continue;
        };
        // Nothing to release: a machine whose every state payload is BitCopy
        // has no drop obligation, and emitting one would only add a call to an
        // empty helper.
        if !machine_payload_needs_release(ty, enum_layouts, type_classes) {
            continue;
        }
        // Same conjunction the enum composite arm applies: EVERY leaf of EVERY
        // state must be releasable by the shell's thunk chain. A state that
        // mixes a leaf the shell can free with one it cannot refuses whole.
        if !enum_payloads_are_shell_drop_safe(
            ty,
            enum_layouts,
            record_field_orders,
            type_classes,
            record_layouts,
            opaque_handle_names,
            lifecycle_registry,
        ) {
            continue;
        }
        let Some(local) = binding_locals.get(binding).copied().and_then(base_local) else {
            continue;
        };
        candidates.push((*binding, local, mangle_machine_step(&layout_name)));
    }
    if candidates.is_empty() {
        return admitted;
    }

    // Destinations of a step call, paired with the local the result is stored
    // back into. Built once for the whole function.
    let step_round_trips = collect_step_round_trips(blocks);

    for (binding, local, step_symbol) in candidates {
        if machine_local_reads_are_all_non_escaping(
            blocks,
            suspend_kinds,
            local,
            &step_symbol,
            &step_round_trips,
        ) {
            admitted.insert(binding);
        }
    }
    admitted
}

/// True when at least one state payload field of the machine `ty` carries a
/// release obligation — an affine `#[resource]`/`@linear` handle or a heap leaf.
fn machine_payload_needs_release(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
    type_classes: &hew_hir::TypeClassTable,
) -> bool {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    let Some(layout) = crate::model::find_enum_layout(name, args, enum_layouts) else {
        return false;
    };
    layout.variants.iter().any(|variant| {
        variant.field_tys.iter().any(|field_ty| {
            matches!(
                hew_hir::ValueClass::of_ty(field_ty, type_classes),
                hew_hir::ValueClass::AffineResource | hew_hir::ValueClass::Linear
            ) || crate::model::ty_owns_heap_mir(field_ty, &HashMap::new(), enum_layouts)
        })
    })
}

/// For every `Terminator::Call` whose destination is a plain local, record
/// `(callee, dest_local) -> store_back_local` when the result is stored into
/// exactly one local by a whole-value `Move` and read nowhere else.
///
/// This is the shape a machine step round-trip takes:
/// `dest = call mc$$M$$__step(m, e)` then `m = move dest`.
fn collect_step_round_trips(blocks: &[BasicBlock]) -> HashMap<(String, u32), u32> {
    // Count every source read of each local so a result used more than once
    // (or used for anything other than the store-back) is rejected.
    let mut reads: HashMap<u32, usize> = HashMap::new();
    let mut store_back: HashMap<u32, Vec<u32>> = HashMap::new();
    for block in blocks {
        for instr in &block.instructions {
            for place in instr_source_places(instr) {
                if let Some(l) = base_local(place) {
                    *reads.entry(l).or_insert(0) += 1;
                }
            }
            if let Instr::Move {
                dest: Place::Local(dest_local),
                src: Place::Local(src_local),
            } = instr
            {
                store_back.entry(*src_local).or_default().push(*dest_local);
            }
        }
        for place in terminator_source_places(&block.terminator, None) {
            if let Some(l) = base_local(place) {
                *reads.entry(l).or_insert(0) += 1;
            }
        }
    }
    let mut round_trips = HashMap::new();
    for block in blocks {
        let Terminator::Call {
            callee,
            dest: Some(Place::Local(dest_local)),
            ..
        } = &block.terminator
        else {
            continue;
        };
        // The result must be consumed exactly once, by a whole-value store into
        // a single local.
        if reads.get(dest_local).copied().unwrap_or(0) != 1 {
            continue;
        }
        let Some([target]) = store_back.get(dest_local).map(Vec::as_slice) else {
            continue;
        };
        round_trips.insert((callee.clone(), *dest_local), *target);
    }
    round_trips
}

/// Whether every source read of `local` in the function is one of the three
/// non-escaping machine shapes. See the module docs for the rule.
fn machine_local_reads_are_all_non_escaping(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    local: u32,
    step_symbol: &str,
    step_round_trips: &HashMap<(String, u32), u32>,
) -> bool {
    for block in blocks {
        for instr in &block.instructions {
            // A `MachineStateName` read loads the tag and yields a static
            // string. Its operand classification is a whole-local read, so it
            // is exempted by instruction identity rather than by place shape.
            if matches!(instr, Instr::MachineStateName { src_local, .. } if *src_local == local) {
                continue;
            }
            for place in instr_source_places(instr) {
                if base_local(place) != Some(local) {
                    continue;
                }
                // A tag read touches no payload.
                if matches!(place, Place::MachineTag(_)) {
                    continue;
                }
                return false;
            }
        }
        let reads_local = terminator_source_places(&block.terminator, suspend_kinds.get(&block.id))
            .into_iter()
            .any(|place| base_local(place) == Some(local));
        if !reads_local {
            continue;
        }
        // The one non-escaping terminator read: this machine's own step call,
        // whose result is stored straight back into this local.
        let Terminator::Call {
            callee,
            dest: Some(Place::Local(dest_local)),
            ..
        } = &block.terminator
        else {
            return false;
        };
        if callee != step_symbol {
            return false;
        }
        if step_round_trips.get(&(callee.clone(), *dest_local)) != Some(&local) {
            return false;
        }
    }
    true
}
