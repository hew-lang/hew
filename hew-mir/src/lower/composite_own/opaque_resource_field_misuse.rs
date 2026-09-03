//! RAII-1 opaque-resource field-misuse gate, carved from `composite_own.rs`
//! as a pure move to stay under the lowering module line ceiling.

use super::{
    base_local, render_owned_handle_ty, AggregateOwner, BasicBlock, BindingId, HashMap, HashSet,
    Instr, MirCheck, MirStatement, Place, ResolvedTy,
};

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
/// builtin-handle W3.053 aggregate double-free defect class tracked by #3119.
pub(in crate::lower) fn detect_opaque_resource_field_misuse(
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
        // resolved type NAME against the registry, not the flag. The match is
        // exact nominal identity: a same-leaf declaration from another owner
        // must not acquire this resource's close discipline.
        matches!(
            ty,
            ResolvedTy::Named { name, .. } if opaque_resource_names.contains(name.as_str())
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
