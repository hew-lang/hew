use super::{
    base_local, place_is_interior_projection, BasicBlock, HashMap, HashSet, Instr, Place,
    ResolvedTy,
};

/// Finds direct match payload binders whose resource lifecycle is independent
/// from their enclosing enum shell. Opaque resources are independent by type;
/// field-bearing user-close resources become independent only after a proven
/// consuming handoff neutralizes their carrier slot.
pub(super) fn direct_independent_resource_payloads(
    blocks: &[BasicBlock],
    alias_of: &HashMap<u32, u32>,
    local_tys: &[ResolvedTy],
    type_classes: &hew_hir::TypeClassTable,
    lifecycle_registry: &hew_hir::LifecycleRegistry,
    local_is_heap_owning: &dyn Fn(u32) -> bool,
) -> HashSet<u32> {
    let neutralized_payload_slots: HashSet<Place> = blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::NeutralizePayloadSlot { place, .. } => Some(*place),
            _ => None,
        })
        .collect();
    let mut payloads = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            let Instr::Move { dest, src } = instr else {
                continue;
            };
            if !place_is_interior_projection(*src) {
                continue;
            }
            let (Some(source), Some(dest)) = (base_local(*src), base_local(*dest)) else {
                continue;
            };
            if !alias_of.contains_key(&source) || !local_is_heap_owning(dest) {
                continue;
            }
            let Some(ty) = local_tys.get(dest as usize) else {
                continue;
            };
            let opaque_resource = matches!(
                ty,
                ResolvedTy::Named {
                    is_opaque: true,
                    ..
                }
            ) && lifecycle_registry.opaque_resource_for_ty(ty).is_some();
            let consumed_resource_close = matches!(
                super::super::drop_plan::resource_drop_fn(ty, type_classes),
                Some(
                    crate::model::DropFnSpec::Runtime(
                        hew_types::runtime_call::RuntimeDropDescriptor::SinkClose
                    ) | crate::model::DropFnSpec::UserClose(_)
                )
            ) && neutralized_payload_slots.contains(src);
            if opaque_resource || consumed_resource_close {
                payloads.insert(dest);
            }
        }
    }
    payloads
}
