use super::{
    base_local, instr_source_places, string_field_load_producer_dest, BasicBlock, HashSet, Instr,
    ResolvedTy, Terminator,
};

/// Prove retained string field-load destinations are predicate-only owners.
///
/// Admission is intentionally exact: the load, equality comparison feeding
/// the block's branch, and one `hew_string_drop` must occur in that order in
/// one block, and the loaded place must have no other read. Such a temporary
/// owns only the retain minted by codegen; it never takes ownership from the
/// aggregate field and therefore must not suppress the aggregate's composite
/// scope-exit drop.
pub(super) fn predicate_string_temp_drop_proof(
    blocks: &[BasicBlock],
    local_tys: &[ResolvedTy],
) -> HashSet<u32> {
    let mut proven = HashSet::new();
    for block in blocks {
        let Terminator::Branch { cond, .. } = block.terminator else {
            continue;
        };
        for (load_idx, load) in block.instructions.iter().enumerate() {
            let Some(loaded) = string_field_load_producer_dest(load, local_tys) else {
                continue;
            };
            let Some(loaded_local) = base_local(loaded) else {
                continue;
            };
            let comparisons = block
                .instructions
                .iter()
                .enumerate()
                .filter_map(|(idx, instr)| match instr {
                    Instr::IntCmp { lhs, rhs, dest, .. }
                        if *dest == cond && (*lhs == loaded || *rhs == loaded) =>
                    {
                        Some(idx)
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            let drops = block
                .instructions
                .iter()
                .enumerate()
                .filter_map(|(idx, instr)| match instr {
                    Instr::Drop {
                        place,
                        ty: ResolvedTy::String,
                        drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                    } if *place == loaded => Some(idx),
                    _ => None,
                })
                .collect::<Vec<_>>();
            let ([cmp_idx], [drop_idx]) = (comparisons.as_slice(), drops.as_slice()) else {
                continue;
            };
            if !(load_idx < *cmp_idx && *cmp_idx < *drop_idx) {
                continue;
            }
            let reads_are_exact = block.instructions.iter().enumerate().all(|(idx, instr)| {
                !instr_source_places(instr).contains(&loaded) || idx == *cmp_idx || idx == *drop_idx
            });
            if reads_are_exact {
                proven.insert(loaded_local);
            }
        }
    }
    proven
}
