use super::{
    base_local, block_dominators, instr_site_dominates, propagate_whole_value_alias_roots,
    single_dominating_local_generation, string_field_load_producer_dest, BasicBlock, HashMap,
    HashSet, Instr, InstrSite, Place, ResolvedTy,
};

/// String field loads are emitted with an independent `+1` retain. Follow
/// their Move aliases so the aggregate sole-owner provers do not mistake that
/// independently owned share for an extraction of the source aggregate's
/// original field ownership.
pub(super) fn retained_string_field_load_aliases(
    blocks: &[BasicBlock],
    local_tys: &[ResolvedTy],
) -> HashSet<u32> {
    let seeds = blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| string_field_load_producer_dest(instr, local_tys))
        .filter_map(base_local);
    propagate_whole_value_alias_roots(blocks, seeds)
        .into_keys()
        .collect()
}

/// The generation-safe subset of retained string field-load aliases.
///
/// The aggregate provers key payload ownership by MIR local, while a local may
/// be reused for multiple generations. Removing payload-binder taint is sound
/// only when the field load and every onward alias each uniquely define their
/// destination. This keeps a retained string clone from masking a different
/// payload generation that later reuses the same local.
pub(super) fn uniquely_defined_retained_string_field_load_aliases(
    blocks: &[BasicBlock],
    local_tys: &[ResolvedTy],
) -> HashSet<u32> {
    let dominators = block_dominators(blocks);
    let mut proven_defs: HashMap<u32, InstrSite> = HashMap::new();
    for block in blocks {
        for (index, instr) in block.instructions.iter().enumerate() {
            let Some(dest) = string_field_load_producer_dest(instr, local_tys).and_then(base_local)
            else {
                continue;
            };
            let site = InstrSite {
                block: block.id,
                index,
            };
            if single_dominating_local_generation(blocks, &dominators, dest, site) {
                proven_defs.insert(dest, site);
            }
        }
    }

    loop {
        let mut changed = false;
        for block in blocks {
            for (index, instr) in block.instructions.iter().enumerate() {
                let Instr::Move {
                    dest: Place::Local(dest),
                    src: Place::Local(src),
                } = instr
                else {
                    continue;
                };
                let site = InstrSite {
                    block: block.id,
                    index,
                };
                let Some(&src_def) = proven_defs.get(src) else {
                    continue;
                };
                if single_dominating_local_generation(blocks, &dominators, *dest, site)
                    && instr_site_dominates(&dominators, src_def, site)
                    && proven_defs.insert(*dest, site).is_none()
                {
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    proven_defs.into_keys().collect()
}
