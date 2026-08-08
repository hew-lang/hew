use super::{
    base_local, blocks_reachable_from, bytes_place_is_typed, BasicBlock, BindingId, HashMap,
    HashSet, Instr, Place, ResolvedTy,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct BytesPayloadHandoff {
    pub(super) source: Place,
    pub(super) dest_local: u32,
    pub(super) dest_binding: BindingId,
}

fn local_static_write_counts(blocks: &[BasicBlock]) -> HashMap<u32, usize> {
    let mut write_counts = HashMap::new();
    for block in blocks {
        for instr in &block.instructions {
            for place in crate::dataflow::instr_reads_writes(instr).1 {
                if let Some(local) = base_local(place) {
                    *write_counts.entry(local).or_default() += 1;
                }
            }
        }
        for place in crate::dataflow::terminator_write_places(&block.terminator) {
            if let Some(local) = base_local(place) {
                *write_counts.entry(local).or_default() += 1;
            }
        }
    }
    write_counts
}

fn cyclic_block_ids(blocks: &[BasicBlock]) -> HashSet<u32> {
    blocks
        .iter()
        .filter(|block| blocks_reachable_from(blocks, block.id).contains(&block.id))
        .map(|block| block.id)
        .collect()
}

/// Raw bytes-payload shares for which the bytes prover may mint one retained
/// destination owner.
///
/// The admitted shape is intentionally exact: a uniquely-defined bytes match
/// binder is loaded directly from an enum/machine payload and immediately
/// copied into a uniquely-defined owned bytes binding in the same acyclic
/// block. The source must be a named binding slot because the later generic
/// local-share rule uses that same fact to place `BytesRetain(source)` before
/// the copy. Any intervening instruction, local reuse, terminator overwrite,
/// wrong type, or cyclic execution keeps ordinary projection taint.
pub(super) fn provable_bytes_payload_handoff_sites(
    blocks: &[BasicBlock],
    local_tys: &[ResolvedTy],
    candidate_local_to_binding: &HashMap<u32, BindingId>,
    binding_local_bases: &HashSet<u32>,
) -> HashMap<(u32, usize), BytesPayloadHandoff> {
    let write_counts = local_static_write_counts(blocks);
    let cyclic_blocks = cyclic_block_ids(blocks);
    let mut sites = HashMap::new();

    for block in blocks {
        if cyclic_blocks.contains(&block.id) {
            continue;
        }
        for handoff_index in 1..block.instructions.len() {
            let (
                Instr::Move {
                    dest: Place::Local(source_local),
                    src: Place::EnumVariant { .. } | Place::MachineVariant { .. },
                },
                Instr::Move {
                    dest: Place::Local(dest_local),
                    src: Place::Local(handoff_source),
                },
            ) = (
                &block.instructions[handoff_index - 1],
                &block.instructions[handoff_index],
            )
            else {
                continue;
            };
            let Some(&dest_binding) = candidate_local_to_binding.get(dest_local) else {
                continue;
            };
            if source_local == dest_local
                || source_local != handoff_source
                || !binding_local_bases.contains(source_local)
                || !bytes_place_is_typed(Place::Local(*source_local), local_tys)
                || !bytes_place_is_typed(Place::Local(*dest_local), local_tys)
                || write_counts.get(source_local).copied() != Some(1)
                || write_counts.get(dest_local).copied() != Some(1)
            {
                continue;
            }
            sites.insert(
                (block.id, handoff_index),
                BytesPayloadHandoff {
                    source: Place::Local(*source_local),
                    dest_local: *dest_local,
                    dest_binding,
                },
            );
        }
    }
    sites
}
