use super::{BasicBlock, HashSet, Instr, Place, StringRetainCondition};

pub(super) fn retained_string_values_before(
    block: &BasicBlock,
    instr_index: usize,
) -> HashSet<Place> {
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
            // Only `Always` duplicates the ownership of `value` itself.
            // Aggregate/state ingress retains string leaves, not ownership of
            // an enclosing affine handle such as `Sink<string>`.
            Instr::StringRetain {
                value,
                condition: StringRetainCondition::Always,
            } => Some(*value),
            _ => None,
        })
        .collect()
}
