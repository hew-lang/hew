use super::{BasicBlock, HashSet, Instr, Place, StringRetainCondition};

pub(super) fn retained_owner_values_before(
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
            // A bytes retain and an unconditional string retain duplicate
            // ownership of the value itself. Aggregate/state string ingress
            // retains leaves, not an enclosing affine handle such as
            // `Sink<string>`.
            Instr::BytesRetain { value }
            | Instr::StringRetain {
                value,
                condition: StringRetainCondition::Always,
            } => Some(*value),
            _ => None,
        })
        .collect()
}
