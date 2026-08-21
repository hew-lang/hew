use super::{
    base_local, place_is_owned_handoff_member, BasicBlock, ClosureEnvFieldOwnership, HashSet,
    Instr, Place, StringRetainCondition,
};

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

/// The set of backing locals that whole-value flow into this function's
/// `ReturnSlot` — either directly or as a member of an aggregate that does.
pub(super) fn compute_returned_flow_locals(blocks: &[BasicBlock]) -> HashSet<u32> {
    // Seed: every owned hand-off slot (Local or handle place) whole-value
    // moved into the ReturnSlot.
    let mut flows_to_return: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::Move { dest, src } = instr {
                if matches!(dest, Place::ReturnSlot) {
                    if let Some(sl) = base_local(*src) {
                        if place_is_owned_handoff_member(*src) {
                            flows_to_return.insert(sl);
                        }
                    }
                }
            }
        }
    }
    if flows_to_return.is_empty() {
        return HashSet::new();
    }

    // Fixpoint: grow the set backward along whole-value Moves and downward
    // through aggregate constructors whose dest already flows to the return.
    // Add an owned hand-off source (Local or handle place) to the set,
    // reporting whether it grew. Interior-projection places are rejected.
    let add_member = |place: &Place, set: &mut HashSet<u32>| -> bool {
        match base_local(*place) {
            Some(local) if place_is_owned_handoff_member(*place) => set.insert(local),
            _ => false,
        }
    };
    loop {
        let mut changed = false;
        for block in blocks {
            for (instr_index, instr) in block.instructions.iter().enumerate() {
                match instr {
                    // Whole-value rebind/temp: `Move { dest: in-set, src }`
                    // means `src` flowed onward into a local that reaches the
                    // ReturnSlot, so `src` reaches it too.
                    // A divergent-arm selection transfer nulls `src` right
                    // after this move, so the source does NOT flow to the
                    // caller: the value the caller receives lives only in
                    // `dest`, and on every path that took a different arm the
                    // source still owns its own value and must keep its
                    // scope-exit release. Following the edge here would strip
                    // that release path-insensitively (the losing-arm leak).
                    Instr::Move { dest, src }
                        if matches!(dest, Place::Local(_))
                            && base_local(*dest)
                                .is_some_and(|dl| flows_to_return.contains(&dl))
                            && !crate::lower::split_consume::is_divergent_selection_transfer_move(
                                block,
                                instr_index,
                            ) =>
                    {
                        changed |= add_member(src, &mut flows_to_return);
                    }
                    // Aggregate construction whose dest reaches the return: each
                    // element source is a member handed to the caller.
                    Instr::TupleConstruct { elements, dest }
                        if base_local(*dest).is_some_and(|dl| flows_to_return.contains(&dl)) =>
                    {
                        let retained = retained_owner_values_before(block, instr_index);
                        for elem in elements {
                            if !retained.contains(elem) {
                                changed |= add_member(elem, &mut flows_to_return);
                            }
                        }
                    }
                    // Record construction whose dest reaches the return: each
                    // field source is a member handed to the caller.
                    Instr::RecordInit { fields, dest, .. }
                        if base_local(*dest).is_some_and(|dl| flows_to_return.contains(&dl)) =>
                    {
                        let retained = retained_owner_values_before(block, instr_index);
                        for (_offset, field) in fields {
                            if !retained.contains(field) {
                                changed |= add_member(field, &mut flows_to_return);
                            }
                        }
                    }
                    Instr::ClosureEnvInit { fields, dest, .. }
                        if base_local(*dest).is_some_and(|dl| flows_to_return.contains(&dl)) =>
                    {
                        for field in fields
                            .iter()
                            .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
                        {
                            changed |= add_member(&field.src, &mut flows_to_return);
                        }
                    }
                    // Variant payload stores do not lower through the aggregate
                    // construction instructions, so follow their source into a
                    // return-bound enum or machine carrier explicitly.
                    Instr::Move { dest, src }
                        if matches!(
                            dest,
                            Place::MachineVariant { .. } | Place::EnumVariant { .. }
                        ) && base_local(*dest)
                            .is_some_and(|dl| flows_to_return.contains(&dl)) =>
                    {
                        changed |= add_member(src, &mut flows_to_return);
                    }
                    _ => {}
                }
            }
        }
        if !changed {
            break;
        }
    }

    flows_to_return
}
