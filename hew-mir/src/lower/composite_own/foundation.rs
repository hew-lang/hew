use super::{base_local, BasicBlock, HashMap, HashSet, Instr, ScopeId, ScopeInfoEntry, Terminator};

pub(super) fn generator_env_snapshot_init_locals(blocks: &[BasicBlock]) -> HashSet<u32> {
    blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            Terminator::MakeGenerator { env: Some(env), .. } => base_local(env.place),
            _ => None,
        })
        .collect()
}

pub(super) fn initializes_generator_env_snapshot(instr: &Instr, env_locals: &HashSet<u32>) -> bool {
    matches!(
        instr,
        Instr::RecordInit { dest, .. }
            if base_local(*dest).is_some_and(|local| env_locals.contains(&local))
    )
}

/// Whether `destination` closes no later than `source`.
///
/// Scope ids are opaque identities, so lexical containment must follow the
/// builder's parent graph. Missing or cyclic ancestry fails closed.
pub(super) fn scope_is_same_or_nested(
    destination: ScopeId,
    source: ScopeId,
    scope_info: &HashMap<ScopeId, ScopeInfoEntry>,
) -> bool {
    let mut current = destination;
    let mut visited = HashSet::new();
    let mut contains_source = false;
    loop {
        if !visited.insert(current) {
            return false;
        }
        contains_source |= current == source;
        let Some(entry) = scope_info.get(&current) else {
            return false;
        };
        let Some(parent) = entry.parent else {
            return contains_source;
        };
        current = parent;
    }
}
