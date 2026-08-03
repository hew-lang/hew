use super::{
    base_local, propagate_whole_value_alias_roots, ty_is_owned_handle_leaf, BasicBlock, BindingId,
    HashMap, HashSet, Instr, Place, ResolvedTy,
};
#[cfg(test)]
use super::{BuiltinType, Terminator};

/// Builtin-handle bindings that take ownership from a scope-exit tuple whose
/// own recursive drop has been excluded.
///
/// A tuple field load is physically a pointer copy, so the general projection
/// taint marks its destination as an alias. When the source is itself a tracked
/// tuple owner, however, the tuple sole-owner prover excludes that root after
/// the loaded handle reaches an independently tracked binding. The binding is
/// then the only remaining close authority. This exact structural handoff is
/// distinct from an enum payload borrow, whose live parent keeps its recursive
/// drop and therefore continues to suppress the projected binding's close.
///
/// The proof requires all of the following: a registered heap-owning tuple
/// root, an exact `TupleFieldLoad` from its move closure, an exact builtin
/// handle destination type, a move-only path to a registered binding, and the
/// absence of the tuple root from the recursive-drop allow-set. Ambiguous move
/// fan-out is rejected by `propagate_whole_value_alias_roots`.
#[must_use]
pub(in crate::lower) fn derive_owned_tuple_handle_projection_bindings(
    blocks: &[BasicBlock],
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    tuple_composite_drop_allowed: &HashSet<BindingId>,
) -> HashSet<BindingId> {
    let tuple_roots: HashMap<u32, BindingId> = owned_locals
        .iter()
        .filter(|(binding, _name, ty)| {
            matches!(ty, ResolvedTy::Tuple(_)) && !tuple_composite_drop_allowed.contains(binding)
        })
        .filter_map(|(binding, _name, _ty)| {
            binding_locals
                .get(binding)
                .and_then(|place| base_local(*place))
                .map(|local| (local, *binding))
        })
        .collect();
    if tuple_roots.is_empty() {
        return HashSet::new();
    }

    let tuple_aliases = propagate_whole_value_alias_roots(blocks, tuple_roots.keys().copied());
    let projection_seeds: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| {
            let Instr::TupleFieldLoad { tuple, dest, .. } = instr else {
                return None;
            };
            let source = base_local(*tuple)?;
            let dest = base_local(*dest)?;
            (tuple_aliases.contains_key(&source)
                && local_tys
                    .get(dest as usize)
                    .is_some_and(ty_is_owned_handle_leaf))
            .then_some(dest)
        })
        .collect();
    if projection_seeds.is_empty() {
        return HashSet::new();
    }

    let projection_closure =
        propagate_whole_value_alias_roots(blocks, projection_seeds.iter().copied());
    binding_locals
        .iter()
        .filter_map(|(binding, place)| {
            let local = base_local(*place)?;
            (projection_closure.contains_key(&local)
                && local_tys
                    .get(local as usize)
                    .is_some_and(ty_is_owned_handle_leaf))
            .then_some(*binding)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn excluded_tuple_owner_transfers_builtin_handle_projection() {
        let tuple_owner = BindingId(10);
        let projected_owner = BindingId(11);
        let stream_ty =
            ResolvedTy::named_builtin("std.io.Stream", BuiltinType::Stream, vec![ResolvedTy::I64]);
        let blocks = vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::TupleFieldLoad {
                    tuple: Place::Local(10),
                    field_index: 0,
                    dest: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }];
        let mut local_tys = vec![ResolvedTy::Unit; 11];
        local_tys[1] = stream_ty.clone();
        local_tys[2] = stream_ty.clone();
        local_tys[10] = ResolvedTy::Tuple(vec![stream_ty.clone(), ResolvedTy::I64]);
        let binding_locals = HashMap::from([
            (tuple_owner, Place::Local(10)),
            (projected_owner, Place::Local(2)),
        ]);
        let owned_locals = vec![(
            tuple_owner,
            "pair".to_string(),
            ResolvedTy::Tuple(vec![stream_ty, ResolvedTy::I64]),
        )];

        assert_eq!(
            derive_owned_tuple_handle_projection_bindings(
                &blocks,
                &owned_locals,
                &binding_locals,
                &local_tys,
                &HashSet::new(),
            ),
            HashSet::from([projected_owner]),
            "the binding must inherit close authority when the tuple root cannot drop"
        );
        assert!(derive_owned_tuple_handle_projection_bindings(
            &blocks,
            &owned_locals,
            &binding_locals,
            &local_tys,
            &HashSet::from([tuple_owner]),
        )
        .is_empty());
    }
}
