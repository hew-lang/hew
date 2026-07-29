use super::{
    base_local, user_record_layout_key, BasicBlock, HashMap, HashSet, Instr, Place, ResolvedTy,
    StringRetainCondition,
};
#[cfg(test)]
use super::{FieldOffset, Terminator};

/// Whether the explicit recursive-string retain immediately before an aggregate
/// sink cloned every owning leaf of `source`.
///
/// `StringRetainCondition::AggregateBorrowedIngress` is emitted for a borrowed
/// projection stored into a new aggregate. For a string/bit-copy-only record,
/// tuple, or array it turns that byte-copy into an independent owner, so the
/// projection's original root keeps its complete composite drop. A shape with
/// bytes, collections, enums, resources, or opaque handles is rejected: the
/// string marker does not clone those leaves, and treating it as a whole-value
/// clone would double-release them.
pub(super) fn aggregate_borrowed_ingress_clones_source(
    block: &BasicBlock,
    instr_index: usize,
    source: Place,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
) -> bool {
    let Some(source_ty) = base_local(source).and_then(|local| local_tys.get(local as usize)) else {
        return false;
    };
    block.instructions[..instr_index]
        .iter()
        .rev()
        .take_while(|instr| matches!(instr, Instr::StringRetain { .. }))
        .any(|instr| {
            aggregate_borrowed_ingress_retain_clones_value(
                instr,
                source,
                source_ty,
                record_field_orders,
            )
        })
}

pub(super) fn aggregate_borrowed_ingress_retain_clones_value(
    instr: &Instr,
    source: Place,
    source_ty: &ResolvedTy,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
) -> bool {
    matches!(
        instr,
        Instr::StringRetain {
            value,
            condition: StringRetainCondition::AggregateBorrowedIngress,
        } if *value == source
    ) && string_or_bitcopy_tree(source_ty, record_field_orders, &mut HashSet::new())
}

pub(super) fn aggregate_borrowed_ingress_site_clones_source(
    block: u32,
    instr_index: usize,
    source: Place,
    sites: &HashSet<(u32, usize, Place)>,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
) -> bool {
    sites.contains(&(block, instr_index, source))
        && base_local(source)
            .and_then(|local| local_tys.get(local as usize))
            .is_some_and(|ty| string_or_bitcopy_tree(ty, record_field_orders, &mut HashSet::new()))
}

fn string_or_bitcopy_tree(
    ty: &ResolvedTy,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    visiting: &mut HashSet<String>,
) -> bool {
    if matches!(ty, ResolvedTy::String) || crate::return_provenance::ty_is_scalar_non_heap(ty) {
        return true;
    }
    match ty {
        ResolvedTy::Tuple(elements) => elements
            .iter()
            .all(|element| string_or_bitcopy_tree(element, record_field_orders, visiting)),
        ResolvedTy::Array(element, _) => {
            string_or_bitcopy_tree(element, record_field_orders, visiting)
        }
        ResolvedTy::Named { builtin: None, .. } => {
            let Some(key) = user_record_layout_key(ty) else {
                return false;
            };
            if !visiting.insert(key.clone()) {
                return false;
            }
            let result = record_field_orders.get(&key).is_some_and(|fields| {
                fields.iter().all(|(_, field_ty)| {
                    string_or_bitcopy_tree(field_ty, record_field_orders, visiting)
                })
            });
            visiting.remove(&key);
            result
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sink_block(condition: StringRetainCondition) -> BasicBlock {
        BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::StringRetain {
                    value: Place::Local(0),
                    condition,
                },
                Instr::RecordInit {
                    ty: ResolvedTy::named_user("Holder", vec![]),
                    fields: vec![(FieldOffset(0), Place::Local(0))],
                    dest: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }
    }

    #[test]
    fn marker_proves_string_only_projection_clone() {
        let block = sink_block(StringRetainCondition::AggregateBorrowedIngress);
        let fields = HashMap::from([(
            "Leaf".to_string(),
            vec![
                ("label".to_string(), ResolvedTy::String),
                ("ordinal".to_string(), ResolvedTy::I64),
            ],
        )]);
        assert!(aggregate_borrowed_ingress_clones_source(
            &block,
            1,
            Place::Local(0),
            &[ResolvedTy::named_user("Leaf", vec![])],
            &fields,
        ));
    }

    #[test]
    fn ordinary_retain_is_not_aggregate_clone_authority() {
        let block = sink_block(StringRetainCondition::Always);
        let fields = HashMap::from([(
            "Leaf".to_string(),
            vec![("label".to_string(), ResolvedTy::String)],
        )]);
        assert!(!aggregate_borrowed_ingress_clones_source(
            &block,
            1,
            Place::Local(0),
            &[ResolvedTy::named_user("Leaf", vec![])],
            &fields,
        ));
    }

    #[test]
    fn string_marker_cannot_launder_uncloned_owned_leaf() {
        let block = sink_block(StringRetainCondition::AggregateBorrowedIngress);
        let fields = HashMap::from([(
            "Mixed".to_string(),
            vec![
                ("label".to_string(), ResolvedTy::String),
                ("payload".to_string(), ResolvedTy::Bytes),
            ],
        )]);
        assert!(!aggregate_borrowed_ingress_clones_source(
            &block,
            1,
            Place::Local(0),
            &[ResolvedTy::named_user("Mixed", vec![])],
            &fields,
        ));
    }
}
