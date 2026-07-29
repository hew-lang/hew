use super::super::temp_drop::string_share_sink_places;
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
/// tuple, array, or inline enum it turns that byte-copy into an independent
/// owner, so the projection's original root keeps its complete composite drop.
/// A shape with bytes, collections, indirect enums, resources, or opaque
/// handles is rejected: the string marker does not clone those leaves, and
/// treating it as a whole-value clone would double-release them.
pub(super) fn aggregate_borrowed_ingress_clones_source(
    block: &BasicBlock,
    instr_index: usize,
    source: Place,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
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
                enum_layouts,
            )
        })
}

pub(super) fn aggregate_borrowed_ingress_retain_clones_value(
    instr: &Instr,
    source: Place,
    source_ty: &ResolvedTy,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
) -> bool {
    matches!(
        instr,
        Instr::StringRetain {
            value,
            condition: StringRetainCondition::AggregateBorrowedIngress,
        } if *value == source
    ) && string_or_bitcopy_tree(
        source_ty,
        record_field_orders,
        enum_layouts,
        &mut HashSet::new(),
    )
}

pub(super) fn aggregate_borrowed_ingress_site_clones_source(
    block: u32,
    instr_index: usize,
    source: Place,
    sites: &HashSet<(u32, usize, Place)>,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
) -> bool {
    sites.contains(&(block, instr_index, source))
        && base_local(source)
            .and_then(|local| local_tys.get(local as usize))
            .is_some_and(|ty| {
                string_or_bitcopy_tree(ty, record_field_orders, enum_layouts, &mut HashSet::new())
            })
}

/// Whether `instr` is one of the real aggregate owner sinks for which the
/// retain derivation cloned `source` recursively.
///
/// The sink-membership check and the pre/post-marker proofs deliberately share
/// [`string_share_sink_places`] with the retain derivation. This keeps tuple
/// construction, field stores, actor state, spawn state, and retained closure
/// environments symmetric with `RecordInit`; an unrelated instruction after a
/// marker cannot borrow the exemption.
pub(super) fn aggregate_borrowed_ingress_sink_clones_source(
    block: &BasicBlock,
    instr_index: usize,
    source: Place,
    sites: Option<&HashSet<(u32, usize, Place)>>,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
) -> bool {
    let Some(instr) = block.instructions.get(instr_index) else {
        return false;
    };
    string_share_sink_places(instr).contains(&source)
        && (aggregate_borrowed_ingress_clones_source(
            block,
            instr_index,
            source,
            local_tys,
            record_field_orders,
            enum_layouts,
        ) || sites.is_some_and(|sites| {
            aggregate_borrowed_ingress_site_clones_source(
                block.id,
                instr_index,
                source,
                sites,
                local_tys,
                record_field_orders,
                enum_layouts,
            )
        }))
}

fn string_or_bitcopy_tree(
    ty: &ResolvedTy,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
    visiting: &mut HashSet<String>,
) -> bool {
    if matches!(ty, ResolvedTy::String) || crate::return_provenance::ty_is_scalar_non_heap(ty) {
        return true;
    }
    match ty {
        ResolvedTy::Tuple(elements) => elements.iter().all(|element| {
            string_or_bitcopy_tree(element, record_field_orders, enum_layouts, visiting)
        }),
        ResolvedTy::Array(element, _) => {
            string_or_bitcopy_tree(element, record_field_orders, enum_layouts, visiting)
        }
        ResolvedTy::Named {
            name,
            args,
            builtin: None,
            ..
        } => {
            if let Some(layout) = crate::model::find_enum_layout(name, args, enum_layouts) {
                if layout.is_indirect {
                    return false;
                }
                let key = format!("enum:{}", layout.name);
                if !visiting.insert(key.clone()) {
                    return false;
                }
                let result = layout.variants.iter().all(|variant| {
                    variant.field_tys.iter().all(|field_ty| {
                        string_or_bitcopy_tree(
                            field_ty,
                            record_field_orders,
                            enum_layouts,
                            visiting,
                        )
                    })
                });
                visiting.remove(&key);
                return result;
            }
            let Some(key) = user_record_layout_key(ty) else {
                return false;
            };
            if !visiting.insert(key.clone()) {
                return false;
            }
            let result = record_field_orders.get(&key).is_some_and(|fields| {
                fields.iter().all(|(_, field_ty)| {
                    string_or_bitcopy_tree(field_ty, record_field_orders, enum_layouts, visiting)
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
    use crate::model::{EnumLayout, MachineVariantLayout};

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

    fn leaf_fields() -> HashMap<String, Vec<(String, ResolvedTy)>> {
        HashMap::from([(
            "Leaf".to_string(),
            vec![
                ("label".to_string(), ResolvedTy::String),
                ("ordinal".to_string(), ResolvedTy::I64),
            ],
        )])
    }

    fn enum_layout(is_indirect: bool, extra_variant_field: ResolvedTy) -> EnumLayout {
        EnumLayout {
            name: "Payload".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Text".to_string(),
                    field_tys: vec![ResolvedTy::String],
                    field_names: vec!["text".to_string()],
                },
                MachineVariantLayout {
                    name: "Extra".to_string(),
                    field_tys: vec![extra_variant_field],
                    field_names: vec!["value".to_string()],
                },
            ],
            is_indirect,
        }
    }

    #[test]
    fn marker_proves_string_only_projection_clone() {
        let block = sink_block(StringRetainCondition::AggregateBorrowedIngress);
        let fields = leaf_fields();
        assert!(aggregate_borrowed_ingress_clones_source(
            &block,
            1,
            Place::Local(0),
            &[ResolvedTy::named_user("Leaf", vec![])],
            &fields,
            &[],
        ));
    }

    #[test]
    fn proof_applies_to_each_real_retained_sink() {
        let source_ty = ResolvedTy::named_user("Leaf", vec![]);
        let local_tys = [source_ty.clone()];
        let fields = leaf_fields();
        let sinks = [
            Instr::TupleConstruct {
                elements: vec![Place::Local(0)],
                dest: Place::Local(1),
            },
            Instr::RecordFieldStore {
                record: Place::Local(1),
                field_offset: FieldOffset(0),
                src: Place::Local(0),
            },
        ];

        for sink in sinks {
            let block = BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![
                    Instr::StringRetain {
                        value: Place::Local(0),
                        condition: StringRetainCondition::AggregateBorrowedIngress,
                    },
                    sink,
                ],
                terminator: Terminator::Return,
            };
            assert!(aggregate_borrowed_ingress_sink_clones_source(
                &block,
                1,
                Place::Local(0),
                None,
                &local_tys,
                &fields,
                &[],
            ));
        }
    }

    #[test]
    fn marker_does_not_exempt_an_unrelated_instruction() {
        let block = BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::StringRetain {
                    value: Place::Local(0),
                    condition: StringRetainCondition::AggregateBorrowedIngress,
                },
                Instr::Move {
                    dest: Place::Local(1),
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        };
        assert!(!aggregate_borrowed_ingress_sink_clones_source(
            &block,
            1,
            Place::Local(0),
            None,
            &[ResolvedTy::named_user("Leaf", vec![])],
            &leaf_fields(),
            &[],
        ));
    }

    #[test]
    fn inline_enum_is_cloneable_only_when_all_variants_are_total() {
        let block = sink_block(StringRetainCondition::AggregateBorrowedIngress);
        let ty = ResolvedTy::named_user("Payload", vec![]);
        assert!(aggregate_borrowed_ingress_clones_source(
            &block,
            1,
            Place::Local(0),
            std::slice::from_ref(&ty),
            &HashMap::new(),
            &[enum_layout(false, ResolvedTy::I64)],
        ));
        assert!(!aggregate_borrowed_ingress_clones_source(
            &block,
            1,
            Place::Local(0),
            std::slice::from_ref(&ty),
            &HashMap::new(),
            &[enum_layout(false, ResolvedTy::Bytes)],
        ));
        assert!(!aggregate_borrowed_ingress_clones_source(
            &block,
            1,
            Place::Local(0),
            std::slice::from_ref(&ty),
            &HashMap::new(),
            &[enum_layout(
                false,
                ResolvedTy::Named {
                    name: "Handle".to_string(),
                    args: vec![],
                    builtin: None,
                    is_opaque: true,
                },
            )],
        ));
        assert!(!aggregate_borrowed_ingress_clones_source(
            &block,
            1,
            Place::Local(0),
            &[ty],
            &HashMap::new(),
            &[enum_layout(true, ResolvedTy::I64)],
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
            &[],
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
            &[],
        ));
    }
}
