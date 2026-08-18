use super::{
    base_local, place_is_interior_projection, BasicBlock, HashMap, HashSet, Instr, Place,
    ResolvedTy,
};

/// Finds direct match payload binders whose resource lifecycle is independent
/// from their enclosing enum shell. Opaque resources are independent by type;
/// field-bearing user-close resources become independent only after a proven
/// consuming handoff neutralizes their carrier slot.
pub(super) fn direct_independent_resource_payloads(
    blocks: &[BasicBlock],
    alias_of: &HashMap<u32, u32>,
    local_tys: &[ResolvedTy],
    type_classes: &hew_hir::TypeClassTable,
    lifecycle_registry: &hew_hir::LifecycleRegistry,
    proven_owning_destinations: &HashSet<u32>,
    local_is_heap_owning: &dyn Fn(u32) -> bool,
) -> HashSet<u32> {
    let neutralized_payload_slots: HashSet<Place> = blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::NeutralizePayloadSlot { place, .. } => Some(*place),
            _ => None,
        })
        .collect();
    let mut payloads = HashSet::new();
    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            let Instr::Move { dest, src } = instr else {
                continue;
            };
            if !place_is_interior_projection(*src) {
                continue;
            }
            let moved_dest = *dest;
            let (Some(source), Some(dest)) = (base_local(*src), base_local(moved_dest)) else {
                continue;
            };
            if !alias_of.contains_key(&source) || !local_is_heap_owning(dest) {
                continue;
            }
            let Some(ty) = local_tys.get(dest as usize) else {
                continue;
            };
            let opaque_resource = matches!(
                ty,
                ResolvedTy::Named {
                    is_opaque: true,
                    ..
                }
            ) && lifecycle_registry.opaque_resource_for_ty(ty).is_some();
            let consumed_user_close = matches!(
                super::super::drop_plan::resource_drop_fn(ty, type_classes),
                Some(crate::model::DropFnSpec::UserClose(_))
            ) && neutralized_payload_slots.contains(src);
            let source_is_result_payload =
                local_tys.get(source as usize).is_some_and(|source_ty| {
                    matches!(
                        (source_ty, src),
                        (
                            ResolvedTy::Named {
                                args,
                                builtin: Some(hew_types::BuiltinType::Result),
                                ..
                            },
                            Place::MachineVariant {
                                variant_idx: 0,
                                field_idx: 0,
                                ..
                            } | Place::EnumVariant {
                                variant_idx: 0,
                                field_idx: 0,
                                ..
                            }
                        ) if args.first() == Some(ty)
                    )
                });
            let transferred_sink_close = matches!(
                super::super::drop_plan::resource_drop_fn(ty, type_classes),
                Some(crate::model::DropFnSpec::Runtime(
                    hew_types::runtime_call::RuntimeDropDescriptor::SinkClose
                ))
            ) && source_is_result_payload
                && proven_owning_destinations.contains(&dest)
                && matches!(
                    block.instructions.get(instr_index + 1),
                    Some(Instr::NeutralizePayloadSlot {
                        place,
                        transferee: Some(transferee),
                        authority: crate::model::NeutralizeAuthority::EphemeralTempConsume,
                    }) if *place == *src && *transferee == moved_dest
                );
            if opaque_resource || consumed_user_close || transferred_sink_close {
                payloads.insert(dest);
            }
        }
    }
    payloads
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::NeutralizeAuthority;
    use hew_types::BuiltinType;

    const SOURCE_LOCAL: u32 = 10;
    const DEST_LOCAL: u32 = 1;

    fn sink_ty() -> ResolvedTy {
        ResolvedTy::named_builtin("Sink", BuiltinType::Sink, vec![ResolvedTy::String])
    }

    fn result_ty() -> ResolvedTy {
        ResolvedTy::named_builtin(
            "Result",
            BuiltinType::Result,
            vec![sink_ty(), ResolvedTy::String],
        )
    }

    fn source() -> Place {
        Place::EnumVariant {
            local: SOURCE_LOCAL,
            variant_idx: 0,
            field_idx: 0,
        }
    }

    fn block(instructions: Vec<Instr>) -> BasicBlock {
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions,
            terminator: crate::model::Terminator::Return,
        }
    }

    fn admitted(
        neutralization: Instr,
        carrier_ty: ResolvedTy,
        alias_of: &HashMap<u32, u32>,
        proven_destinations: &HashSet<u32>,
    ) -> bool {
        admitted_with_destination_ty(
            neutralization,
            carrier_ty,
            alias_of,
            proven_destinations,
            sink_ty(),
        )
    }

    fn admitted_with_destination_ty(
        neutralization: Instr,
        carrier_ty: ResolvedTy,
        alias_of: &HashMap<u32, u32>,
        proven_destinations: &HashSet<u32>,
        destination_ty: ResolvedTy,
    ) -> bool {
        admitted_instructions(
            vec![
                Instr::Move {
                    dest: Place::Local(DEST_LOCAL),
                    src: source(),
                },
                neutralization,
            ],
            carrier_ty,
            alias_of,
            proven_destinations,
            destination_ty,
        )
    }

    fn admitted_instructions(
        instructions: Vec<Instr>,
        carrier_ty: ResolvedTy,
        alias_of: &HashMap<u32, u32>,
        proven_destinations: &HashSet<u32>,
        destination_ty: ResolvedTy,
    ) -> bool {
        let blocks = vec![block(instructions)];
        let mut local_tys = vec![ResolvedTy::Unit; SOURCE_LOCAL as usize + 1];
        local_tys[DEST_LOCAL as usize] = destination_ty;
        local_tys[SOURCE_LOCAL as usize] = carrier_ty;
        direct_independent_resource_payloads(
            &blocks,
            alias_of,
            &local_tys,
            &hew_hir::TypeClassTable::default(),
            &hew_hir::LifecycleRegistry::default(),
            proven_destinations,
            &|local| local == DEST_LOCAL,
        )
        .contains(&DEST_LOCAL)
    }

    fn move_to_destination() -> Instr {
        Instr::Move {
            dest: Place::Local(DEST_LOCAL),
            src: source(),
        }
    }

    fn neutralize(transferee: Option<Place>, authority: NeutralizeAuthority) -> Instr {
        Instr::NeutralizePayloadSlot {
            place: source(),
            transferee,
            authority,
        }
    }

    fn owned_carrier() -> HashMap<u32, u32> {
        HashMap::from([(SOURCE_LOCAL, SOURCE_LOCAL)])
    }

    fn owning_destination() -> HashSet<u32> {
        HashSet::from([DEST_LOCAL])
    }

    #[test]
    fn exact_sink_payload_handoff_is_admitted() {
        assert!(admitted(
            neutralize(
                Some(Place::Local(DEST_LOCAL)),
                NeutralizeAuthority::EphemeralTempConsume,
            ),
            result_ty(),
            &owned_carrier(),
            &owning_destination(),
        ));
    }

    #[test]
    fn wrong_transferee_cannot_admit_sink_payload() {
        assert!(!admitted(
            neutralize(
                Some(Place::Local(DEST_LOCAL + 1)),
                NeutralizeAuthority::EphemeralTempConsume,
            ),
            result_ty(),
            &owned_carrier(),
            &owning_destination(),
        ));
    }

    #[test]
    fn missing_transferee_cannot_admit_sink_payload() {
        assert!(!admitted(
            neutralize(None, NeutralizeAuthority::EphemeralTempConsume),
            result_ty(),
            &owned_carrier(),
            &owning_destination(),
        ));
    }

    #[test]
    fn wrong_neutralization_authority_cannot_admit_sink_payload() {
        assert!(!admitted(
            neutralize(
                Some(Place::Local(DEST_LOCAL)),
                NeutralizeAuthority::SendTransferLastUse,
            ),
            result_ty(),
            &owned_carrier(),
            &owning_destination(),
        ));
    }

    #[test]
    fn separated_neutralization_cannot_admit_sink_payload() {
        assert!(!admitted_instructions(
            vec![
                move_to_destination(),
                Instr::ConstI64 {
                    dest: Place::Local(2),
                    value: 0,
                },
                neutralize(
                    Some(Place::Local(DEST_LOCAL)),
                    NeutralizeAuthority::EphemeralTempConsume,
                ),
            ],
            result_ty(),
            &owned_carrier(),
            &owning_destination(),
            sink_ty(),
        ));
    }

    #[test]
    fn unproved_destination_cannot_admit_sink_payload() {
        assert!(!admitted(
            neutralize(
                Some(Place::Local(DEST_LOCAL)),
                NeutralizeAuthority::EphemeralTempConsume,
            ),
            result_ty(),
            &owned_carrier(),
            &HashSet::new(),
        ));
    }

    #[test]
    fn non_result_carrier_cannot_admit_sink_payload() {
        let option = ResolvedTy::named_builtin("Option", BuiltinType::Option, vec![sink_ty()]);
        assert!(!admitted(
            neutralize(
                Some(Place::Local(DEST_LOCAL)),
                NeutralizeAuthority::EphemeralTempConsume,
            ),
            option,
            &owned_carrier(),
            &owning_destination(),
        ));
    }

    #[test]
    fn result_with_wrong_payload_type_cannot_admit_sink_payload() {
        let wrong_payload = ResolvedTy::named_builtin(
            "Result",
            BuiltinType::Result,
            vec![ResolvedTy::String, ResolvedTy::String],
        );
        assert!(!admitted(
            neutralize(
                Some(Place::Local(DEST_LOCAL)),
                NeutralizeAuthority::EphemeralTempConsume,
            ),
            wrong_payload,
            &owned_carrier(),
            &owning_destination(),
        ));
    }

    #[test]
    fn unproved_result_carrier_cannot_admit_sink_payload() {
        assert!(!admitted(
            neutralize(
                Some(Place::Local(DEST_LOCAL)),
                NeutralizeAuthority::EphemeralTempConsume,
            ),
            result_ty(),
            &HashMap::new(),
            &owning_destination(),
        ));
    }

    #[test]
    fn other_runtime_resource_payload_cannot_acquire_sink_handoff_policy() {
        let stream =
            ResolvedTy::named_builtin("Stream", BuiltinType::Stream, vec![ResolvedTy::String]);
        assert!(!admitted_with_destination_ty(
            neutralize(
                Some(Place::Local(DEST_LOCAL)),
                NeutralizeAuthority::EphemeralTempConsume,
            ),
            result_ty(),
            &owned_carrier(),
            &owning_destination(),
            stream,
        ));
    }
}
