use super::*;
use encoding_fixture as fixture;

fn physical(semantic: &SemModule) -> PhysicalModule {
    let mut target = super::tests::target_for_inventory(semantic);
    for ty in physical_type_inventory(semantic).types() {
        if encoding_format(ty).is_some() {
            target.insert_layout(
                ty.clone(),
                PhysicalLayout {
                    size: 8,
                    align: 8,
                    repr: PhysicalRepr::Pointer,
                },
            );
        }
    }
    for (ty, repr) in [
        (ResolvedTy::I32, PhysicalRepr::Integer { bits: 32 }),
        (ResolvedTy::U64, PhysicalRepr::Integer { bits: 64 }),
        (ResolvedTy::F64, PhysicalRepr::Float { bits: 64 }),
    ] {
        let bytes = if ty == ResolvedTy::I32 { 4 } else { 8 };
        target.insert_layout(
            ty,
            PhysicalLayout {
                size: bytes,
                align: u32::try_from(bytes).unwrap(),
                repr,
            },
        );
    }
    lower_physical_module(semantic, target)
        .unwrap()
        .into_unverified()
}

#[test]
fn encoding_operations_lower_through_shared_ownership_contracts() {
    for family in hew_types::runtime_call::all_runtime_call_families()
        .into_iter()
        .filter(|family| family.encoding_format().is_some())
    {
        physical(&fixture::operation(family));
    }
    for format in [EncodingFormat::Json, EncodingFormat::Yaml] {
        physical(&fixture::copy(format));
    }
}

#[test]
fn encoding_recipes_reject_bitwise_copy_and_cross_format_glue() {
    for format in [EncodingFormat::Json, EncodingFormat::Yaml] {
        let other = if format == EncodingFormat::Json {
            EncodingFormat::Yaml
        } else {
            EncodingFormat::Json
        };
        for action in [CloneAction::Bitwise, CloneAction::Encoding(other)] {
            let mut module = physical(&fixture::copy(format));
            let PhysicalOp::Clone { action: actual, .. } =
                &mut module.functions[0].blocks[0].ops[0]
            else {
                panic!("copy")
            };
            *actual = action;
            let error = verify_physical_module(&module).unwrap_err();
            assert!(error.message.contains("clone action"), "{error:?}");
        }
        let mut module = physical(&fixture::operation(RuntimeCallFamily::Encoding {
            format,
            op: EncodingOp::Type,
        }));
        let PhysicalOp::Destroy { action, .. } = &mut module.functions[0].blocks[1].ops[0] else {
            panic!("drop")
        };
        *action = DestroyAction::Encoding(other);
        let error = verify_physical_module(&module).unwrap_err();
        assert!(error.message.contains("destroy action"), "{error:?}");
    }
}

#[test]
fn encoding_mutation_rejects_wrong_transfers_missing_receiver_result_and_failure_edges() {
    for op in [EncodingOp::ObjectSet, EncodingOp::ArrayPush] {
        let base = physical(&fixture::operation(RuntimeCallFamily::Encoding {
            format: EncodingFormat::Json,
            op,
        }));
        for mutation in 0..5 {
            let mut module = base.clone();
            let PhysicalTerminator::RuntimeCall {
                action,
                args,
                result,
                normal,
                failure,
            } = &mut module.functions[0].blocks[0].terminator
            else {
                panic!("runtime call")
            };
            match mutation {
                0 => {
                    let ArgumentTransfer::Move(source) = args[0] else {
                        panic!("receiver")
                    };
                    args[0] = ArgumentTransfer::Borrow(source);
                }
                1 => {
                    let last = args.last_mut().unwrap();
                    let ArgumentTransfer::Move(source) = *last else {
                        panic!("child")
                    };
                    *last = ArgumentTransfer::Clone {
                        source,
                        action: CloneAction::Encoding(EncodingFormat::Json),
                    };
                }
                2 => *result = None,
                3 => *failure = Some(normal.clone()),
                4 => {
                    *action = PhysicalRuntimeAction::Encoding {
                        format: EncodingFormat::Yaml,
                        op,
                    }
                }
                _ => unreachable!(),
            }
            assert!(
                verify_physical_module(&module).is_err(),
                "{op:?}, mutation {mutation}"
            );
        }
    }
}
