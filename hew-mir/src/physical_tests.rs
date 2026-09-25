//! Behavioural tests for physical MIR lowering and verification.

mod borrow_fixture {
    include!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../hew-sir/tests/support/borrowed_aggregate.rs"
    ));
}

mod utf8_fixture {
    include!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../hew-sir/tests/support/runtime_utf8.rs"
    ));
}

use hew_hir::{lower_program_host_target, ItemId, ResolutionCtx};
use hew_sir::{
    BoundaryOperand, CallableInstance, CheckedFailure, FunctionSourceOrigin, Operand, Provenance,
    SemBlock, SemCallConv, SemCallable, SemCallableKind, SemSignature, ValueDef,
};
use hew_types::{module_registry::ModuleRegistry, Checker, DefId, SendFact, TypeFacts, ValueClass};

use super::*;

fn exclusive_receiver() -> (PhysicalModule, CallableId) {
    let semantic = lower_source(
        r"
            fn inspect(values: Vec<i64>) -> i64 { values.len() }
            fn main() -> i64 {
                var values: Vec<i64> = Vec.new();
                values.push(3);
                inspect(values)
            }
            ",
    );
    let mut physical = lower_physical_module(&semantic, target_for_inventory(&semantic))
        .unwrap()
        .into_unverified();
    let callee = physical
        .callables
        .iter_mut()
        .find(|callee| physical.defs.path(callee.declaration) == "inspect")
        .unwrap();
    callee.params[0].passing = SemParamPassing::BorrowMut;
    callee.params[0].carrier = ParamCarrier::Indirect;
    let id = callee.id;
    for function in &mut physical.functions {
        for block in &mut function.blocks {
            if let PhysicalTerminator::Call { callee, args, .. } = &mut block.terminator {
                if *callee == id {
                    let ArgumentTransfer::Borrow(source) = args[0] else {
                        panic!("source fixture must borrow its vector");
                    };
                    args[0] = ArgumentTransfer::BorrowMut(source);
                }
            }
        }
    }
    verify_physical_module(&physical).unwrap();
    (physical, id)
}

#[test]
fn exclusive_receiver_rejects_value_carriers_and_shared_call_arguments() {
    let (physical, id) = exclusive_receiver();
    let mut invalid = physical.clone();
    invalid.callables[id.0 as usize].params[0].carrier = ParamCarrier::Direct;
    assert!(verify_physical_module(&invalid)
        .unwrap_err()
        .message
        .contains("caller storage by address"));

    let mut invalid = physical;
    for function in &mut invalid.functions {
        for block in &mut function.blocks {
            if let PhysicalTerminator::Call { callee, args, .. } = &mut block.terminator {
                if *callee == id {
                    let ArgumentTransfer::BorrowMut(source) = args[0] else {
                        unreachable!()
                    };
                    args[0] = ArgumentTransfer::Borrow(source);
                }
            }
        }
    }
    assert!(verify_physical_module(&invalid)
        .unwrap_err()
        .message
        .contains("transfer disagrees with parameter passing"));
}

fn i64_layout() -> PhysicalLayout {
    PhysicalLayout {
        size: 8,
        align: 8,
        repr: PhysicalRepr::Integer { bits: 64 },
    }
}

fn target() -> PhysicalTarget {
    let mut target = PhysicalTarget::new("x86_64-unknown-linux-gnu", "e-p:64:64-i64:64");
    target.insert_layout(ResolvedTy::I64, i64_layout());
    target.insert_layout(ResolvedTy::Duration, i64_layout());
    target.insert_layout(
        ResolvedTy::Bool,
        PhysicalLayout {
            size: 1,
            align: 1,
            repr: PhysicalRepr::Integer { bits: 8 },
        },
    );
    target.insert_layout(
        ResolvedTy::U8,
        PhysicalLayout {
            size: 1,
            align: 1,
            repr: PhysicalRepr::Integer { bits: 8 },
        },
    );
    target.insert_layout(
        ResolvedTy::I8,
        PhysicalLayout {
            size: 1,
            align: 1,
            repr: PhysicalRepr::Integer { bits: 8 },
        },
    );
    target.insert_layout(
        ResolvedTy::U64,
        PhysicalLayout {
            size: 8,
            align: 8,
            repr: PhysicalRepr::Integer { bits: 64 },
        },
    );
    target.insert_layout(
        ResolvedTy::String,
        PhysicalLayout {
            size: 8,
            align: 8,
            repr: PhysicalRepr::Pointer,
        },
    );
    target.insert_layout(
        ResolvedTy::Bytes,
        PhysicalLayout {
            size: 16,
            align: 8,
            repr: PhysicalRepr::Struct(vec![
                PhysicalLayout {
                    size: 8,
                    align: 8,
                    repr: PhysicalRepr::Pointer,
                },
                PhysicalLayout {
                    size: 4,
                    align: 4,
                    repr: PhysicalRepr::Integer { bits: 32 },
                },
                PhysicalLayout {
                    size: 4,
                    align: 4,
                    repr: PhysicalRepr::Integer { bits: 32 },
                },
            ]),
        },
    );
    target.insert_layout(
        ResolvedTy::Unit,
        PhysicalLayout {
            size: 0,
            align: 1,
            repr: PhysicalRepr::Unit,
        },
    );
    target
}

fn utf8_target(module: &SemModule) -> PhysicalTarget {
    // This fixture uses the existing fixed 64-bit test target. Its two
    // variants have an eight-byte-aligned payload following a one-byte tag.
    fn variant(target: &mut PhysicalTarget, ty: ResolvedTy, cases: Vec<PhysicalLayout>) {
        let payload_size = cases.iter().map(|case| case.size).max().unwrap();
        let payload = PhysicalLayout {
            size: payload_size,
            align: 8,
            repr: PhysicalRepr::Array {
                element: Box::new(i64_layout()),
                len: u32::try_from(payload_size / 8).unwrap(),
            },
        };
        let object = PhysicalLayout {
            size: 8 + payload.size,
            align: 8,
            repr: PhysicalRepr::Struct(vec![
                PhysicalLayout {
                    size: 1,
                    align: 1,
                    repr: PhysicalRepr::Integer { bits: 8 },
                },
                payload,
            ]),
        };
        target.insert_layout(ty.clone(), object.clone());
        target.insert_variant_layout(PhysicalVariantLayout {
            ty,
            is_indirect: false,
            object,
            variants: cases,
        });
    }
    let mut target = target();
    let option_ty = module.variant_shapes[1].enum_ty.clone();
    variant(
        &mut target,
        option_ty.clone(),
        vec![
            PhysicalLayout {
                size: 8,
                align: 8,
                repr: PhysicalRepr::Struct(vec![i64_layout()]),
            },
            PhysicalLayout {
                size: 0,
                align: 1,
                repr: PhysicalRepr::Struct(vec![]),
            },
        ],
    );
    let error_ty = module.aggregate_shapes[0].aggregate_ty.clone();
    let error_layout = PhysicalLayout {
        size: 24,
        align: 8,
        repr: PhysicalRepr::Struct(vec![
            i64_layout(),
            target.layout(&option_ty).unwrap().clone(),
        ]),
    };
    target.insert_layout(error_ty, error_layout.clone());
    let string = target.layout(&ResolvedTy::String).unwrap().clone();
    variant(
        &mut target,
        module.variant_shapes[0].enum_ty.clone(),
        vec![
            PhysicalLayout {
                size: 8,
                align: 8,
                repr: PhysicalRepr::Struct(vec![string]),
            },
            PhysicalLayout {
                size: 24,
                align: 8,
                repr: PhysicalRepr::Struct(vec![error_layout]),
            },
        ],
    );
    target
}

#[test]
fn utf8_decode_rejects_a_foreign_optional_payload_descriptor() {
    let semantic = utf8_fixture::decode_module();
    let mut physical = lower_physical_module(&semantic, utf8_target(&semantic))
        .expect("valid UTF-8 runtime result must lower")
        .into_unverified();
    let PhysicalTerminator::RuntimeCall {
        action:
            PhysicalRuntimeAction {
                carrier:
                    PhysicalRuntimeCarrier::Utf8Decode {
                        result, error_len, ..
                    },
                ..
            },
        failure,
        ..
    } = &mut physical.functions[0].blocks[0].terminator
    else {
        panic!("expected the typed decoder runtime action");
    };
    assert!(
        failure.is_none(),
        "invalid UTF-8 is not a native fault edge"
    );
    *error_len = *result;
    let error = verify_physical_module(&physical)
        .expect_err("Result storage cannot stand in for Option<i64>");
    assert!(
        error.message.contains("UTF-8 decode physical descriptors"),
        "{error:?}"
    );
}

fn test_struct_layout(fields: Vec<PhysicalLayout>) -> PhysicalLayout {
    let align = fields.iter().map(|field| field.align).max().unwrap_or(1);
    let mut size = 0_u64;
    for field in &fields {
        size = size.next_multiple_of(u64::from(field.align)) + field.size;
    }
    PhysicalLayout {
        size: size.next_multiple_of(u64::from(align)),
        align,
        repr: PhysicalRepr::Struct(fields),
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "the fixed test target realizes resource and composite layouts from one inventory"
)]
pub(super) fn target_for_inventory(module: &SemModule) -> PhysicalTarget {
    let inventory = physical_type_inventory(module);
    let mut target = target();
    // Source fixtures retain checked callable headers as well as bodies.
    // Realize their resource carriers from the same inventory as codegen.
    for resource in inventory.resources() {
        let layout = match resource.release.carrier().unwrap() {
            ResourceCarrier::Pointer => PhysicalLayout {
                size: 8,
                align: 8,
                repr: PhysicalRepr::Pointer,
            },
            ResourceCarrier::I32 => PhysicalLayout {
                size: 4,
                align: 4,
                repr: PhysicalRepr::Integer { bits: 32 },
            },
            ResourceCarrier::Record => continue,
        };
        target.insert_layout(resource.ty.clone(), layout);
    }
    for ty in inventory
        .types()
        .filter(|ty| collection_type_arguments(ty).is_some())
    {
        target.insert_layout(
            ty.clone(),
            PhysicalLayout {
                size: 8,
                align: 8,
                repr: PhysicalRepr::Pointer,
            },
        );
    }
    for variant in inventory.variants().filter(|variant| variant.is_indirect) {
        target.insert_layout(
            variant.ty.clone(),
            PhysicalLayout {
                size: 8,
                align: 8,
                repr: PhysicalRepr::Pointer,
            },
        );
    }
    let mut aggregates = inventory.aggregates().collect::<Vec<_>>();
    let mut variants = inventory.variants().collect::<Vec<_>>();
    while !aggregates.is_empty() || !variants.is_empty() {
        let previous = aggregates.len() + variants.len();
        aggregates.retain(|aggregate| {
            let Some(fields) = aggregate
                .fields
                .iter()
                .map(|field| target.layout(field).cloned())
                .collect::<Option<Vec<_>>>()
            else {
                return true;
            };
            target.insert_layout(aggregate.ty.clone(), test_struct_layout(fields));
            false
        });
        variants.retain(|variant| {
            let Some(cases) = variant
                .variants
                .iter()
                .map(|fields| {
                    fields
                        .iter()
                        .map(|field| target.layout(field).cloned())
                        .collect::<Option<Vec<_>>>()
                        .map(test_struct_layout)
                })
                .collect::<Option<Vec<_>>>()
            else {
                return true;
            };
            let align = cases.iter().map(|case| case.align).max().unwrap();
            let size = cases.iter().map(|case| case.size).max().unwrap();
            let count = size.div_ceil(u64::from(align));
            let payload = PhysicalLayout {
                size: count * u64::from(align),
                align,
                repr: PhysicalRepr::Array {
                    element: Box::new(PhysicalLayout {
                        size: u64::from(align),
                        align,
                        repr: PhysicalRepr::Integer {
                            bits: u16::try_from(align * 8).unwrap(),
                        },
                    }),
                    len: u32::try_from(count).unwrap(),
                },
            };
            let object = test_struct_layout(vec![
                PhysicalLayout {
                    size: 1,
                    align: 1,
                    repr: PhysicalRepr::Integer { bits: 8 },
                },
                payload,
            ]);
            if !variant.is_indirect {
                target.insert_layout(variant.ty.clone(), object.clone());
            }
            target.insert_variant_layout(PhysicalVariantLayout {
                ty: variant.ty.clone(),
                is_indirect: variant.is_indirect,
                object,
                variants: cases,
            });
            false
        });
        assert!(
            aggregates.len() + variants.len() < previous,
            "test layouts must terminate through concrete fields or vector pointers"
        );
    }
    target
}

fn lower_source(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR errors: {:#?}",
        hir.diagnostics
    );
    let lowered = hew_sir::lower_module(&hir.module, &facts);
    assert!(
        lowered.statuses.iter().any(|status| {
            status.name == "main" && matches!(status.status, hew_sir::SirLoweringStatus::Lowered)
        }),
        "source main did not lower: {:#?}",
        lowered.statuses
    );
    assert!(
        !lowered.module.functions.is_empty(),
        "source fixture must exercise a lowered function"
    );
    lowered.module
}

#[test]
fn extern_byte_result_rejects_storage_aggregate_return() {
    let semantic = borrow_fixture::lower_source(
        r#"
            extern "C" { fn make_bytes() -> bytes; }
            fn main() { println(unsafe { make_bytes() }.len()); }
            "#,
    );
    let mut physical = lower_physical_module(&semantic, target_for_inventory(&semantic))
        .expect("declared byte result must have a C return ABI")
        .into_unverified();
    let call = physical
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .find_map(|block| match &mut block.terminator {
            PhysicalTerminator::ExternCall { result_abi, .. } => Some(result_abi),
            _ => None,
        })
        .expect("source must reach its declared byte producer");
    *call = PhysicalExternResultAbi::Direct;
    let error = verify_physical_module(&physical)
        .expect_err("a byte storage aggregate is not the C return ABI");
    assert!(error.message.contains("extern result ABI"), "{error:?}");
}

fn module_with_return() -> SemModule {
    let declaration = DefId::for_test("main");
    let callable = SemCallable {
        id: CallableId(0),
        function: ItemId(0),
        declaration,
        instance: CallableInstance::Monomorphic,
        symbol: "main".to_string(),
        source_origin: FunctionSourceOrigin::RootUnit,
        signature: SemSignature {
            params: vec![],
            return_ty: ResolvedTy::I64,
        },
        call_conv: SemCallConv::Default,
        kind: SemCallableKind::HewDirect,
    };
    let value = ValueDef {
        id: ValueId(0),
        ty: ResolvedTy::I64,
        own: OwnKind::None,
    };
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration,
        name: "main".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::RootUnit,
        terminal_receiver: None,
        params: vec![],
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(0),
            args: vec![],
            ops: vec![SemOp {
                id: hew_sir::OpId(0),
                results: vec![value],
                kind: SemOpKind::ConstInteger(7),
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(BoundaryOperand {
                    operand: Operand { value: ValueId(0) },
                    decision: BoundaryDecision::Move,
                }),
            },
        }],
        places: vec![],
        bindings: vec![],
    };
    let mut type_facts = BTreeMap::new();
    type_facts.insert(
        TypeInstanceKey(ResolvedTy::I64),
        TypeFacts {
            class: ValueClass::BitCopy,
            clone: CloneKind::Bits,
            send: SendFact::Known(true),
            hash: true,
            eq: true,
        },
    );
    SemModule {
        defs: hew_types::DefTable::fixture(),
        structural_display: BTreeMap::new(),
        debug: hew_sir::SemDebugFacts::default(),
        regex_patterns: Vec::new(),
        actors: Vec::new(),
        supervisors: Vec::new(),
        resources: BTreeMap::new(),
        closures: Vec::new(),
        vtables: Vec::new(),
        value_capabilities: BTreeMap::new(),
        callables: vec![callable],
        generic_templates: vec![],
        root_unit_callables: vec![CallableId(0)],
        entry_exit_plan: None,
        entry_callable: Some(CallableId(0)),
        functions: vec![function],
        aggregate_shapes: vec![],
        variant_shapes: vec![],
        type_facts,
        string_literals: BTreeMap::new(),
        bytes_literals: BTreeMap::new(),
    }
}

fn module_with_call_and_unwind() -> SemModule {
    let mut module = module_with_return();
    let helper_declaration = DefId::for_test("helper");
    let mut helper = module.functions[0].clone();
    helper.id = ItemId(1);
    helper.callable = CallableId(1);
    helper.declaration.clone_from(&helper_declaration);
    helper.name = "helper".to_string();
    helper.source_origin = FunctionSourceOrigin::Unknown;
    module.callables.push(SemCallable {
        id: CallableId(1),
        function: ItemId(1),
        declaration: helper_declaration,
        instance: CallableInstance::Monomorphic,
        symbol: "helper".to_string(),
        source_origin: FunctionSourceOrigin::Unknown,
        signature: SemSignature {
            params: vec![],
            return_ty: ResolvedTy::I64,
        },
        call_conv: SemCallConv::Default,
        kind: SemCallableKind::HewDirect,
    });
    let main = &mut module.functions[0];
    main.blocks = vec![
        SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(0),
            args: vec![],
            ops: vec![],
            terminator: SemTerminator::Call {
                id: hew_sir::OpId(0),
                callee: CallableId(1),
                args: vec![],
                result: CallResult::Value(ValueDef {
                    id: ValueId(0),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }),
                normal: Some(Edge {
                    target: BlockId(1),
                    args: vec![Operand { value: ValueId(0) }],
                }),
                unwind: CallUnwind::Cleanup(Edge {
                    target: BlockId(2),
                    args: vec![],
                }),
                handback: None,
            },
        },
        SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(1),
            args: vec![hew_sir::BlockArg {
                value: ValueId(1),
                ty: ResolvedTy::I64,
                own: OwnKind::None,
            }],
            ops: vec![],
            terminator: SemTerminator::Return {
                value: Some(BoundaryOperand {
                    operand: Operand { value: ValueId(1) },
                    decision: BoundaryDecision::Move,
                }),
            },
        },
        SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(2),
            args: vec![],
            ops: vec![],
            terminator: SemTerminator::ResumeUnwind { handback: None },
        },
    ];
    module.functions.push(helper);
    module
}

fn module_with_checked_add() -> SemModule {
    let mut module = module_with_return();
    module.functions[0].blocks = vec![
        SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(0),
            args: vec![],
            ops: vec![
                SemOp {
                    id: hew_sir::OpId(0),
                    results: vec![ValueDef {
                        id: ValueId(0),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    kind: SemOpKind::ConstInteger(40),
                    provenance: Provenance::Synthesized,
                },
                SemOp {
                    id: hew_sir::OpId(1),
                    results: vec![ValueDef {
                        id: ValueId(1),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    kind: SemOpKind::ConstInteger(2),
                    provenance: Provenance::Synthesized,
                },
            ],
            terminator: SemTerminator::CheckedBinary {
                id: hew_sir::OpId(2),
                op: BinaryOp::Add,
                lhs: Operand { value: ValueId(0) },
                rhs: Operand { value: ValueId(1) },
                result: ValueDef {
                    id: ValueId(2),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                },
                normal: Edge {
                    target: BlockId(1),
                    args: vec![Operand { value: ValueId(2) }],
                },
                failures: vec![CheckedFailure {
                    kind: TrapKind::IntegerOverflow,
                    edge: Edge {
                        target: BlockId(2),
                        args: vec![],
                    },
                }],
            },
        },
        SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(1),
            args: vec![hew_sir::BlockArg {
                value: ValueId(3),
                ty: ResolvedTy::I64,
                own: OwnKind::None,
            }],
            ops: vec![],
            terminator: SemTerminator::Return {
                value: Some(BoundaryOperand {
                    operand: Operand { value: ValueId(3) },
                    decision: BoundaryDecision::Move,
                }),
            },
        },
        SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(2),
            args: vec![],
            ops: vec![],
            terminator: SemTerminator::Trap {
                kind: TrapKind::IntegerOverflow,
            },
        },
    ];
    module
}

#[test]
fn wire_schema_cannot_read_another_physical_field() {
    let semantic = lower_source(
        r#"
            #[wire]
            type WireRecordProbe { label: string @7, code: u8 @2 }
            fn main() {
                let message = WireRecordProbe { label: "owned", code: 7 };
                let encoded = message.encode();
                let decoded = WireRecordProbe.decode(encoded);
                println(decoded.label);
            }
        "#,
    );
    let mut physical = lower_physical_module(&semantic, target_for_inventory(&semantic))
        .unwrap()
        .into_unverified();
    let mut changed = false;
    for block in physical
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
    {
        if let PhysicalTerminator::WireCodec { plan, .. } = &mut block.terminator {
            let plan = std::sync::Arc::make_mut(plan);
            let SemWireKind::Record { fields, .. } = &mut plan.kind else {
                panic!("record codec");
            };
            fields[0].index = 0;
            changed = true;
            break;
        }
    }
    assert!(changed);
    assert!(verify_physical_module(&physical)
        .unwrap_err()
        .message
        .contains("wire schema selects a different physical value shape"));
}

#[test]
fn lowers_scalar_return_to_private_result_out_contract() {
    let physical = lower_physical_module(&module_with_return(), target()).expect("lower");
    let module = physical.module();
    assert_eq!(module.entry_callable, Some(CallableId(0)));
    assert_eq!(module.callables[0].return_layout, Some(i64_layout()));
    assert!(matches!(
        module.functions[0].blocks[0].terminator,
        PhysicalTerminator::Return {
            value: Some(ReturnTransfer::Move(StorageId(0)))
        }
    ));
}

#[test]
fn inventories_only_types_used_by_concrete_sir_bodies() {
    let module = lower_source(
        r"
            fn pair_second(x: i64, y: i64) -> i64 {
                let pair = (x, y);
                pair.1
            }

            fn main() -> i64 { pair_second(0, 42) }
            ",
    );
    let inventory = physical_type_inventory(&module);
    assert!(inventory.contains(&ResolvedTy::I64));
    assert!(inventory.contains(&ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64,])));
    // Negative control: the inventory is demand-driven, not a dump of every
    // registered type. Both are registered and no body this module admits
    // mentions either.
    assert!(!inventory.contains(&ResolvedTy::String));
    assert!(!inventory.contains(&ResolvedTy::F64));
}

#[test]
fn lowers_scalar_tuple_construction_and_projection_to_explicit_ops() {
    let module = lower_source(
        r"
            fn main() -> i64 {
                let pair = (0, 42);
                pair.1
            }
            ",
    );
    let verified =
        lower_physical_module(&module, target_for_inventory(&module)).expect("physical tuple");
    let operations = verified
        .module()
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.ops)
        .collect::<Vec<_>>();
    assert!(operations.iter().any(|operation| matches!(
        operation,
        PhysicalOp::TupleMake { elements, .. } if elements.len() == 2
    )));
    assert!(operations
        .iter()
        .any(|operation| matches!(operation, PhysicalOp::TupleGet { index: 1, .. })));
}

#[test]
fn lowers_owned_aggregate_operations_with_exact_recursive_glue() {
    let module = lower_source(
        r#"
            type Packet { label: string, payload: bytes }

            fn main() {
                let pair = ("tuple", b"T");
                let pair_copy = pair;
                let tuple_label = pair_copy.0;
                let packet = Packet { payload: b"P", label: "record" };
                let packet_copy = packet;
                let record_label = packet_copy.label;
            }
            "#,
    );
    let verified = lower_physical_module(&module, target_for_inventory(&module))
        .expect("owned aggregate physical lowering");
    let physical = verified.module();
    // Select the shapes this source demands by their recipe rather than
    // counting every row.
    let demanded: Vec<_> = physical
        .aggregate_glue
        .iter()
        .filter(|glue| {
            glue.fields.len() == 2
                && matches!(glue.fields[0].clone, Some(CloneAction::StringRetain))
        })
        .collect();
    // The tuple `("tuple", b"T")` and the record `Packet` each publish one.
    assert_eq!(demanded.len(), 2);
    assert!(demanded.iter().all(|glue| {
        matches!(glue.fields[1].clone, Some(CloneAction::BytesRetain))
            && matches!(glue.fields[0].destroy, Some(DestroyAction::StringRelease))
            && matches!(glue.fields[1].destroy, Some(DestroyAction::BytesRelease))
    }));
    let operations = physical
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.ops)
        .collect::<Vec<_>>();
    assert!(operations
        .iter()
        .any(|operation| matches!(operation, PhysicalOp::AggregateMake { .. })));
    assert!(operations.iter().any(|operation| matches!(
        operation,
        PhysicalOp::Clone {
            action: CloneAction::Aggregate(_),
            ..
        }
    )));
    let projected_copies = physical
        .functions
        .iter()
        .flat_map(|function| {
            function
                .blocks
                .iter()
                .flat_map(|block| &block.ops)
                .filter_map(|operation| {
                    let PhysicalOp::Clone {
                        source,
                        action: CloneAction::StringRetain,
                        ..
                    } = operation
                    else {
                        return None;
                    };
                    function.place_storage.get(source)
                })
        })
        .collect::<Vec<_>>();
    assert_eq!(projected_copies.len(), 2);
    assert!(projected_copies
        .iter()
        .all(|projection| { projection.path.len() == 1 && projection.path[0].field == 0 }));
    assert!(operations.iter().any(|operation| matches!(
        operation,
        PhysicalOp::StorageDead {
            destroy: Some(DestroyAction::Aggregate(_)),
            ..
        }
    )));
}

fn borrowed_aggregate_fixture() -> PhysicalModule {
    let semantic = borrow_fixture::nested_borrow_module();
    lower_physical_module(&semantic, target_for_inventory(&semantic))
        .expect("nested field loans lower through physical storage")
        .into_unverified()
}

#[test]
fn borrowed_aggregate_fields_retain_exact_sir_parent_dependencies() {
    let physical = borrowed_aggregate_fixture();
    let function = &physical.functions[0];
    let mut loans = 0;
    for operation in function.blocks.iter().flat_map(|block| &block.ops) {
        if let PhysicalOp::AggregateProjectBorrow {
            dest, aggregate, ..
        } = operation
        {
            loans += 1;
            let slot = &function.storage[dest.0 as usize];
            assert_eq!(slot.own, OwnKind::Guaranteed);
            assert_eq!(slot.borrow_parent, Some(*aggregate));
        }
    }
    assert_eq!(loans, 2);
}

#[test]
fn projected_guard_loans_protect_fields_and_ancestors_but_not_siblings() {
    let module = lower_source(
        r"
            #[resource] type Ticket { id: i64 }
            impl Ticket { fn close(consume self) {} }
            type Pair { first: Ticket, second: Ticket }
            fn main() {
                var pair = Pair { first: Ticket { id: 1 }, second: Ticket { id: 2 } };
                match pair {
                    Pair { first: ticket, .. } if { pair.second = Ticket { id: 3 }; true } => ticket.close(),
                    _ => {},
                }
            }
            ",
    );
    let physical = lower_physical_module(&module, target_for_inventory(&module))
        .expect("disjoint projected guard mutation");
    let main = module
        .functions
        .iter()
        .find(|function| function.name.ends_with("main"))
        .unwrap()
        .callable;
    let mut function = physical
        .module()
        .functions
        .iter()
        .find(|function| function.callable == main)
        .unwrap()
        .clone();
    let (loan, field) = function
        .storage
        .iter()
        .find_map(|slot| {
            let parent = slot.borrow_parent?;
            let projection = function.place_storage.get(&parent)?;
            (projection.path.len() == 1 && projection.path[0].field == 0)
                .then_some((slot.id, parent))
        })
        .expect("the pattern loans the first field");
    let root = function.place_storage[&field].root;
    let sibling = *function
        .place_storage
        .iter()
        .find(|(_, projection)| {
            projection.root == root && projection.path.len() == 1 && projection.path[0].field == 1
        })
        .unwrap()
        .0;
    let mut state = FlowState {
        slots: vec![InitState::Uninitialized; function.storage.len()],
        active: vec![InitState::Initialized; function.storage.len()],
        fault: FaultState::None,
        exit: defer::ORDINARY,
        defers: defer::State::default(),
    };
    state.slots[loan.0 as usize] = InitState::Initialized;
    let borrows = BorrowDependents::of(&function);
    assert!(require_no_live_borrows(&function, &borrows, &state, sibling).is_ok());
    assert!(require_no_live_borrows(&function, &borrows, &state, field).is_err());
    assert!(require_no_live_borrows(&function, &borrows, &state, root).is_err());
    function.storage[loan.0 as usize].borrow_parent = Some(root);
    assert!(
        require_no_live_borrows(&function, &BorrowDependents::of(&function), &state, sibling)
            .is_err()
    );
}

#[test]
fn physical_field_loans_refuse_wrong_fields_and_forged_dependencies() {
    for wrong_field in [false, true] {
        let mut physical = borrowed_aggregate_fixture();
        let function = &mut physical.functions[0];
        let operation = function
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find(|op| matches!(op, PhysicalOp::AggregateProjectBorrow { .. }))
            .unwrap();
        let PhysicalOp::AggregateProjectBorrow { dest, field, .. } = operation else {
            unreachable!()
        };
        if wrong_field {
            *field = u32::MAX;
        } else {
            function.storage[dest.0 as usize].borrow_parent = None;
        }
        let error = verify_physical_module(&physical).expect_err("malformed field loan");
        assert!(
            error.message.contains("out of bounds")
                || error.message.contains("no SIR parent dependency"),
            "{error}"
        );
    }
}

#[test]
fn physical_borrow_roots_cannot_end_before_their_dependent_reads() {
    for end_owner in [false, true] {
        let mut physical = borrowed_aggregate_fixture();
        let function = &mut physical.functions[0];
        let loans = function
            .storage
            .iter()
            .filter(|slot| slot.borrow_parent.is_some())
            .map(|slot| (slot.id, slot.borrow_parent.unwrap()))
            .collect::<Vec<_>>();
        let (parent, owner) = loans[0];
        let value = if end_owner { owner } else { parent };
        let mut cleanup = None;
        for block in &mut function.blocks {
            if let Some(index) = block.ops.iter().position(|op| {
                matches!(op,
                    PhysicalOp::Destroy { source, .. } | PhysicalOp::EndBorrow { source }
                        if *source == value)
            }) {
                cleanup = Some(block.ops.remove(index));
                break;
            }
        }
        let read = vector_block(function, VecValueOp::Index);
        read.ops.push(cleanup.unwrap());
        let error = verify_physical_module(&physical).expect_err("live dependent loan");
        assert!(error.message.contains("dependent loan is live"), "{error}");
    }
}

#[test]
fn physical_fault_cleanup_must_end_its_field_loans() {
    let mut physical = borrowed_aggregate_fixture();
    let fault = physical.functions[0]
        .blocks
        .iter_mut()
        .find(|block| {
            matches!(
                block.terminator,
                PhysicalTerminator::CheckedRaiseFault { .. }
            )
        })
        .unwrap();
    let before = fault.ops.len();
    fault
        .ops
        .retain(|op| !matches!(op, PhysicalOp::EndBorrow { .. }));
    assert!(fault.ops.len() < before);
    let error = verify_physical_module(&physical).expect_err("loan cleanup on fault edge");
    assert!(error.message.contains("dependent loan is live"), "{error}");
}

fn borrowed_variant_fixture() -> PhysicalModule {
    let semantic = lower_source(
        r#"
            enum Choice { Values(Vec<string>, i64), Empty }

            fn drive(consume choice: Option<Choice>) -> string {
                match choice {
                    .Some(.Values(_, 0)) => "zero",
                    .Some(.Values(values, weight)) => { let _kept = values; "kept" }
                    .Some(.Empty) => "empty",
                    .None => "none",
                }
            }

            fn keep_text(value: string) {}
            fn main() {
                keep_text(drive(.Some(Choice.Values(["word"], 7))));
            }
            "#,
    );
    lower_physical_module(&semantic, target_for_inventory(&semantic))
        .expect("probed variant payloads lower through physical storage")
        .into_unverified()
}

#[test]
fn borrowed_variant_payloads_retain_their_enum_dependency() {
    let physical = borrowed_variant_fixture();
    let mut loans = 0;
    for function in &physical.functions {
        for operation in function.blocks.iter().flat_map(|block| &block.ops) {
            if let PhysicalOp::VariantProjectBorrow { dest, source, .. } = operation {
                loans += 1;
                let slot = &function.storage[dest.0 as usize];
                assert_eq!(slot.own, OwnKind::Guaranteed);
                assert_eq!(slot.borrow_parent, Some(*source));
            }
        }
    }
    assert_eq!(
        loans, 2,
        "each candidate borrows the generator payload once"
    );
    verify_physical_module(&physical).expect("probed payload loans verify");
}

#[test]
fn physical_variant_loans_refuse_forged_dependencies_and_fields() {
    for wrong_field in [false, true] {
        let mut physical = borrowed_variant_fixture();
        let function = physical
            .functions
            .iter_mut()
            .find(|function| {
                function
                    .blocks
                    .iter()
                    .flat_map(|block| &block.ops)
                    .any(|op| matches!(op, PhysicalOp::VariantProjectBorrow { .. }))
            })
            .unwrap();
        let operation = function
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find(|op| matches!(op, PhysicalOp::VariantProjectBorrow { .. }))
            .unwrap();
        let PhysicalOp::VariantProjectBorrow { dest, field, .. } = operation else {
            unreachable!()
        };
        if wrong_field {
            *field = u32::MAX;
        } else {
            function.storage[dest.0 as usize].borrow_parent = None;
        }
        let error = verify_physical_module(&physical).expect_err("malformed payload loan");
        assert!(
            error.message.contains("out of bounds")
                || error.message.contains("no SIR parent dependency"),
            "{error}"
        );
    }
}

#[test]
fn verifier_rejects_variant_payload_carriers_that_cannot_hold_every_case() {
    let module = lower_source(
        r#"
            enum Payload { Wide(i64, string), Empty }

            fn main() -> i64 {
                let payload = Payload.Wide(7, "wide");
                match payload {
                    .Wide(number, text) => { let copy = text; number },
                    .Empty => 0,
                }
            }
            "#,
    );
    let enum_ty = module.variant_shapes[0].enum_ty.clone();
    let wide = PhysicalLayout {
        size: 16,
        align: 8,
        repr: PhysicalRepr::Struct(vec![
            i64_layout(),
            target().layout(&ResolvedTy::String).unwrap().clone(),
        ]),
    };
    let empty = PhysicalLayout {
        size: 0,
        align: 1,
        repr: PhysicalRepr::Struct(vec![]),
    };
    let target_with_carrier = |carrier: PhysicalLayout| {
        let mut target = target_for_inventory(&module);
        let object = PhysicalLayout {
            size: 24,
            align: 8,
            repr: PhysicalRepr::Struct(vec![
                PhysicalLayout {
                    size: 1,
                    align: 1,
                    repr: PhysicalRepr::Integer { bits: 8 },
                },
                carrier,
            ]),
        };
        target.insert_layout(enum_ty.clone(), object.clone());
        target.insert_variant_layout(PhysicalVariantLayout {
            ty: enum_ty.clone(),
            is_indirect: false,
            object,
            variants: vec![wide.clone(), empty.clone()],
        });
        target
    };

    let short = target_with_carrier(PhysicalLayout {
        size: 8,
        align: 8,
        repr: PhysicalRepr::Array {
            element: Box::new(i64_layout()),
            len: 1,
        },
    });
    let short_error = lower_physical_module(&module, short)
        .expect_err("variant payload carrier must fit its widest case");
    assert!(short_error.message.contains("payload carrier"));

    let under_aligned = target_with_carrier(PhysicalLayout {
        size: 16,
        align: 1,
        repr: PhysicalRepr::Array {
            element: Box::new(PhysicalLayout {
                size: 1,
                align: 1,
                repr: PhysicalRepr::Integer { bits: 8 },
            }),
            len: 16,
        },
    });
    let alignment_error = lower_physical_module(&module, under_aligned)
        .expect_err("variant payload carrier must meet every case alignment");
    assert!(alignment_error.message.contains("payload carrier"));
}

#[test]
fn verifier_refuses_malformed_aggregate_copy_and_consumption() {
    let module = lower_source(
        r#"
            type Packet { first: string, second: string }
            fn main() {
                let packet = Packet { first: "one", second: "two" };
                let label = packet.first;
            }
            "#,
    );
    let mut physical = lower_physical_module(&module, target_for_inventory(&module))
        .expect("valid aggregate physical lowering")
        .into_unverified();
    let mut duplicate_consume = physical.clone();
    let fields = duplicate_consume
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .flat_map(|block| &mut block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::AggregateMake { fields, .. } => Some(fields),
            _ => None,
        })
        .expect("aggregate construction");
    fields[1] = fields[0];
    let error = verify_physical_module(&duplicate_consume)
        .expect_err("aggregate construction must not consume one owner twice");
    assert!(error.message.contains("more than once"));

    let mut bad_path = physical.clone();
    let function = &mut physical.functions[0];
    let projected_sources = function
        .place_storage
        .keys()
        .copied()
        .collect::<BTreeSet<_>>();
    let (source, action) = function
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::Clone { source, action, .. } if projected_sources.contains(source) => {
                Some((*source, action))
            }
            _ => None,
        })
        .expect("clone from an aggregate field alias");
    assert_eq!(*action, CloneAction::StringRetain);
    assert_eq!(function.place_storage[&source].path[0].field, 0);
    *action = CloneAction::BytesRetain;
    let error = verify_physical_module(&physical)
        .expect_err("projected clone must use its exact field recipe");
    assert!(error.message.contains("physical clone action"), "{error:?}");

    bad_path.functions[0]
        .place_storage
        .get_mut(&source)
        .unwrap()
        .path[0]
        .field = u32::MAX;
    let error = verify_physical_module(&bad_path)
        .expect_err("projected clone must address a declared aggregate field");
    assert!(error.message.contains("aggregate"), "{error:?}");
}

#[test]
fn verifier_rejects_owned_or_out_of_bounds_tuple_operations() {
    let module = lower_source(
        r"
            fn main() -> i64 {
                let pair = (0, 42);
                pair.1
            }
            ",
    );

    let mut owned_tuple = lower_physical_module(&module, target_for_inventory(&module))
        .expect("valid physical tuple")
        .into_unverified();
    let tuple_dest = owned_tuple.functions[0]
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::TupleMake { dest, .. } => Some(*dest),
            _ => None,
        })
        .expect("tuple construction");
    owned_tuple.functions[0].storage[tuple_dest.0 as usize].own = OwnKind::Owned;
    let error = verify_physical_module(&owned_tuple)
        .expect_err("physical tuple must not infer aggregate ownership");
    assert!(error.message.contains("limited to no-drop values"));

    let mut bad_index = lower_physical_module(&module, target_for_inventory(&module))
        .expect("valid physical tuple")
        .into_unverified();
    let index = bad_index.functions[0]
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::TupleGet { index, .. } => Some(index),
            _ => None,
        })
        .expect("tuple projection");
    *index = 2;
    let error = verify_physical_module(&bad_index)
        .expect_err("physical tuple projection must stay in bounds");
    assert!(error.message.contains("index 2 is out of bounds"));
}

#[test]
fn refuses_a_missing_target_layout_before_codegen() {
    let error = lower_physical_module(
        &module_with_return(),
        PhysicalTarget::new("x86_64-unknown-linux-gnu", "e-p:64:64"),
    )
    .expect_err("layout must be required");
    assert!(error.message.contains("no concrete layout for `i64`"));
}

#[test]
fn runtime_families_lower_to_closed_physical_actions() {
    let module = lower_source(
        r#"
            fn main() -> i64 {
                let upper = "core".to_upper();
                if upper != "CORE" { return 1; }
                if !upper.starts_with("CO") { return 2; }
                if upper.is_empty() { return 3; }
                println(upper);
                0
            }
            "#,
    );
    let verified = lower_physical_module(&module, target_for_inventory(&module))
        .expect("physical runtime calls");
    let actions = verified
        .module()
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .filter_map(|block| match block.terminator {
            PhysicalTerminator::RuntimeCall { action, .. } => Some(action.family),
            _ => None,
        })
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(
        actions,
        std::collections::HashSet::from([
            RuntimeCallFamily::StringEquals,
            RuntimeCallFamily::StringStartsWith,
            RuntimeCallFamily::StringIsEmpty,
            RuntimeCallFamily::StringToUppercase,
            RuntimeCallFamily::Print {
                kind: hew_types::runtime_call::PrintKind::Str,
                newline: true
            },
        ])
    );
}

#[test]
fn scalar_print_lowers_to_the_exact_physical_runtime_action() {
    let module = lower_source("fn main() { println(1 + 2); }");
    let verified = lower_physical_module(&module, target_for_inventory(&module))
        .expect("physical scalar print");
    assert!(verified
        .module()
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .any(|block| matches!(
            block.terminator,
            PhysicalTerminator::RuntimeCall {
                action: PhysicalRuntimeAction {
                    family: RuntimeCallFamily::Print {
                        kind: hew_types::runtime_call::PrintKind::I64,
                        newline: true
                    },
                    ..
                },
                ..
            }
        )));
}

#[test]
fn bytes_transform_and_bounds_failure_are_physical_contracts() {
    let module = lower_source(include_str!(
        "../../tests/core-acceptance/cases/bytes-copy-mutate.hew"
    ));
    let verified = lower_physical_module(&module, target_for_inventory(&module))
        .expect("physical bytes calls");
    let actions = verified
        .module()
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .filter_map(|block| match block.terminator {
            PhysicalTerminator::RuntimeCall { action, .. } => Some(action.family),
            _ => None,
        })
        .collect::<std::collections::HashSet<_>>();
    assert!(actions.contains(&RuntimeCallFamily::StringToBytes));
    assert!(actions.contains(&RuntimeCallFamily::BytesPush));
    assert!(actions.contains(&RuntimeCallFamily::BytesLen));
    assert!(actions.contains(&RuntimeCallFamily::BytesIndex));
}

#[test]
fn verifier_rejects_changed_runtime_transfer_and_failure_contracts() {
    let module = lower_source(include_str!(
        "../../tests/core-acceptance/cases/bytes-copy-mutate.hew"
    ));
    let mut wrong_transfer = lower_physical_module(&module, target_for_inventory(&module))
        .expect("valid physical bytes module")
        .into_unverified();
    let push_args = wrong_transfer
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .find_map(|block| match &mut block.terminator {
            PhysicalTerminator::RuntimeCall {
                action:
                    PhysicalRuntimeAction {
                        family: RuntimeCallFamily::BytesPush,
                        ..
                    },
                args,
                ..
            } => Some(args),
            _ => None,
        })
        .expect("bytes push physical action");
    let moved = match push_args[0] {
        ArgumentTransfer::Move(source) => source,
        other => panic!("expected moved bytes receiver, got {other:?}"),
    };
    push_args[0] = ArgumentTransfer::Borrow(moved);
    let error = verify_physical_module(&wrong_transfer)
        .expect_err("borrow must not replace the bytes owner move");
    assert!(error.message.contains("argument disagrees"));

    let mut missing_failure = lower_physical_module(&module, target_for_inventory(&module))
        .expect("valid physical bytes module")
        .into_unverified();
    let failure = missing_failure
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .find_map(|block| match &mut block.terminator {
            PhysicalTerminator::RuntimeCall {
                action:
                    PhysicalRuntimeAction {
                        family: RuntimeCallFamily::BytesIndex,
                        ..
                    },
                failure,
                ..
            } => Some(failure),
            _ => None,
        })
        .expect("bytes index physical action");
    *failure = None;
    let error = verify_physical_module(&missing_failure)
        .expect_err("bytes index must retain its SIR-authored failure edge");
    assert!(error.message.contains("failure edge disagrees"));
}

#[test]
fn copy_boundary_is_resolved_to_a_concrete_clone_action() {
    let mut module = module_with_return();
    let function = &mut module.functions[0];
    let SemTerminator::Return { value: Some(value) } = &mut function.blocks[0].terminator else {
        panic!("return fixture");
    };
    value.decision = BoundaryDecision::Copy;
    let physical = lower_physical_module(&module, target_for_inventory(&module)).expect("lower");
    assert!(matches!(
        physical.module().functions[0].blocks[0].terminator,
        PhysicalTerminator::Return {
            value: Some(ReturnTransfer::Clone {
                action: CloneAction::Bitwise,
                ..
            })
        }
    ));
}

#[test]
fn verifier_rejects_noncanonical_callable_identity() {
    let verified = lower_physical_module(&module_with_return(), target()).expect("lower");
    let mut physical = verified.into_unverified();
    physical.callables[0].id = CallableId(4);
    let error = verify_physical_module(&physical).expect_err("identity must be checked");
    assert!(error.message.contains("canonical table index"));
}

#[test]
fn call_result_is_initialized_only_on_the_normal_edge() {
    let verified =
        lower_physical_module(&module_with_call_and_unwind(), target()).expect("lower call");
    let main = &verified.module().functions[0];
    assert!(matches!(
        main.blocks[0].terminator,
        PhysicalTerminator::Call {
            result: Some(StorageId(0)),
            unwind: Some(_),
            ..
        }
    ));
}

#[test]
fn verifier_rejects_call_result_missing_from_normal_contract() {
    let verified =
        lower_physical_module(&module_with_call_and_unwind(), target()).expect("lower call");
    let mut physical = verified.into_unverified();
    let PhysicalTerminator::Call { result, .. } = &mut physical.functions[0].blocks[0].terminator
    else {
        panic!("call fixture");
    };
    *result = None;
    let error = verify_physical_module(&physical).expect_err("result-out must be checked");
    assert!(error.message.contains("result-out presence"));
}

#[test]
fn verifier_rejects_call_result_read_on_the_unwind_edge() {
    let verified =
        lower_physical_module(&module_with_call_and_unwind(), target()).expect("lower call");
    let mut physical = verified.into_unverified();
    let PhysicalTerminator::Call { unwind, .. } = &mut physical.functions[0].blocks[0].terminator
    else {
        panic!("call fixture");
    };
    *unwind = Some(PhysicalEdge {
        target: BlockId(1),
        transfers: vec![(StorageId(0), StorageId(1))],
        leaf_transfers: vec![],
    });
    let error = verify_physical_module(&physical).expect_err("fault cannot expose result");
    assert!(error.message.contains("reads uninitialized storage 0"));
}

#[test]
fn displaced_release_cannot_bypass_its_fault_dispatch() {
    let semantic = lower_source(
        r#"
            #[resource]
            type Connection { id: i64 }
            impl Connection {
                fn close(consume self) { panic("close failed"); }
            }
            fn main() {
                var values: Vec<Connection> = [];
                values.push(Connection { id: 1 });
                values.set(0, Connection { id: 2 });
                println("unreached");
            }
            "#,
    );
    let mut physical = lower_physical_module(&semantic, target_for_inventory(&semantic))
        .expect("displaced release lowers with fault cleanup")
        .into_unverified();
    let function = physical
        .functions
        .iter_mut()
        .find(|function| {
            function.blocks.iter().any(|block| {
                matches!(
                    block.terminator,
                    PhysicalTerminator::RuntimeCall {
                        action: PhysicalRuntimeAction {
                            family: RuntimeCallFamily::Vector(hew_types::VecValueOp::Set),
                            ..
                        },
                        ..
                    }
                )
            })
        })
        .expect("vector set caller");
    // Removing the dispatch must not let a failing close resume source
    // execution on the runtime call's normal successor.
    for block in &mut function.blocks {
        if let PhysicalTerminator::CleanupDispatch { normal, .. } = &block.terminator {
            block.terminator = PhysicalTerminator::Goto(normal.clone());
        }
    }
    let error = verify_physical_module(&physical)
        .expect_err("the caller owns a possible displaced-release fault");
    assert!(error.message.contains("fault"), "{}", error.message);
}

#[test]
fn verifier_rejects_propagating_an_uninitialized_fault() {
    let verified = lower_physical_module(&module_with_return(), target()).expect("lower");
    let mut physical = verified.into_unverified();
    physical.functions[0].blocks[0].terminator =
        PhysicalTerminator::PropagateFault { handback: None };
    let error = verify_physical_module(&physical).expect_err("fault must be initialized");
    assert!(error.message.contains("fault that is not initialized"));
}

#[test]
fn verifier_rejects_terminals_that_abandon_an_active_fault() {
    let verified =
        lower_physical_module(&module_with_call_and_unwind(), target()).expect("lower call");
    for (terminal, expected) in [
        (
            PhysicalTerminator::Trap(TrapKind::IntegerOverflow),
            "creates a trap while an earlier fault is active",
        ),
        (
            PhysicalTerminator::Unreachable,
            "abandons an active fault at unreachable",
        ),
    ] {
        let mut physical = verified.clone().into_unverified();
        physical.functions[0]
            .blocks
            .iter_mut()
            .find(|block| block.id == BlockId(2))
            .expect("unwind cleanup block")
            .terminator = terminal;
        let error = verify_physical_module(&physical)
            .expect_err("an active fault owner must be propagated exactly once");
        assert!(error.message.contains(expected), "{}", error.message);
    }
}

#[test]
fn verifier_checks_assign_source_before_initialization_analysis() {
    let module = lower_source(
        r#"
            fn main() {
                let value = "first";
            }
            "#,
    );
    let mut physical = lower_physical_module(&module, target_for_inventory(&module))
        .expect("valid assignment")
        .into_unverified();
    let operation = physical.functions[0]
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.ops)
        .find(|operation| {
            matches!(
                operation,
                PhysicalOp::StorageDead {
                    destroy: Some(DestroyAction::StringRelease),
                    ..
                }
            )
        })
        .expect("physical string Local cleanup");
    let PhysicalOp::StorageDead { storage: dest, .. } = *operation else {
        unreachable!("matched storage lifetime end")
    };
    let PhysicalOp::StorageDead { cleanup, .. } = operation.clone() else {
        unreachable!("matched storage lifetime end")
    };
    *operation = PhysicalOp::Assign {
        dest,
        source: StorageId(u32::MAX),
        destroy_old: Some(DestroyAction::StringRelease),
        cleanup,
    };
    let error = verify_physical_module(&physical)
        .expect_err("invalid assignment storage must fail without indexing it");
    assert!(error.message.contains("unknown physical storage"));
}

#[test]
fn verifier_checks_binary_result_and_constant_payload_types() {
    let module = lower_source(
        r"
            fn main() -> i64 {
                let value = 1 &+ 2;
                if value == 3 { 0 } else { 1 }
            }
            ",
    );
    let verified = lower_physical_module(&module, target_for_inventory(&module))
        .expect("valid scalar operations");

    let mut wrong_binary = verified.clone().into_unverified();
    let bool_dest = wrong_binary.functions[0]
        .storage
        .iter()
        .find(|slot| slot.ty == ResolvedTy::Bool)
        .expect("boolean result")
        .id;
    let destination = wrong_binary.functions[0]
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::Binary {
                op: BinaryOp::WrappingAdd,
                dest,
                ..
            } => Some(dest),
            _ => None,
        })
        .expect("wrapping binary operation");
    *destination = bool_dest;
    let error = verify_physical_module(&wrong_binary)
        .expect_err("binary result must retain the operand type");
    assert!(error.message.contains("binary result type"));

    let mut wrong_constant = verified.into_unverified();
    let constant = wrong_constant.functions[0]
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::Const {
                value: value @ PhysicalConst::IntegerBits(_),
                ..
            } => Some(value),
            _ => None,
        })
        .expect("integer constant");
    *constant = PhysicalConst::Bool(true);
    let error = verify_physical_module(&wrong_constant)
        .expect_err("constant payload must agree with its destination");
    assert!(error.message.contains("constant payload"));
}

#[test]
fn full_range_u64_literal_lowers_to_its_exact_bit_pattern() {
    let module = lower_source("fn main() { let value: u64 = 18446744073709551615; }");
    let physical = lower_physical_module(&module, target_for_inventory(&module))
        .expect("full-range u64 literal")
        .into_unverified();
    let bits = physical.functions[0]
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::Const {
                value: PhysicalConst::IntegerBits(bits),
                ..
            } => Some(*bits),
            _ => None,
        })
        .expect("integer constant");
    assert_eq!(bits, u64::MAX);
}

#[test]
fn negative_signed_literal_lowers_to_its_destination_width_twos_complement() {
    let module = lower_source("fn main() { let value: i8 = -128; }");
    let physical = lower_physical_module(&module, target_for_inventory(&module))
        .expect("i8 minimum literal")
        .into_unverified();
    let bits = physical.functions[0]
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::Const {
                value: PhysicalConst::IntegerBits(bits),
                ..
            } => Some(*bits),
            _ => None,
        })
        .expect("integer constant");
    // The eight-bit two's-complement encoding of -128, with no stray high
    // bits from the wider carrier.
    assert_eq!(bits, 0x80);
}

#[test]
fn verifier_rejects_a_constant_with_bits_above_its_destination_width() {
    let module = lower_source("fn main() { let value: u8 = 7; }");
    let verified =
        lower_physical_module(&module, target_for_inventory(&module)).expect("u8 literal");
    let mut malformed = verified.into_unverified();
    let constant = malformed.functions[0]
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::Const {
                value: value @ PhysicalConst::IntegerBits(_),
                ..
            } => Some(value),
            _ => None,
        })
        .expect("integer constant");
    // Bit 8 cannot belong to an eight-bit destination: the backend would
    // emit the pattern as written and produce a different value.
    *constant = PhysicalConst::IntegerBits(0x107);
    let error = verify_physical_module(&malformed)
        .expect_err("a constant must be canonical for its destination width");
    assert!(error.message.contains("constant payload"));
}

#[test]
fn verifier_rejects_missing_physical_literal_pool_entries() {
    let module = lower_source(r#"fn main() { let value = "literal"; }"#);
    let mut physical = lower_physical_module(&module, target_for_inventory(&module))
        .expect("valid string literal")
        .into_unverified();
    let literal = physical.functions[0]
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.ops)
        .find_map(|operation| match operation {
            PhysicalOp::Const {
                value: PhysicalConst::String(id),
                ..
            } => Some(id),
            _ => None,
        })
        .expect("string constant");
    *literal = hew_sir::StringLiteralId(u32::MAX);
    let error = verify_physical_module(&physical)
        .expect_err("physical string constant must name a module literal");
    assert!(error.message.contains("unknown literal"));
}

#[test]
fn checked_binary_defines_its_result_only_on_the_normal_edge() {
    let verified = lower_physical_module(&module_with_checked_add(), target())
        .expect("checked add should lower");
    let PhysicalTerminator::CheckedBinary {
        result,
        normal,
        failures,
        ..
    } = &verified.module().functions[0].blocks[0].terminator
    else {
        panic!("checked add should remain an explicit physical terminator");
    };
    assert_eq!(*result, StorageId(2));
    assert_eq!(normal.target, BlockId(1));
    assert_eq!(failures.len(), 1);
    assert_eq!(failures[0].kind, TrapKind::IntegerOverflow);
    assert_eq!(failures[0].edge.target, BlockId(2));
}

#[test]
fn verifier_rejects_checked_result_on_a_failure_edge() {
    let verified =
        lower_physical_module(&module_with_checked_add(), target()).expect("checked add");
    let mut physical = verified.into_unverified();
    let PhysicalTerminator::CheckedBinary { failures, .. } =
        &mut physical.functions[0].blocks[0].terminator
    else {
        panic!("checked add fixture");
    };
    failures[0].edge = PhysicalEdge {
        target: BlockId(1),
        transfers: vec![(StorageId(2), StorageId(3))],
        leaf_transfers: vec![],
    };
    let error = verify_physical_module(&physical)
        .expect_err("a failure edge cannot observe the normal-only result");
    assert!(error.message.contains("reads uninitialized storage 2"));
}

#[test]
fn verifier_rejects_a_changed_checked_failure_kind() {
    let verified =
        lower_physical_module(&module_with_checked_add(), target()).expect("checked add");
    let mut physical = verified.into_unverified();
    let PhysicalTerminator::CheckedBinary { failures, .. } =
        &mut physical.functions[0].blocks[0].terminator
    else {
        panic!("checked add fixture");
    };
    failures[0].kind = TrapKind::DivideByZero;
    let error = verify_physical_module(&physical)
        .expect_err("physical failure kinds must preserve SIR semantics");
    assert!(error.message.contains("failure set disagrees"));
}

#[test]
fn scalar_loop_reinitializes_dynamic_ssa_storage() {
    let module = lower_source(
        r"
            fn main() -> i64 {
                var value = 0;
                while value < 3 {
                    value = value &+ 1;
                }
                value
            }
            ",
    );
    lower_physical_module(&module, target_for_inventory(&module))
        .expect("scalar loop should verify physically");
}

#[test]
fn owned_loop_discharges_each_dynamic_owner_before_reinitialization() {
    let module = lower_source(
        r#"
            fn main() {
                var selected = "start";
                var keep = true;
                while keep {
                    selected = "loop";
                    keep = false;
                }
            }
            "#,
    );
    lower_physical_module(&module, target_for_inventory(&module))
        .expect("owned loop should verify physically");
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the malformed branch fixture must show both owner states and their merge"
)]
fn verifier_rejects_overwriting_a_maybe_live_owner() {
    let mut physical_target = target();
    physical_target.insert_layout(
        ResolvedTy::Unit,
        PhysicalLayout {
            size: 0,
            align: 1,
            repr: PhysicalRepr::Unit,
        },
    );
    let callable = PhysicalCallable {
        id: CallableId(0),
        declaration: hew_types::DefId::for_test("malformed_owner_merge"),
        instance: CallableInstance::Monomorphic,
        symbol: "malformed_owner_merge".to_string(),
        is_resumable: false,
        receiver_handback: false,
        params: vec![],
        return_ty: ResolvedTy::Unit,
        return_layout: None,
    };
    let function = PhysicalFunction {
        callable: CallableId(0),
        entry: BlockId(0),
        parameters: vec![],
        place_storage: BTreeMap::new(),
        storage: vec![
            PhysicalStorage {
                id: StorageId(0),
                ty: ResolvedTy::String,
                layout: physical_target
                    .layout(&ResolvedTy::String)
                    .expect("string layout")
                    .clone(),
                own: OwnKind::Owned,
                origin: StorageOrigin::Value(ValueId(0)),
                borrow_parent: None,
            },
            PhysicalStorage {
                id: StorageId(1),
                ty: ResolvedTy::Bool,
                layout: physical_target
                    .layout(&ResolvedTy::Bool)
                    .expect("bool layout")
                    .clone(),
                own: OwnKind::None,
                origin: StorageOrigin::Value(ValueId(1)),
                borrow_parent: None,
            },
        ],
        blocks: vec![
            PhysicalBlock {
                id: BlockId(0),
                arguments: vec![],
                ops: vec![PhysicalOp::Const {
                    dest: StorageId(1),
                    value: PhysicalConst::Bool(true),
                }],
                terminator: PhysicalTerminator::Branch {
                    condition: StorageId(1),
                    then_target: PhysicalEdge {
                        target: BlockId(1),
                        transfers: vec![],
                        leaf_transfers: vec![],
                    },
                    else_target: PhysicalEdge {
                        target: BlockId(2),
                        transfers: vec![],
                        leaf_transfers: vec![],
                    },
                },
            },
            PhysicalBlock {
                id: BlockId(1),
                arguments: vec![],
                ops: vec![PhysicalOp::Const {
                    dest: StorageId(0),
                    value: PhysicalConst::String(hew_sir::StringLiteralId(0)),
                }],
                terminator: PhysicalTerminator::Goto(PhysicalEdge {
                    target: BlockId(3),
                    transfers: vec![],
                    leaf_transfers: vec![],
                }),
            },
            PhysicalBlock {
                id: BlockId(2),
                arguments: vec![],
                ops: vec![],
                terminator: PhysicalTerminator::Goto(PhysicalEdge {
                    target: BlockId(3),
                    transfers: vec![],
                    leaf_transfers: vec![],
                }),
            },
            PhysicalBlock {
                id: BlockId(3),
                arguments: vec![],
                ops: vec![PhysicalOp::Const {
                    dest: StorageId(0),
                    value: PhysicalConst::String(hew_sir::StringLiteralId(1)),
                }],
                terminator: PhysicalTerminator::Return { value: None },
            },
        ],
    };
    let physical = PhysicalModule {
        defs: hew_types::DefTable::fixture(),
        debug: PhysicalDebug::default(),
        regex_patterns: Vec::new(),
        releases: ReleaseEffects::default(),
        actors: Vec::new(),
        supervisors: Vec::new(),
        actor_recipes: BTreeMap::new(),
        resources: vec![],
        closures: vec![],
        vtables: vec![],
        environment_glue: vec![],
        value_capabilities: BTreeMap::new(),
        target: physical_target,
        aggregate_glue: vec![],
        variant_glue: vec![],
        vector_glue: vec![],
        shared_glue: vec![],
        map_glue: vec![],
        set_glue: vec![],
        structural_glue: vec![],
        type_facts: BTreeMap::new(),
        callables: vec![callable],
        functions: vec![function],
        entry_callable: None,
        entry_exit_plan: None,
        string_literals: BTreeMap::from([
            (hew_sir::StringLiteralId(0), "left".to_string()),
            (hew_sir::StringLiteralId(1), "right".to_string()),
        ]),
        bytes_literals: BTreeMap::new(),
    };
    let error = verify_physical_module(&physical)
        .expect_err("a path-dependent live owner cannot be overwritten");
    assert!(error.message.contains("may overwrite a live obligation"));
}

fn collection_parameter_fixture() -> PhysicalModule {
    let collection = |kind: BuiltinType, args| ResolvedTy::Named {
        head: hew_types::TypeHead::Builtin(kind),
        args,
        is_opaque: false,
    };
    let vector = collection(BuiltinType::Vec, vec![ResolvedTy::String]);
    let set = collection(BuiltinType::HashSet, vec![ResolvedTy::String]);
    let map = collection(BuiltinType::HashMap, vec![ResolvedTy::String, vector]);
    let nested = collection(BuiltinType::HashMap, vec![ResolvedTy::I64, set]);
    let mut module = module_with_return();
    module.functions.clear();
    module.entry_callable = None;
    let mut facts =
        hew_types::TypeFactService::new(hew_types::TypeFactContext::default(), module.type_facts);
    module.callables[0].signature.params = [map, nested]
        .into_iter()
        .map(|ty| {
            facts
                .require(&ty)
                .expect("canonical collection value facts");
            hew_sir::SemAbiParam {
                ty,
                passing: hew_sir::SemParamPassing::Borrow,
                caller_visible_projection: false,
            }
        })
        .collect();
    module.type_facts = facts.into_rows();
    lower_physical_module(&module, target_for_inventory(&module))
        .expect("collection parameters have complete physical value recipes")
        .into_unverified()
}

#[test]
fn collection_callback_cleanup_releases_storage_and_owns_its_fault() {
    // Two mutations of the `contains_key` failure edge, both fail-closed:
    //   1. Replacing the cleanup block with a trap abandons the map's
    //      still-active local storage — the cleanup block is load-bearing,
    //      not decoration.
    //   2. Propagating a fault out of the success return has no fault
    //      owner to propagate.
    let semantic = lower_source(
        r#"fn main() -> i64 {
                var values: HashMap<i64, string> = HashMap.new();
                values.insert(1, "one");
                if values.contains_key(1) { 0 } else { 1 }
            }"#,
    );
    let physical = lower_physical_module(&semantic, target_for_inventory(&semantic))
        .unwrap()
        .into_unverified();
    let function = physical
        .functions
        .iter()
        .position(|function| {
            function.blocks.iter().any(|block| {
                matches!(
                    block.terminator,
                    PhysicalTerminator::RuntimeCall {
                        action: PhysicalRuntimeAction {
                            carrier: PhysicalRuntimeCarrier::Map {
                                operation: PhysicalMapOp::ContainsKey,
                                ..
                            },
                            ..
                        },
                        ..
                    }
                )
            })
        })
        .unwrap();
    let cleanup = physical.functions[function]
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            PhysicalTerminator::RuntimeCall {
                action:
                    PhysicalRuntimeAction {
                        carrier:
                            PhysicalRuntimeCarrier::Map {
                                operation: PhysicalMapOp::ContainsKey,
                                ..
                            },
                        ..
                    },
                failure: Some(edge),
                ..
            } => Some(edge.target),
            _ => None,
        })
        .unwrap();
    let mut invalid = physical.clone();
    invalid.functions[function].blocks[cleanup.0 as usize].terminator =
        PhysicalTerminator::Trap(TrapKind::IndexOutOfBounds);
    let error = verify_physical_module(&invalid).unwrap_err();
    assert!(
        error
            .message
            .contains("physical function exit leaves local storage active"),
        "{error}"
    );

    let mut invalid = physical;
    let success = invalid.functions[function]
        .blocks
        .iter_mut()
        .find(|block| matches!(block.terminator, PhysicalTerminator::Return { .. }))
        .unwrap();
    success.terminator = PhysicalTerminator::PropagateFault { handback: None };
    let error = verify_physical_module(&invalid).unwrap_err();
    assert!(
        error
            .message
            .contains("propagates a fault that is not initialized"),
        "{error}"
    );
}

#[test]
fn selected_key_methods_keep_checked_physical_recipes_and_callable_bodies() {
    let semantic = lower_source(
        r#"
            type Key { id: i64 }
            impl Hash for Key { fn hash(self) -> i64 { self.id % 10 } }
            type Outer { key: Key }
            fn main() -> i64 {
                var values: HashMap<Outer, string> = HashMap.new();
                values.insert(Outer { key: Key { id: 7 } }, "kept");
                values.len()
            }
        "#,
    );
    let target = target_for_inventory(&semantic);
    let physical = lower_physical_module(&semantic, target)
        .unwrap()
        .into_unverified();
    let (user_key, user) = physical
        .value_capabilities
        .iter()
        .find(|(_, plan)| matches!(plan.method, PhysicalValueMethod::User(_)))
        .expect("selected user hash");
    let PhysicalValueMethod::User(callable) = user.method else {
        unreachable!()
    };
    let mut missing_body = physical.clone();
    missing_body
        .functions
        .retain(|function| function.callable != callable);
    assert!(verify_physical_module(&missing_body)
        .unwrap_err()
        .message
        .contains("signature or body"));

    let mut substituted = physical.clone();
    substituted
        .value_capabilities
        .get_mut(user_key)
        .unwrap()
        .method = PhysicalValueMethod::Scalar;
    assert!(verify_physical_module(&substituted)
        .unwrap_err()
        .message
        .contains("changed its selected callable"));

    let mut forged_identity = physical.clone();
    forged_identity.callables[callable.0 as usize].declaration =
        hew_types::DefId::for_test("unselected_compatible_hash");
    assert!(verify_physical_module(&forged_identity)
        .unwrap_err()
        .message
        .contains("checker selection"));

    let mut transplanted = physical.clone();
    let derived = physical
        .value_capabilities
        .get(&(user_key.0.clone(), hew_types::ValueCapability::Eq))
        .unwrap();
    transplanted
        .value_capabilities
        .get_mut(user_key)
        .unwrap()
        .selection = derived.selection.clone();
    assert!(verify_physical_module(&transplanted)
        .unwrap_err()
        .message
        .contains("another type or capability"));

    let mut missing_component = physical.clone();
    missing_component.value_capabilities.remove(user_key);
    assert!(verify_physical_module(&missing_component)
        .unwrap_err()
        .message
        .contains("selected component"));

    let mut absent_keys = physical;
    absent_keys.value_capabilities.clear();
    assert!(
        verify_physical_module(&absent_keys).is_err(),
        "a map requires its selected key capabilities"
    );
}

#[test]
fn map_and_set_owners_compose_the_shared_value_recipes() {
    let module = collection_parameter_fixture();
    for map in &module.map_glue {
        verify_clone_action(&module, &map.ty, OwnKind::Owned, CloneAction::Map(map.id)).unwrap();
        verify_destroy_action(&module, &map.ty, OwnKind::Owned, DestroyAction::Map(map.id))
            .unwrap();
        match collection_type_arguments(&map.value.ty).unwrap().0 {
            BuiltinType::Vec => {
                assert!(matches!(map.value.clone, Some(CloneAction::Vector(_))));
            }
            BuiltinType::HashSet => {
                assert!(matches!(map.value.clone, Some(CloneAction::Set(_))));
            }
            other => panic!("unexpected nested collection {other:?}"),
        }
    }
    let set = &module.set_glue[0];
    assert_eq!(set.element.clone, Some(CloneAction::StringRetain));
    assert_eq!(set.element.destroy, Some(DestroyAction::StringRelease));
}

#[test]
fn collection_glue_rejects_wrong_identity_layout_and_lifetime_recipes() {
    let original = collection_parameter_fixture();
    for mutation in 0..5 {
        let mut module = original.clone();
        match mutation {
            0 => module.map_glue[0].key.ty = ResolvedTy::Bool,
            1 => module.set_glue[0].element.destroy = None,
            2 => module.map_glue[0].id = PhysicalMapId(99),
            3 => module.map_glue.push(module.map_glue[0].clone()),
            4 => {
                let ty = module.map_glue[0].ty.clone();
                module.target.insert_layout(ty, i64_layout());
            }
            _ => unreachable!(),
        }
        verify_physical_module(&module).expect_err("forged collection glue must be refused");
    }
    let map = &original.map_glue[0];
    verify_clone_action(
        &original,
        &map.ty,
        OwnKind::Owned,
        CloneAction::Map(original.map_glue[1].id),
    )
    .expect_err("foreign map key/value recipe");
    verify_destroy_action(
        &original,
        &map.ty,
        OwnKind::Owned,
        DestroyAction::Set(original.set_glue[0].id),
    )
    .expect_err("set drop cannot consume a map");
}

fn vector_fixture() -> PhysicalModule {
    let module = lower_source(
        r#"
            fn main() -> i64 {
                var values = ["one", "two"];
                var numbers = [7, 8];
                let snapshot = values;
                let independent = values[0];
                let optional = values.get(99);
                let numeric_option = numbers.get(0);
                values.set(0, "replacement");
                let removed = values.pop();
                let number = numbers.pop();
                values.clear();
                return snapshot.len();
            }
            "#,
    );
    lower_physical_module(&module, target_for_inventory(&module))
        .expect("vector source must have a complete physical realization")
        .into_unverified()
}

fn vector_block(function: &mut PhysicalFunction, op: VecValueOp) -> &mut PhysicalBlock {
    function
        .blocks
        .iter_mut()
        .find(|block| {
            matches!(block.terminator,
                    PhysicalTerminator::RuntimeCall { action, .. }
                        if action.family == RuntimeCallFamily::Vector(op))
        })
        .expect("fixture vector operation")
}

#[test]
fn vector_values_use_shared_copy_drop_recipes_for_every_element_shape() {
    for (declarations, element, value) in [
        ("", "i64", "7"),
        ("", "string", "\"payload\""),
        (
            "type Leaf { label: string, }",
            "Leaf",
            "Leaf { label: \"payload\" }",
        ),
        (
            "enum Choice { Text(string), Empty, }",
            "Choice",
            "Choice.Text(\"payload\")",
        ),
        ("", "Vec<string>", "[\"payload\"]"),
        ("", "(Vec<string>, i64)", "([\"payload\"], 1)"),
    ] {
        let source = format!(
            r"
                {declarations}
                fn duplicate<T>(values: Vec<T>) -> Vec<T> {{ values }}
                fn main() -> i64 {{
                    var values: Vec<{element}> = Vec.new();
                    values.push({value});
                    let snapshot = duplicate(values);
                    let independent = values[0];
                    let optional = values.get(9);
                    values.set(0, {value});
                    let removed = values.pop();
                    values.clear();
                    return snapshot.len();
                }}"
        );
        let module = lower_source(&source);
        let physical = lower_physical_module(&module, target_for_inventory(&module))
            .unwrap_or_else(|error| panic!("{element}: {error}"));
        let physical = physical.module();
        let operations = physical
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .filter_map(|block| match block.terminator {
                PhysicalTerminator::RuntimeCall {
                    action:
                        PhysicalRuntimeAction {
                            family: RuntimeCallFamily::Vector(operation),
                            ..
                        },
                    ..
                } => Some(operation),
                _ => None,
            })
            .collect::<Vec<_>>();
        for operation in [
            VecValueOp::New,
            VecValueOp::Len,
            VecValueOp::Index,
            VecValueOp::Get,
            VecValueOp::Push,
            VecValueOp::Set,
            VecValueOp::Pop,
            VecValueOp::Clear,
        ] {
            assert!(
                operations.contains(&operation),
                "{element}: missing {operation:?}"
            );
        }
        assert!(
            physical
                .functions
                .iter()
                .flat_map(|function| &function.blocks)
                .flat_map(|block| &block.ops)
                .any(|op| matches!(
                    op,
                    PhysicalOp::Clone {
                        action: CloneAction::Vector(_),
                        ..
                    }
                )),
            "{element}: ordinary vector value must use the shared vector clone"
        );
        for glue in &physical.vector_glue {
            assert_eq!(sequence_element_type(&glue.ty), Some(&glue.element.ty));
            assert_eq!(
                glue.element.own,
                OwnKind::of_ty(&glue.element.ty, &module.type_facts).unwrap()
            );
            assert!(glue.element.clone.is_some());
            assert_eq!(
                glue.element.destroy.is_some(),
                glue.element.own == OwnKind::Owned
            );
        }
    }
}

#[test]
fn vector_inventory_demands_recursive_element_shapes_without_payload_construction() {
    let module = lower_source(
        r"
            type Leaf { text: string, }
            enum Tree { Value(Leaf), Children(Vec<Tree>), }
            fn main() -> i64 {
                let trees: Vec<Tree> = Vec.new();
                let snapshot = trees;
                return snapshot.len();
            }
        ",
    );
    let inventory = physical_type_inventory(&module);
    assert!(inventory
        .aggregates()
        .any(|shape| shape.fields == [ResolvedTy::String]));
    // Name the vector this source demands by its copy recipe rather than
    // indexing the glue table.
    let physical = lower_physical_module(&module, target_for_inventory(&module)).unwrap();
    let physical = physical.module();
    let vector = physical
        .vector_glue
        .iter()
        .find(|glue| matches!(glue.element.clone, Some(CloneAction::Variant(_))))
        .expect("the Vec<Tree> element demands a variant copy recipe");
    let Some(CloneAction::Variant(tree)) = vector.element.clone else {
        panic!("tree copy recipe");
    };
    let tree = variant_glue(physical, tree).unwrap();
    assert_eq!(
        tree.variants[1].fields[0].clone,
        Some(CloneAction::Vector(vector.id))
    );
    assert_eq!(
        tree.variants[1].fields[0].destroy,
        Some(DestroyAction::Vector(vector.id))
    );
}

#[test]
fn vector_zero_sized_elements_preserve_the_exact_target_layout() {
    let module = lower_source(
        r"
            type Empty {}
            fn main() -> i64 {
                var values: Vec<Empty> = Vec.new();
                values.push(Empty {});
                let snapshot = values;
                let extracted = values[0];
                let optional = values.get(0);
                let removed = values.pop();
                values.clear();
                return snapshot.len();
            }
        ",
    );
    // Name the vector this source demands by its zero-sized element rather
    // than indexing the glue table.
    let physical = lower_physical_module(&module, target_for_inventory(&module)).unwrap();
    let physical = physical.module();
    let element = &physical
        .vector_glue
        .iter()
        .find(|glue| physical.target.layout(&glue.element.ty).unwrap().size == 0)
        .expect("the Vec<Empty> element is zero sized")
        .element;
    assert_eq!(physical.target.layout(&element.ty).unwrap().size, 0);
    assert_eq!(element.clone, Some(CloneAction::Bitwise));
    assert_eq!(element.destroy, None);
    assert_eq!(element.own, OwnKind::None);
}

#[test]
fn verifier_rejects_vector_descriptor_identity_and_recipe_drift() {
    let original = vector_fixture();
    let string_id = original
        .vector_glue
        .iter()
        .position(|glue| glue.element.ty == ResolvedTy::String)
        .unwrap();
    for mutation in 0..4 {
        let mut physical = original.clone();
        let glue = &mut physical.vector_glue[string_id];
        match mutation {
            0 => glue.element.ty = ResolvedTy::I64,
            1 => glue.element.clone = None,
            2 => glue.element.destroy = None,
            3 => {
                glue.element.own = OwnKind::None;
                glue.element.clone = Some(CloneAction::Bitwise);
                glue.element.destroy = None;
            }
            _ => unreachable!(),
        }
        let error = verify_physical_module(&physical).expect_err("forged vector recipe");
        let expected = match mutation {
            0 => "exact element identity",
            1 => "physical clone action Vector",
            2 => "physical destroy action Vector",
            3 => "semantic ownership or cloneability",
            _ => unreachable!(),
        };
        assert!(error.message.contains(expected), "{mutation}: {error}");
    }
    let mut physical = original;
    physical.vector_glue[string_id].id = PhysicalVectorId(999);
    let error = verify_physical_module(&physical).unwrap_err();
    assert!(
        error.message.contains("unknown physical vector glue"),
        "{error}"
    );
}

#[test]
fn verifier_rejects_foreign_vector_and_extraction_descriptors() {
    let original = vector_fixture();
    for operation in [VecValueOp::Len, VecValueOp::Get, VecValueOp::Pop] {
        let mut physical = original.clone();
        let foreign_vector = physical
            .vector_glue
            .iter()
            .find(|glue| glue.element.ty == ResolvedTy::I64)
            .unwrap()
            .id;
        let foreign_variant = physical
            .variant_glue
            .iter()
            .find(|glue| {
                matches!(&glue.ty,
                ResolvedTy::Named { head: hew_types::TypeHead::Builtin(hew_types::BuiltinType::Option), args, .. }
                    if args == &[ResolvedTy::I64])
            })
            .unwrap()
            .id;
        let foreign_tuple = physical
            .aggregate_glue
            .iter()
            .find(|glue| {
                matches!(&glue.ty,
                ResolvedTy::Tuple(fields) if fields.last() == Some(&ResolvedTy::I64))
            })
            .unwrap()
            .id;
        let block = vector_block(&mut physical.functions[0], operation);
        let PhysicalTerminator::RuntimeCall {
            action:
                PhysicalRuntimeAction {
                    carrier:
                        PhysicalRuntimeCarrier::Vector {
                            operation: action,
                            glue,
                        },
                    ..
                },
            ..
        } = &mut block.terminator
        else {
            unreachable!()
        };
        match operation {
            VecValueOp::Len => *glue = foreign_vector,
            VecValueOp::Get => {
                *action = PhysicalVectorOp::Get {
                    result: foreign_variant,
                }
            }
            VecValueOp::Pop => {
                *action = PhysicalVectorOp::Pop {
                    result: foreign_tuple,
                }
            }
            _ => unreachable!(),
        }
        let error = verify_physical_module(&physical).expect_err("foreign physical descriptor");
        assert!(
            error.message.contains("foreign vector descriptor")
                || error.message.contains("result descriptor"),
            "{operation:?}: {error}"
        );
    }
}

#[test]
fn verifier_checks_vector_element_types_and_result_ownership() {
    let original = vector_fixture();
    let mut physical = original.clone();
    let integer = physical.functions[0]
        .storage
        .iter()
        .find(|slot| slot.ty == ResolvedTy::I64)
        .unwrap()
        .id;
    let block = vector_block(&mut physical.functions[0], VecValueOp::Set);
    let PhysicalTerminator::RuntimeCall { args, .. } = &mut block.terminator else {
        unreachable!()
    };
    args[2] = ArgumentTransfer::Borrow(integer);
    let error = verify_physical_module(&physical).expect_err("right arity, wrong element type");
    assert!(error.message.contains("runtime argument 2"), "{error}");

    for operation in [VecValueOp::Index, VecValueOp::Get, VecValueOp::Pop] {
        let mut physical = original.clone();
        let block = vector_block(&mut physical.functions[0], operation);
        let PhysicalTerminator::RuntimeCall {
            result: Some(result),
            ref args,
            ..
        } = block.terminator
        else {
            unreachable!()
        };
        let parent = match args[0] {
            ArgumentTransfer::Borrow(source)
            | ArgumentTransfer::BorrowMut(source)
            | ArgumentTransfer::Move(source)
            | ArgumentTransfer::Clone { source, .. } => source,
        };
        let slot = &mut physical.functions[0].storage[result.0 as usize];
        slot.own = OwnKind::Guaranteed;
        // Even a forged loan dependency cannot change an owning extraction
        // into a borrowed result of this runtime operation.
        slot.borrow_parent = Some(parent);
        let error = verify_physical_module(&physical)
            .expect_err("extraction cannot yield an interior borrow");
        assert!(
            error.message.contains("result ownership"),
            "{operation:?}: {error}"
        );
    }
}

#[test]
fn verifier_checks_vector_failure_presence_and_consuming_receiver_contracts() {
    let original = vector_fixture();
    for operation in [VecValueOp::Index, VecValueOp::Set, VecValueOp::Pop] {
        let mut physical = original.clone();
        let block = vector_block(&mut physical.functions[0], operation);
        let PhysicalTerminator::RuntimeCall { failure, .. } = &mut block.terminator else {
            unreachable!()
        };
        *failure = None;
        let error = verify_physical_module(&physical).expect_err("missing vector failure edge");
        assert!(
            error.message.contains("failure edge"),
            "{operation:?}: {error}"
        );
    }
    for operation in [
        VecValueOp::Push,
        VecValueOp::Set,
        VecValueOp::Pop,
        VecValueOp::Clear,
    ] {
        let mut physical = original.clone();
        let block = vector_block(&mut physical.functions[0], operation);
        let PhysicalTerminator::RuntimeCall { args, .. } = &mut block.terminator else {
            unreachable!()
        };
        let ArgumentTransfer::Move(receiver) = args[0] else {
            panic!("receiver must transfer");
        };
        args[0] = ArgumentTransfer::Borrow(receiver);
        let error =
            verify_physical_module(&physical).expect_err("mutation must transfer its receiver");
        assert!(
            error.message.contains("argument disagrees"),
            "{operation:?}: {error}"
        );
    }
}

#[test]
fn vector_transfers_on_success_and_retains_inputs_on_failure() {
    let original = vector_fixture();
    for operation in [VecValueOp::Set, VecValueOp::Pop] {
        for failed in [false, true] {
            let mut physical = original.clone();
            let block = vector_block(&mut physical.functions[0], operation);
            let PhysicalTerminator::RuntimeCall {
                action:
                    PhysicalRuntimeAction {
                        carrier: PhysicalRuntimeCarrier::Vector { glue, .. },
                        ..
                    },
                args,
                normal,
                failure,
                ..
            } = &block.terminator
            else {
                unreachable!()
            };
            let ArgumentTransfer::Move(receiver) = args[0] else {
                unreachable!()
            };
            let action = CloneAction::Vector(*glue);
            let target = if failed {
                failure.as_ref().unwrap().target
            } else {
                normal.target
            };
            physical.functions[0]
                .blocks
                .iter_mut()
                .find(|block| block.id == target)
                .unwrap()
                .ops
                .insert(
                    0,
                    PhysicalOp::Clone {
                        dest: receiver,
                        source: receiver,
                        action,
                    },
                );
            let error =
                verify_physical_module(&physical).expect_err("transferred receiver was reused");
            assert!(
                error.message.contains(if failed {
                    "overwrites initialized"
                } else {
                    "uninitialized"
                }),
                "{operation:?} failed={failed}: {error}"
            );
        }
    }
}

#[test]
fn vector_index_failure_never_initializes_its_result_storage() {
    let mut physical = vector_fixture();
    let block = vector_block(&mut physical.functions[0], VecValueOp::Index);
    let PhysicalTerminator::RuntimeCall {
        result: Some(result),
        failure: Some(failure),
        ..
    } = &block.terminator
    else {
        unreachable!()
    };
    let result = *result;
    let target = failure.target;
    physical.functions[0]
        .blocks
        .iter_mut()
        .find(|block| block.id == target)
        .unwrap()
        .ops
        .insert(
            0,
            PhysicalOp::Clone {
                dest: result,
                source: result,
                action: CloneAction::StringRetain,
            },
        );
    let error =
        verify_physical_module(&physical).expect_err("failed read did not produce an element");
    assert!(error.message.contains("uninitialized"), "{error}");
}

#[test]
fn vector_copies_and_mutations_preserve_loop_and_early_return_storage() {
    let module = lower_source(
        r#"
            fn grow(values: Vec<string>, stop: bool) -> i64 {
                var current = values;
                for i in 0..3 {
                    let snapshot = current;
                    current.push("loop");
                    if stop && i == 1 { return snapshot.len(); }
                }
                return current.len();
            }
            fn main() -> i64 {
                let input = ["seed"];
                grow(input, true) + grow(input, false)
            }
        "#,
    );
    let physical = lower_physical_module(&module, target_for_inventory(&module))
        .expect("each dynamic vector owner must discharge before its storage is reused");
    assert_eq!(physical.module().functions.len(), 2);
}

#[test]
fn failed_vector_index_clears_a_previous_iteration_scalar_result() {
    let module = lower_source("fn main() -> i64 { let values = [1]; values[0] }");
    let mut physical = lower_physical_module(&module, target_for_inventory(&module))
        .unwrap()
        .into_unverified();
    let block = vector_block(&mut physical.functions[0], VecValueOp::Index).clone();
    let function = &physical.functions[0];
    let PhysicalTerminator::RuntimeCall {
        result: Some(result),
        failure: Some(failure),
        normal,
        ..
    } = &block.terminator
    else {
        unreachable!()
    };
    assert_eq!(function.storage[result.0 as usize].own, OwnKind::None);
    // Scalar storage may still hold the previous dynamic iteration's bits.
    // A failed read initializes no result of the current invocation.
    let state = FlowState {
        slots: vec![InitState::Initialized; function.storage.len()],
        active: vec![InitState::Uninitialized; function.storage.len()],
        fault: FaultState::None,
        exit: defer::ORDINARY,
        defers: defer::State::default(),
    };
    let successors = terminator_successors(
        &physical,
        function,
        &BorrowDependents::of(function),
        &block.terminator,
        state,
        block.id,
        &defer::verify_regions(function).unwrap(),
    )
    .unwrap();
    let failed = &successors
        .iter()
        .find(|(id, _)| *id == failure.target)
        .unwrap()
        .1;
    assert_eq!(failed.slots[result.0 as usize], InitState::Uninitialized);
    let succeeded = &successors
        .iter()
        .find(|(id, _)| *id == normal.target)
        .unwrap()
        .1;
    assert_eq!(succeeded.slots[result.0 as usize], InitState::Initialized);
}

fn selected_value_call_module(capability: ValueCapability) -> PhysicalModule {
    let (method, invocation) = match capability {
        ValueCapability::Hash => ("fn selected(a: i64) -> i64 { a }", "selected(7)"),
        ValueCapability::Eq => (
            "fn selected(a: i64, b: i64) -> bool { a == b }",
            "if selected(1, 2) { 1 } else { 0 }",
        ),
    };
    let mut module = lower_source(&format!("{method} fn main() -> i64 {{ let map: HashMap<i64, string> = HashMap.new(); {invocation} }}"));
    let selected = module
        .functions
        .iter()
        .find(|function| module.defs.path(function.declaration) == "selected")
        .unwrap()
        .callable;
    let mut converted = 0;
    for function in &mut module.functions {
        for block in &mut function.blocks {
            if let SemTerminator::Call {
                id,
                callee,
                args,
                result,
                normal,
                unwind,
                ..
            } = &block.terminator
            {
                if *callee == selected {
                    let mut args = args.clone();
                    for argument in &mut args {
                        argument.decision = BoundaryDecision::Borrow;
                    }
                    block.terminator = SemTerminator::ValueCall {
                        id: *id,
                        ty: ResolvedTy::I64,
                        capability,
                        args,
                        result: result.clone(),
                        normal: normal.clone().expect("returning call"),
                        unwind: unwind.clone(),
                    };
                    converted += 1;
                }
            }
        }
    }
    assert_eq!(converted, 1);
    assert!(
        hew_sir::verify_module(&module).is_empty(),
        "{:?}",
        hew_sir::verify_module(&module)
    );
    lower_physical_module(&module, target_for_inventory(&module))
        .unwrap()
        .module()
        .clone()
}

#[test]
fn selected_value_calls_keep_borrowed_operands_and_success_only_results() {
    for capability in [ValueCapability::Hash, ValueCapability::Eq] {
        let module = selected_value_call_module(capability);
        let (function, block) = module
            .functions
            .iter()
            .find_map(|function| {
                function
                    .blocks
                    .iter()
                    .find(|block| matches!(block.terminator, PhysicalTerminator::ValueCall { .. }))
                    .map(|block| (function, block))
            })
            .unwrap();
        let PhysicalTerminator::ValueCall {
            args,
            result,
            normal,
            unwind,
            ..
        } = &block.terminator
        else {
            unreachable!()
        };
        let mut state = FlowState {
            slots: vec![InitState::Initialized; function.storage.len()],
            active: vec![InitState::Uninitialized; function.storage.len()],
            fault: FaultState::None,
            exit: defer::ORDINARY,
            defers: defer::State::default(),
        };
        state.slots[result.0 as usize] = InitState::Uninitialized;
        let successors = terminator_successors(
            &module,
            function,
            &BorrowDependents::of(function),
            &block.terminator,
            state,
            block.id,
            &defer::verify_regions(function).unwrap(),
        )
        .unwrap();
        for (edge, outcome) in successors {
            assert_eq!(
                outcome.slots[result.0 as usize],
                if edge == normal.target {
                    InitState::Initialized
                } else {
                    InitState::Uninitialized
                }
            );
            assert_eq!(
                outcome.fault,
                if edge == unwind.target {
                    FaultState::Active
                } else {
                    FaultState::None
                }
            );
            for arg in args {
                let ArgumentTransfer::Borrow(source) = arg else {
                    panic!("selected calls must borrow")
                };
                assert_eq!(outcome.slots[source.0 as usize], InitState::Initialized);
            }
        }
    }
}

#[test]
fn selected_value_call_verifier_rejects_signature_and_selection_drift() {
    for mutation in 0..5 {
        let mut module = selected_value_call_module(ValueCapability::Eq);
        let function_index = module
            .functions
            .iter()
            .position(|function| {
                function
                    .blocks
                    .iter()
                    .any(|block| matches!(block.terminator, PhysicalTerminator::ValueCall { .. }))
            })
            .unwrap();
        let function = &mut module.functions[function_index];
        let block = function
            .blocks
            .iter_mut()
            .find(|block| matches!(block.terminator, PhysicalTerminator::ValueCall { .. }))
            .unwrap();
        let PhysicalTerminator::ValueCall {
            ty, args, result, ..
        } = &mut block.terminator
        else {
            unreachable!()
        };
        match mutation {
            0 => {
                module
                    .value_capabilities
                    .remove(&(ResolvedTy::I64, ValueCapability::Eq));
            }
            1 => {
                args.pop();
            }
            2 => {
                let ArgumentTransfer::Borrow(source) = args[0] else {
                    unreachable!()
                };
                args[0] = ArgumentTransfer::Move(source);
            }
            3 => *ty = ResolvedTy::Bool,
            4 => {
                let ArgumentTransfer::Borrow(source) = args[0] else {
                    unreachable!()
                };
                *result = source;
            }
            _ => unreachable!(),
        }
        let function = &module.functions[function_index];
        let block = function
            .blocks
            .iter()
            .find(|block| matches!(block.terminator, PhysicalTerminator::ValueCall { .. }))
            .unwrap();
        let blocks = function.blocks.iter().map(|block| block.id).collect();
        assert!(
            verify_terminator(&module, function, &blocks, &block.terminator).is_err(),
            "mutation {mutation}"
        );
        assert!(
            verify_physical_module(&module).is_err(),
            "mutation {mutation}"
        );
    }
}

#[test]
fn selected_value_call_cleanup_cannot_discard_fault_or_read_failed_result() {
    for discard_fault in [false, true] {
        let mut module = selected_value_call_module(ValueCapability::Eq);
        let function = module
            .functions
            .iter_mut()
            .find(|function| {
                function
                    .blocks
                    .iter()
                    .any(|block| matches!(block.terminator, PhysicalTerminator::ValueCall { .. }))
            })
            .unwrap();
        let (result, cleanup) = function
            .blocks
            .iter()
            .find_map(|block| match &block.terminator {
                PhysicalTerminator::ValueCall { result, unwind, .. } => {
                    Some((*result, unwind.target))
                }
                _ => None,
            })
            .unwrap();
        let cleanup = function
            .blocks
            .iter_mut()
            .find(|block| block.id == cleanup)
            .unwrap();
        if discard_fault {
            cleanup.terminator = PhysicalTerminator::Unreachable;
        } else {
            cleanup.terminator = PhysicalTerminator::Branch {
                condition: result,
                then_target: PhysicalEdge {
                    target: cleanup.id,
                    transfers: vec![],
                    leaf_transfers: vec![],
                },
                else_target: PhysicalEdge {
                    target: cleanup.id,
                    transfers: vec![],
                    leaf_transfers: vec![],
                },
            };
        }
        assert!(verify_physical_module(&module).is_err());
    }
}
