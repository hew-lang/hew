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

use std::collections::BTreeMap;

use hew_hir::{lower_program_host_target, ItemId, ResolutionCtx};
use hew_sir::{
    BlockArg, BoundaryDecision, BoundaryOperand, CallableInstance, CheckedFailure, Edge,
    FunctionSourceOrigin, Operand, Provenance, SemBlock, SemCallConv, SemCallable, SemCallableKind,
    SemFunction, SemModule, SemOp, SemOpKind, SemSignature, SemTerminator, ValueDef, ValueId,
};
use hew_types::{
    module_registry::ModuleRegistry, Checker, CloneKind, DefId, EntryExitPlan, SendFact, TypeFacts,
    TypeInstanceKey, ValueClass,
};

use super::*;

#[test]
fn borrowed_field_chain_emits_no_vector_clone_for_its_reads() {
    use inkwell::values::AnyValue;

    fn vector_clone_calls(semantic: &SemModule) -> usize {
        let triple = native_emission_triple();
        let inventory = hew_mir::physical::physical_type_inventory(semantic);
        let target = physical_target_for_inventory(&triple, &inventory).unwrap();
        let physical = hew_mir::lower_physical_module(semantic, target).unwrap();
        let ctx = Context::create();
        let machine =
            crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0).unwrap();
        let llvm = build_module(&ctx, physical.module(), "field_loans", &machine).unwrap();
        llvm.verify().unwrap();
        let symbol = emitted_symbol(physical.module(), &physical.module().callables[0]);
        llvm.get_functions()
            .filter(|function| {
                let name = function.get_name().to_string_lossy();
                name == symbol || name.starts_with(&format!("{symbol}$resume"))
            })
            .map(|function| {
                function
                    .print_to_string()
                    .to_string()
                    .matches("call ptr @hew_vec_clone_owned(")
                    .count()
            })
            .sum()
    }

    let borrowed = borrow_fixture::nested_borrow_module();
    let mut copied = borrowed.clone();
    for operation in copied
        .functions
        .iter_mut()
        .flat_map(|f| &mut f.blocks)
        .flat_map(|b| &mut b.ops)
    {
        match &operation.kind {
            SemOpKind::AggregateProjectBorrow {
                shape,
                aggregate,
                field,
            } => {
                operation.kind = SemOpKind::AggregateProjectCopy {
                    shape: *shape,
                    aggregate: aggregate.clone(),
                    field: *field,
                };
                operation.results[0].own = OwnKind::Owned;
            }
            SemOpKind::EndBorrow { borrow } => {
                operation.kind = SemOpKind::DestroyValue {
                    value: borrow.clone(),
                };
            }
            _ => {}
        }
    }
    assert_eq!(
        vector_clone_calls(&copied) - vector_clone_calls(&borrowed),
        2,
        "both the nested record and vector projection must avoid cloning their vector"
    );
}

#[test]
fn vector_descriptor_matches_the_runtime_c_abi() {
    use hew_runtime::vec::{HewValueLayout, HewVec};
    use std::mem::{align_of, offset_of, size_of};

    let triple = native_emission_triple();
    let physical = physical_target_for_triple(&triple).unwrap();
    let target = TargetData::create(&physical.data_layout);
    let ctx = Context::create();
    let header = collections::vector_header_type(&ctx, &target);
    for (index, expected) in [
        offset_of!(HewVec, data),
        offset_of!(HewVec, len),
        offset_of!(HewVec, front),
        offset_of!(HewVec, cap),
        offset_of!(HewVec, elem_size),
    ]
    .into_iter()
    .enumerate()
    {
        assert_eq!(
            target.offset_of_element(&header, u32::try_from(index).unwrap()),
            Some(expected as u64)
        );
    }
    let descriptor = value_descriptor_type(&ctx, &target);
    assert_eq!(
        target.get_abi_size(&descriptor),
        size_of::<HewValueLayout>() as u64
    );
    assert_eq!(
        target.get_abi_alignment(&descriptor) as usize,
        align_of::<HewValueLayout>()
    );
    for (index, expected) in [
        offset_of!(HewValueLayout, size),
        offset_of!(HewValueLayout, align),
        offset_of!(HewValueLayout, ownership_kind),
        offset_of!(HewValueLayout, clone_fn),
        offset_of!(HewValueLayout, drop_fn),
        offset_of!(HewValueLayout, release_start),
        offset_of!(HewValueLayout, code_owner),
        offset_of!(HewValueLayout, managed),
    ]
    .into_iter()
    .enumerate()
    {
        assert_eq!(
            target.offset_of_element(&descriptor, u32::try_from(index).unwrap()),
            Some(expected as u64)
        );
    }
}

#[test]
fn utf8_decode_emits_typed_outcomes_with_reusable_scratch() {
    let semantic = utf8_fixture::decode_module();
    assert!(hew_sir::verify_module(&semantic).is_empty());
    let triple = native_emission_triple();
    let inventory = hew_mir::physical::physical_type_inventory(&semantic);
    let target = physical_target_for_inventory(&triple, &inventory).unwrap();
    let physical = hew_mir::lower_physical_module(&semantic, target).unwrap();
    let ctx = Context::create();
    let machine =
        crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0).unwrap();
    let module = build_module(&ctx, physical.module(), "utf8_decode", &machine).unwrap();
    module.verify().unwrap();
    let decoder = module
        .get_function("hew_bytes_decode_utf8")
        .expect("decoder ABI call");
    assert_eq!(
        decoder.get_type().get_return_type(),
        Some(ctx.i8_type().into())
    );
    assert_eq!(
        decoder.get_type().get_param_types(),
        vec![ctx.ptr_type(AddressSpace::default()).into(); 4]
    );
    for function in module.get_functions() {
        // Typed destruction adds helper callbacks with bounded invocation-local
        // scratch. Only the source body's loop can accumulate decoder scratch.
        if !function
            .get_basic_blocks()
            .iter()
            .any(|block| block.get_name().to_bytes() == b"physical.prologue")
        {
            continue;
        }
        for block in function.get_basic_blocks() {
            if block.get_name().to_bytes() == b"physical.prologue" {
                continue;
            }
            for instruction in block.get_instructions() {
                assert_ne!(
                    instruction.get_opcode(),
                    inkwell::values::InstructionOpcode::Alloca,
                    "decoder scratch must not grow per loop iteration"
                );
            }
        }
    }
}

/// Exercise the physical runtime boundary with a collection supplied by a
/// caller. Construction's key-capability demand is tested at its producer.
fn collection_operation_module(family: hew_types::RuntimeCallFamily) -> SemModule {
    use hew_types::{BuiltinType, RuntimeArgumentEffect, TypeFactContext, TypeFactService};
    let kind = match family {
        hew_types::RuntimeCallFamily::Map(_) => BuiltinType::HashMap,
        hew_types::RuntimeCallFamily::Set(_) => BuiltinType::HashSet,
        _ => panic!("fixture requires a map or set operation"),
    };
    let mut arguments = vec![ResolvedTy::String];
    if kind == BuiltinType::HashMap {
        arguments.push(ResolvedTy::named_builtin(
            BuiltinType::Vec,
            vec![ResolvedTy::String],
        ));
    }
    let receiver = ResolvedTy::named_builtin(kind, arguments);
    let contract = family.semantic_contract().unwrap();
    let params = contract
        .arguments
        .iter()
        .map(|argument| {
            argument
                .ty
                .resolve(&hew_types::DefTable::new(), Some(&receiver))
                .unwrap()
        })
        .collect::<Vec<_>>();
    let result_ty = contract
        .instantiate(&hew_types::DefTable::new(), &params, &ResolvedTy::Unit)
        .unwrap()
        .result_ty;
    let mut module = scalar_entry_module();
    module.value_capabilities = lower_source(
        r#"fn main() {
            let keys: HashMap<string, string> = HashMap.new();
        }"#,
    )
    .value_capabilities;

    module.entry_callable = None;
    module.entry_exit_plan = None;
    if matches!(
        family,
        hew_types::RuntimeCallFamily::Map(
            hew_types::runtime_call::MapValueOp::Get
                | hew_types::runtime_call::MapValueOp::GetBorrow
                | hew_types::runtime_call::MapValueOp::Remove
        )
    ) {
        let seed = lower_source(
            r#"fn main() -> i64 {
                let nested = [["value"]];
                let optional = nested.get(0);
                return 0;
            }"#,
        );
        module.variant_shapes = seed.variant_shapes;
    }
    let mut facts = TypeFactService::new(TypeFactContext::default(), module.type_facts);
    facts.require(&ResolvedTy::Unit).unwrap();
    facts.require(&result_ty).unwrap();
    let mut copies = Vec::new();
    let mut operands = Vec::new();
    let count = u32::try_from(params.len()).unwrap();
    // A borrowed result is a loan: the fixture ends it rather than
    // handing an owner back to a caller.
    let borrowed_result = matches!(contract.result, hew_types::RuntimeResultEffect::Borrowed(_));
    let return_ty = if borrowed_result {
        ResolvedTy::Unit
    } else {
        result_ty.clone()
    };
    let signature = &mut module.callables[0].signature;
    signature.params.clear();
    signature.return_ty = return_ty.clone();
    let function = &mut module.functions[0];
    function.params.clear();
    function.return_ty = return_ty;
    for (index, (ty, contract)) in params.iter().zip(contract.arguments).enumerate() {
        let index = u32::try_from(index).unwrap();
        let own = OwnKind::of_class(facts.require(ty).unwrap().class);
        let borrowed = own == OwnKind::Owned;
        signature.params.push(hew_sir::SemAbiParam {
            ty: ty.clone(),
            passing: if borrowed {
                hew_sir::SemParamPassing::Borrow
            } else {
                hew_sir::SemParamPassing::ReadOnly
            },
            caller_visible_projection: false,
        });
        function.params.push(BlockArg {
            value: ValueId(index),
            ty: ty.clone(),
            own: if borrowed { OwnKind::Guaranteed } else { own },
        });
        let (value, decision) = match contract
            .effect
            .resolve_operand(facts.require(ty).unwrap().class)
        {
            RuntimeArgumentEffect::Value => unreachable!("value ingress was resolved"),
            RuntimeArgumentEffect::Borrow => (ValueId(index), BoundaryDecision::Borrow),
            RuntimeArgumentEffect::Copy => (ValueId(index), BoundaryDecision::Copy),
            RuntimeArgumentEffect::Move => {
                let value = ValueId(count + index);
                copies.push(SemOp {
                    id: hew_sir::OpId(index),
                    results: vec![ValueDef {
                        id: value,
                        ty: ty.clone(),
                        own,
                    }],
                    kind: SemOpKind::CopyValue {
                        source: Operand {
                            value: ValueId(index),
                        },
                    },
                    provenance: Provenance::Synthesized,
                });
                (value, BoundaryDecision::Move)
            }
        };
        operands.push(BoundaryOperand {
            operand: Operand { value },
            decision,
        });
    }
    // A borrowed result is a loan of argument zero: it carries no release
    // obligation regardless of its type's class.
    let own = if matches!(contract.result, hew_types::RuntimeResultEffect::Borrowed(_)) {
        OwnKind::Guaranteed
    } else {
        OwnKind::of_class(facts.require(&result_ty).unwrap().class)
    };
    let raw = ValueId(2 * count);
    let value = ValueId(2 * count + 1);
    let failed_inputs = operands
        .iter()
        .filter(|argument| {
            contract.preserves_inputs_on_failure() && argument.decision == BoundaryDecision::Move
        })
        .enumerate()
        .map(|(index, argument)| SemOp {
            id: hew_sir::OpId(count + 1 + u32::try_from(index).unwrap()),
            results: vec![],
            kind: SemOpKind::DestroyValue {
                value: argument.operand.clone(),
            },
            provenance: Provenance::Synthesized,
        })
        .collect();
    let unwind = if contract.failures.is_empty() {
        hew_sir::CallUnwind::NotApplicable
    } else {
        hew_sir::CallUnwind::Cleanup(Edge {
            target: BlockId(2),
            args: vec![],
        })
    };
    function.blocks = vec![
        SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(0),
            args: vec![],
            ops: copies,
            terminator: SemTerminator::RtCall {
                id: hew_sir::OpId(count),
                family,
                args: operands,
                result: hew_sir::CallResult::Value(ValueDef {
                    id: raw,
                    ty: result_ty.clone(),
                    own,
                }),
                normal: Edge {
                    target: BlockId(1),
                    args: vec![Operand { value: raw }],
                },
                unwind,
            },
        },
        SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(1),
            args: vec![BlockArg {
                value,
                ty: result_ty,
                own,
            }],
            ops: if borrowed_result {
                vec![SemOp {
                    id: hew_sir::OpId(2 * count + 2),
                    results: vec![],
                    kind: SemOpKind::EndBorrow {
                        borrow: Operand { value },
                    },
                    provenance: Provenance::Synthesized,
                }]
            } else {
                vec![]
            },
            terminator: if borrowed_result {
                SemTerminator::Return { value: None }
            } else {
                SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: Operand { value },
                        decision: BoundaryDecision::Move,
                    }),
                }
            },
        },
    ];
    if let Some(failure) = contract.failures.first() {
        function.blocks.push(SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(2),
            args: vec![],
            ops: failed_inputs,
            terminator: if contract.propagates_fault() {
                SemTerminator::ResumeUnwind { handback: None }
            } else {
                SemTerminator::Trap {
                    kind: hew_sir::runtime_failure_trap_kind(*failure).unwrap(),
                }
            },
        });
    }
    module.type_facts = facts.into_rows();
    module
}

#[test]
fn collection_operations_emit_verified_runtime_abi_and_owner_transfers() {
    use hew_types::runtime_call::{MapValueOp as Map, SetValueOp as Set};
    use hew_types::RuntimeCallFamily;
    let families = [
        Map::Len,
        Map::Index,
        Map::Get,
        Map::GetBorrow,
        Map::Remove,
        Map::ContainsKey,
        Map::Insert,
        Map::Clear,
        Map::Keys,
        Map::Values,
        Map::Entries,
    ]
    .into_iter()
    .map(RuntimeCallFamily::Map)
    .chain(
        [
            Set::Len,
            Set::Contains,
            Set::Insert,
            Set::Remove,
            Set::Clear,
            Set::Elements,
        ]
        .into_iter()
        .map(RuntimeCallFamily::Set),
    );
    for family in families {
        let semantic = collection_operation_module(family);
        for triple in [
            native_emission_triple(),
            "x86_64-pc-windows-msvc".to_string(),
            "aarch64-apple-darwin".to_string(),
        ] {
            let target = physical_target_for_inventory(
                &triple,
                &hew_mir::physical::physical_type_inventory(&semantic),
            )
            .unwrap();
            let physical = hew_mir::lower_physical_module(&semantic, target)
                .unwrap_or_else(|error| panic!("{family:?}: {error}"));
            let context = Context::create();
            let machine =
                crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
                    .unwrap();
            let module = build_module(&context, physical.module(), "collection_boundary", &machine)
                .unwrap_or_else(|error| panic!("{family:?}: {error}"));
            module.verify().unwrap();
        }
    }
}

#[test]
fn fixed_arrays_check_target_allocation_geometry_without_stack_limits() {
    for (triple, pointer_bytes) in [
        ("x86_64-unknown-linux-gnu", 8),
        ("x86_64-pc-windows-msvc", 8),
        ("aarch64-apple-darwin", 8),
        ("wasm32-wasip1", 4),
    ] {
        let large = ResolvedTy::Array(Box::new(ResolvedTy::I64), 4_000_000);
        let target = physical_target_for_types(triple, [&large]).unwrap();
        let layout = target.layout(&large).unwrap();
        assert_eq!(layout.size, pointer_bytes, "{triple}");
        assert_eq!(layout.repr, PhysicalRepr::Pointer);
        let limit = if pointer_bytes == 8 {
            i64::MAX as u64
        } else {
            i32::MAX as u64
        };
        let length_limit = if pointer_bytes == 8 {
            i64::MAX as u64
        } else {
            u64::from(u32::MAX)
        };
        for invalid in [
            ResolvedTy::Array(Box::new(ResolvedTy::I64), limit / 8 + 1),
            ResolvedTy::Array(Box::new(ResolvedTy::Unit), length_limit + 1),
        ] {
            let error = physical_target_for_types(triple, [&invalid]).unwrap_err();
            assert!(
                error
                    .to_string()
                    .contains("target allocation or runtime length range"),
                "{triple}: {error}"
            );
        }
    }
}

#[test]
fn fixed_array_repeat_emits_constant_size_glue_and_no_array_stack_temporary() {
    let triple = native_emission_triple();
    let machine =
        crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0).unwrap();
    let mut instruction_counts = Vec::new();
    for length in [3, 4_000_000] {
        let semantic = lower_source(&format!(
            "fn main() -> i64 {{ let values: [i64; {length}] = [7; {length}]; values[0] }}"
        ));
        let inventory = hew_mir::physical::physical_type_inventory(&semantic);
        let target = physical_target_for_inventory(&triple, &inventory).unwrap();
        let verified = hew_mir::lower_physical_module(&semantic, target).unwrap();
        let physical = verified.module();
        // Select the fixed-array element rather than counting every row:
        // the pin is about this source's own vector, not the module's
        // total glue count.
        assert_eq!(
            physical
                .vector_glue
                .iter()
                .filter(|glue| glue.element.ty == ResolvedTy::I64)
                .count(),
            1
        );
        for function in &physical.functions {
            for storage in &function.storage {
                if matches!(storage.ty, ResolvedTy::Array(_, _)) {
                    assert_eq!(storage.layout.repr, PhysicalRepr::Pointer);
                }
            }
        }
        let ctx = Context::create();
        let llvm = build_module(&ctx, physical, "fixed_array_repeat", &machine).unwrap();
        llvm.verify().unwrap();
        instruction_counts.push(
            llvm.get_functions()
                .flat_map(|function| function.get_basic_blocks())
                .map(|block| block.get_instructions().count())
                .sum::<usize>(),
        );
    }
    assert_eq!(instruction_counts[0], instruction_counts[1]);
}

#[test]
fn canonical_maps_and_sets_use_the_target_pointer_carrier() {
    let map = ResolvedTy::named_builtin(
        hew_types::BuiltinType::HashMap,
        vec![ResolvedTy::String, ResolvedTy::I64],
    );
    let set = ResolvedTy::named_builtin(hew_types::BuiltinType::HashSet, vec![ResolvedTy::String]);
    for triple in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let target = physical_target_for_types(triple, [&map, &set]).unwrap();
        let pointer = target.layout(&ResolvedTy::String).unwrap();
        assert_eq!(target.layout(&map), Some(pointer), "{triple}");
        assert_eq!(target.layout(&set), Some(pointer), "{triple}");
    }
    let lookalike = ResolvedTy::user_for_test("HashMap", vec![ResolvedTy::String, ResolvedTy::I64]);
    assert!(physical_target_for_types("x86_64-unknown-linux-gnu", [&lookalike]).is_err());
    let wrong_arity = ResolvedTy::named_builtin(hew_types::BuiltinType::HashSet, vec![]);
    assert!(physical_target_for_types("x86_64-unknown-linux-gnu", [&wrong_arity]).is_err());
}

fn lower_source(source: &str) -> SemModule {
    lower_source_with_registry(source, ModuleRegistry::new(Vec::new()))
}

fn lower_source_with_registry(source: &str, registry: ModuleRegistry) -> SemModule {
    let mut parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    hew_compile::attach_prelude_std_modules(&mut parsed.program);
    let mut checker = Checker::new(registry);
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
        hew_sir::verify_module(&lowered.module).is_empty(),
        "source must produce verified SIR: {:#?}",
        hew_sir::verify_module(&lowered.module)
    );
    lowered.module
}

fn scalar_entry_module() -> SemModule {
    let declaration = DefId::for_test("main");
    let callable = SemCallable {
        id: CallableId(0),
        function: ItemId(0),
        declaration,
        instance: CallableInstance::Monomorphic,
        symbol: "main".to_string(),
        source_origin: FunctionSourceOrigin::RootUnit,
        signature: SemSignature {
            failure_domain: hew_types::CallableFailureDomain::HewFault,
            params: vec![],
            return_ty: ResolvedTy::I64,
        },
        call_conv: SemCallConv::Default,
        kind: SemCallableKind::HewDirect,
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
                results: vec![ValueDef {
                    id: ValueId(0),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }],
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
    SemModule {
        coverage: hew_sir::CoveragePlan::default(),
        offloads: Vec::new(),
        defs: hew_types::DefTable::fixture(),
        structural_display: BTreeMap::new(),
        debug: hew_sir::SemDebugFacts::default(),
        regex_patterns: Vec::new(),
        actors: Vec::new(),
        supervisors: Vec::new(),
        resources: BTreeMap::new(),
        closures: Vec::new(),
        callable_specializations: Vec::new(),
        vtables: Vec::new(),
        value_capabilities: BTreeMap::new(),
        callables: vec![callable],
        generic_templates: vec![],
        root_unit_callables: vec![CallableId(0)],
        entry_exit_plan: Some(EntryExitPlan {
            entry: declaration,
            action: EntryExitAction::Integer(EntryIntegerType::I64),
        }),
        entry_callable: Some(CallableId(0)),
        test_entries: Vec::new(),
        functions: vec![function],
        aggregate_shapes: vec![],
        variant_shapes: vec![],
        type_facts: BTreeMap::from([(
            TypeInstanceKey(ResolvedTy::I64),
            TypeFacts {
                class: ValueClass::BitCopy,
                clone: CloneKind::Bits,
                send: SendFact::Known(true),
                hash: true,
                eq: true,
                partial_ord: true,
            },
        )]),
        string_literals: BTreeMap::new(),
        bytes_literals: BTreeMap::new(),
    }
}

fn verified_scalar_for(triple: &str) -> VerifiedPhysicalModule {
    let target = physical_target_for_triple(triple).expect("target layout");
    hew_mir::lower_physical_module(&scalar_entry_module(), target).expect("physical lowering")
}

fn checked_add_entry_module() -> SemModule {
    let mut module = scalar_entry_module();
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
                    kind: SemOpKind::ConstInteger(i128::from(i64::MAX)),
                    provenance: Provenance::Synthesized,
                },
                SemOp {
                    id: hew_sir::OpId(1),
                    results: vec![ValueDef {
                        id: ValueId(1),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    kind: SemOpKind::ConstInteger(1),
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
            args: vec![BlockArg {
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

fn bytes_copy_module() -> SemModule {
    let declaration = DefId::for_test("copy_bytes");
    let callable = SemCallable {
        id: CallableId(0),
        function: ItemId(0),
        declaration,
        instance: CallableInstance::Monomorphic,
        symbol: "copy_bytes".to_string(),
        source_origin: FunctionSourceOrigin::RootUnit,
        signature: SemSignature {
            failure_domain: hew_types::CallableFailureDomain::HewFault,
            params: vec![],
            return_ty: ResolvedTy::Bytes,
        },
        call_conv: SemCallConv::Default,
        kind: SemCallableKind::HewDirect,
    };
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration,
        name: "copy_bytes".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::RootUnit,
        terminal_receiver: None,
        params: vec![],
        return_ty: ResolvedTy::Bytes,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            terminator_provenance: hew_sir::Provenance::Synthesized,
            id: BlockId(0),
            args: vec![],
            ops: vec![
                SemOp {
                    id: hew_sir::OpId(0),
                    results: vec![ValueDef {
                        id: ValueId(0),
                        ty: ResolvedTy::Bytes,
                        own: OwnKind::Owned,
                    }],
                    kind: SemOpKind::ConstBytes(hew_sir::BytesLiteralId(0)),
                    provenance: Provenance::Synthesized,
                },
                SemOp {
                    id: hew_sir::OpId(1),
                    results: vec![ValueDef {
                        id: ValueId(1),
                        ty: ResolvedTy::Bytes,
                        own: OwnKind::Owned,
                    }],
                    kind: SemOpKind::CopyValue {
                        source: Operand { value: ValueId(0) },
                    },
                    provenance: Provenance::Synthesized,
                },
                SemOp {
                    id: hew_sir::OpId(2),
                    results: vec![],
                    kind: SemOpKind::DestroyValue {
                        value: Operand { value: ValueId(0) },
                    },
                    provenance: Provenance::Synthesized,
                },
            ],
            terminator: SemTerminator::Return {
                value: Some(BoundaryOperand {
                    operand: Operand { value: ValueId(1) },
                    decision: BoundaryDecision::Move,
                }),
            },
        }],
        places: vec![],
        bindings: vec![],
    };
    SemModule {
        coverage: hew_sir::CoveragePlan::default(),
        offloads: Vec::new(),
        defs: hew_types::DefTable::fixture(),
        structural_display: BTreeMap::new(),
        debug: hew_sir::SemDebugFacts::default(),
        regex_patterns: Vec::new(),
        actors: Vec::new(),
        supervisors: Vec::new(),
        resources: BTreeMap::new(),
        closures: Vec::new(),
        callable_specializations: Vec::new(),
        vtables: Vec::new(),
        callables: vec![callable],
        generic_templates: vec![],
        root_unit_callables: vec![CallableId(0)],
        entry_exit_plan: None,
        entry_callable: None,
        test_entries: Vec::new(),
        functions: vec![function],
        aggregate_shapes: vec![],
        variant_shapes: vec![],
        type_facts: BTreeMap::from([(
            TypeInstanceKey(ResolvedTy::Bytes),
            TypeFacts {
                class: ValueClass::CowValue,
                clone: CloneKind::Retain,
                send: SendFact::Known(true),
                hash: true,
                eq: true,
                partial_ord: false,
            },
        )]),
        string_literals: BTreeMap::new(),
        bytes_literals: BTreeMap::from([(hew_sir::BytesLiteralId(0), b"ok".to_vec())]),
        value_capabilities: BTreeMap::new(),
    }
}

#[test]
fn windows_uses_the_same_status_result_fault_abi() {
    let triple = "x86_64-pc-windows-msvc";
    let verified = verified_scalar_for(triple);
    let ctx = Context::create();
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(triple, OptLevel::O0)
        .expect("Windows target machine");
    let module = build_module(&ctx, verified.module(), "windows_status_abi", &machine)
        .expect("Windows physical module");
    let body = module
        .get_function(entry_body_symbol_for_triple(triple))
        .expect("selected entry body");
    assert_eq!(
        body.get_type().get_return_type(),
        Some(ctx.i32_type().into())
    );
    let params = body.get_type().get_param_types();
    assert_eq!(params.len(), 2, "result-out and fault-out");
    assert!(params.iter().all(|parameter| parameter.is_pointer_type()));
    assert!(module.get_function("main").is_some(), "process adapter");
}

#[test]
fn scalar_entry_builds_and_llvm_verifies() {
    let triple = native_emission_triple();
    let verified = verified_scalar_for(&triple);
    validate_physical_codegen(&verified, "scalar_entry").expect("verified LLVM module");
}

#[test]
fn tuple_layout_is_measured_by_the_active_target_data() {
    let triple = native_emission_triple();
    let tuple = ResolvedTy::Tuple(vec![ResolvedTy::I8, ResolvedTy::I64]);
    let target = physical_target_for_types(&triple, [&tuple]).expect("tuple target layout");
    let layout = target.layout(&tuple).expect("measured tuple layout");

    let ctx = Context::create();
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
        .expect("target machine");
    let llvm_tuple = ctx.struct_type(&[ctx.i8_type().into(), ctx.i64_type().into()], false);
    assert_eq!(
        layout.size,
        machine.get_target_data().get_abi_size(&llvm_tuple)
    );
    assert_eq!(
        layout.align,
        machine.get_target_data().get_abi_alignment(&llvm_tuple)
    );
    assert!(matches!(
        layout.repr,
        PhysicalRepr::Struct(ref fields)
            if matches!(fields.as_slice(), [
                PhysicalLayout { repr: PhysicalRepr::Integer { bits: 8 }, .. },
                PhysicalLayout { repr: PhysicalRepr::Integer { bits: 64 }, .. },
            ])
    ));
}

#[test]
fn owned_record_layout_and_recursive_glue_emit_verified_llvm() {
    let semantic = lower_source(
        r#"
            type Packet { label: string; payload: bytes; }

            fn duplicate(packet: Packet) -> Packet { packet }

            fn main() {
                let packet = Packet { payload: b"P", label: "record" };
                let packet_copy = duplicate(packet);
                let first = packet_copy.label;
                let second = packet.label;
            }
            "#,
    );
    let triple = native_emission_triple();
    let inventory = hew_mir::physical::physical_type_inventory(&semantic);
    let target =
        physical_target_for_inventory(&triple, &inventory).expect("exact aggregate target layout");
    // Name the shape this source demands rather than indexing the table.
    let shape = semantic
        .aggregate_shapes
        .iter()
        .find(|shape| semantic.defs.display(shape.instance.nominal.declaration()) == "Packet")
        .expect("source must demand one exact record shape");
    assert!(matches!(
        target.layout(&shape.aggregate_ty),
        Some(PhysicalLayout {
            repr: PhysicalRepr::Struct(fields),
            ..
        }) if fields.len() == 2
    ));
    let verified =
        hew_mir::lower_physical_module(&semantic, target).expect("owned record physical lowering");
    assert!(verified.module().callables.iter().any(|callable| {
        callable.return_ty == shape.aggregate_ty
            && matches!(
                callable.params.as_slice(),
                [hew_mir::PhysicalParam {
                    carrier: ParamCarrier::Indirect,
                    ..
                }]
            )
    }));
    let ctx = Context::create();
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
        .expect("target machine");
    let module = build_module(&ctx, verified.module(), "owned_record", &machine)
        .expect("owned record LLVM module");
    module.verify().expect("owned record LLVM verification");
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("aggregate.clone.field"),
        "whole aggregate copy must execute the resolved recursive glue"
    );
    assert!(module.get_function("hew_string_clone").is_some());
    assert!(module.get_function("hew_bytes_clone_ref").is_some());
    assert!(module.get_function("hew_string_drop_managed").is_some());
    assert!(module.get_function("hew_bytes_drop_managed").is_some());
}

#[test]
fn owned_variant_layout_and_active_case_glue_emit_verified_llvm() {
    use inkwell::values::InstructionOpcode;

    let semantic = lower_source(
        r#"enum Choice {
    Text(string);
    Empty;
}

fn inspect(value: Choice) -> i64 {
    match value {
        .Text(text) => {
            let copy = text;
            1
        }
        .Empty => 0,
    }
}

fn main() -> i64 {
    let original = Choice.Text("hello");
    let first = inspect(original);
    let second = inspect(original);
    if first == 1 && second == 1 {
        0
    } else {
        1
    }
}
"#,
    );
    let triple = native_emission_triple();
    let inventory = hew_mir::physical::physical_type_inventory(&semantic);
    let target =
        physical_target_for_inventory(&triple, &inventory).expect("exact variant target layout");
    let [shape] = semantic.variant_shapes.as_slice() else {
        panic!("source must demand one exact variant shape")
    };
    let layout = target
        .variant_layout(&shape.enum_ty)
        .expect("target must realize the demanded variant");
    assert!(!layout.is_indirect);
    assert_eq!(layout.variants.len(), 2);

    let verified =
        hew_mir::lower_physical_module(&semantic, target).expect("owned variant physical lowering");
    assert!(verified.module().functions.iter().any(|function| {
        function.blocks.iter().any(|block| {
            matches!(
                block.terminator,
                PhysicalTerminator::SwitchVariant { ref arms, .. }
                    if arms.len() == 2
                        && arms[0].fields.len() == 1
                        && arms[1].fields.is_empty()
            )
        })
    }));

    for level in [OptLevel::O0, OptLevel::O2] {
        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, level)
            .expect("target machine");
        let module = build_module(&ctx, verified.module(), "owned_variant", &machine)
            .expect("owned variant LLVM module");
        module.verify().expect("owned variant LLVM verification");
        let ir = module.print_to_string().to_string();
        assert!(ir.contains("variant.clone.case.0"));
        assert!(ir.contains("variant.destroy.case.0"));
        assert!(ir.contains("call void @llvm.trap"));
        assert!(module.get_function("llvm.trap").is_some());
        assert!(module.get_function("hew_string_clone").is_some());
        assert!(module.get_function("hew_string_drop_managed").is_some());
        for callable in &verified.module().callables {
            let function = module
                .get_function(&emitted_symbol(verified.module(), callable))
                .expect("physical callable definition");
            for block in function.get_basic_blocks() {
                if block.get_name().to_bytes() == b"physical.prologue" {
                    continue;
                }
                let mut instruction = block.get_first_instruction();
                while let Some(current) = instruction {
                    assert_ne!(
                        current.get_opcode(),
                        InstructionOpcode::Alloca,
                        "dynamic CFG blocks must not grow scratch storage at runtime"
                    );
                    instruction = current.get_next_instruction();
                }
            }
        }
    }
}

#[test]
fn string_length_uses_the_widened_runtime_abi() {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs must live under the repository root")
        .to_path_buf();
    let semantic = lower_source_with_registry(
        r#"
            import std.string;
            fn main() -> i64 { "length".len() + "Aé中🙂".byte_len() }
            "#,
        ModuleRegistry::new(vec![repo_root]),
    );
    let triple = native_emission_triple();
    let inventory = hew_mir::physical::physical_type_inventory(&semantic);
    let target =
        physical_target_for_inventory(&triple, &inventory).expect("string length target layout");
    let verified =
        hew_mir::lower_physical_module(&semantic, target).expect("string length physical lowering");
    let ctx = Context::create();
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
        .expect("target machine");
    let module = build_module(&ctx, verified.module(), "string_length", &machine)
        .expect("string length LLVM module");
    module.verify().expect("string length LLVM verification");
    let length = module
        .get_function("hew_string_length")
        .expect("exact runtime length declaration");
    assert_eq!(
        length.get_type().get_return_type(),
        Some(ctx.i64_type().into())
    );
    assert_eq!(length.get_type().count_param_types(), 1);
    let byte_length = module
        .get_function("hew_string_byte_length")
        .expect("explicit byte length must call its distinct runtime operation");
    assert_eq!(
        byte_length.get_type().get_return_type(),
        Some(ctx.i64_type().into())
    );
    assert_eq!(byte_length.get_type().count_param_types(), 1);
}

#[test]
fn string_prefix_uses_the_runtime_boolean_abi_across_native_targets() {
    let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    let semantic = lower_source_with_registry(
        r#"
            import std.string;
            fn main() -> i64 {
                if "éclair".starts_with("é") { 0 } else { 1 }
            }
            "#,
        ModuleRegistry::new(vec![root]),
    );
    let inventory = hew_mir::physical::physical_type_inventory(&semantic);
    for triple in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let target = physical_target_for_inventory(triple, &inventory).unwrap();
        let verified = hew_mir::lower_physical_module(&semantic, target).unwrap();
        let ctx = Context::create();
        let machine =
            crate::llvm::target_machine_for_triple_with_opt_level(triple, OptLevel::O0).unwrap();
        let module = build_module(&ctx, verified.module(), "string_prefix", &machine).unwrap();
        module.verify().unwrap();
        let prefix = module.get_function("hew_string_starts_with").unwrap();
        assert_eq!(
            prefix.get_type().get_return_type(),
            Some(ctx.bool_type().into())
        );
        assert_eq!(
            prefix.get_type().get_param_types(),
            vec![ctx.ptr_type(AddressSpace::default()).into(); 2]
        );
    }
}

#[test]
fn physical_emit_option_instruments_generated_code_with_asan() {
    let triple = native_emission_triple();
    let verified = verified_scalar_for(&triple);
    let dir = tempfile::tempdir().expect("physical ASan output directory");
    let artefacts = emit_physical_object(
        &verified,
        &PhysicalEmitOptions {
            module_name: "physical_asan",
            out_dir: dir.path(),
            target_triple: Some(&triple),
            opt_level: OptLevel::O0,
            emit_llvm: true,
            address_sanitizer: true,
            debug_source: None,
            link_freestanding_wasm: false,
            build_info: None,
        },
    )
    .expect("emit ASan-instrumented physical module");
    let ir = std::fs::read_to_string(artefacts.ll_path.expect("diagnostic LLVM IR"))
        .expect("read physical ASan LLVM IR");
    assert!(
        ir.contains("__asan_init"),
        "physical emitter must write LLVM IR after ASan instrumentation"
    );
}

#[test]
fn checked_add_emits_a_real_overflow_branch() {
    let triple = native_emission_triple();
    let target = physical_target_for_triple(&triple).expect("target layout");
    let verified = hew_mir::lower_physical_module(&checked_add_entry_module(), target)
        .expect("physical lowering");
    let ctx = Context::create();
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
        .expect("target machine");
    let module = build_module(&ctx, verified.module(), "checked_add", &machine)
        .expect("checked add LLVM module");
    module.verify().expect("checked add LLVM verification");
    assert!(
        module.get_function("llvm.sadd.with.overflow.i64").is_some(),
        "signed overflow must be detected before choosing the failure edge"
    );
    assert!(
        module.get_function("hew_fault_new").is_some(),
        "the declared overflow edge must reach typed fault creation"
    );
}

#[test]
fn bytes_retain_is_void_and_preserves_the_aggregate() {
    let triple = native_emission_triple();
    let target = physical_target_for_triple(&triple).expect("target layout");
    let verified =
        hew_mir::lower_physical_module(&bytes_copy_module(), target).expect("physical bytes copy");
    let ctx = Context::create();
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
        .expect("target machine");
    let module = build_module(&ctx, verified.module(), "bytes_copy", &machine)
        .expect("bytes copy LLVM module");
    let retain = module
        .get_function("hew_bytes_clone_ref")
        .expect("bytes retain declaration");
    assert_eq!(
        retain.get_type().get_return_type(),
        None,
        "bytes retain mutates only the refcount and returns no pointer"
    );
}

#[test]
fn conflicting_runtime_declaration_fails_closed() {
    let ctx = Context::create();
    let module = ctx.create_module("conflicting_runtime");
    module.add_function("hew_fault_drop", ctx.i32_type().fn_type(&[], false), None);
    let error = external_fault_drop(&ctx, &module).expect_err("ABI mismatch must refuse");
    assert!(error.to_string().contains("hew_fault_drop"));
}

#[test]
fn extern_byte_calls_reuse_entry_storage() {
    use inkwell::values::InstructionOpcode;

    let semantic = lower_source(
        r#"
            extern "C" {
                fn make_bytes(seed: i32) -> bytes;
                fn relay_bytes(consume value: bytes) -> bytes;
            }
            fn main() {
                for i in 0..10 {
                    let value = unsafe { relay_bytes(make_bytes(i)) };
                    println(value.len());
                }
            }
            "#,
    );
    let triple = native_emission_triple();
    let inventory = hew_mir::physical::physical_type_inventory(&semantic);
    let target = physical_target_for_inventory(&triple, &inventory).unwrap();
    let verified = hew_mir::lower_physical_module(&semantic, target).unwrap();
    let ctx = Context::create();
    let machine =
        crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0).unwrap();
    let module = build_module(&ctx, verified.module(), "extern_bytes", &machine).unwrap();
    for function in module.get_functions() {
        for block in function.get_basic_blocks() {
            let mut instruction = block.get_first_instruction();
            while let Some(current) = instruction {
                if current.get_opcode() == InstructionOpcode::Alloca {
                    assert_eq!(
                        Some(block),
                        function.get_first_basic_block(),
                        "extern calls in loops must not grow scratch storage"
                    );
                }
                instruction = current.get_next_instruction();
            }
        }
    }
}

#[test]
fn build_info_record_survives_optimization_only_when_requested() {
    let triple = native_emission_triple();
    let verified = verified_scalar_for(&triple);
    let expected = build_info::build_info_text("9.9.9-test", &triple);
    for opt_level in [OptLevel::O0, OptLevel::O2] {
        for build_info in [Some("9.9.9-test"), None] {
            let dir = tempfile::tempdir().expect("build info output directory");
            let artefacts = emit_physical_object(
                &verified,
                &PhysicalEmitOptions {
                    module_name: "build_info",
                    out_dir: dir.path(),
                    target_triple: Some(&triple),
                    opt_level,
                    emit_llvm: false,
                    address_sanitizer: false,
                    debug_source: None,
                    link_freestanding_wasm: false,
                    build_info,
                },
            )
            .expect("emit build info object");
            let object = std::fs::read(artefacts.native_obj_path.expect("native object"))
                .expect("read build info object");
            let record = format!("{expected}\0");
            let found = object
                .windows(record.len())
                .any(|window| window == record.as_bytes());
            assert_eq!(found, build_info.is_some(), "{opt_level:?} {build_info:?}");
        }
    }
}

#[test]
fn scalar_and_vector_loop_chunks_preserve_the_shared_budget_across_serial_calls() {
    use std::cell::Cell;
    thread_local! {
        static BUDGET: Cell<u32> = const { Cell::new(0) };
        static POLLS: Cell<u32> = const { Cell::new(0) };
        static SUPPRESSED: Cell<bool> = const { Cell::new(false) };
        static CANCELLING: Cell<bool> = const { Cell::new(false) };
        static VECTOR_RELEASES: Cell<u32> = const { Cell::new(0) };
    }
    unsafe extern "C" fn countdown(_: *mut std::ffi::c_void, _: u32) -> *mut u32 {
        if SUPPRESSED.get() {
            std::ptr::null_mut()
        } else {
            BUDGET.with(Cell::as_ptr)
        }
    }
    unsafe extern "C" fn poll(state: *mut std::ffi::c_void, budget: u32) -> i32 {
        assert_eq!(
            BUDGET.get(),
            0,
            "poll only after the shared budget is consumed"
        );
        BUDGET.set(budget);
        POLLS.set(POLLS.get() + 1);
        if CANCELLING.get() {
            // SAFETY: generated polling borrows the live invocation state;
            // its execution owner retains the token throughout this call.
            unsafe {
                let token = hew_runtime::coro_state::hew_coro_state_token(state.cast());
                hew_runtime::cancel_token::hew_cancel_token_cancel(
                    token,
                    hew_runtime::fault::HEW_FAULT_CANCELLED,
                );
            }
            2
        } else {
            0
        }
    }
    unsafe extern "C-unwind" fn release_vector(vector: *mut std::ffi::c_void) {
        VECTOR_RELEASES.set(VECTOR_RELEASES.get() + 1);
        // SAFETY: scalar-vector cleanup is synchronous and transfers one live
        // owner to the consuming walk, including cancellation cleanup.
        unsafe { hew_runtime::vec::hew_vec_free_owned_walk(vector.cast()) };
    }
    let semantic = lower_source(
        r"
        fn short(limit: i64) -> i64 {
            var values: Vec<i64> = Vec.new();
            values.push(0);
            values.push(1);
            values.push(2);
            var sum: i64 = 0;
            for index in 0..limit { sum += values[index]; }
            sum
        }
        fn main() -> i64 {
            var sum: i64 = 0;
            for _ in 0..7 { sum += short(3); }
            sum
        }
        ",
    );
    let triple = native_emission_triple();
    let target = physical_target_for_inventory(
        &triple,
        &hew_mir::physical::physical_type_inventory(&semantic),
    )
    .unwrap();
    let verified = hew_mir::lower_physical_module(&semantic, target).unwrap();
    // One main entry, eight outer headers, and seven calls each taking one
    // entry plus four inner headers: 44 ticks, even when a chunk ends early.
    let ticks = 44;
    for opt in [OptLevel::O0, OptLevel::O2] {
        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, opt).unwrap();
        let llvm = build_module(&ctx, verified.module(), "shared_loop_budget", &machine).unwrap();
        crate::llvm::run_module_pipeline(&llvm, &machine, opt).unwrap();
        let engine = llvm
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        super::test_runtime::bind_runtime(&llvm, &engine);
        for (name, address) in [
            ("hew_coro_safepoint_budget", countdown as *const () as usize),
            ("hew_coro_safepoint", poll as *const () as usize),
            (
                "hew_vec_free_owned_walk",
                release_vector as *const () as usize,
            ),
        ] {
            engine.add_global_mapping(
                &llvm
                    .get_function(name)
                    .unwrap_or_else(|| panic!("missing runtime boundary {name}")),
                address,
            );
        }
        type Main = unsafe extern "C" fn(*mut i64, *mut *mut hew_runtime::fault::HewFault) -> i32;
        // SAFETY: the generated main wrapper has the physical result/fault ABI.
        let main = unsafe { engine.get_function::<Main>("__hew_main_body").unwrap() };
        for initial in [0, 1, 2, 17, 4000] {
            BUDGET.set(initial);
            POLLS.set(0);
            let mut result = 0;
            let mut fault = std::ptr::null_mut();
            // SAFETY: output carriers remain live through the synchronous root.
            assert_eq!(unsafe { main.call(&raw mut result, &raw mut fault) }, 0);
            assert_eq!(result, 21);
            assert!(fault.is_null());
            let polls = u32::from(initial != 0 && initial <= ticks);
            assert_eq!(POLLS.get(), polls, "{opt:?}, initial={initial}");
            assert_eq!(
                BUDGET.get(),
                (initial + polls * hew_mir::SAFEPOINT_BUDGET).wrapping_sub(ticks),
                "{opt:?}, initial={initial}"
            );
        }
        SUPPRESSED.set(true);
        BUDGET.set(17);
        POLLS.set(0);
        let mut result = 0;
        let mut fault = std::ptr::null_mut();
        // SAFETY: output carriers remain live through the synchronous root.
        assert_eq!(unsafe { main.call(&raw mut result, &raw mut fault) }, 0);
        assert_eq!(result, 21);
        assert!(fault.is_null());
        assert_eq!(BUDGET.get(), 17, "suppression leaves the budget untouched");
        assert_eq!(POLLS.get(), 0);
        SUPPRESSED.set(false);

        // Main entry, outer header and callee entry consume three ticks.
        // Cancel on the next scan header after one element, with its vector
        // owner and private recurrence live across the chunk's budget exit.
        BUDGET.set(5);
        POLLS.set(0);
        VECTOR_RELEASES.set(0);
        CANCELLING.set(true);
        let mut result = -77;
        let mut fault = std::ptr::null_mut();
        // SAFETY: output carriers remain live through the synchronous root.
        assert_eq!(
            unsafe { main.call(&raw mut result, &raw mut fault) },
            hew_runtime::fault::HEW_FAULT_CANCELLED
        );
        assert_eq!(result, -77, "cancelled scans must not publish a result");
        assert!(!fault.is_null());
        assert_eq!(POLLS.get(), 1);
        assert_eq!(
            VECTOR_RELEASES.get(),
            1,
            "cancellation releases the live vector once"
        );
        // SAFETY: the cancelled call published one owned fault.
        unsafe {
            hew_runtime::fault::hew_fault_drop(fault);
        }
        CANCELLING.set(false);
    }
}

#[test]
fn vector_loop_chunks_preserve_empty_scans_bounds_and_overflow_at_o0_o2() {
    let semantic = lower_source(
        r"
        fn probe(first: i64, limit: i64) -> i64 {
            var values: Vec<i64> = Vec.new();
            values.push(first);
            values.push(1);
            var sum: i64 = 0;
            for index in 0..limit { sum += values[index]; }
            sum
        }
        fn main() -> i64 { probe(2, 2) }
        ",
    );
    let triple = native_emission_triple();
    let target = physical_target_for_inventory(
        &triple,
        &hew_mir::physical::physical_type_inventory(&semantic),
    )
    .unwrap();
    let verified = hew_mir::lower_physical_module(&semantic, target).unwrap();
    for opt in [OptLevel::O0, OptLevel::O2] {
        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, opt).unwrap();
        let llvm = build_module(&ctx, verified.module(), "vector_loop_bounds", &machine).unwrap();
        crate::llvm::run_module_pipeline(&llvm, &machine, opt).unwrap();
        let engine = llvm
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        super::test_runtime::bind_runtime(&llvm, &engine);
        type Probe =
            unsafe extern "C" fn(i64, i64, *mut i64, *mut *mut hew_runtime::fault::HewFault) -> i32;
        // SAFETY: the source function has two i64 arguments and the verified
        // private result/fault ABI. All output slots live through each call.
        let probe = unsafe { engine.get_function::<Probe>("__hew_fn_probe").unwrap() };
        for (first, limit, expected, status) in [
            (2, 0, 0, 0),
            (2, 2, 3, 0),
            (2, 3, 0, HEW_TRAP_INDEX_OUT_OF_BOUNDS),
            (i64::MAX, 2, 0, HEW_TRAP_INTEGER_OVERFLOW),
        ] {
            let mut result = -77;
            let mut fault = std::ptr::null_mut();
            // SAFETY: the output carriers remain live through the synchronous root.
            assert_eq!(
                unsafe { probe.call(first, limit, &raw mut result, &raw mut fault) },
                status,
                "{opt:?}, first={first}, limit={limit}"
            );
            if status == 0 {
                assert_eq!(result, expected);
                assert!(fault.is_null());
            } else {
                assert_eq!(result, -77, "failed scans must not publish a result");
                assert!(!fault.is_null());
                // SAFETY: a failed call publishes one owned runtime fault.
                unsafe {
                    hew_runtime::fault::hew_fault_drop(fault);
                }
            }
        }
    }
}

#[test]
fn active_operation_root_uses_the_split_coroutine_owner_slot() {
    use std::cell::Cell;
    use std::ffi::c_void;

    thread_local! {
        static FRAME_BYTES: Cell<usize> = const { Cell::new(0) };
    }
    unsafe extern "C" fn allocate(size: u64) -> *mut c_void {
        FRAME_BYTES.set(usize::try_from(size).unwrap());
        // SAFETY: the coroutine requests its complete frame allocation.
        unsafe { hew_runtime::cont::hew_cont_frame_alloc(size) }
    }

    for opt in [OptLevel::O0, OptLevel::O2] {
        let ctx = Context::create();
        let llvm = ctx.create_module("operation_root");
        let triple = native_emission_triple();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, opt).unwrap();
        llvm.set_data_layout(&machine.get_target_data().get_data_layout());
        let pointer = ctx.ptr_type(AddressSpace::default());
        let function =
            llvm.add_function("probe", pointer.fn_type(&[pointer.into(); 2], false), None);
        let builder = ctx.create_builder();
        builder.position_at_end(ctx.append_basic_block(function, "entry"));
        let state = function.get_nth_param(0).unwrap().into_pointer_value();
        let owner = function.get_nth_param(1).unwrap().into_pointer_value();
        let frame = coro::begin(
            &ctx,
            &machine.get_target_data(),
            &llvm,
            &builder,
            function,
            state,
        )
        .unwrap();
        let descriptor = llvm.add_global(ctx.i8_type(), None, "descriptor");
        descriptor.set_initializer(&ctx.i8_type().const_zero());
        frame
            .bind_operation(&ctx, &builder, descriptor.as_pointer_value(), owner)
            .unwrap();
        let done = ctx.append_basic_block(function, "done");
        frame.suspend(&ctx, &builder, done, done).unwrap();
        builder.position_at_end(done);
        frame.clear_operation(&ctx, &builder).unwrap();
        builder.build_unconditional_branch(frame.finish).unwrap();
        llvm.verify().unwrap();
        crate::llvm::run_module_pipeline(&llvm, &machine, opt).unwrap();
        let engine = llvm
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        super::test_runtime::bind_runtime(&llvm, &engine);
        engine.add_global_mapping(
            &llvm.get_function("hew_cont_frame_alloc").unwrap(),
            allocate as *const () as usize,
        );
        let (_ready, wake) = hew_runtime::wake::blocking::Readiness::new();
        // SAFETY: the state, dummy operation and generated module outlive the
        // suspended frame. Inspect the registered slot before dereferencing it.
        unsafe {
            let state = hew_runtime::coro_state::hew_coro_state_new(
                wake.descriptor(),
                std::ptr::null_mut(),
            );
            let mut operation = 17_u64;
            let expected = (&raw mut operation).cast::<c_void>();
            let probe = engine
                .get_function::<unsafe extern "C" fn(
                    *mut hew_runtime::coro_state::HewCoroState,
                    *mut c_void,
                ) -> *mut c_void>("probe")
                .unwrap();
            let handle = probe.call(state, expected);
            let view = (*state.cast::<hew_cabi::coro::HewCoroInvocation>())
                .managed_frame
                .as_ref()
                .unwrap();
            let root = view.operation.as_ref().unwrap();
            let address = root.slot as usize;
            assert!(
                (handle as usize..handle as usize + FRAME_BYTES.get()).contains(&address),
                "{opt:?}: active operation slot must belong to the retained coroutine frame"
            );
            assert_eq!(root.slot.cast::<*mut c_void>().read(), expected);
            hew_runtime::cont::hew_cont_resume(handle);
            hew_runtime::cont::hew_cont_destroy(handle);
            hew_runtime::coro_state::hew_coro_state_free(state);
        }
    }
}
