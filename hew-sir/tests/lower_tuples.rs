use hew_hir::{lower_program_host_target, ItemId, ResolutionCtx};
use hew_sir::{
    build_def_use, dump_sir, lower_module, verify_function, verify_module, BlockId, CallableId,
    CallableInstance, EffectSet, FunctionSourceOrigin, OpId, Operand, Provenance, SemBlock,
    SemFunction, SemOp, SemOpKind, SemTerminator, UseMode, ValueDef, ValueId,
};
use hew_types::{module_registry::ModuleRegistry, Checker, DefId, ResolvedTy};

fn lower_hir(source: &str) -> hew_hir::HirModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "source must parse before the SIR tuple lowering test: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let type_check_output = checker.check_program(&parsed.program);
    let hir = lower_program_host_target(&parsed.program, &type_check_output, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "source must lower to HIR before the SIR tuple lowering test: {:#?}",
        hir.diagnostics
    );
    hir.module
}

#[test]
fn immutable_scalar_tuple_lowering_keeps_aggregate_semantics_in_sir() {
    let hir = lower_hir(
        r"
        fn main() -> i64 {
            let pair = (0, 42);
            pair.0
        }
        ",
    );
    let lowered = lower_module(&hir);
    let entry = lowered
        .module
        .entry_callable
        .expect("root main must have a resolved SIR callable");
    let main = lowered
        .module
        .function_for_callable(entry)
        .expect("the tuple main must lower into SIR");

    assert!(
        verify_module(&lowered.module).is_empty(),
        "tuple SIR must verify before crossing to Raw MIR: {:#?}",
        verify_module(&lowered.module)
    );
    let ops = &main.blocks[0].ops;
    assert!(matches!(ops[0].kind, SemOpKind::ConstI64(0)));
    assert!(matches!(ops[1].kind, SemOpKind::ConstI64(42)));
    let tuple_value = ops[2]
        .results
        .first()
        .expect("tuple.make must define one semantic value");
    assert_eq!(
        tuple_value.ty,
        ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64])
    );
    assert!(matches!(
        &ops[2].kind,
        SemOpKind::TupleMake { elements }
            if elements.iter().map(|element| element.value).collect::<Vec<_>>()
                == vec![ops[0].results[0].id, ops[1].results[0].id]
                && elements.iter().all(|element| element.mode == UseMode::Read)
    ));
    assert!(matches!(
        &ops[3].kind,
        SemOpKind::TupleGet { tuple, index: 0 }
            if tuple.value == tuple_value.id && tuple.mode == UseMode::Read
    ));
    assert!(ops[2].kind.effects().is_pure());
    assert!(ops[3].kind.effects().is_pure());
    assert_eq!(
        build_def_use(main).use_count(tuple_value.id),
        1,
        "tuple.get must participate in the shared operand visitor/def-use index"
    );

    let dump = dump_sir(&lowered.module);
    assert_eq!(
        dump,
        concat!(
            "fn main() -> i64 {\n",
            "bb0:\n",
            "    %0 = const 0\n",
            "    %1 = const 42\n",
            "    %2 = tuple.make(%0, %1)\n",
            "    %3 = tuple.get %2, 0\n",
            "    return %3\n",
            "}\n"
        )
    );
    assert!(
        !dump.contains("offset") && !dump.contains("alloca") && !dump.contains("field.load"),
        "SIR tuple text must not expose representation details: {dump}"
    );
}

#[test]
fn generic_scalar_instances_substitute_tuple_values_before_raw_mir() {
    let hir = lower_hir(
        r"
        fn first<T>(left: T, right: T) -> T {
            let pair = (left, right);
            pair.0
        }

        fn main() -> i64 {
            first(7, 9)
        }
        ",
    );
    let lowered = lower_module(&hir);
    assert!(
        verify_module(&lowered.module).is_empty(),
        "the generic tuple instance must verify: {:#?}",
        verify_module(&lowered.module)
    );
    let instance = lowered
        .module
        .callables
        .iter()
        .find(|callable| matches!(callable.instance, CallableInstance::Generic(_)))
        .expect("main's generic call must request one concrete SIR instance");
    let function = lowered
        .module
        .function_for_callable(instance.id)
        .expect("the concrete generic instance must lower a SIR body");
    let tuple_make = function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find(|op| matches!(op.kind, SemOpKind::TupleMake { .. }))
        .expect("the concrete generic body must retain tuple.make");
    assert_eq!(
        tuple_make.results[0].ty,
        ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
        "SIR must specialize semantic types before Raw MIR chooses a layout"
    );
}

#[test]
fn nested_bitcopy_tuples_remain_abstract_through_projection() {
    let hir = lower_hir(
        r"
        fn main() -> i64 {
            let pair = (0, (41, 42));
            let inner = pair.1;
            inner.0
        }
        ",
    );
    let lowered = lower_module(&hir);
    assert!(
        verify_module(&lowered.module).is_empty(),
        "nested tuple SIR must verify: {:#?}",
        verify_module(&lowered.module)
    );
    let entry = lowered
        .module
        .entry_callable
        .expect("nested tuple main must be the entry callable");
    let main = lowered
        .module
        .function_for_callable(entry)
        .expect("nested tuple main must lower to SIR");
    let tuple_makes = main
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .filter(|op| matches!(op.kind, SemOpKind::TupleMake { .. }))
        .count();
    let tuple_gets = main
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .filter(|op| matches!(op.kind, SemOpKind::TupleGet { .. }))
        .count();
    assert_eq!(tuple_makes, 2);
    assert_eq!(tuple_gets, 2);
}

#[test]
fn tuple_verifier_rejects_non_tuple_construction_and_projection() {
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("malformed_tuple"),
        name: "malformed_tuple".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::default(),
        params: Vec::new(),
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: vec![
                SemOp {
                    id: OpId(0),
                    results: vec![ValueDef {
                        id: ValueId(0),
                        ty: ResolvedTy::I64,
                    }],
                    kind: SemOpKind::ConstI64(0),
                    provenance: Provenance::Synthesized,
                },
                SemOp {
                    id: OpId(1),
                    results: vec![ValueDef {
                        id: ValueId(1),
                        ty: ResolvedTy::I64,
                    }],
                    kind: SemOpKind::TupleMake {
                        elements: vec![Operand {
                            value: ValueId(0),
                            mode: UseMode::Read,
                        }],
                    },
                    provenance: Provenance::Synthesized,
                },
                SemOp {
                    id: OpId(2),
                    results: vec![ValueDef {
                        id: ValueId(2),
                        ty: ResolvedTy::I64,
                    }],
                    kind: SemOpKind::TupleGet {
                        tuple: Operand {
                            value: ValueId(0),
                            mode: UseMode::Read,
                        },
                        index: 0,
                    },
                    provenance: Provenance::Synthesized,
                },
            ],
            terminator: SemTerminator::Return {
                value: Some(Operand {
                    value: ValueId(2),
                    mode: UseMode::Read,
                }),
            },
        }],
    };

    let diagnostics = verify_function(&function);
    assert!(diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidOperation { op: OpId(1), reason }
                if reason.contains("tuple.make result must have a semantic tuple type")
        )
    }));
    assert!(diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidOperation { op: OpId(2), reason }
                if reason.contains("tuple.get operand has non-tuple semantic type")
        )
    }));
    assert_eq!(
        EffectSet::PURE,
        SemOpKind::TupleMake {
            elements: Vec::new()
        }
        .effects()
    );
}
