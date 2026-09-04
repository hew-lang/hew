use hew_hir::{lower_program_host_target, ItemId, ResolutionCtx};
use hew_sir::{
    build_def_use, dump_sir, lower_module, verify_function, verify_module, BlockId,
    BoundaryDecision, BoundaryOperand, CallableId, CallableInstance, EffectSet,
    FunctionSourceOrigin, OpId, Operand, OwnKind, Provenance, SemBlock, SemFunction, SemOp,
    SemOpKind, SemTerminator, ValueDef, ValueId,
};
use hew_types::{module_registry::ModuleRegistry, Checker, DefId, ResolvedTy};

fn lower_hir(source: &str) -> (hew_hir::HirModule, hew_types::TypeCheckOutput) {
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
    (hir.module, type_check_output)
}

#[test]
fn immutable_scalar_tuple_lowering_keeps_aggregate_semantics_in_sir() {
    let (hir, type_facts) = lower_hir(
        r"
        fn main() -> i64 {
            let pair = (0, 42);
            pair.0
        }
        ",
    );
    let lowered = lower_module(&hir, &type_facts);
    let entry = lowered
        .module
        .entry_callable
        .expect("root main must have a resolved SIR callable");
    let main = lowered
        .module
        .function_index()
        .function(entry)
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
    ));
    assert!(matches!(
        &ops[3].kind,
        SemOpKind::TupleGet { tuple, index: 0 }
            if tuple.value == tuple_value.id
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
            "    return move %3\n",
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
    let (hir, type_facts) = lower_hir(
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
    let lowered = lower_module(&hir, &type_facts);
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
        .function_index()
        .function(instance.id)
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
    let (hir, type_facts) = lower_hir(
        r"
        fn main() -> i64 {
            let pair = (0, (41, 42));
            let inner = pair.1;
            inner.0
        }
        ",
    );
    let lowered = lower_module(&hir, &type_facts);
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
        .function_index()
        .function(entry)
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
                        own: OwnKind::None,
                    }],
                    kind: SemOpKind::ConstI64(0),
                    provenance: Provenance::Synthesized,
                },
                SemOp {
                    id: OpId(1),
                    results: vec![ValueDef {
                        id: ValueId(1),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    kind: SemOpKind::TupleMake {
                        elements: vec![Operand { value: ValueId(0) }],
                    },
                    provenance: Provenance::Synthesized,
                },
                SemOp {
                    id: OpId(2),
                    results: vec![ValueDef {
                        id: ValueId(2),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    kind: SemOpKind::TupleGet {
                        tuple: Operand { value: ValueId(0) },
                        index: 0,
                    },
                    provenance: Provenance::Synthesized,
                },
            ],
            terminator: SemTerminator::Return {
                value: Some(BoundaryOperand {
                    operand: Operand { value: ValueId(2) },
                    decision: BoundaryDecision::Move,
                }),
            },
        }],
        places: Vec::new(),
        bindings: Vec::new(),
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

/// §1.6: every source binding is recorded with the value it names, so a rule 2,
/// 3, 4 or 6 wall rooted in a user binding renders its `E_OWN_*` code rather
/// than `E_SIR_ICE`, and rule 6a has a mutability bit to read.
///
/// A binding is not a property of a value. `let alias = doubled` names the same
/// SSA value `doubled` names, so both bindings are present and both point at
/// it; recording the name on the definition kept only the first and every later
/// alias vanished. The counterfactual is in the same body: the values no
/// binding names carry no row, so a table where everything has a name says
/// nothing.
#[test]
fn every_binding_is_recorded_with_the_value_it_names() {
    let (hir, type_facts) = lower_hir(
        r"
        fn main() -> i64 {
            let seed = 7;
            let doubled = seed;
            let alias = doubled;
            let pair = (alias, (seed, seed));
            let taken = pair.0;
            taken
        }
        ",
    );
    let lowered = lower_module(&hir, &type_facts);
    let entry = lowered
        .module
        .entry_callable
        .expect("root main must have a resolved SIR callable");
    let twice = lowered
        .module
        .function_index()
        .function(entry)
        .expect("the binding fixture must lower into SIR");

    let names: Vec<&str> = twice
        .bindings
        .iter()
        .map(|binding| binding.name.as_str())
        .collect();
    assert_eq!(
        vec!["seed", "doubled", "alias", "pair", "taken"],
        names,
        "every binding in source order: {:#?}",
        twice.bindings
    );

    let value_of = |name: &str| {
        let binding = twice
            .bindings
            .iter()
            .find(|binding| binding.name == name)
            .unwrap_or_else(|| panic!("`{name}` must be recorded"));
        match binding.target {
            hew_sir::BindingTarget::Value(value) => value,
            hew_sir::BindingTarget::Place(place) => {
                panic!("`{name}` unexpectedly targets place {place:?}")
            }
        }
    };
    assert_eq!(
        value_of("seed"),
        value_of("doubled"),
        "an alias names the value its initializer already names"
    );
    assert_eq!(
        value_of("doubled"),
        value_of("alias"),
        "and so does an alias of an alias"
    );
    assert_eq!(
        "alias",
        twice
            .binding_naming(value_of("doubled"))
            .expect("the shared value has a binding")
            .name,
        "the most recent binding is the user-facing name"
    );
    assert!(
        twice.bindings.iter().all(|binding| !binding.mutable),
        "this lowering emits no mutable binding: {:#?}",
        twice.bindings
    );

    let bound_values: std::collections::BTreeSet<_> = twice
        .bindings
        .iter()
        .filter_map(|binding| match binding.target {
            hew_sir::BindingTarget::Value(value) => Some(value),
            hew_sir::BindingTarget::Place(_) => None,
        })
        .collect();
    let anonymous = twice
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .flat_map(|op| &op.results)
        .filter(|result| !bound_values.contains(&result.id))
        .count();
    assert!(
        anonymous > 0,
        "a lowering temp no binding names must stay unnamed: {twice:#?}"
    );
}

/// The module carries the §6.2 rows its own bodies mention, so a consumer of
/// SIR reads a decided class rather than recomputing one. An empty table is
/// the defect this replaces: rules 5, 6b and 6c and the layout fill all key on
/// it.
///
/// The counterfactual is the type the program never mentions: the projection
/// is closed under the components of the types this module holds, not a copy
/// of the checker's whole table.
#[test]
fn the_module_carries_the_rows_its_own_bodies_mention() {
    let (hir, type_facts) = lower_hir(
        r"
        fn main() -> i64 {
            let pair = (1, true);
            if pair.1 {
                pair.0
            } else {
                0
            }
        }
        ",
    );
    let lowered = lower_module(&hir, &type_facts);
    let rows = &lowered.module.type_facts;
    let key = |ty: ResolvedTy| hew_types::TypeInstanceKey(ty);

    assert!(
        rows.contains_key(&key(ResolvedTy::I64)),
        "a type the body holds must have a row: {rows:?}"
    );
    assert!(
        rows.contains_key(&key(ResolvedTy::Bool)),
        "a type the body holds must have a row: {rows:?}"
    );
    assert!(
        rows.contains_key(&key(ResolvedTy::Tuple(vec![
            ResolvedTy::I64,
            ResolvedTy::Bool
        ]))),
        "the tuple the body builds must have a row: {rows:?}"
    );
    assert!(
        !rows.contains_key(&key(ResolvedTy::String)),
        "a type this module never mentions must not be projected: {rows:?}"
    );
    assert_eq!(
        Some(hew_types::ValueClass::BitCopy),
        rows.get(&key(ResolvedTy::I64)).map(|row| row.class),
        "the row must carry the class the checker decided"
    );
}
