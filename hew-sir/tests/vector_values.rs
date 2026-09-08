use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BoundaryDecision, CallResult, CallUnwind, OwnKind, SemOpKind,
    SemTerminator, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker, RuntimeCallFamily, VecValueOp};

fn lower_source(source: &str) -> hew_sir::SemModule {
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
    let lowered = lower_module(&hir.module, &facts);
    assert!(
        lowered
            .statuses
            .iter()
            .filter(|status| status.name == "main")
            .all(|status| matches!(status.status, SirLoweringStatus::Lowered)),
        "SIR statuses: {:#?}",
        lowered.statuses
    );
    assert!(!lowered.module.functions.is_empty());
    assert!(
        verify_module(&lowered.module).is_empty(),
        "SIR errors: {:#?}\n{}",
        verify_module(&lowered.module),
        hew_sir::dump_sir(&lowered.module)
    );
    lowered.module
}

#[test]
fn ordinary_vector_values_share_one_operation_family() {
    for (declarations, element, value) in [
        ("", "i64", "42"),
        ("", "string", "\"hello\""),
        (
            "type Leaf { text: string, }",
            "Leaf",
            "Leaf { text: \"hello\" }",
        ),
        (
            "enum Item { Text(string), Empty, }",
            "Item",
            "Item.Text(\"hello\")",
        ),
        ("", "Vec<string>", "[\"inner\"]"),
    ] {
        let module = lower_source(&format!(
            r"{declarations}
            fn first(values: Vec<{element}>) -> {element} {{ values[0] }}
            fn main() -> i64 {{
                var values: Vec<{element}> = Vec.new();
                values.push({value});
                let independent = values;
                let extracted = first(values);
                values.push(values[0]);
                return independent.len();
            }}"
        ));
        let ops = module
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .filter_map(|block| match block.terminator {
                SemTerminator::RtCall {
                    family: RuntimeCallFamily::Vector(op),
                    ..
                } => Some(op),
                _ => None,
            })
            .collect::<Vec<_>>();
        for expected in [
            VecValueOp::New,
            VecValueOp::Push,
            VecValueOp::Len,
            VecValueOp::Index,
        ] {
            assert!(ops.contains(&expected), "{element}: missing {expected:?}");
        }
        let main = module
            .functions
            .iter()
            .find(|function| function.declaration.full_path() == "main")
            .unwrap();
        let local = |name| match main
            .bindings
            .iter()
            .find(|binding| binding.name == name)
            .unwrap()
            .target
        {
            hew_sir::BindingTarget::Place(place) => place,
            hew_sir::BindingTarget::Value(_) => {
                panic!("{element}: vector binding must own local storage")
            }
        };
        let source = local("values");
        let independent = local("independent");
        assert_ne!(source, independent);
        let copied = main
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .find_map(|op| match &op.kind {
                SemOpKind::StoreInit { place, value } if *place == independent => Some(value.value),
                _ => None,
            })
            .unwrap();
        assert!(
            main.blocks.iter().flat_map(|block| &block.ops).any(
                |op| matches!(op.kind, SemOpKind::LoadCopy { place } if place == source)
                    && op.results[0].id == copied
            ),
            "{element}: ordinary vector alias must copy into independent storage"
        );
    }
}

#[test]
fn optional_get_and_mutations_keep_receiver_writeback_explicit() {
    let module = lower_source(
        r#"
        fn main() -> i64 {
            var values = ["one", "two"];
            let absent = values.get(9);
            values.set(0, "replacement");
            let removed = values.pop();
            values.clear();
            let empty = values.pop();
            return values.len();
        }
    "#,
    );
    assert!(module.functions.iter().flat_map(|function| &function.blocks).any(|block| matches!(
        &block.terminator,
        SemTerminator::RtCall { family: RuntimeCallFamily::Vector(VecValueOp::Pop), args, result: CallResult::Value(value), unwind: CallUnwind::Cleanup(_), .. }
            if args[0].decision == BoundaryDecision::Move && value.own == OwnKind::Owned
                && matches!(&value.ty, hew_types::ResolvedTy::Tuple(fields) if fields.len() == 2)
    )));
    assert!(module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.ops)
        .any(|op| matches!(op.kind, SemOpKind::Destructure { .. })));
}

#[test]
fn nested_vector_demands_shapes_without_constructing_elements() {
    let module = lower_source(
        r"
        type Leaf { text: string, }
        enum Item { Entry(Leaf), Children(Vec<Leaf>), }
        fn main() -> i64 {
            let values: Vec<Vec<Item>> = Vec.new();
            return values.len();
        }
    ",
    );
    assert!(module.aggregate_shapes.iter().any(|shape| shape
        .instance
        .nominal
        .declaration()
        .full_path()
        == "Leaf"));
    assert!(module
        .variant_shapes
        .iter()
        .any(|shape| shape.enum_ty.user_facing().to_string() == "Item"));
}

#[test]
fn vector_mutation_counts_as_a_write_but_reads_do_not() {
    for method in ["push(3)", "set(0, 3)", "pop()", "clear()"] {
        let source =
            format!("fn main() -> i64 {{ var values = [1]; values.{method}; values.len() }}");
        let parsed = hew_parser::parse(&source);
        let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
        let facts = checker.check_program(&parsed.program);
        assert!(facts.errors.is_empty(), "{method}: {:?}", facts.errors);
        assert!(
            !facts
                .warnings
                .iter()
                .any(|warning| warning.kind == hew_types::error::TypeErrorKind::UnusedMut),
            "{method}: {:?}",
            facts.warnings
        );
        let immutable = source.replace("var values", "let values");
        let parsed = hew_parser::parse(&immutable);
        let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
        let facts = checker.check_program(&parsed.program);
        assert!(
            facts
                .errors
                .iter()
                .any(|error| error.kind == hew_types::error::TypeErrorKind::MutabilityError),
            "{method}: immutable receiver must be rejected"
        );
    }
    let parsed = hew_parser::parse("fn main() -> i64 { var values = [1]; values.len() }");
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts
        .warnings
        .iter()
        .any(|warning| warning.kind == hew_types::error::TypeErrorKind::UnusedMut));
}

#[test]
fn generic_vector_copy_and_early_return_keep_exact_elements() {
    let module = lower_source(
        r#"
        fn first<T>(values: Vec<T>) -> T { values[0] }
        fn collect(stop: bool) -> Vec<string> {
            var values: Vec<string> = Vec.new();
            for i in 0..3 {
                values.push("entry");
                if stop { return values; }
            }
            values
        }
        fn main() -> i64 {
            let values = collect(false);
            let text = first(values);
            return values.len();
        }
    "#,
    );
    assert!(module
        .functions
        .iter()
        .any(|function| function.declaration.full_path() == "first"));
}

fn index_module() -> hew_sir::SemModule {
    lower_source(
        r#"
        fn main() -> i64 {
            let wrong = 7;
            var values = ["one"];
            values.push("two");
            let text = values[0];
            return wrong;
        }
    "#,
    )
}

#[test]
fn verifier_rejects_a_wrong_element_even_with_the_right_arity() {
    let mut module = index_module();
    let function = module
        .functions
        .iter_mut()
        .find(|function| function.declaration.full_path() == "main")
        .unwrap();
    let wrong = function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find(|op| matches!(op.kind, SemOpKind::ConstInteger(7)))
        .unwrap()
        .results[0]
        .id;
    let push = function
        .blocks
        .iter_mut()
        .find_map(|block| match &mut block.terminator {
            SemTerminator::RtCall {
                family: RuntimeCallFamily::Vector(VecValueOp::Push),
                args,
                ..
            } => Some(args),
            _ => None,
        })
        .unwrap();
    push[1].operand.value = wrong;
    assert!(verify_module(&module).iter().any(|diagnostic| matches!(&diagnostic.kind,
        hew_sir::SirDiagnosticKind::InvalidOperation { reason, .. } if reason.contains("runtime argument 1"))));
}

#[test]
fn verifier_rejects_an_interior_borrow_disguised_as_extracted_data() {
    let mut module = index_module();
    let result = module
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .find_map(|block| match &mut block.terminator {
            SemTerminator::RtCall {
                family: RuntimeCallFamily::Vector(VecValueOp::Index),
                result: CallResult::Value(result),
                ..
            } => Some(result),
            _ => None,
        })
        .unwrap();
    result.own = OwnKind::Guaranteed;
    assert!(verify_module(&module).iter().any(|diagnostic| matches!(&diagnostic.kind,
        hew_sir::SirDiagnosticKind::InvalidOperation { reason, .. } if reason.contains("expected `string`/Owned"))));
}

#[test]
fn verifier_requires_index_failure_and_its_owner_cleanup() {
    let valid = index_module();
    let mut missing_edge = valid.clone();
    let unwind = missing_edge
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .find_map(|block| match &mut block.terminator {
            SemTerminator::RtCall {
                family: RuntimeCallFamily::Vector(VecValueOp::Index),
                unwind,
                ..
            } => Some(unwind),
            _ => None,
        })
        .unwrap();
    *unwind = CallUnwind::NotApplicable;
    assert!(verify_module(&missing_edge).iter().any(|diagnostic| matches!(&diagnostic.kind,
        hew_sir::SirDiagnosticKind::InvalidTerminator { reason } if reason.contains("exact failure set"))));

    let mut missing_cleanup = valid;
    let function = missing_cleanup
        .functions
        .iter_mut()
        .find(|function| function.declaration.full_path() == "main")
        .unwrap();
    let hew_sir::BindingTarget::Place(owner) = function
        .bindings
        .iter()
        .find(|binding| binding.name == "values")
        .unwrap()
        .target
    else {
        panic!("vector must own local storage")
    };
    let failure = function
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::RtCall {
                family: RuntimeCallFamily::Vector(VecValueOp::Index),
                unwind: CallUnwind::Cleanup(edge),
                ..
            } => Some(edge.target),
            _ => None,
        })
        .unwrap();
    let failure = match &function.blocks[failure.0 as usize].terminator {
        SemTerminator::CheckedRaiseFault { cleanup, .. } => cleanup.target,
        other => panic!("index failure must create the fault before cleanup: {other:?}"),
    };
    let mut pending = vec![failure];
    let mut seen = std::collections::BTreeSet::new();
    let (cleanup, destroy) =
        loop {
            let id = pending.pop().expect("failure must reach owner cleanup");
            if !seen.insert(id) {
                continue;
            }
            let block = &function.blocks[id.0 as usize];
            if let Some(index) = block.ops.iter().position(
                |op| matches!(op.kind, SemOpKind::EndLifetime { place } if place == owner),
            ) {
                break (id, index);
            }
            block
                .terminator
                .visit_successors(|edge| pending.push(edge.target));
        };
    function.blocks[cleanup.0 as usize].ops.remove(destroy);
    assert!(
        verify_module(&missing_cleanup).iter().any(|diagnostic| matches!(diagnostic.kind,
            hew_sir::SirDiagnosticKind::PlaceLifetime { place, reason, .. } if place == owner && reason == "local storage remains active at exit")),
        "dropping failure cleanup must invalidate the owner graph"
    );
}

#[test]
fn verifier_requires_recursive_vector_shape_and_type_facts() {
    let valid = lower_source(
        r"
        type Leaf { text: string, }
        fn main() -> i64 { let values: Vec<Leaf> = Vec.new(); values.len() }
    ",
    );
    let mut missing_shape = valid.clone();
    missing_shape.aggregate_shapes.clear();
    assert!(verify_module(&missing_shape)
        .iter()
        .any(|diagnostic| matches!(
            diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidCollectionType { .. }
        )));
    let mut missing_fact = valid;
    missing_fact
        .type_facts
        .remove(&hew_types::TypeInstanceKey(hew_types::ResolvedTy::String));
    assert!(verify_module(&missing_fact)
        .iter()
        .any(|diagnostic| matches!(
            diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidCollectionType { .. }
        )));
}

#[test]
fn affine_vector_accepts_nested_non_clone_values_without_copy_capability() {
    let module = lower_source(
        r"
        type Holder { callbacks: Vec<fn() -> i64>, }
        fn main() -> i64 {
            var values: Vec<Holder> = [];
            let holder = Holder { callbacks: [] };
            values.push(holder);
            return values.len();
        }
        ",
    );
    let pushed = module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .find_map(|block| match &block.terminator {
            SemTerminator::RtCall {
                family: RuntimeCallFamily::Vector(VecValueOp::Push),
                args,
                ..
            } => Some(args),
            _ => None,
        })
        .unwrap();
    assert_eq!(pushed[1].decision, hew_sir::BoundaryDecision::Move);
    assert!(module
        .type_facts
        .iter()
        .filter(|(key, _)| hew_types::runtime_call::vector_element_type(&key.0).is_some())
        .all(|(_, facts)| facts.clone == hew_types::CloneKind::None));
}

#[test]
fn selected_value_close_rejects_an_owner_used_as_its_index() {
    let mut module = lower_source(
        r"
        gen fn numbers() -> i64 { yield 1; }
        fn main() {
            var values: Vec<Generator<i64, ()>> = [];
            values.push(numbers());
            values.set(0, numbers());
        }
        ",
    );
    let inputs = module
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .find_map(|block| match &mut block.terminator {
            SemTerminator::Suspend {
                kind:
                    hew_sir::SuspendKind::ValueClose {
                        selection: hew_sir::ValueCloseSelection::VectorElement,
                        ..
                    },
                inputs,
                ..
            } => Some(inputs),
            _ => None,
        })
        .expect("replacement must close its selected element");
    inputs[1].operand.value = inputs[0].operand.value;
    assert!(verify_module(&module)
        .iter()
        .any(|diagnostic| format!("{diagnostic:?}")
            .contains("no matching input/result/resume contract")));
}
