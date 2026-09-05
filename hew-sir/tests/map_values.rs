use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, OwnKind, SemModule, SemOpKind, SemTerminator, SirLoweringStatus,
};
use hew_types::{
    module_registry::ModuleRegistry,
    runtime_call::{MapValueOp, SetValueOp},
    Checker, RuntimeCallFamily,
};

fn lower_source(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:#?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "checker: {:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "HIR: {:#?}", hir.diagnostics);
    let lowered = lower_module(&hir.module, &facts);
    assert!(
        lowered.statuses.iter().any(|status| {
            status.name == "main" && matches!(status.status, SirLoweringStatus::Lowered)
        }),
        "SIR: {:#?}",
        lowered.statuses
    );
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| { !matches!(status, SirLoweringStatus::Unsupported { .. }) }),
        "a demanded callable did not lower: {:#?}",
        lowered.callable_statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "verify: {:#?}\n{}",
        verify_module(&lowered.module),
        hew_sir::dump_sir(&lowered.module)
    );
    lowered.module
}

fn operation_families(module: &SemModule) -> Vec<RuntimeCallFamily> {
    module
        .functions
        .iter()
        .flat_map(|f| &f.blocks)
        .filter_map(|b| match b.terminator {
            SemTerminator::RtCall { family, .. } => Some(family),
            _ => None,
        })
        .collect()
}

#[test]
fn map_bindings_copies_calls_and_returns_share_semantic_value_contracts() {
    let module = lower_source(
        r#"
        fn keep(value: HashMap<i64, string>) -> HashMap<i64, string> { value }
        fn main() -> i64 {
            var original: HashMap<i64, string> = HashMap.new();
            original.insert(7, "first");
            let copied = original;
            let returned = keep(copied);
            original.clear();
            returned.len()
        }
    "#,
    );
    let families = operation_families(&module);
    for operation in [
        MapValueOp::New,
        MapValueOp::Insert,
        MapValueOp::Clear,
        MapValueOp::Len,
    ] {
        assert!(families.contains(&RuntimeCallFamily::Map(operation)));
    }
    assert!(module
        .functions
        .iter()
        .flat_map(|f| &f.blocks)
        .flat_map(|b| &b.ops)
        .any(|op| {
            matches!(op.kind, SemOpKind::CopyValue { .. }) && op.results[0].own == OwnKind::Owned
        }));
    assert!(
        module.aggregate_shapes.is_empty(),
        "ordinary maps must not need fabricated record shapes"
    );
}

#[test]
fn set_mutations_return_values_and_rebind_the_receiver() {
    let module = lower_source(
        r#"
        fn main() -> i64 {
            var values: HashSet<string> = HashSet.new();
            let inserted = values.insert("member");
            var copied = values;
            let removed = values.remove("member");
            copied.clear();
            if inserted && removed && !copied.contains("member") { values.len() } else { 9 }
        }
    "#,
    );
    let families = operation_families(&module);
    for operation in [
        SetValueOp::New,
        SetValueOp::Insert,
        SetValueOp::Remove,
        SetValueOp::Clear,
        SetValueOp::Contains,
        SetValueOp::Len,
    ] {
        assert!(families.contains(&RuntimeCallFamily::Set(operation)));
    }
    assert!(
        module
            .functions
            .iter()
            .flat_map(|f| &f.blocks)
            .flat_map(|b| &b.ops)
            .any(|op| {
                matches!(op.kind, SemOpKind::Destructure { .. })
                    && op.results.len() == 2
                    && op.results[0].own == OwnKind::Owned
                    && op.results[1].own == OwnKind::None
            }),
        "set mutation must transfer its receiver and Boolean result separately"
    );
}

#[test]
fn map_literal_uses_mutable_semantic_construction() {
    let module = lower_source(
        r#"
        fn main() -> i64 {
            let values = {"one": 1, "two": 2};
            values.len()
        }
    "#,
    );
    let families = operation_families(&module);
    assert!(families.contains(&RuntimeCallFamily::Map(MapValueOp::New)));
    assert_eq!(
        families
            .iter()
            .filter(|family| **family == RuntimeCallFamily::Map(MapValueOp::Insert))
            .count(),
        2
    );
}

#[test]
fn permanent_map_value_copy() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-value-copy.hew"
    ));
}

#[test]
fn permanent_map_scalar_growth_remove() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-scalar-growth-remove.hew"
    ));
}

#[test]
fn permanent_map_owned_projections() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-owned-projections.hew"
    ));
}

#[test]
fn permanent_map_record_key() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-record-key.hew"
    ));
}

#[test]
fn permanent_map_index_fault() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-index-fault.hew"
    ));
}

#[test]
fn permanent_set_value_copy() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/set-value-copy.hew"
    ));
}

#[test]
fn permanent_selected_key_methods() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-selected-key-methods.hew"
    ));
}

#[test]
fn permanent_composite_key_equality() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-composite-key-equality.hew"
    ));
}

#[test]
fn permanent_hash_callback_fault() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-hash-callback-fault.hew"
    ));
}

#[test]
fn permanent_eq_callback_fault() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-eq-callback-fault.hew"
    ));
}

#[test]
fn permanent_index_callback_fault() {
    lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-index-callback-fault.hew"
    ));
}

#[test]
fn collection_clone_and_set_emptiness_compose_without_new_runtime_operations() {
    let module = lower_source(
        r#"
        type Holder { values: HashMap<i64, Vec<string>> }
        fn main() -> i64 {
            var values: HashMap<i64, Vec<string>> = HashMap.new();
            values.insert(7, ["kept"]);
            let holder = Holder { values: values };
            let cloned = holder.values.clone();
            var members: HashSet<string> = HashSet.new();
            let before = members.is_empty();
            members.insert("first");
            let copied = members.clone();
            members.clear();
            if before && members.is_empty() && !copied.is_empty() && cloned.contains_key(7) {
                cloned[7].len()
            } else { 0 }
        }
    "#,
    );
    let families = operation_families(&module);
    assert!(families.contains(&RuntimeCallFamily::Map(MapValueOp::ContainsKey)));
    assert_eq!(
        families
            .iter()
            .filter(|family| **family == RuntimeCallFamily::Set(SetValueOp::Len))
            .count(),
        3
    );
    assert!(
        !families.iter().any(|family| matches!(
            family,
            RuntimeCallFamily::HashMapCloneLayout | RuntimeCallFamily::HashSetCloneLayout
        )),
        "explicit clones must use semantic copies, not old ABI operations"
    );
    let operations: Vec<_> = module
        .functions
        .iter()
        .flat_map(|f| &f.blocks)
        .flat_map(|b| &b.ops)
        .collect();
    let field_loan = operations
        .iter()
        .find(|op| matches!(op.kind, SemOpKind::AggregateProjectBorrow { .. }))
        .expect("cloning the map field must borrow its parent")
        .results[0]
        .id;
    assert!(
        operations.iter().any(|op| {
            matches!(&op.kind, SemOpKind::CopyValue { source } if source.value == field_loan)
                && op.results[0].own == OwnKind::Owned
        }),
        "a borrowed field clone must produce an independent owner"
    );
}

#[test]
fn map_lookup_borrows_a_field_and_preserves_the_fault_after_ending_its_loan() {
    let module = lower_source(
        r#"
        type Holder { values: HashMap<i64, string> }
        fn main() -> i64 {
            var values: HashMap<i64, string> = HashMap.new();
            values.insert(1, "present");
            let holder = Holder { values: values };
            holder.values[9].len()
        }
    "#,
    );
    let main = module.functions.iter().find(|f| f.name == "main").unwrap();
    let (borrowed, fault) = main
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::RtCall {
                family: RuntimeCallFamily::Map(MapValueOp::Index),
                args,
                result: hew_sir::CallResult::Value(result),
                unwind: hew_sir::CallUnwind::Cleanup(edge),
                ..
            } => {
                assert_eq!(
                    result.own,
                    OwnKind::Owned,
                    "lookup returns an independent value"
                );
                Some((args[0].operand.value, edge.target))
            }
            _ => None,
        })
        .unwrap();
    let parent = main
        .blocks
        .iter()
        .flat_map(|b| &b.ops)
        .find_map(|op| match &op.kind {
            SemOpKind::AggregateProjectBorrow { aggregate, .. } if op.results[0].id == borrowed => {
                Some(aggregate.value)
            }
            _ => None,
        })
        .unwrap();
    let fault = main.blocks.iter().find(|b| b.id == fault).unwrap();
    assert!(
        matches!(fault.ops[0].kind, SemOpKind::EndBorrow { ref borrow } if borrow.value == borrowed)
    );
    assert_eq!(
        fault
            .ops
            .iter()
            .filter(
                |op| matches!(&op.kind, SemOpKind::DestroyValue { value } if value.value == parent)
            )
            .count(),
        1,
        "the failure edge must destroy the containing owner after ending its loan"
    );
    assert!(matches!(fault.terminator, SemTerminator::ResumeUnwind));

    let fault_id = fault.id;
    let mut replaced = module.clone();
    replaced
        .functions
        .iter_mut()
        .find(|function| function.name == "main")
        .unwrap()
        .blocks
        .iter_mut()
        .find(|block| block.id == fault_id)
        .unwrap()
        .terminator = SemTerminator::Trap {
        kind: hew_sir::TrapKind::IndexOutOfBounds,
    };
    assert!(verify_module(&replaced).iter().any(|diagnostic| matches!(
        diagnostic.kind,
        hew_sir::SirDiagnosticKind::FaultLifetime { .. }
    )));
}

#[test]
fn set_to_vec_is_a_semantic_owned_projection() {
    let module = lower_source(
        r#"
        fn main() -> i64 {
            var values: HashSet<string> = HashSet.new();
            values.insert("kept");
            let snapshot = values.to_vec();
            values.clear();
            snapshot[0].len()
        }
    "#,
    );
    assert!(operation_families(&module).contains(&RuntimeCallFamily::Set(SetValueOp::Elements)));
}

#[test]
fn map_projections_and_optional_results_keep_owned_values_after_mutation() {
    let module = lower_source(
        r#"
        fn main() -> i64 {
            var values: HashMap<i64, string> = HashMap.new();
            values.insert(7, "kept");
            let keys = values.keys();
            let snapshot = values.values();
            let entries = values.entries();
            let found = values.get(7);
            let removed = values.remove(7);
            let missing = values.remove(8);
            values.clear();
            let first = match found { .Some(value) => value.len(), .None => 0 };
            let second = match removed { .Some(value) => value.len(), .None => 0 };
            let absent = match missing { .Some(value) => value.len(), .None => 1 };
            first + second + absent + keys.len() + snapshot[0].len() + entries.len()
        }
    "#,
    );
    let families = operation_families(&module);
    for operation in [
        MapValueOp::Keys,
        MapValueOp::Values,
        MapValueOp::Entries,
        MapValueOp::Get,
        MapValueOp::Remove,
    ] {
        assert!(families.contains(&RuntimeCallFamily::Map(operation)));
    }
}

#[test]
fn nested_set_mutations_preserve_the_rebound_owner() {
    let module = lower_source(
        r#"
        fn observe(value: bool) -> i64 { if value { 1 } else { 0 } }
        fn main() -> i64 {
            var source: HashSet<string> = HashSet.new();
            let value = "member".to_upper();
            var count = observe(source.insert(value));
            count += observe(source.insert(value));
            var copy = source;
            count += observe(source.remove(value));
            count += observe(source.contains(value));
            count += observe(copy.contains(value));
            copy.clear();
            count + value.len() + copy.len()
        }
    "#,
    );
    assert!(operation_families(&module).contains(&RuntimeCallFamily::Set(SetValueOp::Remove)));
}

#[test]
fn set_iteration_uses_the_semantic_elements_projection() {
    let module = lower_source(
        r#"
        fn main() -> i64 {
            var values: HashSet<string> = HashSet.new();
            values.insert("kept");
            var count = 0;
            for value in values { count += value.len(); }
            values.clear();
            count
        }
    "#,
    );
    assert!(operation_families(&module).contains(&RuntimeCallFamily::Set(SetValueOp::Elements)));
}

#[test]
fn map_emptiness_uses_the_semantic_length_operation() {
    let module = lower_source(
        r#"
        fn main() -> i64 {
            var values: HashMap<i64, string> = HashMap.new();
            let before = values.is_empty();
            values.insert(7, "kept");
            if before && !values.is_empty() { 1 } else { 0 }
        }
    "#,
    );
    assert!(operation_families(&module).contains(&RuntimeCallFamily::Map(MapValueOp::Len)));
}

#[test]
fn collection_keys_demand_selected_methods_even_without_direct_source_calls() {
    use hew_types::{ValueCapability, ValueMethodPlan};
    let module = lower_source(
        r#"
        type Key { id: i64 }
        impl Hash for Key { fn hash(self) -> i64 { self.id % 10 } }
        impl Eq for Key { fn eq(self, other: Key) -> bool { self.id % 10 == other.id % 10 } }
        type Outer { key: Key }
        fn main() -> i64 {
            var values: HashMap<Outer, string> = HashMap.new();
            values.insert(Outer { key: Key { id: 7 } }, "kept");
            values.len()
        }
    "#,
    );
    let mut selected = Vec::new();
    for capability in [ValueCapability::Hash, ValueCapability::Eq] {
        let (key, plan) = module
            .value_capabilities
            .iter()
            .find(|((ty, op), _)| ty.user_facing().to_string() == "Key" && *op == capability)
            .expect("derived outer operation must select the field override");
        let ValueMethodPlan::User {
            method: declaration,
            type_args,
        } = plan.selection.plan()
        else {
            panic!("must preserve the user implementation");
        };
        let callable = plan.callable.expect("selected executable body");
        assert!(type_args.is_empty());
        assert_eq!(&module.callable(callable).unwrap().declaration, declaration);
        assert!(
            module.function_index().function(callable).is_some(),
            "callback body must be demanded"
        );
        selected.push((key.clone(), callable));
    }
    assert_ne!(selected[0].1, selected[1].1);

    let mut missing = module.clone();
    missing.value_capabilities.remove(&selected[0].0);
    assert!(
        verify_module(&missing).iter().any(|diagnostic| matches!(
            diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidValueCapability { .. }
        )),
        "a derived operation must not silently substitute for a missing field plan"
    );

    let mut mismatched = module.clone();
    mismatched
        .value_capabilities
        .get_mut(&selected[0].0)
        .unwrap()
        .callable = Some(selected[1].1);
    assert!(
        verify_module(&mismatched).iter().any(|diagnostic| matches!(
            diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidValueCapability { .. }
        )),
        "a selected method must retain its exact callable identity"
    );
}

#[test]
fn collection_keys_demand_the_exact_generic_impl_specialization() {
    use hew_sir::CallableInstance;
    use hew_types::ValueMethodPlan;
    let module = lower_source(
        r#"
        type Key<T> { value: T }
        impl<T> Hash for Key<T> { fn hash(self) -> i64 { 1 } }
        fn main() -> i64 {
            var values: HashMap<Key<i64>, string> = HashMap.new();
            values.insert(Key { value: 7 }, "kept");
            values.len()
        }
    "#,
    );
    let plan = module
        .value_capabilities
        .iter()
        .find_map(|((_, capability), plan)| {
            (*capability == hew_types::ValueCapability::Hash
                && matches!(plan.selection.plan(), ValueMethodPlan::User { .. }))
            .then_some(plan)
        })
        .expect("generic key hash implementation");
    let ValueMethodPlan::User {
        method: declaration,
        type_args,
    } = plan.selection.plan()
    else {
        unreachable!()
    };
    let callable = plan.callable.expect("selected executable body");
    assert_eq!(type_args, &[hew_types::ResolvedTy::I64]);
    let selected = module.callable(callable).unwrap();
    let CallableInstance::Generic(instance) = &selected.instance else {
        panic!("concrete specialization")
    };
    assert_eq!(&instance.template.declaration, declaration);
    assert_eq!(&instance.type_args, type_args);
    assert!(module.function_index().function(callable).is_some());
}

#[test]
fn selected_key_capabilities_reject_forged_evidence_and_compatible_substitutes() {
    use hew_types::{ValueCapability, ValueMethodPlan};
    let module = lower_source(
        r"
        type Key { id: i64 }
        impl Hash for Key { fn hash(self) -> i64 { self.id % 10 } }
        fn other_hash(value: Key) -> i64 { value.id }
        fn main() -> i64 {
            let values: HashMap<Key, string> = HashMap.new();
            values.len() + other_hash(Key { id: 7 })
        }
        ",
    );
    let (user_key, user) = module
        .value_capabilities
        .iter()
        .find(|(_, plan)| matches!(plan.selection.plan(), ValueMethodPlan::User { .. }))
        .unwrap();
    let alternative = module
        .callables
        .iter()
        .find(|callable| callable.symbol.ends_with("other_hash"))
        .unwrap();
    let selected = module.callable(user.callable.unwrap()).unwrap();
    assert_eq!(selected.signature, alternative.signature);
    let derived_key = (user_key.0.clone(), ValueCapability::Eq);
    let derived = &module.value_capabilities[&derived_key];
    assert!(matches!(derived.selection.plan(), ValueMethodPlan::Derived));

    for mutation in 0..5 {
        let mut altered = module.clone();
        match mutation {
            0 => {
                altered
                    .value_capabilities
                    .get_mut(user_key)
                    .unwrap()
                    .callable = Some(alternative.id);
            }
            1 => {
                altered
                    .value_capabilities
                    .get_mut(user_key)
                    .unwrap()
                    .callable = None;
            }
            2 => {
                let plan = altered.value_capabilities.get_mut(user_key).unwrap();
                plan.selection = derived.selection.clone();
                plan.callable = None;
            }
            3 => {
                altered
                    .value_capabilities
                    .get_mut(&derived_key)
                    .unwrap()
                    .callable = user.callable;
            }
            4 => {
                let scalar = module
                    .value_capabilities
                    .get(&(hew_types::ResolvedTy::I64, ValueCapability::Eq))
                    .unwrap();
                altered
                    .value_capabilities
                    .get_mut(&derived_key)
                    .unwrap()
                    .selection = scalar.selection.clone();
            }
            _ => unreachable!(),
        }
        assert!(
            verify_module(&altered).iter().any(|diagnostic| matches!(
                diagnostic.kind,
                hew_sir::SirDiagnosticKind::InvalidValueCapability { .. }
            )),
            "forged capability evidence must be rejected (mutation {mutation})"
        );
    }
}

#[test]
fn collection_construction_requires_both_selected_key_operations() {
    use hew_types::{ResolvedTy, ValueCapability};
    for collection in ["HashMap<i64, string>", "HashSet<i64>"] {
        let module = lower_source(&format!(
            "fn main() -> i64 {{ let values: {collection} = {}.new(); values.len() }}",
            collection.split('<').next().unwrap()
        ));
        for capability in [ValueCapability::Hash, ValueCapability::Eq] {
            let mut missing = module.clone();
            missing
                .value_capabilities
                .remove(&(ResolvedTy::I64, capability));
            assert!(
                verify_module(&missing).iter().any(|diagnostic| matches!(
                    diagnostic.kind,
                    hew_sir::SirDiagnosticKind::InvalidValueCapability { .. }
                )),
                "construction requires the {capability:?} plan for {collection}"
            );
        }
    }
}

#[test]
fn borrowed_collection_reads_do_not_demand_key_callbacks() {
    let parsed = hew_parser::parse(
        r"
        type Key { id: i64 }
        impl Hash for Key { fn hash(self) -> i64 { self.id % 10 } }
        fn size(values: HashMap<Key, string>, keys: HashSet<Key>) -> i64 {
            values.len() + keys.len()
        }
        ",
    );
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:#?}", hir.diagnostics);
    let declaration = hir
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(function) if function.name == "size" => {
                Some(function.declaration.clone())
            }
            _ => None,
        })
        .unwrap();
    let lowered = hew_sir::lower_module_with_roots(&hir.module, &facts, &[declaration]).unwrap();
    let families = operation_families(&lowered.module);
    assert!(families.contains(&RuntimeCallFamily::Map(MapValueOp::Len)));
    assert!(families.contains(&RuntimeCallFamily::Set(SetValueOp::Len)));
    assert!(lowered.module.value_capabilities.is_empty());
    assert!(verify_module(&lowered.module).is_empty());
}

#[test]
fn zero_sized_keys_and_empty_entry_pairs_share_collection_contracts() {
    let module = lower_source(include_str!(
        "../../tests/core-acceptance/cases/map-zero-sized-keys.hew"
    ));
    let families = operation_families(&module);
    assert!(families.contains(&RuntimeCallFamily::Map(MapValueOp::Entries)));
    assert!(families.contains(&RuntimeCallFamily::Set(SetValueOp::Elements)));
}
