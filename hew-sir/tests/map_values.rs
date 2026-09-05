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
fn map_lookup_borrows_a_field_and_ends_the_loan_on_the_missing_key_edge() {
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
        "the missing-key edge must destroy the containing owner after ending its loan"
    );
    assert!(matches!(
        fault.terminator,
        SemTerminator::Trap {
            kind: hew_sir::TrapKind::IndexOutOfBounds
        }
    ));
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
