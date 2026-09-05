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
