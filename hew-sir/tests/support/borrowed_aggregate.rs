use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{SemModule, SemOpKind, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// A temporary owner with a genuine two-level loan chain. Local aggregate
/// places borrow their leaf directly, so they cannot witness parent-loan rules.
pub(super) fn nested_borrow_module() -> SemModule {
    let module = lower_source(
        r#"
        type Inner { items: Vec<string>, }
        type Outer { inner: Inner, sibling: string, }
        fn main() -> i64 {
            (Outer { inner: Inner { items: ["first", "second"] }, sibling: "keep" }).inner.items[0].len()
        }
        "#,
    );
    assert_eq!(
        module.functions[0]
            .blocks
            .iter()
            .flat_map(|b| &b.ops)
            .filter(|op| matches!(op.kind, SemOpKind::AggregateProjectBorrow { .. }))
            .count(),
        2
    );
    module
}

pub(super) fn lower_source(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = hew_sir::lower_module(&hir.module, &facts);
    assert!(lowered.statuses.iter().any(|status| {
        status.name == "main" && matches!(status.status, SirLoweringStatus::Lowered)
    }));
    let errors = hew_sir::verify_module(&lowered.module);
    assert!(errors.is_empty(), "{errors:#?}");
    lowered.module
}
