use std::collections::BTreeSet;

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{OwnKind, SemModule, SemOpKind, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// An explicit two-level loan fixture independent of call-site copy elision.
/// Source binding initializers demand real owning projections; replace those
/// projections and their normal/fault cleanup with the local loan contract.
pub(super) fn nested_borrow_module() -> SemModule {
    let parsed = hew_parser::parse(
        r#"
        type Inner { items: Vec<string>, }
        type Outer { inner: Inner, sibling: string, }
        fn main() -> i64 {
            let outer = Outer { inner: Inner { items: ["first", "second"] }, sibling: "keep" };
            let inner = outer.inner;
            let items = inner.items;
            items[0].len()
        }
        "#,
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let mut lowered = hew_sir::lower_module(&hir.module, &facts);
    assert!(lowered.statuses.iter().any(|status| {
        status.name == "main" && matches!(status.status, SirLoweringStatus::Lowered)
    }));
    for function in &mut lowered.module.functions {
        let mut loans = BTreeSet::new();
        for op in function.blocks.iter_mut().flat_map(|block| &mut block.ops) {
            if let SemOpKind::AggregateProjectCopy {
                shape,
                aggregate,
                field,
            } = &op.kind
            {
                assert_eq!(op.results[0].own, OwnKind::Owned);
                op.kind = SemOpKind::AggregateProjectBorrow {
                    shape: *shape,
                    aggregate: aggregate.clone(),
                    field: *field,
                };
                op.results[0].own = OwnKind::Guaranteed;
                loans.insert(op.results[0].id);
            }
        }
        assert_eq!(
            loans.len(),
            2,
            "fixture must exercise the nested field chain"
        );
        for op in function.blocks.iter_mut().flat_map(|block| &mut block.ops) {
            if let SemOpKind::DestroyValue { value } = &op.kind {
                if loans.contains(&value.value) {
                    op.kind = SemOpKind::EndBorrow {
                        borrow: value.clone(),
                    };
                }
            }
        }
    }
    let errors = hew_sir::verify_module(&lowered.module);
    assert!(errors.is_empty(), "{errors:#?}");
    lowered.module
}
