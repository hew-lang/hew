//! Static trait calls retain checked method arguments until impl selection.

use hew_hir::{lower_program_host_target, HirItem, ResolutionCtx};
use hew_sir::{lower_module, verify_module, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn checked_hir(source: &str) -> (hew_hir::HirModule, hew_types::TypeCheckOutput) {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(Vec::new())).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let lowered = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
    (lowered.module, checked)
}

#[test]
fn generic_trait_method_instances_preserve_owned_values() {
    let (hir, checked) = checked_hir(include_str!(
        "../../tests/core-acceptance/cases/generic-trait-method-values.hew"
    ));
    let lowered = lower_module(&hir, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    let errors = verify_module(&lowered.module);
    assert!(
        errors.is_empty(),
        "{errors:?}\n{}",
        hew_sir::dump_sir(&lowered.module)
    );
}

#[test]
fn generic_trait_method_requires_its_checked_instantiation() {
    let source = r"
        trait Identity { fn keep<U>(self, consume value: U) -> U; }
        type Holder { value: i64 }
        impl Identity for Holder {
            fn keep<U>(self, consume value: U) -> U { value }
        }
        fn via<T: Identity>(item: T) -> i64 { item.keep(42) }
        fn main() -> i64 { via(Holder { value: 0 }) }
    ";
    let (hir, checked) = checked_hir(source);
    let valid = lower_module(&hir, &checked);
    assert!(
        valid
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        valid.callable_statuses
    );
    assert!(verify_module(&valid.module).is_empty());

    let site = hir
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "via" => {
                Some(function.body.tail.as_ref().expect("bound call").site)
            }
            _ => None,
        })
        .expect("generic caller");

    // The argument value and result still identify i64, but downstream stages
    // must not reconstruct a missing or malformed checker instantiation.
    for replacement in [None, Some(vec![ResolvedTy::I64, ResolvedTy::String])] {
        let mut broken = hir.clone();
        if let Some(arguments) = replacement {
            broken.call_site_type_args.insert(site, arguments);
        } else {
            assert!(broken.call_site_type_args.remove(&site).is_some());
        }
        let lowered = lower_module(&broken, &checked);
        assert!(
            lowered.callable_statuses.iter().any(|(_, status)| matches!(
                status,
                SirLoweringStatus::Unsupported { reason }
                    if reason.contains("requires 1 checker-resolved method type argument(s)")
            )),
            "{:?}",
            lowered.callable_statuses
        );
    }
}
