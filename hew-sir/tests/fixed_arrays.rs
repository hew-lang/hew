use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{lower_module, verify_module, SemOpKind, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_source(source: &str) -> hew_sir::SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let facts = Checker::new(ModuleRegistry::new(Vec::new())).check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:#?}", hir.diagnostics);
    let lowered = lower_module(&hir.module, &facts);
    assert!(
        lowered
            .statuses
            .iter()
            .any(|status| status.name == "main"
                && matches!(status.status, SirLoweringStatus::Lowered)),
        "{:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "{:#?}",
        verify_module(&lowered.module)
    );
    lowered.module
}

#[test]
fn fixed_array_construction_verifier_requires_exact_element_count() {
    let mut module =
        lower_source("fn main() -> i64 { let values: [i64; 3] = [1, 2, 3]; values[0] }");
    let operation = module
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .flat_map(|block| &mut block.ops)
        .find(|operation| matches!(operation.kind, SemOpKind::ArrayMake { .. }))
        .unwrap();
    let SemOpKind::ArrayMake { fields } = &mut operation.kind else {
        unreachable!()
    };
    fields.pop();
    let diagnostics = verify_module(&module);
    assert!(diagnostics.iter().any(|diagnostic| matches!(&diagnostic.kind, hew_sir::SirDiagnosticKind::InvalidOperation { reason, .. } if reason.contains("array.make elements"))), "{diagnostics:#?}");
}

#[test]
fn fixed_array_repeat_keeps_one_seed_independent_of_length() {
    for length in [3, 4_000_000] {
        let module = lower_source(&format!(
            "fn main() -> i64 {{ let values: [i64; {length}] = [7; {length}]; values[0] }}"
        ));
        let repeats = module
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .flat_map(|block| &block.ops)
            .filter(|operation| matches!(operation.kind, SemOpKind::ArrayRepeat { .. }))
            .collect::<Vec<_>>();
        assert_eq!(repeats.len(), 1);
        assert_eq!(
            repeats[0].results[0].ty,
            hew_types::ResolvedTy::Array(Box::new(hew_types::ResolvedTy::I64), length)
        );
        assert_eq!(repeats[0].results[0].own, hew_sir::OwnKind::Owned);
    }
}
