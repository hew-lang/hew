//! Scalar patterns preserve checked widths, source ordering and match ownership.

use hew_hir::{
    lower_program_host_target, HirExprKind, HirItem, HirMatchArmPredicate, HirModule, ResolutionCtx,
};
use hew_sir::{lower_module, verify_module, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

const SOURCE: &str = include_str!("fixtures/scalar_match.hew");

fn checked_hir(source: &str) -> (HirModule, TypeCheckOutput) {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(Vec::new())).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    (hir.module, checked)
}

#[test]
fn scalar_patterns_preserve_enum_results_guards_and_owned_values() {
    let (hir, checked) = checked_hir(SOURCE);
    let lowered = lower_module(&hir, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    let diagnostics = verify_module(&lowered.module);
    assert!(
        diagnostics.is_empty(),
        "{diagnostics:?}\n{}",
        hew_sir::dump_sir(&lowered.module)
    );
}

#[test]
fn scalar_literal_predicates_cannot_change_the_checked_width_or_kind() {
    let (hir, checked) = checked_hir(SOURCE);
    for wrong_width in [true, false] {
        let mut invalid = hir.clone();
        let function = invalid
            .items
            .iter_mut()
            .find_map(|item| match item {
                HirItem::Function(function) if function.declaration.full_path() == "tag_kind" => {
                    Some(function)
                }
                _ => None,
            })
            .unwrap();
        let HirExprKind::Match { arms, .. } = &mut function.body.tail.as_mut().unwrap().kind else {
            panic!("tag mapping must be a match")
        };
        let HirMatchArmPredicate::Literal { lit, ty } = &mut arms[0].predicate else {
            panic!("tag arm must have a literal predicate")
        };
        if wrong_width {
            *ty = hew_types::ResolvedTy::I64;
        } else {
            *lit = hew_hir::HirLiteral::Bool(false);
        }
        let lowered = lower_module(&invalid, &checked);
        assert!(
            lowered.callable_statuses.iter().any(|(_, status)| matches!(
                status,
                SirLoweringStatus::Unsupported { reason }
                    if reason.contains("scalar match") || reason.contains("literal match")
            )),
            "{:?}",
            lowered.callable_statuses
        );
    }
}
