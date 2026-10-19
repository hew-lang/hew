//! Implicit loop calls have their own checker identity and ordinary HIR calls.
use crate::support;
use hew_hir::{HirExprKind, HirItem, HirStmtKind, HirVarSelfMethodTarget};
use hew_types::{CallTarget, ResolvedTy};

#[test]
fn checked_for_loops_preserve_bound_calls_and_associated_cursor_identity() {
    let source = r"
fn total<I>(input: I) -> i64 where I: IntoIterator<Item = i64> {
    var total = 0;
    for value in input { total += value; }
    total
}
";
    let (parsed, checked) = support::checker_pipeline::typecheck_source(source);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let plans: Vec<_> = checked.for_loop_plans.iter().collect();
    let [(key, plan)] = plans.as_slice() else {
        panic!("one ordinary checked loop plan required");
    };
    assert!(!checked.method_call_rewrites.contains_key(key));
    let into_iter = plan.into_iter.as_ref().expect("IntoIterator call");
    assert!(matches!(
        into_iter.target,
        CallTarget::StaticTraitMethod { .. }
    ));
    assert!(matches!(
        plan.next.target,
        CallTarget::StaticTraitMethod { .. }
    ));
    assert_ne!(into_iter.receiver_parameter, plan.next.receiver_parameter);
    assert_eq!(plan.item_type, hew_types::Ty::I64);
    assert_eq!(
        hew_types::ResolvedTy::from_ty(&plan.cursor_type).unwrap(),
        ResolvedTy::from_ty(&into_iter.return_type).unwrap()
    );
    let lowered =
        hew_hir::lower_program_host_target(&parsed.program, &checked, &hew_hir::ResolutionCtx);
    assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
    let function = lowered
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "total" => Some(function),
            _ => None,
        })
        .unwrap();
    let block = function
        .body
        .statements
        .iter()
        .find_map(|statement| match &statement.kind {
            HirStmtKind::Expr(expr) => match &expr.kind {
                HirExprKind::Block(block) => Some(block),
                _ => None,
            },
            _ => None,
        })
        .expect("loop block");
    let HirStmtKind::Let(_, Some(init)) = &block.statements[0].kind else {
        panic!("cursor binding");
    };
    assert!(matches!(
        init.kind,
        HirExprKind::CallTraitMethodStatic { .. }
    ));
    let HirStmtKind::Expr(loop_expr) = &block.statements[1].kind else {
        panic!("loop statement");
    };
    let HirExprKind::While { body, .. } = &loop_expr.kind else {
        panic!("loop body");
    };
    let HirStmtKind::Expr(match_expr) = &body.statements[0].kind else {
        panic!("next match");
    };
    let HirExprKind::Match { scrutinee, .. } = &match_expr.kind else {
        panic!("next result");
    };
    assert!(matches!(
        scrutinee.kind,
        HirExprKind::VarSelfMethodCall {
            target: HirVarSelfMethodTarget::StaticTrait { .. },
            ..
        }
    ));
}

#[test]
fn checked_for_loops_refuse_a_missing_bound_without_boundary_diagnostics() {
    let (_, checked) = support::checker_pipeline::typecheck_source(
        "fn invalid<I>(input: I) { for value in input { let _ = value; } }",
    );
    assert!(checked
        .errors
        .iter()
        .any(|error| error.message == "type is not iterable"));
    assert!(checked.for_loop_plans.is_empty());
}

#[test]
fn checked_for_loops_diagnose_unresolved_plan_types() {
    for role in ["cursor", "item", "call result", "trait argument"] {
        let (parsed, mut checked) = support::checker_pipeline::typecheck_source(
            "fn total<I>(input: I) -> i64 where I: IntoIterator<Item = i64> { \
             var sum = 0; for value in input { sum += value; } sum }",
        );
        assert!(checked.errors.is_empty(), "{:?}", checked.errors);
        let plan = checked.for_loop_plans.values_mut().next().unwrap();
        let unresolved = hew_types::Ty::Var(hew_types::ty::TypeVar::fresh());
        match role {
            "cursor" => plan.cursor_type = unresolved,
            "item" => plan.item_type = unresolved,
            "call result" => plan.into_iter.as_mut().unwrap().return_type = unresolved,
            "trait argument" => plan.into_iter.as_mut().unwrap().trait_args.push(unresolved),
            _ => unreachable!("all checked loop boundary roles are listed"),
        }
        let lowered =
            hew_hir::lower_program_host_target(&parsed.program, &checked, &hew_hir::ResolutionCtx);
        assert!(
            lowered.diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                hew_hir::HirDiagnosticKind::CheckerBoundaryViolation { name, .. }
                    if name == &format!("for loop {role}")
            )),
            "{role}: {:?}",
            lowered.diagnostics,
        );
    }
}
