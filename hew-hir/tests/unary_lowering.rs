use hew_hir::{lower_program, HirDiagnosticKind, HirExpr, HirExprKind, HirItem, ResolutionCtx};
use hew_parser::ast::UnaryOp;
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

fn checked_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn contains_unary(expr: &HirExpr, expected: UnaryOp) -> bool {
    match &expr.kind {
        HirExprKind::Unary { op, operand, .. } => {
            *op == expected || contains_unary(operand, expected)
        }
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            contains_unary(left, expected) || contains_unary(right, expected)
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            contains_unary(condition, expected)
                || contains_unary(then_expr, expected)
                || else_expr
                    .as_deref()
                    .is_some_and(|expr| contains_unary(expr, expected))
        }
        HirExprKind::Block(block) => block
            .tail
            .as_deref()
            .is_some_and(|expr| contains_unary(expr, expected)),
        _ => false,
    }
}

#[test]
fn lowers_checker_typed_bool_not_to_hir_unary() {
    let output = checked_lower("fn main() -> bool { !false }");
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let HirItem::Function(function) = &output.module.items[0] else {
        panic!("expected function item");
    };
    let tail = function.body.tail.as_deref().expect("main has tail expr");
    assert!(contains_unary(tail, UnaryOp::Not), "tail: {tail:#?}");
}

#[test]
fn unary_missing_checker_type_info_fails_closed() {
    let parsed = hew_parser::parse("fn main() -> bool { !false }");
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.iter().any(|diag| matches!(
            &diag.kind,
            HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                if name == "unary expression" && reason.contains("missing expr_types")
        )),
        "expected checker-boundary diagnostic, got: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_err());
}
