use hew_hir::{HirExprKind, HirItem, HirStmtKind};

use crate::support;

fn main_fn(output: &hew_hir::LowerOutput) -> &hew_hir::HirFn {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(f) if f.name == "main" => Some(f),
            _ => None,
        })
        .expect("main function must be present")
}

#[test]
fn top_level_wildcard_let_lowers_rhs_as_expression_statement() {
    let source = r"
        fn side() -> i64 {
            return 7;
        }

        fn main() -> i64 {
            let _ = side();
            return 0;
        }
    ";

    let output = support::checker_pipeline::lower_through_checker(source);
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);

    let main = main_fn(&output);
    let first = main
        .body
        .statements
        .first()
        .expect("main should contain the discard statement");
    match &first.kind {
        HirStmtKind::Expr(expr) => assert!(
            matches!(expr.kind, HirExprKind::Call { .. }),
            "discarded side() call should lower as a call expression statement; got {expr:#?}"
        ),
        other => panic!("top-level wildcard discard must not create a binding; got {other:#?}"),
    }

    let verify = hew_hir::verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify errors: {verify:#?}");
}
