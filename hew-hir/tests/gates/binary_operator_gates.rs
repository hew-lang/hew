//! Integer operators retain their checked widths through HIR.

use hew_hir::{lower_program, ResolutionCtx, TargetArch};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let tco = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "{:?}", tco.errors);
    lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host())
}

#[test]
fn binary_operators_preserve_checked_integer_widths() {
    for (params, ret, expr, require_ok) in [
        ("x: isize, y: isize", "isize", "x / y", true),
        ("x: isize, y: isize", "isize", "x % y", true),
        ("x: i64, y: i64", "i64", "x / y", false),
        ("x: isize, n: isize", "isize", "x << n", true),
        ("x: usize, n: usize", "usize", "x >> n", true),
        ("x: i32, n: i32", "i32", "x << n", false),
        ("x: isize, y: isize", "bool", "x < y", false),
        ("x: isize, y: isize", "bool", "x <= y", false),
        ("x: isize, y: isize", "bool", "x > y", false),
        ("x: isize, y: isize", "bool", "x >= y", false),
    ] {
        let output = typecheck_and_lower(&format!("fn op({params}) -> {ret} {{ {expr} }}"));
        assert!(
            output.diagnostics.is_empty(),
            "`{expr}` on ({params}) must lower cleanly; diagnostics: {:?}",
            output.diagnostics
        );
        if require_ok {
            assert!(output.into_result().is_ok(), "`{expr}` into_result failed");
        }
    }
}
