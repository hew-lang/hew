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
fn isize_divide_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> isize {
            x / y
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "isize division must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_ok());
}

#[test]
fn isize_modulo_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> isize {
            x % y
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "isize modulo must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_ok());
}

#[test]
fn i64_divide_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: i64, y: i64) -> i64 {
            x / y
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "i64 division must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn isize_shift_left_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, n: isize) -> isize {
            x << n
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "isize shift-left must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_ok());
}

#[test]
fn usize_shift_right_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: usize, n: usize) -> usize {
            x >> n
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "usize shift-right must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_ok());
}

#[test]
fn i32_shift_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: i32, n: i32) -> i32 {
            x << n
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "i32 shift must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn comparison_on_isize_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> bool {
            x < y
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "comparison on isize must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn comparison_le_on_isize_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> bool {
            x <= y
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "<= on isize must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn comparison_gt_on_isize_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> bool {
            x > y
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "> on isize must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn comparison_ge_on_isize_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> bool {
            x >= y
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        ">= on isize must lower cleanly; diagnostics: {:?}",
        output.diagnostics
    );
}
