//! Regression tests for the builtin-type annotation identity bug.
//!
//! Before the fix, `fn f() -> Option<i64> { return Some(5); }` produced
//! `ReturnTypeMismatch { expected: Named { builtin: None }, actual: Named { builtin: Some(Option) } }`.
//! The annotation side was lowered via `named_user` (no builtin discriminator)
//! while constructor values carry `builtin: Some(BuiltinType::Option)` —
//! they didn't unify at the `Stmt::Return` comparison.
//!
//! The fix adds a `lookup_builtin_type` probe in the `_ =>` arm of
//! `lower_type` so Option/Result and any other shared builtin absent from
//! the HIR-local registry are stamped correctly when they appear as annotations.
//!
//! These tests pin four contracts:
//! - `return Some(5)` from `Option<i64>` — no `ReturnTypeMismatch`
//! - `return Ok(7)` from `Result<i64, i64>` — no `ReturnTypeMismatch`
//! - expression-form `Option<i64>` return still works (regression guard)
//! - a user type in return position is NOT mis-stamped as builtin

use hew_hir::{lower_program, HirDiagnosticKind, HirItem, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn has_return_type_mismatch(output: &hew_hir::LowerOutput) -> bool {
    output
        .diagnostics
        .iter()
        .any(|d| matches!(&d.kind, HirDiagnosticKind::ReturnTypeMismatch { .. }))
}

/// Headline regression: `return Some(5)` from `Option<i64>` must lower
/// without `ReturnTypeMismatch`. Before the fix this produced:
/// `expected: Named { builtin: None }, actual: Named { builtin: Some(Option) }`.
#[test]
fn return_some_from_option_no_type_mismatch() {
    let output = lower(
        r"
        fn f() -> Option<i64> {
            return Some(5);
        }

        fn main() -> i64 {
            match f() {
                Some(x) => x,
                None => 0,
            }
        }
        ",
    );
    assert!(
        !has_return_type_mismatch(&output),
        "`return Some(5)` from `Option<i64>` must not produce ReturnTypeMismatch; \
         diagnostics: {:#?}",
        output.diagnostics
    );
}

/// Result analogue: `return Ok(7)` from `Result<i64, i64>` must lower
/// without `ReturnTypeMismatch`.
#[test]
fn return_ok_from_result_no_type_mismatch() {
    let output = lower(
        r"
        fn g() -> Result<i64, i64> {
            return Ok(7);
        }

        fn main() -> i64 {
            match g() {
                Ok(x) => x,
                Err(e) => e,
            }
        }
        ",
    );
    assert!(
        !has_return_type_mismatch(&output),
        "`return Ok(7)` from `Result<i64, i64>` must not produce ReturnTypeMismatch; \
         diagnostics: {:#?}",
        output.diagnostics
    );
}

/// Regression guard: expression-form `Option<i64>` return must still lower
/// without `ReturnTypeMismatch`. This was the working path before the fix;
/// confirm the fix does not break it.
#[test]
fn expression_return_option_still_lowers_cleanly() {
    let output = lower(
        r"
        fn f() -> Option<i64> {
            Some(5)
        }

        fn main() -> i64 {
            match f() {
                Some(x) => x,
                None => 0,
            }
        }
        ",
    );
    assert!(
        !has_return_type_mismatch(&output),
        "expression-form `Some(5)` return from `Option<i64>` must not produce \
         ReturnTypeMismatch; diagnostics: {:#?}",
        output.diagnostics
    );
}

/// Fail-closed guard: a user-defined type used in return position must NOT be
/// mis-stamped as a builtin. The fix relies on `lookup_builtin_type` returning
/// `None` for user names — this test pins that the `named_user` branch is
/// still reached for genuine user types.
#[test]
fn user_type_in_return_position_not_mis_stamped_as_builtin() {
    let output = lower(
        r"
        pub type Wrapper {
            v: i64;
        }

        fn make() -> Wrapper {
            return Wrapper { v: 42 };
        }
        ",
    );
    // A user type must not produce ReturnTypeMismatch from mis-stamping.
    assert!(
        !has_return_type_mismatch(&output),
        "user type `Wrapper` in return position must not produce ReturnTypeMismatch; \
         diagnostics: {:#?}",
        output.diagnostics
    );

    let make_fn = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Function(f) = item {
                (f.name == "make").then_some(f)
            } else {
                None
            }
        })
        .expect("make fn must be present in HIR");

    // The return type on the HIR function must have `builtin: None` —
    // i.e. it resolved to `named_user`, NOT `named_builtin`.
    match &make_fn.return_ty {
        hew_types::ResolvedTy::Named { builtin, .. } => {
            assert!(
                builtin.is_none(),
                "user type `Wrapper` must have `builtin: None` on its HIR return type; \
                 got: {builtin:?}"
            );
        }
        other => {
            panic!("make fn return type must be ResolvedTy::Named; got: {other:?}");
        }
    }
}
