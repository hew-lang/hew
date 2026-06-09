//! Regression for `collect_call_sites_in_expr` walker completeness:
//! inner generic calls inside `while` conditions and `for` bodies of a
//! generic function must surface in the monomorphisation registry via
//! closure-under-substitution. Before the fix the walker fell through
//! `_ => {}` and silently dropped these call sites.
//!
//! Architecture: `collect_call_sites_in_expr` is walked on the body of
//! an *already-monomorphised* outer generic function to discover inner
//! generic calls with substituted concrete types. The outer function
//! must itself be generic (so its monomorphisation seeds the worklist)
//! and contain an inner generic call inside a loop expression.
//!
//! LESSONS: `block-walker-completeness` (P1).

use hew_hir::{lower_program, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "typecheck errors: {:#?}",
        tc_output.errors
    );
    lower_program(&parsed.program, &tc_output, &ResolutionCtx)
}

/// An inner generic call inside a `while` condition in a generic outer
/// function body must be discovered via closure-under-substitution.
/// Before the fix, `collect_call_sites_in_expr` had no `While` arm and
/// the condition expression was unreachable via `_ => {}`.
#[test]
fn inner_generic_call_in_while_condition_surfaces_via_substitution() {
    // `wrap<T>` is generic and is called from `main` at T=i64.
    // `should_continue<T>` is a generic function called inside the while
    // condition of `wrap`. Before the fix, the walker missed the While arm
    // so `should_continue$$i64` was never discovered during
    // closure-under-substitution.
    //
    // `should_continue` accepts T and returns bool using concrete comparison.
    // The outer `wrap` passes a concrete `i64` counter into `should_continue`
    // so the checker records `should_continue(counter)` with T=i64.
    let source = r"
        pub fn should_continue<T>(x: T, limit: T) -> bool {
            x < limit
        }

        pub fn wrap<T>(limit: T) -> i64 {
            var count = 0;
            while should_continue(count, 3) {
                count = count + 1;
            }
            return count;
        }

        fn main() -> i64 {
            return wrap(10);
        }
    ";

    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        output.diagnostics
    );

    // Both `wrap$$i64` and `should_continue$$i64` must appear in the
    // registry. `should_continue` is discovered only if the walker
    // traverses the while condition.
    let mono = &output.module.monomorphisations;
    assert!(
        mono.iter().any(|m| m.key.origin_name == "wrap"),
        "expected `wrap` in monomorphisation registry; got {mono:#?}"
    );
    assert!(
        mono.iter().any(|m| m.key.origin_name == "should_continue"),
        "expected `should_continue` in monomorphisation registry \
         (discovered via while-condition walker); got {mono:#?}"
    );
}

/// An inner generic call inside a `for` range body in a generic outer
/// function body must be discovered via closure-under-substitution.
/// Before the fix, `collect_call_sites_in_expr` had no `ForRange` arm.
#[test]
fn inner_generic_call_in_for_range_body_surfaces_via_substitution() {
    // `accumulate<T>` is generic and is called from `main` at T=i64.
    // `passthrough<T>` is a generic function called inside the for-range body
    // of `accumulate`. Before the fix, the walker missed the ForRange arm
    // so `passthrough$$i64` was never discovered during
    // closure-under-substitution.
    let source = r"
        pub fn passthrough<T>(x: T) -> T {
            x
        }

        pub fn accumulate<T>(n: T) -> i64 {
            var result = 0;
            for i in 0..3 {
                result = passthrough(result);
            }
            return result;
        }

        fn main() -> i64 {
            return accumulate(5);
        }
    ";

    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        output.diagnostics
    );

    // Both `accumulate$$i64` and `passthrough$$i64` must appear in the
    // registry. `passthrough` is discovered only if the walker traverses
    // the for-range body.
    let mono = &output.module.monomorphisations;
    assert!(
        mono.iter().any(|m| m.key.origin_name == "accumulate"),
        "expected `accumulate` in monomorphisation registry; got {mono:#?}"
    );
    assert!(
        mono.iter().any(|m| m.key.origin_name == "passthrough"),
        "expected `passthrough` in monomorphisation registry \
         (discovered via for-range-body walker); got {mono:#?}"
    );
}
