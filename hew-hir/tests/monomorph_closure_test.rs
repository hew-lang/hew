//! Closure-under-substitution test: the HIR monomorphisation registry
//! discovers inner generic-fn calls inside specialised bodies and adds
//! per-instantiation entries.

use hew_hir::{lower_program, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:#?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "typecheck: {:#?}", tco.errors);
    lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

#[test]
fn inner_generic_call_inside_generic_body_closes_under_substitution() {
    // `outer<U>(y)` calls `id(y)` internally. When `outer<i64>` is
    // instantiated, the closure pass discovers `id<i64>` and adds it
    // to the registry, even though no outer caller invoked `id`
    // directly.
    let source = r"
        pub fn id<T>(x: T) -> T {
            x
        }

        pub fn outer<U>(y: U) -> U {
            id(y)
        }

        fn main() -> i64 {
            let a: i64 = outer(42);
            0
        }
    ";
    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "diagnostics: {:#?}",
        output.diagnostics
    );

    let names: std::collections::HashSet<String> = output
        .module
        .monomorphisations
        .iter()
        .map(|m| m.mangled_name.clone())
        .collect();

    assert!(
        names.contains("outer$$i64"),
        "outer$$i64 must be in registry: {names:?}"
    );
    assert!(
        names.contains("id$$i64"),
        "id$$i64 must be discovered by closure-under-substitution: {names:?}"
    );
}

#[test]
fn call_site_type_args_records_symbolic_inner_calls() {
    // Inner call `id(y)` inside `outer<U>` records
    // `call_site_type_args[site] = [Named "U"]` so MIR can substitute
    // and emit the mangled symbol per monomorphisation.
    let source = r"
        pub fn id<T>(x: T) -> T {
            x
        }

        pub fn outer<U>(y: U) -> U {
            id(y)
        }

        fn main() -> i64 {
            let a: i64 = outer(42);
            0
        }
    ";
    let output = typecheck_and_lower(source);
    // At least one call_site_type_args entry should mention the
    // outer fn's type-param symbol (the inner id(y) call).
    let has_symbolic = output.module.call_site_type_args.values().any(|v| {
        v.iter().any(|t| {
            matches!(
                t,
                hew_types::ResolvedTy::Named { name, args, .. }
                    if args.is_empty() && (name == "U" || name == "T")
            )
        })
    });
    assert!(
        has_symbolic,
        "expected at least one symbolic call_site_type_args entry; got {:?}",
        output.module.call_site_type_args
    );
}
