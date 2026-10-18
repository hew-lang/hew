//! Record initialization consumes the exact checker-resolved expression type.
use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

#[test]
fn missing_record_expression_type_refuses_initialization() {
    let source = "type Box<T> { value: T; } fn main() { let a: Box<i64> = Box { value: 42 }; }";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let sites: Vec<_> = checked.record_init_type_args.keys().cloned().collect();
    assert!(!sites.is_empty());
    for site in sites {
        checked.expr_types.remove(&site);
        checked.resolved_expr_types.remove(&site);
    }
    let output = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(
        output.into_result().is_err(),
        "missing record type must fail closed"
    );
}
