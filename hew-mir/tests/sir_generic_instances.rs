//! End-to-end proof for the first SIR-owned generic specialization slice.
//!
//! The HIR generic registry is intentionally cleared before SIR lowering.
//! A successful strict component therefore proves concrete semantic instances
//! came from SIR's `CallTarget::User` + call-site substitution service, then
//! crossed the unchanged scalar raw/checked/elaborated MIR ladder without a
//! legacy generic-body fallback.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_mir::lower_closed_scalar_component;
use hew_types::{module_registry::ModuleRegistry, Checker};

#[test]
fn strict_scalar_component_realizes_sir_owned_generic_instances() {
    let parsed = hew_parser::parse(
        r"
        pub fn id<T>(x: T) -> T {
            x
        }

        pub fn relay<U>(y: U) -> U {
            id(id(y))
        }

        fn main() -> i64 {
            relay(41)
        }
        ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let type_check = checker.check_program(&parsed.program);
    assert!(
        type_check.errors.is_empty(),
        "type errors: {:#?}",
        type_check.errors
    );
    let mut hir = lower_program_host_target(&parsed.program, &type_check, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    assert!(
        !hir.module.monomorphisations.is_empty(),
        "fixture must have a legacy HIR generic registry entry before it is cleared"
    );
    hir.module.monomorphisations.clear();

    let sir = hew_sir::lower_module(&hir.module);
    assert!(
        hew_sir::verify_module(&sir.module).is_empty(),
        "generic SIR must verify: {:#?}",
        hew_sir::verify_module(&sir.module)
    );
    let entry = sir
        .module
        .entry_callable
        .expect("scalar root main must be a strict SIR entry");
    let component = lower_closed_scalar_component(&sir.module, &[entry])
        .expect("closed generic SIR component must lower without an HIR/MIR template");
    assert_eq!(
        component.callables().len(),
        3,
        "the component must contain main, relay<i64>, and one cached id<i64> instance"
    );
    let pipeline = component.into_pipeline();
    let names = pipeline
        .raw_mir
        .iter()
        .map(|function| function.name.as_str())
        .collect::<Vec<_>>();
    assert!(names.contains(&"main"));
    assert!(names.contains(&"relay$$i64"));
    assert!(names.contains(&"id$$i64"));
    assert_eq!(
        pipeline.raw_mir.len(),
        pipeline.checked_mir.len(),
        "every generic SIR body must enter the unchanged checked-MIR ladder"
    );
    assert_eq!(
        pipeline.raw_mir.len(),
        pipeline.elaborated_mir.len(),
        "every generic SIR body must receive explicit elaboration"
    );
}
