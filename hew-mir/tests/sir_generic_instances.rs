//! Proof that a call-free SIR-owned generic instance reaches MIR.
//!
//! The HIR generic registry is intentionally cleared before SIR lowering.
//! A successful strict component therefore proves concrete semantic instances
//! came from SIR's `CallTarget::User` + call-site substitution service. The
//! selected identity body then crosses the unchanged scalar MIR ladder without
//! requiring MIR to discard the caller's explicit unwind edge.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_mir::lower_closed_scalar_component;
use hew_types::{module_registry::ModuleRegistry, Checker};

#[test]
fn call_free_sir_owned_generic_instance_reaches_mir_without_a_legacy_template() {
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

    let sir = hew_sir::lower_module(&hir.module, &type_check);
    assert!(
        hew_sir::verify_module(&sir.module).is_empty(),
        "generic SIR must verify: {:#?}",
        hew_sir::verify_module(&sir.module)
    );
    let instance = sir
        .module
        .callables
        .iter()
        .find(|callable| callable.symbol == "id$$i64")
        .expect("SIR must own the requested concrete identity instance");
    let component = lower_closed_scalar_component(&sir.module, &[instance.id])
        .expect("the call-free generic identity body must lower without an HIR/MIR template");
    assert_eq!(component.callables(), &[instance.id]);
    let pipeline = component.into_pipeline();
    let names = pipeline
        .raw_mir
        .iter()
        .map(|function| function.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(names, vec!["id$$i64"]);
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
