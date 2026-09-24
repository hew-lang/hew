//! A dynamic dispatch names the trait method its slot publishes.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, SemModule, SemTerminator, SirDiagnosticKind, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

const TWO_BOUNDS: &str = r#"
    trait Alpha { fn alpha(self) -> string; }
    trait Beta { fn beta(self) -> string; }
    type Both { n: i64 }
    impl Alpha for Both { fn alpha(self) -> string { "alpha" } }
    impl Beta for Both { fn beta(self) -> string { "beta" } }
    fn main() {
        let x: dyn (Alpha + Beta) = Both { n: 3 };
        println(x.beta());
    }
"#;

fn lower(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(Vec::new())).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = lower_module(&hir.module, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    lowered.module
}

fn dyn_calls(module: &mut SemModule) -> Vec<&mut SemTerminator> {
    module
        .functions
        .iter_mut()
        .flat_map(|function| function.blocks.iter_mut())
        .map(|block| &mut block.terminator)
        .filter(|terminator| matches!(terminator, SemTerminator::DynCall { .. }))
        .collect()
}

#[test]
fn dispatch_names_the_method_its_slot_publishes() {
    let mut module = lower(TWO_BOUNDS);
    assert_eq!(verify_module(&module), Vec::new());

    let [table] = module.vtables.as_slice() else {
        panic!("expected one dispatch table: {:?}", module.vtables);
    };
    let [alpha, beta] = table.slots.as_slice() else {
        panic!("expected two slots: {:?}", table.slots);
    };
    assert_eq!(
        (alpha.method_name.as_str(), beta.method_name.as_str()),
        ("alpha", "beta")
    );
    let (beta_slot, beta_method) = (beta.slot, beta.method);
    let calls = dyn_calls(&mut module);
    let [SemTerminator::DynCall { slot, method, .. }] = calls.as_slice() else {
        panic!("expected one dynamic dispatch");
    };
    assert_eq!((*slot, method), (beta_slot, &beta_method));
}

/// The two slots share a signature, so only the method identity tells the
/// verifier that a call reaches the wrong body.
#[test]
fn dispatch_to_a_slot_holding_another_method_is_refused() {
    let mut module = lower(TWO_BOUNDS);
    let alpha_slot = module.vtables[0].slots[0].slot;
    for call in dyn_calls(&mut module) {
        let SemTerminator::DynCall { slot, .. } = call else {
            unreachable!()
        };
        *slot = alpha_slot;
    }

    let errors = verify_module(&module);
    assert!(
        errors.iter().any(|error| matches!(
            &error.kind,
            SirDiagnosticKind::InvalidOperation { reason, .. }
                if reason.contains("names slot 3, which `Both` fills with")
        )),
        "{errors:?}"
    );
}

#[test]
fn table_repeating_a_method_is_refused() {
    let mut module = lower(TWO_BOUNDS);
    let alpha = module.vtables[0].slots[0].method;
    module.vtables[0].slots[1].method = alpha;

    let errors = verify_module(&module);
    assert!(
        errors.iter().any(|error| matches!(
            &error.kind,
            SirDiagnosticKind::InvalidVtable { reason, .. }
                if reason.contains("repeats trait method")
        )),
        "{errors:?}"
    );
}
