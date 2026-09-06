use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    verify_module, CallableInstance, PlaceOrigin, SemModule, SemOpKind, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn capture_module(body: &str) -> SemModule {
    let source = format!(
        r#"
        type Failure {{ message: string, retained: Vec<string> }}
        fn main() -> i64 {{
            let failure = Failure {{ message: "message".to_upper(), retained: ["retained".to_upper()] }};
            let callback = || -> i64 {{ {body} }};
            callback() + callback()
        }}
    "#
    );
    let parsed = hew_parser::parse(&source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = hew_sir::lower_module_with_demand(
        &hir.module,
        &checked,
        hew_sir::SirLoweringDemand::EveryCallable,
    );
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| matches!(status, SirLoweringStatus::Lowered)),
        "{:?}",
        lowered.callable_statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "{:?}",
        verify_module(&lowered.module)
    );
    lowered.module
}

#[test]
fn captured_record_fields_support_repeated_reads() {
    for body in [
        "println(failure.message); 0",
        "println(failure.retained[0]); 0",
    ] {
        capture_module(body);
    }
}

#[test]
fn captured_vector_projection_keeps_its_environment_loan_live() {
    let mut module = capture_module("failure.retained[0].len()");
    let callable = module
        .callables
        .iter()
        .find(|entry| matches!(entry.instance, CallableInstance::Closure(_)))
        .unwrap()
        .id;
    let function = module
        .functions
        .iter_mut()
        .find(|function| function.callable == callable)
        .unwrap();
    let capture_loan = function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find_map(|op| match op.kind {
            SemOpKind::LoadBorrow { place }
                if matches!(
                    function.places[place.0 as usize].origin,
                    PlaceOrigin::Capture { .. }
                ) =>
            {
                Some(op.results[0].id)
            }
            _ => None,
        })
        .expect("borrow the environment field");
    assert!(function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .any(|op| matches!(&op.kind,
        SemOpKind::AggregateProjectBorrow { aggregate, .. } if aggregate.value == capture_loan)));
    let mut removed = false;
    for block in &mut function.blocks {
        let before = block.ops.len();
        block.ops.retain(|op| !matches!(&op.kind, SemOpKind::EndBorrow { borrow } if borrow.value == capture_loan));
        removed |= before != block.ops.len();
    }
    assert!(removed, "the source must close its capture loan");
    assert!(verify_module(&module).iter().any(|diagnostic| matches!(&diagnostic.kind,
        hew_sir::SirDiagnosticKind::OwnershipLifetime { reason, .. } if *reason == "local borrow remains live at exit")));
}
