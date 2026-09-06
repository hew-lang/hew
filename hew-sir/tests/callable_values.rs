use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{lower_module, verify_module, BoundaryDecision, SemModule, SemOpKind, SemTerminator};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_source(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = lower_module(&hir.module, &facts);
    assert!(
        lowered.statuses.iter().any(|status| status.name == "main"
            && matches!(status.status, hew_sir::SirLoweringStatus::Lowered)),
        "{:?}",
        lowered.statuses
    );
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, hew_sir::SirLoweringStatus::Unsupported { .. })),
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
fn function_values_demand_their_bodies_and_share_indirect_call_cleanup() {
    let module = lower_source(
        r"
        fn increment(value: i64) -> i64 { value + 1 }
        fn unrelated(value: i64) -> i64 { value - 1 }
        fn apply(callback: fn(i64) -> i64, value: i64) -> i64 { callback(callback(value)) }
        fn factory() -> fn(i64) -> i64 { increment }
        fn main() -> i64 {
            let selected = increment;
            let erased: fn(i64) -> i64 = selected;
            let returned = factory();
            apply(erased, returned(40))
        }
    ",
    );
    assert!(module
        .functions
        .iter()
        .any(|function| function.name == "increment"));
    assert!(!module
        .functions
        .iter()
        .any(|function| function.name == "unrelated"));
    assert!(module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.ops)
        .any(|operation| matches!(operation.kind, SemOpKind::FunctionMake { .. })));
    assert!(module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.ops)
        .any(|operation| matches!(operation.kind, SemOpKind::CallableCoerce { .. })));
    assert!(module.functions.iter().flat_map(|function| &function.blocks)
        .any(|block| matches!(&block.terminator, SemTerminator::IndirectCall { callee, .. } if callee.decision == BoundaryDecision::Borrow)));
}

#[test]
fn call_once_erasure_consumes_the_receiver_on_both_continuations() {
    let module = lower_source(
        r"
        fn ticket() -> i64 { 7 }
        fn main() -> i64 {
            let invoke: fn[once]() -> i64 = ticket;
            invoke()
        }
    ",
    );
    assert!(module.functions.iter().flat_map(|function| &function.blocks)
        .any(|block| matches!(&block.terminator, SemTerminator::IndirectCall { callee, .. } if callee.decision == BoundaryDecision::Move)));
}
