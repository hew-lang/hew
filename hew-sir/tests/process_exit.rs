//! `exit(code)` and the catalogue builtins that lower through runtime families.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, CallResult, CallUnwind, SemFunction, SemTerminator,
    SirDiagnosticKind, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker, RuntimeCallFamily};

fn lower_source(source: &str) -> hew_sir::LoweredModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR errors: {:#?}",
        hir.diagnostics
    );
    lower_module(&hir.module, &facts)
}

fn lowered_main(lowered: &hew_sir::LoweredModule) -> &SemFunction {
    assert!(
        matches!(
            lowered.statuses.iter().find(|status| status.name == "main"),
            Some(status) if matches!(status.status, SirLoweringStatus::Lowered)
        ),
        "main must lower: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "source must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );
    lowered
        .module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "main")
        .expect("main has a body")
}

fn runtime_families(function: &SemFunction) -> Vec<RuntimeCallFamily> {
    function
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            SemTerminator::RtCall { family, .. } => Some(*family),
            _ => None,
        })
        .collect()
}

const EXIT_WITH_LIVE_OWNERS: &str = r#"
    fn finish(label: string, code: i64) {
        let owned = label + ":owned";
        println(owned);
        if code > 0 {
            exit(code);
        }
        println("kept running");
    }

    fn main() {
        let keep = "kept";
        finish("first", 7);
        println(keep);
    }
"#;

#[test]
fn exit_never_returns_and_leaves_its_live_owners_to_the_process() {
    let lowered = lower_source(EXIT_WITH_LIVE_OWNERS);
    lowered_main(&lowered);
    let finish = lowered
        .module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "finish")
        .expect("finish has a body");
    let exit = finish
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::RtCall {
                family: RuntimeCallFamily::ProcessExit,
                result,
                normal,
                unwind,
                ..
            } => Some((result, normal, unwind)),
            _ => None,
        })
        .expect("exit lowers to a process-exit runtime call");
    let (result, normal, unwind) = exit;
    assert!(matches!(result, CallResult::Never));
    assert!(matches!(unwind, CallUnwind::NotApplicable));
    assert!(normal.args.is_empty());
    let continuation = finish
        .blocks
        .iter()
        .find(|block| block.id == normal.target)
        .expect("normal edge targets a block");
    assert!(
        matches!(continuation.terminator, SemTerminator::Unreachable),
        "the process ends at the call; nothing continues past it"
    );
    assert!(
        !continuation
            .ops
            .iter()
            .any(|op| matches!(op.kind, hew_sir::SemOpKind::DestroyValue { .. })),
        "exit runs no scope cleanup"
    );
}

#[test]
fn a_process_exit_that_continues_is_malformed() {
    let lowered = lower_source(EXIT_WITH_LIVE_OWNERS);
    lowered_main(&lowered);
    let mut module = lowered.module;
    let finish = module
        .functions
        .iter_mut()
        .find(|function| function.declaration.full_path() == "finish")
        .expect("finish has a body");
    let continuation = finish
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::RtCall {
                family: RuntimeCallFamily::ProcessExit,
                normal,
                ..
            } => Some(normal.target),
            _ => None,
        })
        .expect("exit lowers to a process-exit runtime call");
    finish
        .blocks
        .iter_mut()
        .find(|block| block.id == continuation)
        .expect("continuation block exists")
        .terminator = SemTerminator::Return { value: None };
    assert!(
        verify_module(&module).iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidOperation { reason, .. } if reason.contains("never returns")
        )),
        "a continuation after exit must be refused: {:#?}",
        verify_module(&module)
    );
}

#[test]
fn catalogue_builtins_lower_through_their_runtime_families() {
    let lowered = lower_source(
        r#"
        fn main() {
            let flag = true;
            let joined = ("a" + "b");
            let empty: bytes = bytes.new();
            println(f"{joined} {flag} {empty.len()}");
        }
        "#,
    );
    let families = runtime_families(lowered_main(&lowered));
    for expected in [
        RuntimeCallFamily::StringConcat,
        RuntimeCallFamily::BoolToString,
        RuntimeCallFamily::BytesNew,
    ] {
        assert!(
            families.contains(&expected),
            "{expected:?} must lower as a runtime family: {families:?}"
        );
    }
}
