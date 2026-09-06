//! Current projected-consume guards and the owning destructure control.

use hew_hir::{lower_program_host_target, HirExprKind, HirItem, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BoundaryDecision, LoweredModule, SemOpKind, SemTerminator,
    SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, CallableCallMode, Checker, ResolvedTy};

fn lower(source: &str) -> LoweredModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    lower_module(&hir.module, &checked)
}

fn declarations(clone: bool) -> String {
    let capabilities = if clone { "once, clone" } else { "once" };
    format!("type Two {{ a: fn[{capabilities}]() -> i64, b: fn() -> i64 }} fn answer() -> i64 {{ 41 }} fn sibling() -> i64 {{ 1 }}")
}

fn assert_refused(source: &str, function: &str, code: &str) {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(
        checked
            .errors
            .iter()
            .any(|error| error.message.contains(code)),
        "{source}: {:?}",
        checked.errors
    );

    // Source rejection can stop aggregate type-fact publication. Build the
    // independent SIR witness from accepted read-callable source, then change
    // only the projected call receiver's invocation mode. This must reach the
    // ownership guard, never pass because an aggregate descriptor is missing.
    let read_source = source
        .replace("fn[once, clone]", "fn[clone]")
        .replace("fn[once]", "fn");
    let parsed = hew_parser::parse(&read_source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let mut hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let baseline = lower_module(&hir.module, &checked);
    assert!(
        baseline
            .statuses
            .iter()
            .any(|status| status.name == function
                && matches!(status.status, SirLoweringStatus::Lowered)),
        "{:?}",
        baseline.statuses
    );
    assert!(
        verify_module(&baseline.module).is_empty(),
        "{:?}",
        verify_module(&baseline.module)
    );
    let body = hir
        .module
        .items
        .iter_mut()
        .find_map(|item| match item {
            HirItem::Function(body) if body.name == function => Some(body),
            _ => None,
        })
        .expect("guard fixture function");
    let tail = body.body.tail.as_mut().expect("projected call tail");
    let HirExprKind::Call { callee, .. } = &mut tail.kind else {
        panic!("guard fixture tail must be a call");
    };
    assert!(matches!(
        callee.kind,
        HirExprKind::FieldAccess { .. } | HirExprKind::TupleIndex { .. }
    ));
    let ResolvedTy::Function { capabilities, .. } = &mut callee.ty else {
        panic!("projected receiver must have an erased callable type");
    };
    assert_eq!(capabilities.call, CallableCallMode::Read);
    capabilities.call = CallableCallMode::Once;
    let lowered = lower_module(&hir.module, &checked);
    assert!(lowered.statuses.iter().any(|status| {
        status.name == function && matches!(&status.status, SirLoweringStatus::Unsupported { reason } if reason.contains(code))
    }), "{:?}", lowered.statuses);
    assert!(
        !lowered
            .module
            .functions
            .iter()
            .any(|body| body.name == function),
        "refused receiver must not publish an executable copied-field body"
    );
}

#[test]
fn once_record_fields_never_gain_a_hidden_clone_owner() {
    for clone in [false, true] {
        let source = format!(
            "{} fn main() -> i64 {{ let value = Two {{ a: answer, b: sibling }}; value.a() }}",
            declarations(clone)
        );
        assert_refused(&source, "main", "E_OWN_PARTIAL_CONSUME");
    }
}

#[test]
fn once_tuple_fields_never_gain_a_hidden_clone_owner() {
    for capabilities in ["once", "once, clone"] {
        let source = format!("fn answer() -> i64 {{ 41 }} fn main() -> i64 {{ let callback: fn[{capabilities}]() -> i64 = answer; let value = (callback, 1); value.0() }}");
        assert_refused(&source, "main", "E_OWN_PARTIAL_CONSUME");
    }
}

#[test]
fn borrowed_once_record_fields_require_an_owned_destructure() {
    for clone in [false, true] {
        let source = format!("{} fn invoke(value: Two) -> i64 {{ value.a() }} fn main() -> i64 {{ invoke(Two {{ a: answer, b: sibling }}) }}", declarations(clone));
        assert_refused(&source, "invoke", "E_OWN_CONSUME_BORROWED");
    }
}

#[test]
fn explicit_destructure_exposes_owned_callable_fields_and_live_siblings() {
    for clone in [false, true] {
        let source = format!("{} fn main() -> i64 {{ let value = Two {{ a: answer, b: sibling }}; let Two {{ a, b }} = value; let first = a(); first + b() }}", declarations(clone));
        let lowered = lower(&source);
        assert!(
            lowered.statuses.iter().any(|status| status.name == "main"
                && matches!(status.status, SirLoweringStatus::Lowered)),
            "{:?}",
            lowered.statuses
        );
        assert!(
            verify_module(&lowered.module).is_empty(),
            "{:?}",
            verify_module(&lowered.module)
        );
        let main = lowered
            .module
            .functions
            .iter()
            .find(|function| function.name == "main")
            .unwrap();
        let fields = main
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .find(|op| matches!(op.kind, SemOpKind::Destructure { .. }))
            .unwrap();
        assert_eq!(fields.results.len(), 2);
        assert!(fields
            .results
            .iter()
            .all(|field| field.own == hew_sir::OwnKind::Owned));
        assert!(main.blocks.iter().any(|block| matches!(&block.terminator, SemTerminator::IndirectCall { callee, .. } if callee.decision == BoundaryDecision::Move && callee.operand.value == fields.results[0].id)));
        assert!(main.blocks.iter().any(|block| matches!(&block.terminator, SemTerminator::IndirectCall { callee, .. } if callee.decision == BoundaryDecision::Borrow && callee.operand.value == fields.results[1].id)));
    }
}
