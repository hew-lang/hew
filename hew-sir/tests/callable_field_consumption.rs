//! D345: a consuming callable field is acquired through whole-value destructuring.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BoundaryDecision, LoweredModule, SemOpKind, SemTerminator,
    SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower(source: &str, rejection: bool) -> LoweredModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    // Negative cases exercise SIR's independent guard even when the checker
    // has already reported the source ownership diagnostic. The typed HIR and
    // exact SIR refusal below must still be present; unrelated failures cannot
    // satisfy the assertion.
    if !rejection {
        assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    }
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    lower_module(&hir.module, &checked)
}

fn declarations(clone: bool) -> String {
    let capabilities = if clone { "once, clone" } else { "once" };
    format!("type Two {{ a: fn[{capabilities}]() -> i64, b: fn() -> i64 }} fn answer() -> i64 {{ 41 }} fn sibling() -> i64 {{ 1 }}")
}

fn assert_refused(source: &str, function: &str, code: &str) {
    let lowered = lower(source, true);
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
        let lowered = lower(&source, false);
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
