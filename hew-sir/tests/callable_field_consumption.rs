//! Field transfers preserve sibling ownership without manufacturing copies.

use hew_hir::{lower_program_host_target, HirExprKind, HirItem, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BoundaryDecision, LoweredModule, SemOpKind, SemTerminator,
    SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, CallTarget, Checker};

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

fn assert_lowered(source: &str) -> LoweredModule {
    let mut lowered = lower(source);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| matches!(status, SirLoweringStatus::Lowered)),
        "{:?}",
        lowered.callable_statuses
    );
    let diagnostics = verify_module(&lowered.module);
    assert!(
        diagnostics.is_empty(),
        "{diagnostics:?}\n{}",
        hew_sir::dump_sir(&lowered.module)
    );
    hew_sir::canonicalize_module_constant_cfg(&mut lowered.module)
        .expect("verified field partitions must survive compiler CFG canonicalization");
    lowered
}

fn assert_once_field_transfer(source: &str) {
    let lowered = assert_lowered(source);
    let main = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .unwrap();
    let taken = main
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find(|op| matches!(op.kind, SemOpKind::LoadTake { .. }))
        .expect("once field must transfer from its existing owner");
    let moved = main.blocks.iter().flat_map(|block| &block.ops)
        .find(|op| matches!(&op.kind, SemOpKind::Move { source } if source.value == taken.results[0].id))
        .expect("receiver transfers before its arguments");
    assert!(main.blocks.iter().any(|block| matches!(&block.terminator,
        SemTerminator::IndirectCall { callee, .. } if callee.decision == BoundaryDecision::Move
            && callee.operand.value == moved.results[0].id)));
}

#[test]
fn temporary_record_and_tuple_fields_transfer_without_copying_siblings() {
    for value in ["42", "100 / 0"] {
        let source = format!(
            r#"
            type Job {{ run: fn[once](i64) -> i64, label: string }}
            fn make_job() -> Job {{
                let text = "owned callback";
                Job {{ run: move |value: i64| {{ println(text); value }}, label: "sibling owner" }}
            }}
            fn make_pair() -> (Job, string) {{ (make_job(), "outer sibling") }}
            fn invoke(consume run: fn[once](i64) -> i64, value: i64) -> i64 {{ run(value) }}
            fn main() -> i64 {{
                println(make_job().run(42));
                invoke(make_pair().0.run, {value})
            }}
        "#
        );
        let lowered = lower(&source);
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
    }
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

    // Publish the exact once-field facts through an accepted borrowed argument.
    // Then turn that argument into an indirect receiver without changing its
    // type or aggregate descriptor. The refusal must be about ownership.
    let read_source = format!(
        "{} fn observe<T>(callback: T) -> i64 {{ 0 }}",
        source
            .replace("value.a()", "observe(value.a)")
            .replace("value.0()", "observe(value.0)")
    );
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
    let HirExprKind::Call {
        target,
        callee,
        args,
    } = &mut tail.kind
    else {
        panic!("guard fixture tail must be a call");
    };
    assert_eq!(args.len(), 1);
    **callee = args.remove(0);
    *target = CallTarget::IndirectFunctionValue;
    assert!(matches!(
        callee.kind,
        HirExprKind::FieldAccess { .. } | HirExprKind::TupleIndex { .. }
    ));
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
            "{} fn main() -> i64 {{ let value = Two {{ a: answer, b: sibling }}; let first = value.a(); first + value.b() }}",
            declarations(clone)
        );
        assert_once_field_transfer(&source);
    }
}

#[test]
fn once_tuple_fields_never_gain_a_hidden_clone_owner() {
    for capabilities in ["once", "once, clone"] {
        let source = format!("fn answer() -> i64 {{ 41 }} fn main() -> i64 {{ let callback: fn[{capabilities}]() -> i64 = answer; let value = (callback, 1); let first = value.0(); first + value.1 }}");
        assert_once_field_transfer(&source);
    }
}

#[test]
fn partial_jobs_preserve_nested_siblings_reinitialization_and_fault_cleanup() {
    for source in [
        include_str!("../../tests/core-acceptance/cases/partial-job-dispatch.hew"),
        include_str!("../../tests/core-acceptance/cases/partial-job-reuse.hew"),
        include_str!("../../tests/core-acceptance/cases/partial-job-later-argument-fault.hew"),
        include_str!("../../tests/core-acceptance/cases/partial-job-callback-body-fault.hew"),
    ] {
        assert_lowered(source);
    }
}

#[test]
fn a_partial_captured_record_retains_its_remaining_fields() {
    assert_lowered(
        r#"
        type Job { run: fn[once]() -> i64, label: string }
        fn answer() -> i64 { 42 }
        fn main() -> i64 {
            let job = Job { run: answer, label: "captured sibling" };
            let callback = move || { println(job.run()); println(job.label); 0 };
            callback()
        }
    "#,
    );
}

#[test]
fn runtime_field_mutation_preserves_a_partially_consumed_container() {
    assert_lowered(
        r#"
        type Bag { run: fn[once]() -> i64, values: Vec<string> }
        fn answer() -> i64 { 42 }
        fn main() -> i64 {
            var bag = Bag { run: answer, values: Vec.new() };
            println(bag.run());
            bag.values.push("remaining field");
            println(bag.values.len());
            0
        }
    "#,
    );
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
        let transfer = main.blocks.iter().flat_map(|block| &block.ops).find(|op| matches!(&op.kind, SemOpKind::Move { source } if source.value == fields.results[0].id)).expect("the once receiver transfers the extracted field");
        assert!(main.blocks.iter().any(|block| matches!(&block.terminator, SemTerminator::IndirectCall { callee, .. } if callee.decision == BoundaryDecision::Move && callee.operand.value == transfer.results[0].id)));
        assert!(main.blocks.iter().any(|block| matches!(&block.terminator, SemTerminator::IndirectCall { callee, .. } if callee.decision == BoundaryDecision::Borrow && callee.operand.value == fields.results[1].id)));
    }
}
