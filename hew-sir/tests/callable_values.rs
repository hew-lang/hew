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

#[test]
fn captured_snapshots_and_escaped_environments_have_exact_body_instances() {
    let module = lower_source(
        r#"
        fn heading() -> fn() -> string {
            var text = "Draft";
            let read = || text;
            text = "Published";
            read
        }
        fn main() -> i64 {
            let read = heading();
            println(read());
            println(read());
            let amount = 40;
            let add = |value: i64| value + amount;
            add(2)
        }
    "#,
    );
    assert_eq!(module.closures.len(), 2);
    for closure in &module.closures {
        let body = module
            .functions
            .iter()
            .find(|body| body.callable == closure.body)
            .unwrap();
        assert_eq!(body.places.len(), closure.fields.len());
        assert!(body
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .any(|operation| matches!(operation.kind, SemOpKind::LoadCopy { .. })));
    }
}

#[test]
fn copied_private_counter_uses_mutable_receiver_and_capture_storage() {
    let module = lower_source(
        r"
        fn make_counter(start: i64) -> fn[var, clone]() -> i64 {
            let count = start;
            capture(var count) || { count = count + 1; count }
        }
        fn main() -> i64 {
            var first = make_counter(10);
            println(first());
            var second = first;
            println(first());
            println(first());
            second()
        }
    ",
    );
    assert!(module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.ops)
        .any(|op| matches!(op.kind, SemOpKind::StoreAssign { .. })));
    assert!(module.functions.iter().flat_map(|function| &function.blocks)
        .any(|block| matches!(&block.terminator, SemTerminator::IndirectCall { callee, .. } if callee.decision == BoundaryDecision::BorrowMut)));
}

#[test]
fn nested_owned_closure_can_outlive_the_environment_that_created_it() {
    let module = lower_source(
        r#"
        fn factory() -> fn() -> fn() -> string {
            let word = "Ready";
            || { let local = word; || local }
        }
        fn main() -> i64 {
            let outer = factory();
            let inner = outer();
            println(inner());
            0
        }
    "#,
    );
    assert_eq!(module.closures.len(), 2);
    assert_eq!(
        module.closures[1].instance.enclosing,
        module.closures[0].body
    );
}

#[test]
fn consuming_capture_is_taken_before_invocation_and_environment_cleanup() {
    let module = lower_source(
        r"
        fn answer() -> i64 { 42 }
        fn main() -> i64 {
            let callback: fn[once]() -> i64 = answer;
            let outer = move || callback();
            outer()
        }
    ",
    );
    let closure = &module.closures[0];
    let body = module
        .functions
        .iter()
        .find(|function| function.callable == closure.body)
        .unwrap();
    assert_eq!(body.params[0].own, hew_sir::OwnKind::Owned);
    assert!(body
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .any(|op| matches!(op.kind, SemOpKind::LoadTake { .. })));
}

#[test]
fn captured_receiver_loan_ends_when_argument_evaluation_faults() {
    lower_source(
        r"
        fn increment(value: i64) -> i64 { value + 1 }
        fn main() -> i64 {
            let callback = increment;
            let outer = move |divisor: i64| callback(100 / divisor);
            outer(0)
        }
    ",
    );
}

#[test]
fn remaining_capture_can_be_borrowed_after_another_capture_is_consumed() {
    lower_source(
        r"
        fn answer() -> i64 { 41 }
        fn increment(value: i64) -> i64 { value + 1 }
        fn main() -> i64 {
            let once: fn[once]() -> i64 = answer;
            let read = increment;
            let outer = move || { let value = once(); read(value) };
            outer()
        }
    ",
    );
}

#[test]
fn callable_captures_compose_with_conditional_result_and_optional_payloads() {
    for body in [
        "if flag { .Ok(.Some(capture(var count) || { count = count + 1; count })) } else { .Ok(.None) }",
        "match flag { true => .Ok(.Some(capture(var count) || { count = count + 1; count })), false => .Ok(.None), }",
        "match flag { selected if selected => .Ok(.Some(capture(var count) || { count = count + 1; count })), _ => .Ok(.None), }",
    ] {
        lower_source(&format!(r"
            fn choose(flag: bool) -> Result<Option<fn[var, clone]() -> i64>, string> {{
                let count: i64 = 0;
                {body}
            }}
            fn main() -> i64 {{
                match choose(true) {{
                    .Ok(.Some(callback)) => {{ var counter = callback; counter() }},
                    _ => 0,
                }}
            }}
        "));
    }
}

#[test]
fn callable_values_coerce_in_records_and_explicit_clone_preserves_capabilities() {
    lower_source(
        r"
        type Holder { callback: fn[clone](i64) -> i64, }
        fn increment(value: i64) -> i64 { value + 1 }
        fn main() -> i64 {
            let holder = Holder { callback: increment };
            let copied = clone holder.callback;
            copied(41)
        }
    ",
    );
}

#[test]
fn mutable_callable_field_invocation_borrows_the_stored_environment() {
    let module = lower_source(
        r"
        type Holder { next: fn[var, clone]() -> i64, }
        fn main() -> i64 {
            let count = 0;
            var holder = Holder { next: capture(var count) || { count = count + 1; count } };
            println(holder.next());
            holder.next()
        }
    ",
    );
    let main = module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .unwrap();
    for block in &main.blocks {
        if let SemTerminator::IndirectCall { callee, .. } = &block.terminator {
            assert!(main
                .blocks
                .iter()
                .flat_map(|block| &block.ops)
                .any(
                    |op| matches!(op.kind, SemOpKind::AggregateProjectBorrow { .. })
                        && op
                            .results
                            .iter()
                            .any(|value| value.id == callee.operand.value)
                ));
        }
    }
}

#[test]
fn declared_consuming_parameters_own_their_normal_and_fault_cleanup() {
    for argument in ["41", "100 / 0"] {
        let module = lower_source(&format!(
            r#"
            fn invoke(consume callback: fn[once](i64) -> i64, value: i64) -> i64 {{
                let local = "callee owner";
                let answer = callback(value);
                println(local);
                answer
            }}
            fn main() -> i64 {{
                let text = "captured owner";
                let callback: fn[once, clone](i64) -> i64 = move |value: i64| {{
                    println(text);
                    value + 1
                }};
                invoke(callback, {argument})
            }}
        "#
        ));
        let invoke = module
            .functions
            .iter()
            .find(|function| function.name == "invoke")
            .unwrap();
        assert_eq!(invoke.params[0].own, hew_sir::OwnKind::Owned);
        assert_eq!(
            module.callable(invoke.callable).unwrap().signature.params[0].passing,
            hew_sir::SemParamPassing::Consume
        );
    }
}

#[test]
fn generic_consuming_parameter_preserves_its_concrete_owned_contract() {
    lower_source(
        r#"
        fn forward<T>(consume value: T) -> T { value }
        fn main() -> i64 {
            let text = "owned generic value";
            let result = forward(text);
            println(result);
            println(forward(42));
            0
        }
    "#,
    );
}

#[test]
fn captured_callable_transfers_into_a_declared_consuming_parameter() {
    lower_source(
        r"
        fn take(consume callback: fn[once]() -> i64) -> i64 { callback() }
        fn answer() -> i64 { 42 }
        fn main() -> i64 {
            let callback: fn[once, clone]() -> i64 = answer;
            let outer = move || take(callback);
            outer()
        }
    ",
    );
}

#[test]
fn mutable_callable_parameters_keep_private_state_without_caller_visible_borrows() {
    let module = lower_source(
        r"
        fn advance(var callback: fn[var, clone]() -> i64) -> i64 { callback(); callback() }
        fn consume_advance(consume var callback: fn[var]() -> i64) -> i64 { callback(); callback() }
        fn main() -> i64 {
            let count = 10;
            var counter: fn[var, clone]() -> i64 = capture(var count) || { count += 1; count };
            println(advance(counter));
            println(counter());
            consume_advance(counter)
        }
    ",
    );
    let advance = module
        .functions
        .iter()
        .find(|function| function.name == "advance")
        .unwrap();
    assert_eq!(advance.params[0].own, hew_sir::OwnKind::Guaranteed);
    let copied = advance.blocks.iter().flat_map(|block| &block.ops)
        .find(|op| matches!(&op.kind, SemOpKind::CopyValue { source } if source.value == advance.params[0].value)).unwrap();
    for block in &advance.blocks {
        if let SemTerminator::IndirectCall { callee, .. } = &block.terminator {
            assert_eq!(callee.operand.value, copied.results[0].id);
        }
    }
    let consuming = module
        .functions
        .iter()
        .find(|function| function.name == "consume_advance")
        .unwrap();
    assert_eq!(consuming.params[0].own, hew_sir::OwnKind::Owned);
    assert!(!consuming.blocks.iter().flat_map(|block| &block.ops)
        .any(|op| matches!(&op.kind, SemOpKind::CopyValue { source } if source.value == consuming.params[0].value)));
}
