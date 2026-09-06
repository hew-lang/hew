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
        .any(|function| function.declaration.full_path() == "increment"));
    assert!(!module
        .functions
        .iter()
        .any(|function| function.declaration.full_path() == "unrelated"));
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
        .find(|function| function.declaration.full_path() == "main")
        .unwrap();
    let plan = hew_sir::place_plan(main, &module.aggregate_shapes, &module.type_facts).unwrap();
    let mut receivers = Vec::new();
    for block in &main.blocks {
        if let SemTerminator::IndirectCall { callee, .. } = &block.terminator {
            assert_eq!(callee.decision, hew_sir::BoundaryDecision::BorrowMut);
            let borrow = main
                .blocks
                .iter()
                .flat_map(|block| &block.ops)
                .find(|op| {
                    op.results
                        .iter()
                        .any(|value| value.id == callee.operand.value)
                })
                .expect("stored receiver loan");
            let SemOpKind::LoadBorrow { place } = &borrow.kind else {
                panic!("mutable field call must borrow its stored owner");
            };
            let projection = plan.projection(*place).unwrap();
            let hew_sir::OwnerRoot::Local(root) = projection.root else {
                panic!("holder must own local storage")
            };
            assert!(main.bindings.iter().any(|binding| binding.name == "holder"
                && binding.target == hew_sir::BindingTarget::Place(root)));
            assert_eq!(projection.path.len(), 1);
            assert_eq!(projection.path[0].field, 0);
            receivers.push(*place);
        }
    }
    assert_eq!(receivers.len(), 2);
    assert_eq!(receivers[0], receivers[1]);
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
            .find(|function| function.declaration.full_path() == "invoke")
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
        .find(|function| function.declaration.full_path() == "advance")
        .unwrap();
    assert_eq!(advance.params[0].own, hew_sir::OwnKind::Guaranteed);
    let copied = advance.blocks.iter().flat_map(|block| &block.ops)
        .find(|op| matches!(&op.kind, SemOpKind::CopyValue { source } if source.value == advance.params[0].value)).unwrap();
    let private = advance
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find_map(|op| match op.kind {
            SemOpKind::StoreInit { place, ref value } if value.value == copied.results[0].id => {
                Some(place)
            }
            _ => None,
        })
        .expect("the private parameter copy must initialize local storage");
    for block in &advance.blocks {
        if let SemTerminator::IndirectCall { callee, .. } = &block.terminator {
            assert_eq!(callee.decision, hew_sir::BoundaryDecision::BorrowMut);
            assert!(block.ops.iter().any(
                |op| matches!(op.kind, SemOpKind::LoadBorrow { place } if place == private)
                    && op.results[0].id == callee.operand.value
            ));
        }
    }
    let consuming = module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "consume_advance")
        .unwrap();
    assert_eq!(consuming.params[0].own, hew_sir::OwnKind::Owned);
    assert!(!consuming.blocks.iter().flat_map(|block| &block.ops)
        .any(|op| matches!(&op.kind, SemOpKind::CopyValue { source } if source.value == consuming.params[0].value)));
}

#[test]
fn borrowed_callable_replacements_share_local_storage_across_branches() {
    let module = lower_source(include_str!(
        "../../tests/core-acceptance/cases/callable-private-parameter-replacement.hew"
    ));
    for (name, replacements) in [("replace_both", 2), ("replace_or_return", 1)] {
        let function = module
            .functions
            .iter()
            .find(|function| function.declaration.full_path() == name)
            .unwrap();
        assert_eq!(function.params[0].own, hew_sir::OwnKind::Guaranteed);
        let hew_sir::BindingTarget::Place(local) = function
            .bindings
            .iter()
            .find(|binding| binding.name == "cb")
            .unwrap()
            .target
        else {
            panic!("private replacements must share one declaration's storage")
        };
        let entry = &function.blocks[0];
        assert!(entry
            .ops
            .iter()
            .any(|op| matches!(op.kind, SemOpKind::AllocPlace { place } if place == local)));
        assert!(
            !entry
                .ops
                .iter()
                .any(|op| matches!(op.kind, SemOpKind::StoreInit { place, .. } if place == local)),
            "a non-copyable input must not initialize an owned slot"
        );
        assert_eq!(
            function
                .blocks
                .iter()
                .flat_map(|block| &block.ops)
                .filter(
                    |op| matches!(op.kind, SemOpKind::StoreAssign { place, .. } if place == local)
                )
                .count(),
            replacements
        );
        for block in &function.blocks {
            if let SemTerminator::IndirectCall { callee, .. } = &block.terminator {
                assert_eq!(callee.decision, BoundaryDecision::BorrowMut);
                assert!(block.ops.iter().any(
                    |op| matches!(op.kind, SemOpKind::LoadBorrow { place } if place == local)
                        && op.results[0].id == callee.operand.value
                ));
            }
            if matches!(
                block.terminator,
                SemTerminator::Return { .. } | SemTerminator::ResumeUnwind
            ) {
                assert_eq!(block.ops.iter().filter(|op| matches!(op.kind, SemOpKind::EndLifetime { place } if place == local)).count(), 1, "every exit must end the declaration, including an empty early-return or acquisition-fault path");
            }
        }
        hew_sir::place_lifetimes(&module, function)
            .expect("all private replacement paths must have checked cleanup");
    }
}

#[test]
fn mixed_borrowed_and_replaced_parameters_are_not_silently_joined() {
    for body in [
        "if flag { cb = fresh(); } else {}",
        "if flag {} else { cb = fresh(); }",
    ] {
        for use_value in ["cb()", "observe(cb)"] {
            let source = format!(
                r"
                fn fresh() -> fn[var]() -> i64 {{ let count = 0; capture(var count) || {{ count += 1; count }} }}
                fn observe(cb: fn[var]() -> i64) -> i64 {{ 0 }}
                fn inspect(var cb: fn[var]() -> i64, flag: bool) -> i64 {{ {body} {use_value} }}
                fn main() -> i64 {{ inspect(fresh(), true) }}
            "
            );
            let parsed = hew_parser::parse(&source);
            assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
            let facts =
                Checker::new(ModuleRegistry::new(Vec::new())).check_program(&parsed.program);
            if use_value == "cb()" {
                assert!(facts
                    .errors
                    .iter()
                    .any(|error| error.kind == hew_types::error::TypeErrorKind::OwnMutateBorrowed));
                continue;
            }
            assert!(facts.errors.is_empty(), "{:?}", facts.errors);
            let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
            assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
            let lowered = lower_module(&hir.module, &facts);
            let status = &lowered
                .statuses
                .iter()
                .find(|status| status.name == "inspect")
                .unwrap()
                .status;
            assert!(
                matches!(status, hew_sir::SirLoweringStatus::Unsupported { reason } if reason == "lexical place identity changed across a control-flow edge"),
                "{body}: {status:?}"
            );
        }
    }
}

#[test]
fn private_replacement_keeps_pre_assignment_reads_borrowed() {
    let module = lower_source(
        r"
        fn fresh() -> fn[var]() -> i64 { let count = 0; capture(var count) || { count += 1; count } }
        fn observe(cb: fn[var]() -> i64) -> i64 { 0 }
        fn replace(var cb: fn[var]() -> i64) -> i64 {
            let before = observe(cb);
            cb = fresh();
            before + cb()
        }
        fn main() -> i64 { replace(fresh()) }
    ",
    );
    let function = module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "replace")
        .unwrap();
    let first_call = function
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::Call { args, .. } if !args.is_empty() => Some(args),
            _ => None,
        })
        .unwrap();
    assert_eq!(first_call[0].decision, BoundaryDecision::Borrow);
    assert_eq!(first_call[0].operand.value, function.params[0].value);
    hew_sir::place_lifetimes(&module, function)
        .expect("the empty slot must end on a pre-assignment call fault");
}

#[test]
fn mutable_aggregate_parameters_keep_callable_fields_private_across_control_flow() {
    lower_source(
        r#"
        type Holder { next: fn[var, clone]() -> i64, label: string }
        fn advance(var holder: Holder, flag: bool) -> i64 {
            if flag { holder.next(); }
            println(holder.label);
            holder.next()
        }
        fn advance_pair(var pair: (fn[var, clone]() -> i64, string)) -> i64 {
            for i in 0..2 { pair.0(); }
            println(pair.1);
            pair.0()
        }
        fn main() -> i64 {
            let count = 10;
            var holder = Holder {
                next: capture(var count) || { count += 1; count },
                label: "private record",
            };
            println(advance(holder, true));
            println(holder.next());
            var pair: (fn[var, clone]() -> i64, string) = (holder.next, "private tuple");
            println(advance_pair(pair));
            pair.0()
        }
        "#,
    );
}

#[test]
fn consumed_mutable_callable_is_not_revived_by_a_later_unit_if() {
    lower_source(
        r#"
        fn answer() -> i64 { 42 }
        fn probe(flag: bool) -> i64 {
            var callback: fn[once]() -> i64 = answer;
            callback();
            if flag { println("yes"); } else { println("no"); }
            0
        }
        fn main() -> i64 { probe(true) + probe(false) }
    "#,
    );
}

#[test]
fn non_clone_callable_can_transfer_through_either_if_arm() {
    lower_source(
        r"
        fn answer() -> i64 { 42 }
        fn probe(flag: bool) -> i64 {
            let callback: fn[once]() -> i64 = answer;
            let selected = if flag { callback } else { callback };
            selected()
        }
        fn main() -> i64 { probe(true) + probe(false) }
    ",
    );
}

#[test]
fn generator_cleanup_cannot_discard_a_fault_or_cancel_successor() {
    let valid = lower_source(
        r#"
        gen fn values() -> string { yield "first"; yield "second"; }
        fn main() { for value in values() { println(value); break; } }
    "#,
    );
    for alter_cancel in [false, true] {
        let mut invalid = valid.clone();
        let function = invalid
            .functions
            .iter_mut()
            .find(|function| {
                function.blocks.iter().any(|block| {
                    matches!(
                        block.terminator,
                        SemTerminator::Suspend {
                            kind: hew_sir::SuspendKind::GeneratorClose { .. },
                            ..
                        }
                    )
                })
            })
            .expect("producer owner has explicit cleanup");
        let entry = function.entry;
        let close = function
            .blocks
            .iter_mut()
            .find(|block| {
                matches!(
                    block.terminator,
                    SemTerminator::Suspend {
                        kind: hew_sir::SuspendKind::GeneratorClose { .. },
                        ..
                    }
                )
            })
            .unwrap();
        let SemTerminator::Suspend { cancel, unwind, .. } = &mut close.terminator else {
            unreachable!()
        };
        if alter_cancel {
            cancel.target = entry;
        } else {
            unwind.target = entry;
        }
        assert!(verify_module(&invalid).iter().any(|diagnostic|
            matches!(&diagnostic.kind, hew_sir::SirDiagnosticKind::InvalidTerminator { reason }
                if reason.contains("GeneratorClose"))));
    }
}

#[test]
fn generator_producer_descriptor_rejects_ordinary_callable_capabilities() {
    let mut module = lower_source(
        r"
        gen fn values() -> i64 { yield 1; }
        fn main() { for value in values() { println(value); } }
    ",
    );
    let producer = module
        .closures
        .iter_mut()
        .find(|closure| closure.generator_yield.is_some())
        .unwrap();
    let hew_types::ResolvedTy::Closure { capabilities, .. } = &mut producer.ty else {
        unreachable!()
    };
    capabilities.call = hew_types::CallableCallMode::Read;
    assert!(!verify_module(&module).is_empty());
}
