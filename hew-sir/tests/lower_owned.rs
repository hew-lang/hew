use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    canonicalize_module_constant_cfg, lower_module, verify_module, BoundaryDecision, SemOpKind,
    SemParamPassing, SemTerminator, SirLoweringStatus, TrapKind,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

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

#[test]
fn core_owned_runtime_sources_lower_to_verified_sir() {
    let sources = [
        (
            "owned-call-return",
            include_str!("../../tests/core-acceptance/cases/owned-call-return.hew"),
            &["echo", "main"][..],
        ),
        (
            "bytes-copy-mutate",
            include_str!("../../tests/core-acceptance/cases/bytes-copy-mutate.hew"),
            &["main"][..],
        ),
        (
            "owned-branch-reassign",
            include_str!("../../tests/core-acceptance/cases/owned-branch-reassign.hew"),
            &["main"][..],
        ),
        (
            "owned-loop",
            include_str!("../../tests/core-acceptance/cases/owned-loop.hew"),
            &["append_mark", "main"][..],
        ),
        (
            "owned-early-return",
            include_str!("../../tests/core-acceptance/cases/owned-early-return.hew"),
            &["choose", "main"][..],
        ),
    ];

    for (name, source, source_functions) in sources {
        let lowered = lower_source(source);
        assert!(
            source_functions.iter().all(|source_function| matches!(
                lowered
                    .statuses
                    .iter()
                    .find(|status| status.name == *source_function)
                    .map(|status| &status.status),
                Some(SirLoweringStatus::Lowered)
            )),
            "{name} must lower each authored source function: {:#?}",
            lowered.statuses
        );
        assert!(
            verify_module(&lowered.module).is_empty(),
            "{name} must produce verified SIR: {:#?}",
            verify_module(&lowered.module)
        );
    }
}

#[test]
fn constant_owned_branch_that_needs_cleanup_remains_executable() {
    let mut lowered = lower_source(include_str!(
        "../../tests/core-acceptance/cases/owned-branch-reassign.hew"
    ));
    assert!(verify_module(&lowered.module).is_empty());

    let main_before = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("owned branch acceptance entry must lower")
        .clone();
    let reports = canonicalize_module_constant_cfg(&mut lowered.module)
        .expect("an optional CFG fold must preserve an unsafe-to-discard branch");
    let main_report = reports
        .iter()
        .find(|(callable, _)| *callable == main_before.callable)
        .map(|(_, report)| report)
        .expect("the main callable must report canonicalization");
    let main_after = lowered
        .module
        .functions
        .iter()
        .find(|function| function.callable == main_before.callable)
        .expect("canonicalization must retain the main callable");

    assert_eq!(main_report.folded_branches, 0);
    assert_eq!(main_after, &main_before);
    assert!(verify_module(&lowered.module).is_empty());
}

#[test]
fn bytes_runtime_transform_and_failure_edges_are_explicit_and_checked() {
    let lowered = lower_source(include_str!(
        "../../tests/core-acceptance/cases/bytes-copy-mutate.hew"
    ));
    assert!(verify_module(&lowered.module).is_empty());
    let main = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("bytes acceptance entry must lower");

    let (push_result, push_normal) = main
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::RtCall {
                family: hew_types::RuntimeCallFamily::BytesPush,
                args,
                result: hew_sir::CallResult::Value(result),
                normal,
                unwind: hew_sir::CallUnwind::NotApplicable,
                ..
            } => {
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].decision, BoundaryDecision::Move);
                assert_eq!(args[1].decision, BoundaryDecision::Copy);
                assert_eq!(result.ty, hew_types::ResolvedTy::Bytes);
                assert_eq!(result.own, hew_sir::OwnKind::Owned);
                Some((result.id, normal.target))
            }
            _ => None,
        })
        .expect("bytes.push must consume one owner and return its updated owner");
    let push_continuation = main.blocks[push_normal.0 as usize].args[0].value;
    assert_ne!(push_result, push_continuation);
    assert_eq!(
        main.bindings
            .iter()
            .rev()
            .find(|binding| binding.name == "copy")
            .map(|binding| binding.target),
        Some(hew_sir::BindingTarget::Value(push_continuation)),
        "the mutable source binding must name the updated owner"
    );

    let index_unwind = main
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::RtCall {
                family: hew_types::RuntimeCallFamily::BytesIndex,
                unwind: hew_sir::CallUnwind::Cleanup(edge),
                ..
            } => Some(edge.target),
            _ => None,
        })
        .expect("bytes index must have an explicit bounds-failure edge");
    let failure = &main.blocks[index_unwind.0 as usize];
    assert!(failure
        .ops
        .iter()
        .any(|op| matches!(op.kind, SemOpKind::DestroyValue { .. })));
    assert_eq!(
        failure.terminator,
        SemTerminator::Trap {
            kind: TrapKind::IndexOutOfBounds
        }
    );

    let mut wrong_push_boundary = lowered.module.clone();
    let push = wrong_push_boundary
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .find_map(|block| match &mut block.terminator {
            SemTerminator::RtCall {
                family: hew_types::RuntimeCallFamily::BytesPush,
                args,
                ..
            } => Some(args),
            _ => None,
        })
        .expect("fixture must contain bytes.push");
    push[0].decision = BoundaryDecision::Borrow;
    assert!(!verify_module(&wrong_push_boundary).is_empty());

    let mut missing_bounds_edge = lowered.module.clone();
    let index = missing_bounds_edge
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .find_map(|block| match &mut block.terminator {
            SemTerminator::RtCall {
                family: hew_types::RuntimeCallFamily::BytesIndex,
                unwind,
                ..
            } => Some(unwind),
            _ => None,
        })
        .expect("fixture must contain bytes index");
    *index = hew_sir::CallUnwind::NotApplicable;
    assert!(!verify_module(&missing_bounds_edge).is_empty());
}

#[test]
fn owned_string_and_bytes_calls_copy_borrows_and_clean_both_exits() {
    let lowered = lower_source(
        r#"
        fn keep_text(value: string) -> string { return value; }
        fn keep_bytes(value: bytes) -> bytes { value }
        fn use_values(text: string, data: bytes) {
            let kept_text = keep_text(text);
            let kept_bytes = keep_bytes(data);
        }
        fn main() { use_values("hello", b"ok"); }
        "#,
    );
    assert!(
        ["keep_text", "keep_bytes", "use_values", "main"]
            .into_iter()
            .all(|name| matches!(
                lowered
                    .statuses
                    .iter()
                    .find(|status| status.name == name)
                    .map(|status| &status.status),
                Some(SirLoweringStatus::Lowered)
            )),
        "owned call graph must lower: {:#?}",
        lowered.statuses
    );
    assert_eq!(
        lowered
            .module
            .string_literals
            .values()
            .cloned()
            .collect::<Vec<_>>(),
        ["hello"]
    );
    assert_eq!(
        lowered
            .module
            .bytes_literals
            .values()
            .cloned()
            .collect::<Vec<_>>(),
        [b"ok".to_vec()]
    );

    for callable in lowered
        .module
        .callables
        .iter()
        .filter(|callable| callable.symbol != "main")
    {
        assert!(callable
            .signature
            .params
            .iter()
            .all(|param| param.passing == SemParamPassing::Borrow));
    }
    for function in lowered
        .module
        .functions
        .iter()
        .filter(|function| function.name == "keep_text" || function.name == "keep_bytes")
    {
        assert!(function
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .any(|op| { matches!(op.kind, SemOpKind::CopyValue { .. }) }));
    }
    for function in &lowered.module.functions {
        for block in &function.blocks {
            if let SemTerminator::Call { args, .. } = &block.terminator {
                assert!(args
                    .iter()
                    .all(|argument| argument.decision == BoundaryDecision::Borrow));
            }
        }
    }
    let cleanup_blocks = lowered
        .module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .filter(|block| {
            block
                .ops
                .iter()
                .any(|op| matches!(op.kind, SemOpKind::DestroyValue { .. }))
        })
        .collect::<Vec<_>>();
    assert!(cleanup_blocks
        .iter()
        .any(|block| matches!(block.terminator, SemTerminator::ResumeUnwind)));
    assert!(cleanup_blocks
        .iter()
        .any(|block| matches!(block.terminator, SemTerminator::Return { .. })));
    assert!(
        verify_module(&lowered.module).is_empty(),
        "owned source SIR must verify: {:#?}",
        verify_module(&lowered.module)
    );
}

#[test]
fn owned_binding_aliases_copy_and_preserve_source() {
    let lowered = lower_source(
        r#"
        fn keep_text(value: string) {}
        fn keep_bytes(value: bytes) {}

        fn main() {
            let original_text = "original";
            let copied_text = original_text;
            keep_text(original_text);
            keep_text(copied_text);

            let original_bytes = b"original";
            var copied_bytes = original_bytes;
            keep_bytes(original_bytes);
            keep_bytes(copied_bytes);
        }
        "#,
    );
    assert!(
        ["keep_text", "keep_bytes", "main"]
            .into_iter()
            .all(|name| matches!(
                lowered
                    .statuses
                    .iter()
                    .find(|status| status.name == name)
                    .map(|status| &status.status),
                Some(SirLoweringStatus::Lowered)
            )),
        "owned aliases must leave both source and copy usable: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "owned aliases must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );

    let main = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("main must lower");
    assert_eq!(
        main.blocks
            .iter()
            .flat_map(|block| &block.ops)
            .filter(|op| matches!(op.kind, SemOpKind::CopyValue { .. }))
            .count(),
        2,
        "each ordinary owned alias must be an independent copy"
    );
    assert_eq!(
        main.blocks
            .iter()
            .filter_map(|block| match &block.terminator {
                SemTerminator::Call { args, .. } => Some(args),
                _ => None,
            })
            .filter(|args| { args.len() == 1 && args[0].decision == BoundaryDecision::Borrow })
            .count(),
        4,
        "both originals and both aliases must remain available to borrowed calls"
    );
}

#[test]
fn discarded_owned_binding_reads_preserve_source() {
    let lowered = lower_source(
        r#"
        fn keep_text(value: string) {}
        fn keep_bytes(value: bytes) {}

        fn main() {
            let text = "text";
            text;
            keep_text(text);

            let data = b"data";
            data;
            keep_bytes(data);
        }
        "#,
    );
    assert!(
        matches!(
            lowered
                .statuses
                .iter()
                .find(|status| status.name == "main")
                .map(|status| &status.status),
            Some(SirLoweringStatus::Lowered)
        ),
        "discarding a binding read must not consume its owner: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "discarded binding reads must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );
}

#[test]
fn discarded_owned_block_tails_copy_and_preserve_source() {
    let lowered = lower_source(
        r#"
        fn keep_text(value: string) {}
        fn keep_bytes(value: bytes) {}

        fn main() {
            let text = "text";
            { text };
            keep_text(text);

            let data = b"data";
            { data };
            keep_bytes(data);
        }
        "#,
    );
    assert!(
        matches!(
            lowered
                .statuses
                .iter()
                .find(|status| status.name == "main")
                .map(|status| &status.status),
            Some(SirLoweringStatus::Lowered)
        ),
        "discarding a block result must not consume its source: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "discarded block results must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );
}

#[test]
fn owned_block_expressions_destroy_inner_locals_at_scope_exit() {
    let lowered = lower_source(
        r#"
        fn keep(value: string) {}

        fn main() {
            let result = {
                let scratch = "scratch";
                "result"
            };
            keep(result);

            var selected = "initial";
            if true {
                selected = {
                    let branch_scratch = "branch";
                    "left"
                };
            } else {
                selected = "right";
            }
            keep(selected);
        }
        "#,
    );
    assert!(
        matches!(
            lowered
                .statuses
                .iter()
                .find(|status| status.name == "main")
                .map(|status| &status.status),
            Some(SirLoweringStatus::Lowered)
        ),
        "block-local owners must not escape into their enclosing expression state: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "scoped owned block expressions must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );

    let main = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("main must lower");
    for source in ["scratch", "branch"] {
        let literal = lowered
            .module
            .string_literals
            .iter()
            .find_map(|(id, value)| (value == source).then_some(*id))
            .expect("scratch literal must be interned");
        assert!(
            main.blocks.iter().any(|block| {
                let Some(owner) = block.ops.iter().find_map(|op| match op.kind {
                    SemOpKind::ConstStr(id) if id == literal => {
                        op.results.first().map(|result| result.id)
                    }
                    _ => None,
                }) else {
                    return false;
                };
                block.ops.iter().any(|op| {
                    matches!(
                        op.kind,
                        SemOpKind::DestroyValue { ref value } if value.value == owner
                    )
                })
            }),
            "`{source}` must be destroyed in its defining lexical block"
        );
    }
}

#[test]
fn nested_owned_call_arguments_live_until_outer_call() {
    for main_body in ["pair(make(), make());", "pair(\"literal\", make());"] {
        let lowered = lower_source(&format!(
            r#"
            fn make() -> string {{ "made" }}
            fn pair(first: string, second: string) {{}}
            fn main() {{ {main_body} }}
            "#,
        ));
        assert!(
            ["make", "pair", "main"].into_iter().all(|name| matches!(
                lowered
                    .statuses
                    .iter()
                    .find(|status| status.name == name)
                    .map(|status| &status.status),
                Some(SirLoweringStatus::Lowered)
            )),
            "a later argument must not consume an earlier outer-call argument ({main_body}): {:#?}",
            lowered.statuses
        );
        assert!(
            verify_module(&lowered.module).is_empty(),
            "nested argument evaluation must produce verified SIR ({main_body}): {:#?}",
            verify_module(&lowered.module)
        );

        let main = lowered
            .module
            .functions
            .iter()
            .find(|function| function.name == "main")
            .expect("main must lower");
        assert!(main.blocks.iter().any(|block| matches!(
            &block.terminator,
            SemTerminator::Call { args, .. }
                if args.len() == 2
                    && args.iter().all(|argument| argument.decision == BoundaryDecision::Borrow)
        )));
    }
}

#[test]
fn owned_string_reassignment_loop_and_early_return_verify() {
    let lowered = lower_source(
        r#"
        fn choose(flag: bool, input: string) -> string {
            var selected = "fallback";
            if flag {
                selected = input;
            } else {
                selected = "other";
            }

            var keep = flag;
            while keep {
                selected = "loop";
                keep = false;
            }

            if flag {
                return selected;
            }
            return "tail";
        }

        fn main() {
            let chosen = choose(true, "input");
        }
        "#,
    );
    assert!(
        ["choose", "main"].into_iter().all(|name| matches!(
            lowered
                .statuses
                .iter()
                .find(|status| status.name == name)
                .map(|status| &status.status),
            Some(SirLoweringStatus::Lowered)
        )),
        "owned control-flow graph must lower: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "owned control-flow SIR must verify: {:#?}",
        verify_module(&lowered.module)
    );
    let choose = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "choose")
        .expect("choose must lower");
    assert!(choose.blocks.iter().any(|block| {
        block.args.iter().any(|arg| {
            arg.ty == hew_types::ResolvedTy::String && arg.own == hew_sir::OwnKind::Owned
        })
    }));
    assert!(choose.blocks.iter().any(|block| matches!(
        &block.terminator,
        SemTerminator::Goto(edge) if !edge.args.is_empty() && edge.target.0 <= block.id.0
    )));
    assert!(
        choose
            .blocks
            .iter()
            .filter(|block| matches!(block.terminator, SemTerminator::Return { value: Some(_) }))
            .count()
            >= 2
    );
}

#[test]
fn checked_arithmetic_failure_cleans_live_owner_before_exact_trap() {
    let lowered = lower_source(
        r#"
        fn increment(value: i64) -> i64 {
            let live = "owned";
            return value + 1;
        }

        fn main() {
            let answer = increment(41);
        }
        "#,
    );
    let increment = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "increment")
        .expect("increment must lower");
    let failures = increment
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::CheckedBinary { failures, .. } => Some(failures),
            _ => None,
        })
        .expect("integer addition must lower as checked arithmetic");
    assert_eq!(
        failures
            .iter()
            .map(|failure| failure.kind)
            .collect::<Vec<_>>(),
        [TrapKind::IntegerOverflow]
    );
    let failure_block = increment
        .blocks
        .iter()
        .find(|block| block.id == failures[0].edge.target)
        .expect("checked failure edge must target a block");
    assert!(failure_block
        .ops
        .iter()
        .any(|op| matches!(op.kind, SemOpKind::DestroyValue { .. })));
    assert_eq!(
        failure_block.terminator,
        SemTerminator::Trap {
            kind: TrapKind::IntegerOverflow
        }
    );
    assert!(verify_module(&lowered.module).is_empty());

    let mut wrong_kind = lowered.module.clone();
    let failure = wrong_kind
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .find_map(|block| match &mut block.terminator {
            SemTerminator::CheckedBinary { failures, .. } => failures.first_mut(),
            _ => None,
        })
        .expect("mutated fixture must contain checked arithmetic");
    failure.kind = TrapKind::DivideByZero;
    assert!(
        !verify_module(&wrong_kind).is_empty(),
        "the verifier must reject a checked edge with the wrong failure kind"
    );

    let mut non_trapping = lowered.module.clone();
    let (function_index, failure_target) = non_trapping
        .functions
        .iter()
        .enumerate()
        .find_map(|(function_index, function)| {
            function.blocks.iter().find_map(|block| {
                let SemTerminator::CheckedBinary { failures, .. } = &block.terminator else {
                    return None;
                };
                Some((function_index, failures[0].edge.target))
            })
        })
        .expect("mutated fixture must contain a checked failure edge");
    non_trapping.functions[function_index].blocks[failure_target.0 as usize].terminator =
        SemTerminator::Goto(hew_sir::Edge {
            target: failure_target,
            args: Vec::new(),
        });
    assert!(
        !verify_module(&non_trapping).is_empty(),
        "the verifier must reject a checked failure edge that can loop instead of trapping"
    );
}
