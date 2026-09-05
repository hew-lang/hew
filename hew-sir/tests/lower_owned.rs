use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BoundaryDecision, SemOpKind, SemParamPassing, SemTerminator,
    SirLoweringStatus, TrapKind,
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
