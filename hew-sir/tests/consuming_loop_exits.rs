//! Source ownership survives loop exits through lexical Local storage.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{BindingTarget, OwnKind, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn assert_local_source(source: &str) {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = hew_sir::lower_module(&hir.module, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    let diagnostics = hew_sir::verify_module(&lowered.module);
    assert!(
        diagnostics.is_empty(),
        "{diagnostics:?}\n{}",
        hew_sir::dump_sir(&lowered.module)
    );
    for function in &lowered.module.functions {
        for binding in &function.bindings {
            if let BindingTarget::Value(value) = binding.target {
                let own = function
                    .params
                    .iter()
                    .chain(function.blocks.iter().flat_map(|block| &block.args))
                    .find(|argument| argument.value == value)
                    .map(|argument| argument.own)
                    .or_else(|| {
                        function
                            .blocks
                            .iter()
                            .flat_map(|block| &block.ops)
                            .flat_map(|op| &op.results)
                            .find(|result| result.id == value)
                            .map(|result| result.own)
                    });
                assert_ne!(
                    own,
                    Some(OwnKind::Owned),
                    "lexical binding {} retains an SSA owner",
                    binding.name
                );
            }
        }
        hew_sir::place_lifetimes(&lowered.module, function)
            .expect("source Local storage needs checked cleanup on every exit");
    }
}

#[test]
fn lexical_callbacks_survive_skipped_taken_and_nested_loop_exits() {
    assert_local_source(include_str!(
        "../../tests/core-acceptance/cases/local-callback-loop-exits.hew"
    ));
}

#[test]
fn a_later_argument_fault_cleans_transferred_callbacks_and_lexical_places() {
    assert_local_source(include_str!(
        "../../tests/core-acceptance/cases/local-callback-loop-argument-fault.hew"
    ));
}

#[test]
fn callback_body_faults_preserve_caller_and_capture_cleanup() {
    assert_local_source(include_str!(
        "../../tests/core-acceptance/cases/local-callback-loop-body-fault.hew"
    ));
}
