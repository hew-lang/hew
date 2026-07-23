//! Behavioural contract for `RawMirFunction::coroutine_facts` — the single
//! MIR-side derivation of every coroutine-lowering predicate codegen consumes
//! (ramp declaration, coroutine prologue shape, dispatch-trampoline
//! suspendability). Each test pins one classification edge the consumers
//! diverge on; a regression here means a codegen decision site would flip.

use std::collections::{BTreeMap, HashMap};

use hew_mir::{
    BasicBlock, FunctionCallConv, Place, RawMirFunction, SourceOrigin, Terminator, GEN_BODY_PREFIX,
};
use hew_types::ResolvedTy;

/// A minimal hand-built function whose only interesting content is its block
/// terminators (the facts derivation reads `name` + `blocks` and nothing else).
fn fn_with_terminators(name: &str, terminators: Vec<Terminator>) -> RawMirFunction {
    let blocks = terminators
        .into_iter()
        .enumerate()
        .map(|(id, terminator)| BasicBlock {
            id: u32::try_from(id).expect("test block id fits u32"),
            statements: vec![],
            instructions: vec![],
            terminator,
        })
        .collect();
    RawMirFunction {
        source_origin: SourceOrigin::Unknown,
        name: name.to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64],
        local_names: vec![],
        local_scopes: vec![],
        local_decl_bytes: vec![],
        scope_table: vec![],
        blocks,
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: HashMap::new(),
        suspend_kinds: HashMap::new(),
        lambda_actor_user_param_locals: vec![],
        span: None,
        instr_spans: BTreeMap::new(),
    }
}

#[test]
fn plain_return_function_carries_no_coroutine_facts() {
    let facts = fn_with_terminators("plain", vec![Terminator::Return]).coroutine_facts();
    assert!(!facts.is_coroutine, "a Return-only body is not a ramp");
    assert!(!facts.has_suspend_carrier);
    assert!(!facts.is_generator);
    assert!(!facts.has_explicit_final_suspend);
}

#[test]
fn non_final_suspend_marks_suspend_carrier_and_coroutine() {
    let facts = fn_with_terminators(
        "handler",
        vec![
            Terminator::Suspend {
                resume: 1,
                cleanup: 1,
                is_final: false,
            },
            Terminator::Return,
        ],
    )
    .coroutine_facts();
    assert!(facts.is_coroutine);
    assert!(
        facts.has_suspend_carrier,
        "the trampoline must drive this handler as a coro ramp"
    );
    assert!(!facts.is_generator);
    assert!(
        !facts.has_explicit_final_suspend,
        "is_final: false must NOT count as an explicit final suspend — the \
         prologue would skip synthesising the shared final-suspend block"
    );
}

#[test]
fn final_suspend_sets_explicit_final_suspend() {
    let facts = fn_with_terminators(
        "handler",
        vec![Terminator::Suspend {
            resume: 0,
            cleanup: 0,
            is_final: true,
        }],
    )
    .coroutine_facts();
    assert!(facts.has_explicit_final_suspend);
    assert!(facts.has_suspend_carrier);
    assert!(facts.is_coroutine);
}

#[test]
fn yield_marks_generator_coroutine_but_not_suspend_carrier() {
    let facts = fn_with_terminators(
        "gen_like",
        vec![
            Terminator::Yield {
                value: Place::Local(0),
                next: 1,
            },
            Terminator::Return,
        ],
    )
    .coroutine_facts();
    assert!(facts.is_generator);
    assert!(facts.is_coroutine, "a Yield body lowers as a coro ramp");
    assert!(
        !facts.has_suspend_carrier,
        "Yield is excluded from the trampoline's handler-suspendability \
         predicate — a handler is never a generator body"
    );
    assert!(!facts.has_explicit_final_suspend);
}

#[test]
fn suspending_select_marks_suspend_carrier() {
    let facts = fn_with_terminators(
        "selecting",
        vec![
            Terminator::SuspendingSelect {
                arms: vec![],
                resume: 1,
                cleanup: 1,
            },
            Terminator::Return,
        ],
    )
    .coroutine_facts();
    assert!(facts.has_suspend_carrier);
    assert!(facts.is_coroutine);
    assert!(!facts.is_generator);
}

#[test]
fn no_yield_gen_body_name_is_coroutine_by_name_alone() {
    // The no-yield generator (`gen { 1 }`): its body carries no suspend-family
    // terminator, so ONLY the minted `__hew_gen_body_*` name makes it a ramp —
    // its `MakeGenerator` construction site drives it expecting the handle.
    let name = format!("{GEN_BODY_PREFIX}owner_0");
    let facts = fn_with_terminators(&name, vec![Terminator::Return]).coroutine_facts();
    assert!(
        facts.is_coroutine,
        "a no-yield gen body must still lower as a coro ramp"
    );
    assert!(!facts.has_suspend_carrier);
    assert!(!facts.is_generator);
    assert!(!facts.has_explicit_final_suspend);
}
