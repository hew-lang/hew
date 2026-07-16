//! Drop-elaboration unit tests.
//!
//! Cluster 3 introduces structural per-`ExitPath` `DropPlan` lists and
//! `BlockKind::Cleanup` cleanup blocks. The integer-only spine has no
//! `@resource` / `@linear` construction surface yet (no HIR type-decl
//! items in the v0.5 ladder — see plan R-C3.5), so these tests
//! validate the elaboration substrate via hew-lang source programs
//! that hit the existing non-`BitCopy` paths (String -> `CowValue`)
//! plus the structural invariants the pass enforces independent of
//! value class.

use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::{
    lower_hir_module, BlockKind, DropKind, ElaboratedMirFunction, ExitPath, IrPipeline, Place,
    Terminator,
};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");
    lower_hir_module(&output.module)
}

fn checked_pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:?}", tc_output.errors);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");
    lower_hir_module(&output.module)
}

fn main_function<'a, T>(items: &'a [T], name: impl Fn(&'a T) -> &'a str) -> &'a T {
    items
        .iter()
        .find(|item| name(item) == "main")
        .expect("main present")
}

fn main_ask_reply_dest(p: &IrPipeline) -> Place {
    let main = main_function(&p.raw_mir, |func| func.name.as_str());
    let mut ask_reply_dests = main
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            Terminator::Ask { reply_dest, .. } => Some(*reply_dest),
            _ => None,
        });
    let reply_dest = ask_reply_dests
        .next()
        .expect("main should contain one actor ask terminator");
    assert!(
        ask_reply_dests.next().is_none(),
        "main should contain exactly one actor ask terminator"
    );
    reply_dest
}

fn first(p: &IrPipeline) -> &ElaboratedMirFunction {
    &p.elaborated_mir[0]
}

#[test]
fn spine_integer_function_has_single_normal_block_and_empty_return_drop_plan() {
    // hello_int's shape: integer-only, no owned locals, single Return.
    // The elaboration substrate must produce exactly one Normal block
    // and a single ExitPath::Return entry with an empty DropPlan.
    let p = pipeline("fn main() -> i64 { 42 }");
    let func = first(&p);

    assert_eq!(func.blocks.len(), 1);
    assert_eq!(func.blocks[0].kind, BlockKind::Normal);
    assert!(func.blocks[0].drops.is_empty());

    assert_eq!(func.drop_plans.len(), 1);
    let (exit, plan) = &func.drop_plans[0];
    assert!(matches!(exit, ExitPath::Return { .. }));
    assert!(plan.drops.is_empty(), "spine has no @resource owned locals");
}

#[test]
fn coroutine_schema_is_none_on_non_generator_functions() {
    // CoroutineSchema is declared scaffold per plan §1; Cluster 3 does
    // not construct it. Every function shipped through the integer spine
    // must carry `coroutine: None`.
    let p = pipeline("fn main() -> i64 { 1 + 2 }");
    assert!(first(&p).coroutine.is_none());
}

#[test]
fn add42_spine_function_carries_return_exit_path() {
    // add42 shape: a single Return terminator with one parameter.
    // The elaboration pass must enumerate the Return exit even when
    // the function has no owned locals.
    let p = pipeline("fn main() -> i64 { 42 + 0 }");
    let func = first(&p);
    assert_eq!(
        func.drop_plans
            .iter()
            .filter(|(e, _)| matches!(e, ExitPath::Return { .. }))
            .count(),
        1,
        "exactly one Return exit on the spine"
    );
}

#[test]
fn nonescaping_cowvalue_string_gets_function_scope_drop() {
    // W5-011 P3: a `string` local that never escapes (not consumed, not
    // passed to a call, not stored in an aggregate/container, not captured)
    // is the sole owner of its heap buffer at scope exit. The drop
    // elaborator emits a single `DropKind::CowHeap { hew_string_drop }` for
    // it — closing the accumulating helper-local leak the lane targets.
    // This replaces the pre-P3 invariant that asserted CoW strings received
    // *no* structural drop.
    let p = pipeline(r#"fn main() { let _s = "hello"; }"#);
    let func = first(&p);
    let return_plan = func
        .drop_plans
        .iter()
        .find(|(e, _)| matches!(e, ExitPath::Return { .. }))
        .expect("Return exit present on every function");
    assert_eq!(
        return_plan.1.drops.len(),
        1,
        "a non-escaping String local gets exactly one scope-exit drop; got {:?}",
        return_plan.1.drops
    );
    assert!(
        matches!(
            return_plan.1.drops[0].kind,
            DropKind::CowHeap {
                release: hew_mir::CowHeapRelease::String
            }
        ),
        "the drop must be a CowHeap release via hew_string_drop; got {:?}",
        return_plan.1.drops[0].kind
    );
}

#[test]
fn awaited_actor_ask_lowers_to_ask_terminator_with_no_diagnostics() {
    // R-ASK unification (R312/Q368): `await echo.get()` lowers to an Ask
    // terminator with a reply_dest slot for the raw reply value and a
    // result_dest slot for the Result<string, AskError> binding.  The
    // pipeline must be diagnostic-free and the Ask terminator must be present.
    let awaited = checked_pipeline(
        r#"
        actor Echo {
            receive fn get() -> string {
                "pong"
            }
        }

        fn main() {
            let echo = spawn Echo;
            let reply = await echo.get();
        }
        "#,
    );

    assert!(awaited.diagnostics.is_empty(), "{:?}", awaited.diagnostics);

    // The Ask terminator must be present with the R-ASK fields.
    let _reply_dest = main_ask_reply_dest(&awaited);
}

#[test]
fn call_arg_source_and_retained_result_are_both_freed() {
    // By-value string parameters borrow. Returning that parameter mints a
    // retained owner in `id`, so the caller keeps `s`'s drop and `_t` carries
    // its own balancing drop.
    let p = pipeline(
        r#"fn id(x: string) -> string { return x; }
           fn main() { let s = "hello"; let _t = id(s); }"#,
    );
    let func = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main present");
    let return_plan = func
        .drop_plans
        .iter()
        .find(|(e, _)| matches!(e, ExitPath::Return { .. }))
        .expect("Return exit present on every function");
    let cow_drops = return_plan
        .1
        .drops
        .iter()
        .filter(|d| matches!(d.kind, DropKind::CowHeap { .. }))
        .count();
    assert_eq!(
        cow_drops, 2,
        "the borrowed argument source and retained result each own one drop; got {:?}",
        return_plan.1.drops
    );
}

#[test]
fn user_record_string_field_drops_record_once() {
    let p = checked_pipeline(
        r#"
        type User {
            id: i64,
            name: string,
        }

        fn main() {
            let first = "Ada";
            let last = "Lovelace";
            let full = first + " " + last;
            let user = User { id: 7, name: full };
            println(user.name);
        }
        "#,
    );
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main present");
    let return_plan = func
        .drop_plans
        .iter()
        .find(|(e, _)| matches!(e, ExitPath::Return { .. }))
        .expect("Return exit present on every function");
    let record_drops = return_plan
        .1
        .drops
        .iter()
        .filter(|d| matches!(d.kind, DropKind::RecordInPlace))
        .count();
    let cow_drops = return_plan
        .1
        .drops
        .iter()
        .filter(|d| matches!(d.kind, DropKind::CowHeap { .. }))
        .count();
    assert_eq!(
        record_drops, 1,
        "owned-string record must get exactly one in-place record drop; got {:?}",
        return_plan.1.drops
    );
    assert_eq!(
        cow_drops, 2,
        "the borrowed concat operands keep their drops; the last-use `full` handoff \
         is owned only by the record"
    );
}

#[test]
fn elaborated_function_blocks_match_checked_block_id() {
    // Plan §1: "Carries the same `id` as the corresponding
    // `RawMirFunction::blocks[id]` for normal blocks". For the
    // single-block spine the normal block carries id 0; cleanup
    // blocks (when present) take ids starting at max(normal-id)+1.
    let p = pipeline("fn main() -> i64 { 7 }");
    let func = first(&p);
    let normals: Vec<_> = func
        .blocks
        .iter()
        .filter(|b| b.kind == BlockKind::Normal)
        .collect();
    assert_eq!(normals.len(), 1);
    assert_eq!(normals[0].id, 0);
}

// ---------- Slice 4: per-exit live-set drop plan ----------
//
// enumerate_exits now consumes the dataflow's per-block exit-state
// map and narrows the function-wide LIFO drop sequence to bindings
// whose state at each Return exit is Live (or MaybeConsumed, which
// the move-checker rejects upstream but the elaborator treats as
// still-needing-a-drop for graceful failure of pipelines that
// bypassed the rejection). The integer-only spine has no
// AffineResource owned locals in any test, so the live-set narrowing
// is structurally invisible — the elaborated drop plan stays empty.
// This test pins the no-narrowing-equivalence at the single-Return
// shape so a future surface that constructs @resource bindings can
// add per-exit assertions on top.

#[test]
fn single_return_drop_plan_is_function_wide_lifo_on_spine_today() {
    // hello_int has zero owned locals; LIFO is empty; per-exit
    // narrowing of an empty sequence is also empty. The pin is the
    // shape — one Return exit with an empty plan.
    let p = pipeline("fn main() -> i64 { 42 }");
    let func = first(&p);
    let return_plan = func
        .drop_plans
        .iter()
        .find(|(e, _)| matches!(e, ExitPath::Return { .. }))
        .expect("Return exit on every function");
    assert!(
        return_plan.1.drops.is_empty(),
        "spine has no owned locals; per-exit live-set drop plan is empty: {:?}",
        return_plan.1.drops
    );
}

#[test]
fn if_expression_emits_one_return_exit_with_empty_plan_on_spine() {
    // The new CFG for If gives the function 4 blocks; only the join
    // block is Return-terminated. Per-exit live-set narrowing of the
    // (empty) function-wide LIFO is also empty.
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    let func = first(&p);
    let returns: Vec<_> = func
        .drop_plans
        .iter()
        .filter(|(e, _)| matches!(e, ExitPath::Return { .. }))
        .collect();
    assert_eq!(
        returns.len(),
        1,
        "If expression with no early return produces exactly one Return exit"
    );
    assert!(
        returns[0].1.drops.is_empty(),
        "spine has no owned locals; per-exit drop plan is empty: {:?}",
        returns[0].1.drops
    );
}

// ---------------------------------------------------------------------------
// W5-011 P3 alias-site regression battery.
//
// The fail-closed sole-owner derivation (`derive_cow_sole_owner`) admits a
// `string` local to the function-scope drop plan ONLY when its backing MIR
// local is never read as a source operand anywhere in the finalized
// instruction+terminator stream AND is not a projection alias of a still-live
// aggregate. Each test below drives a distinct alias site and asserts the
// aliased SOURCE string contributes no extra `CowHeap` release — dropping it
// in addition to the live owner would double-free the shared `rc==1` buffer.
//
// This harness lowers against `TypeCheckOutput::default()` (no checker
// expr_types), so aggregate-literal alias sites that require checker-
// authoritative element types — tuple/record/variant construction — are
// exercised end-to-end through the full compiler in the runtime fixtures
// (`hew-cli/tests/run_e2e.rs`, `tests/vertical-slice/accept/`). The cases
// below drive the Move / call-arg / control-flow-join alias paths that the
// minimal pipeline supports.
// ---------------------------------------------------------------------------

fn main_cow_drop_count(p: &IrPipeline) -> usize {
    let func = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main present");
    func.drop_plans
        .iter()
        .filter(|(e, _)| matches!(e, ExitPath::Return { .. }))
        .map(|(_, plan)| {
            plan.drops
                .iter()
                .filter(|d| matches!(d.kind, DropKind::CowHeap { .. }))
                .count()
        })
        .sum()
}

#[test]
fn block_tail_move_keeps_single_owner() {
    // `s` is moved out of the inner block as its tail value into `y`. `s` and
    // `y` alias the same buffer; only the outer live owner `y` may be dropped.
    let p = pipeline(r#"fn main() { let _y = { let s = "deep"; s }; }"#);
    assert_eq!(
        main_cow_drop_count(&p),
        1,
        "exactly one owner (the outer binding) survives the block-tail move"
    );
}

#[test]
fn if_expression_result_keeps_single_owner() {
    // Both arm values flow into the `if` result `_y`. The arms are aliases of
    // the join slot; only `_y` is the live sole owner.
    let p = pipeline(r#"fn main() { let c = true; let _y = if c { "a" } else { "b" }; }"#);
    assert_eq!(
        main_cow_drop_count(&p),
        1,
        "the if-result owner is freed once; arm temporaries are not double-dropped"
    );
}

#[test]
fn call_argument_borrow_keeps_source_and_retained_result() {
    // `s` remains caller-owned across the by-value borrow, while `id` retains
    // the value it returns as a second owner.
    let p = pipeline(
        r#"fn id(x: string) -> string { return x; }
           fn main() { let s = "z"; let _t = id(s); }"#,
    );
    assert_eq!(
        main_cow_drop_count(&p),
        2,
        "the caller source and retained result are both freed"
    );
}
