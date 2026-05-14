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
use hew_mir::{lower_hir_module, BlockKind, ElaboratedMirFunction, ExitPath, IrPipeline};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");
    lower_hir_module(&output.module)
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
fn cowvalue_string_does_not_appear_in_structural_drop_plan() {
    // Strings are CowValue, not AffineResource. The pass's structural
    // drop_plan field carries Drop ops only for AffineResource (and
    // declared scaffold for @resource types when the surface lands).
    // CowValue gets its CoW share-on-modify semantics via DecisionFact
    // strategies, not via implicit drop emission. This test enforces
    // that the structural plan stays empty for a String binding —
    // the legacy `statements`-field Drop entries are unrelated.
    let p = pipeline(r#"fn main() { let _s = "hello"; }"#);
    let func = first(&p);
    let return_plan = func
        .drop_plans
        .iter()
        .find(|(e, _)| matches!(e, ExitPath::Return { .. }))
        .expect("Return exit present on every function");
    assert!(
        return_plan.1.drops.is_empty(),
        "CowValue String does not contribute to the structural drop plan; \
         got {:?}",
        return_plan.1.drops
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
