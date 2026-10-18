//! Tests verifying that actor handles and discovery metadata
//! propagate through HIR lowering.
//!
//! These tests exercise the full type-checker → HIR lowering path so that
//! downstream codegen can rely on the checker's resolved type (not re-infer
//! from AST).

use hew_hir::{lower_program, HirItem, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, Ty};
use std::path::Path;

fn repo_root() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-hir crate should live under repo root")
        .to_path_buf()
}

fn lower_with_types(source: &str) -> (hew_types::TypeCheckOutput, hew_hir::LowerOutput) {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![repo_root()]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let lower_out = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    (tc_output, lower_out)
}

// ── spawn produces the actor's own handle type in expr_types ─────────────────

#[test]
fn spawn_expr_type_is_the_actor_handle() {
    // After type-checking, the `spawn` expression must be recorded as the
    // `Counter` actor handle (D489: an actor is the type of its handle).
    // This ensures that the checker's discriminator survives into the
    // type-check output that HIR lowering consumes.
    let source = r"actor Counter {
    let n: i32;
    init() {}
}

fn main() {
    let c = spawn Counter { n: 0 };
}
";
    let (tc, _lower) = lower_with_types(source);
    let has_counter_handle = tc.expr_types.values().any(|ty| {
        ty.actor_handle_identity()
            .map(|(head, args)| (head.spelling.as_str(), args))
            == Some(("Counter", &[][..]))
    });
    assert!(
        has_counter_handle,
        "expr_types should contain at least one Counter actor-handle entry"
    );
    // The spawn-return type is the actor's own name with the `ActorHandle`
    // builtin discriminator; no stray `ActorRef`-named handle exists anywhere
    // in the type table (the family is the actor handle / `RemotePid`).
    let has_stray_actor_ref = tc.expr_types.values().any(
        |ty| matches!(ty, Ty::Named { head: name_head, .. } if name_head.spelling() == "ActorRef"),
    );
    assert!(
        !has_stray_actor_ref,
        "spawn must produce the actor's own handle type; no `ActorRef`-named \
         handle should appear: {:#?}",
        tc.expr_types
            .values()
            .filter(|ty| matches!(ty, Ty::Named { head: name_head, .. } if name_head.spelling() == "ActorRef"))
            .collect::<Vec<_>>()
    );
}

// ── HIR lowers actor declaration without diagnostics ─────────────────────────

#[test]
fn hir_lower_actor_no_diagnostics() {
    // A simple actor declaration (no spawn expression in main) should lower
    // without diagnostics. This verifies that the actor-handle rename (D489)
    // in the checker doesn't break actor declaration lowering.
    let source = r"actor Bot {
    let x: i32;
    init() {}
    receive fn handle(msg: i32) {}
}

fn main() {}
";
    let (_tc, lower) = lower_with_types(source);
    assert!(
        lower.diagnostics.is_empty(),
        "HIR lowering should produce no diagnostics: {:#?}",
        lower.diagnostics
    );
}

// ── HIR module has a function item for main ───────────────────────────────────

#[test]
fn hir_module_has_main() {
    let source = r"actor Foo {
    let v: i32;
    init() {}
}

fn main() {
    let _f = spawn Foo { v: 0 };
}
";
    let (_tc, lower) = lower_with_types(source);
    let has_main = lower.module.items.iter().any(|item| match item {
        HirItem::Function(f) => f.name == "main",
        _ => false,
    });
    assert!(has_main, "HIR module should have a main function item");
}

#[test]
fn node_lookup_lowers_as_discovery_metadata() {
    let source = r#"fn discover() {
        let found: Result<Location, LookupError> = Node.lookup("echo");
        match found {
            .Ok(_location) => {}
            .Err(_) => {}
        }
    }"#;
    let (_, lower) = lower_with_types(source);
    assert!(lower.diagnostics.is_empty(), "{:#?}", lower.diagnostics);
}
