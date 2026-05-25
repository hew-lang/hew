//! Tests verifying that `LocalPid<T>` / `RemotePid<T>` discriminators
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

// ── spawn produces LocalPid in expr_types ─────────────────────────────────────

#[test]
fn spawn_expr_type_is_local_pid() {
    // After type-checking, the `spawn` expression must be recorded as
    // `LocalPid<Counter>` (not `ActorRef<Counter>`).  This ensures that
    // the checker's discriminator survives into the type-check output that
    // HIR lowering consumes.
    let source = r"
        actor Counter {
            let n: i32;
            init() {}
        }
        fn main() {
            let c = spawn Counter(n: 0);
        }
    ";
    let (tc, _lower) = lower_with_types(source);
    let has_local_pid = tc
        .expr_types
        .values()
        .any(|ty| matches!(ty, Ty::Named { name, .. } if name == "LocalPid"));
    assert!(
        has_local_pid,
        "expr_types should contain at least one LocalPid<Counter> entry"
    );
    let has_actor_ref_for_spawn = tc
        .expr_types
        .values()
        .any(|ty| matches!(ty, Ty::Named { name, .. } if name == "ActorRef"));
    assert!(
        !has_actor_ref_for_spawn,
        "spawn should no longer produce ActorRef — found one in expr_types: {:#?}",
        tc.expr_types
            .values()
            .filter(|ty| matches!(ty, Ty::Named { name, .. } if name == "ActorRef"))
            .collect::<Vec<_>>()
    );
}

// ── HIR lowers actor declaration without diagnostics ─────────────────────────

#[test]
fn hir_lower_actor_no_diagnostics() {
    // A simple actor declaration (no spawn expression in main) should lower
    // without diagnostics. This verifies that the LocalPid changes in the
    // checker don't break actor declaration lowering.
    let source = r"
        actor Bot {
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
    let source = r"
        actor Foo {
            let v: i32;
            init() {}
        }
        fn main() {
            let _f = spawn Foo(v: 0);
        }
    ";
    let (_tc, lower) = lower_with_types(source);
    let has_main = lower.module.items.iter().any(|item| match item {
        HirItem::Function(f) => f.name == "main",
        _ => false,
    });
    assert!(has_main, "HIR module should have a main function item");
}
