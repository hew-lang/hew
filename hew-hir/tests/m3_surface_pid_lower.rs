//! Tests verifying that `LocalPid<T>` / `RemotePid<T>` discriminators
//! propagate through HIR lowering.
//!
//! These tests exercise the full type-checker → HIR lowering path so that
//! downstream codegen can rely on the checker's resolved type (not re-infer
//! from AST).

use hew_hir::{lower_program, HirBlock, HirExpr, HirExprKind, HirItem, HirStmtKind, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::BuiltinType;
use hew_types::{Checker, Ty};
use std::path::Path;

fn repo_root() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-hir crate should live under repo root")
        .to_path_buf()
}

fn expr_contains_remote_actor_ask(expr: &HirExpr) -> bool {
    match &expr.kind {
        HirExprKind::RemoteActorAsk { reply_ty, .. } => {
            matches!(reply_ty, hew_types::ResolvedTy::I64)
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. }
        | HirExprKind::GenBlock { body: block, .. } => block_contains_remote_actor_ask(block),
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            expr_contains_remote_actor_ask(callee)
                || args.iter().any(expr_contains_remote_actor_ask)
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. } => {
            expr_contains_remote_actor_ask(receiver)
                || args.iter().any(expr_contains_remote_actor_ask)
        }
        HirExprKind::StructInit { fields, base, .. } => {
            base.as_deref().is_some_and(expr_contains_remote_actor_ask)
                || fields
                    .iter()
                    .any(|(_, expr)| expr_contains_remote_actor_ask(expr))
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            expr_contains_remote_actor_ask(condition)
                || expr_contains_remote_actor_ask(then_expr)
                || else_expr
                    .as_deref()
                    .is_some_and(expr_contains_remote_actor_ask)
        }
        HirExprKind::Match { scrutinee, arms } => {
            expr_contains_remote_actor_ask(scrutinee)
                || arms
                    .iter()
                    .any(|arm| expr_contains_remote_actor_ask(&arm.body))
        }
        _ => false,
    }
}

fn block_contains_remote_actor_ask(block: &HirBlock) -> bool {
    block.statements.iter().any(|stmt| match &stmt.kind {
        HirStmtKind::Let(_, Some(expr))
        | HirStmtKind::Expr(expr)
        | HirStmtKind::Return(Some(expr)) => expr_contains_remote_actor_ask(expr),
        HirStmtKind::Assign { target, value } => {
            expr_contains_remote_actor_ask(target) || expr_contains_remote_actor_ask(value)
        }
        HirStmtKind::Defer { body, .. } => expr_contains_remote_actor_ask(body),
        HirStmtKind::LetElse {
            scrutinee,
            else_body,
            ..
        } => {
            expr_contains_remote_actor_ask(scrutinee) || block_contains_remote_actor_ask(else_body)
        }
        HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => false,
    }) || block
        .tail
        .as_deref()
        .is_some_and(expr_contains_remote_actor_ask)
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
    // `LocalPid<Counter>`. This ensures that the checker's discriminator
    // survives into the type-check output that HIR lowering consumes.
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
    // `LocalPid` is the spawn-return type; no stray `ActorRef`-named handle
    // exists anywhere in the type table (the family is LocalPid/RemotePid).
    let has_stray_actor_ref = tc
        .expr_types
        .values()
        .any(|ty| matches!(ty, Ty::Named { name, .. } if name == "ActorRef"));
    assert!(
        !has_stray_actor_ref,
        "spawn must produce LocalPid; no `ActorRef`-named handle should appear: {:#?}",
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

#[test]
fn remote_pid_ask_lowers_to_hir_remote_actor_ask() {
    let source = r"
        record Job {
            n: i32,
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn run(job: Job) -> i64 { 21 }
        }

        impl ActorMsg for Worker {
            type Msg = Job;
            type Reply = i64;
        }

        fn main() {
            let remote: RemotePid<Worker>;
            let result: Result<i64, AskError> = remote.ask(Job { n: 9 }, 250);
        }
    ";
    let (_tc, lower) = lower_with_types(source);
    assert!(
        lower.diagnostics.is_empty(),
        "HIR lowering should accept RemotePid.ask: {:#?}",
        lower.diagnostics
    );
    let main = lower
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(f) if f.name == "main" => Some(f),
            _ => None,
        })
        .expect("main function should lower");
    assert!(
        block_contains_remote_actor_ask(&main.body),
        "RemotePid.ask should lower to HirExprKind::RemoteActorAsk: {main:#?}"
    );
    let has_result_ask_error_layout = lower.module.enum_layouts.iter().any(|layout| {
        layout.key.origin_name == "Result"
            && matches!(
                layout.key.type_args.as_slice(),
                [
                    hew_types::ResolvedTy::I64,
                    hew_types::ResolvedTy::Named {
                        builtin: Some(BuiltinType::AskError),
                        ..
                    }
                ]
            )
    });
    assert!(
        has_result_ask_error_layout,
        "RemotePid.ask should register Result<i64, AskError> layout: {:#?}",
        lower.module.enum_layouts
    );
}
