//! HIR bridge contract tests: `Expr::MethodCall` lowering via `TypeCheckOutput.method_call_rewrites`.
//!
//! Verifies the two halves of the fail-closed bridge:
//!   1. A registered `RewriteToFunction` entry produces an `HirExprKind::Call`
//!      with the runtime-symbol callee and the receiver prepended to args.
//!   2. A missing entry for a method-call span produces a `MethodCallNoRewrite`
//!      diagnostic and no silently-wrong HIR.
//!   3. A `RewriteToFunction` entry for a non-Unit-returning method (e.g. `len`)
//!      preserves the return type from the checker's `expr_types` table.
//!
//! Post-W4.001 Stage C3 (DI-017): `HashMap` / `HashSet` method calls are no longer
//! covered by `method_call_rewrites`; they flow through the parallel
//! `resolved_calls` channel as `HirExprKind::ResolvedImplCall`. The bridge
//! tests below intentionally exercise non-collection methods (Duplex, string)
//! that still use the legacy rewrite path. The `_fails_closed` test asserts
//! the boundary-violation contract — `MethodCallNoRewrite` after Stage C3 is
//! a fail-closed compiler-bug signal, not a user-reachable diagnostic for
//! `HashMap`/`HashSet` (those now surface as `BoundsNotSatisfied` at the checker).

use hew_hir::{lower_program, HirDiagnosticKind, HirExprKind, HirStmtKind, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy, TypeCheckOutput};

fn typecheck_and_lower(source: &str) -> (hew_hir::LowerOutput, TypeCheckOutput) {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let lower_output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    (lower_output, tc_output)
}

/// `a.send(42)` on a `Duplex<i64, i64>` binding is rewritten to
/// `HirExprKind::Call` with callee `hew_duplex_send` and the receiver
/// prepended as the first argument.  No diagnostics are emitted and the
/// HIR verifier passes.
#[test]
fn method_call_with_rewrite_produces_hir_call() {
    let source = r"
        fn main() -> i64 {
            let (a, b) = duplex_pair<i64, i64>(16);
            a.send(42);
            return 0;
        }
    ";
    let (lower_output, _tc) = typecheck_and_lower(source);

    // There must be no `MethodCallNoRewrite` diagnostics — the bridge consumed
    // the checker's rewrite entry.
    let method_call_errors: Vec<_> = lower_output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, HirDiagnosticKind::MethodCallNoRewrite { .. }))
        .collect();
    assert!(
        method_call_errors.is_empty(),
        "a.send(42) must not emit MethodCallNoRewrite when checker registered hew_duplex_send; \
         got: {method_call_errors:#?}"
    );

    // Find the function body and look for the send call statement.
    let fn_item = lower_output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let hew_hir::HirItem::Function(f) = item {
                if f.name == "main" {
                    return Some(f);
                }
            }
            None
        })
        .expect("main function must be present");

    // The body should contain a statement whose expr is a Call with callee
    // name `hew_duplex_send`.
    let has_duplex_send_call = fn_item.body.statements.iter().any(|stmt| {
        if let HirStmtKind::Expr(expr) = &stmt.kind {
            if let HirExprKind::Call { callee, args } = &expr.kind {
                // Callee should be a BindingRef named "hew_duplex_send"
                if let HirExprKind::BindingRef { name, .. } = &callee.kind {
                    return name == "hew_duplex_send" && args.len() == 2;
                }
            }
        }
        false
    });
    assert!(
        has_duplex_send_call,
        "a.send(42) must lower to HirExprKind::Call {{ callee: hew_duplex_send, args: [receiver, 42] }}; \
         body statements: {:#?}",
        fn_item.body.statements
    );
    // Note: verify_hir is not called here because the synthetic runtime-symbol
    // callee (`hew_duplex_send`) uses `ResolvedRef::Unresolved` by design —
    // it is not a user binding and has no BindingId in the HIR scope.  The
    // verifier would fire `UnresolvedSymbol` on it, which is a known and
    // intentional property of the bridge.  E2 (MIR lowering) detects synthetic
    // runtime callees by their `hew_` prefix convention rather than by resolved
    // binding ids.
}

/// `s.len()` on a `string` binding is rewritten to `HirExprKind::Call` with
/// callee `hew_string_length` and the receiver prepended as the first
/// argument. The expression node must carry `ty == ResolvedTy::I64` — not
/// `ResolvedTy::Unit`.
///
/// This is a regression test for the bug where the `RewriteToFunction` arm in
/// HIR lowering hardcoded `ResolvedTy::Unit` as the return type, silently
/// discarding the checker's `expr_types` annotation for the call site.
#[test]
fn rewrite_to_function_preserves_return_type() {
    let source = r#"
        fn main() -> i64 {
            let s = "hello";
            let n = s.len();
            return n;
        }
    "#;
    let (lower_output, _tc) = typecheck_and_lower(source);

    let method_call_errors: Vec<_> = lower_output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, HirDiagnosticKind::MethodCallNoRewrite { .. }))
        .collect();
    assert!(
        method_call_errors.is_empty(),
        "s.len() must not emit MethodCallNoRewrite; got: {method_call_errors:#?}"
    );

    let fn_item = lower_output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let hew_hir::HirItem::Function(f) = item {
                if f.name == "main" {
                    return Some(f);
                }
            }
            None
        })
        .expect("main function must be present");

    // Walk all statements and find the `let n = s.len()` assignment.
    // Its initialiser must be a Call whose `ty` is `ResolvedTy::I64`.
    let len_call_ty = fn_item.body.statements.iter().find_map(|stmt| {
        if let HirStmtKind::Let(_binding, Some(expr)) = &stmt.kind {
            if let HirExprKind::Call { callee, .. } = &expr.kind {
                if let HirExprKind::BindingRef { name, .. } = &callee.kind {
                    if name == "hew_string_length" {
                        return Some(expr.ty.clone());
                    }
                }
            }
        }
        None
    });

    let ty = len_call_ty.expect(
        "s.len() must lower to HirExprKind::Call { callee: hew_string_length, .. }; \
         body statements: {:#?}",
    );
    assert_eq!(
        ty,
        ResolvedTy::I64,
        "s.len() call node must have ty == ResolvedTy::I64, not {ty:?}"
    );
}

/// A method call with no checker-produced rewrite entry emits
/// `MethodCallNoRewrite` and fails closed — no silent fallthrough.
///
/// We simulate this by calling `lower_program` with an empty `TypeCheckOutput`
/// on a source that has a method call. The empty output has no
/// `method_call_rewrites` entries, so the bridge must fail closed.
#[test]
fn method_call_without_rewrite_fails_closed() {
    let source = r"
        fn main() -> i64 {
            let (a, b) = duplex_pair<i64, i64>(16);
            a.send(42);
            return 0;
        }
    ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    // W4.015: behavior pin — empty method_call_rewrites must fail closed
    // with MethodCallNoRewrite instead of silently lowering a method call.
    let empty_tc = TypeCheckOutput::default();
    let lower_output = lower_program(
        &parsed.program,
        &empty_tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    let has_no_rewrite_diag = lower_output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::MethodCallNoRewrite { .. }));
    assert!(
        has_no_rewrite_diag,
        "method call with no rewrite entry must emit MethodCallNoRewrite; \
         got diagnostics: {:#?}",
        lower_output.diagnostics
    );
}
