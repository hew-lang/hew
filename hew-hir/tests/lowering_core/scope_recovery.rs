use hew_hir::{HirExprKind, HirItem, HirStmtKind};
use hew_types::ResolvedTy;

use crate::support::checker_pipeline;

#[test]
fn scope_recovery_keeps_result_values_and_owned_failure_binding() {
    let source = r"fn main() {
        let value: Result<i64, string> = scope within 2s { Ok(42) } handle failure {
            match failure {
                .Deadline { message } => Err(message),
                .Fault { message } => Err(message),
            }
        };
    }";
    let (parsed, checked) = checker_pipeline::typecheck_source(source);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let output =
        hew_hir::lower_program_host_target(&parsed.program, &checked, &hew_hir::ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let diagnostics = hew_hir::verify_hir(&output.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let function = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function");
    let HirStmtKind::Let(_, Some(value)) = &function.body.statements[0].kind else {
        panic!("expected recovered value binding");
    };
    let HirExprKind::ScopeRecovery {
        scope,
        error,
        handler,
    } = &value.kind
    else {
        panic!("expected scope recovery, got {:?}", value.kind);
    };
    let HirExprKind::ScopeDeadline { duration, body } = &scope.kind else {
        panic!("expected a scope with a deadline");
    };
    assert_eq!(duration.ty, ResolvedTy::Duration);
    assert_eq!(value.ty, scope.ty);
    assert_eq!(value.ty, handler.ty);
    assert_eq!(body.ty, value.ty);
    assert!(matches!(
        &value.ty,
        ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::Result),
            ..
        }
    ));
    assert!(
        matches!(&error.ty, ResolvedTy::Named { name, .. } if name == "std.builtins.ScopeFailure")
    );
    let HirExprKind::Block(block) = &handler.kind else {
        panic!("expected handler block");
    };
    let HirExprKind::Match { scrutinee, .. } = &block.tail.as_deref().expect("handler match").kind
    else {
        panic!("expected a failure match");
    };
    assert!(
        matches!(scrutinee.kind, HirExprKind::BindingRef { resolved: hew_hir::ResolvedRef::Binding(id), .. } if id == error.id)
    );
}
