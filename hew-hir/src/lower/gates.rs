//! Target architecture coroutine admission gates.
//!
//! Native workers (`X86_64`, `Aarch64`) and the WASI cooperative scheduler
//! (`Wasm32`) share the checked actor, supervisor and task-scope ABI.
//! `TargetArch::Other` has no runtime scheduler, so coroutine constructs fail
//! closed through these single-authority helpers during item and expression
//! lowering.

use hew_parser::ast::Ident;

use super::*;

/// Reject an actor declaration on architectures without an actor runtime ABI.
pub(super) fn check_actor_target_gate(ctx: &mut LowerCtx, actor_name: Ident, span: &Span) {
    if matches!(ctx.target_arch, TargetArch::Other) {
        ctx.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::TargetCoroutineUnsupported {
                target_arch: format!("{:?}", ctx.target_arch),
                construct: "actor decl".to_string(),
            },
            span.clone(),
            format!("actor '{actor_name}' requires the actor runtime ABI (unsupported target)"),
        ));
    }
}

/// Reject a supervisor declaration on architectures without supervisor restart
/// machinery.
pub(super) fn check_supervisor_target_gate(
    ctx: &mut LowerCtx,
    supervisor_name: Ident,
    span: &Span,
) {
    if matches!(ctx.target_arch, TargetArch::Other) {
        ctx.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::TargetCoroutineUnsupported {
                target_arch: format!("{:?}", ctx.target_arch),
                construct: "supervisor decl".to_string(),
            },
            span.clone(),
            format!(
                "supervisor '{supervisor_name}' requires supervisor restart machinery \
                 (unsupported target)"
            ),
        ));
    }
}

/// Reject a structured-concurrency `scope` block on architectures without a
/// task scheduler.
pub(super) fn check_scope_target_gate(ctx: &mut LowerCtx, span: &Span) {
    if matches!(ctx.target_arch, TargetArch::Other) {
        ctx.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::TargetCoroutineUnsupported {
                target_arch: format!("{:?}", ctx.target_arch),
                construct: "scope expr".to_string(),
            },
            span.clone(),
            "structured concurrency `scope` requires the actor/task runtime ABI \
             (unsupported target)",
        ));
    }
}

/// Reject a lambda actor spawn on architectures without an actor runtime ABI.
pub(super) fn check_lambda_actor_target_gate(ctx: &mut LowerCtx, span: &Span) {
    if matches!(ctx.target_arch, TargetArch::Other) {
        ctx.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::TargetCoroutineUnsupported {
                target_arch: format!("{:?}", ctx.target_arch),
                construct: "lambda actor".to_string(),
            },
            span.clone(),
            "lambda actor spawn requires the actor runtime ABI (unsupported target)",
        ));
    }
}
