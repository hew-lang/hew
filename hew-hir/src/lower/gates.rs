//! Target architecture coroutine admission gates.
//!
//! Native workers (`X86_64`, `Aarch64`) and the WASI cooperative scheduler
//! (`Wasm32`) share the checked actor, supervisor and task-scope ABI.
//! `TargetArch::Other` has no runtime scheduler, so coroutine constructs fail
//! closed through these single-authority helpers during item and expression
//! lowering.

use hew_parser::ast::Ident;

use super::*;

fn reject_unsupported_target(
    ctx: &mut LowerCtx,
    construct: &str,
    span: &Span,
    message: impl Into<String>,
) {
    if matches!(ctx.target_arch, TargetArch::Other) {
        ctx.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::TargetCoroutineUnsupported {
                target_arch: format!("{:?}", ctx.target_arch),
                construct: construct.to_string(),
            },
            span.clone(),
            message.into(),
        ));
    }
}

/// Reject an actor declaration on architectures without an actor runtime ABI.
pub(super) fn check_actor_target_gate(ctx: &mut LowerCtx, actor_name: Ident, span: &Span) {
    reject_unsupported_target(
        ctx,
        "actor decl",
        span,
        format!("actor '{actor_name}' requires the actor runtime ABI (unsupported target)"),
    );
}

/// Reject a supervisor declaration on architectures without supervisor restart
/// machinery.
pub(super) fn check_supervisor_target_gate(
    ctx: &mut LowerCtx,
    supervisor_name: Ident,
    span: &Span,
) {
    reject_unsupported_target(
        ctx,
        "supervisor decl",
        span,
        format!(
            "supervisor '{supervisor_name}' requires supervisor restart machinery \
             (unsupported target)"
        ),
    );
}

/// Reject a structured-concurrency `scope` block on architectures without a
/// task scheduler.
pub(super) fn check_scope_target_gate(ctx: &mut LowerCtx, span: &Span) {
    reject_unsupported_target(
        ctx,
        "scope expr",
        span,
        "structured concurrency `scope` requires the actor/task runtime ABI \
         (unsupported target)",
    );
}

/// Reject a structured-concurrency `select` expression on architectures without
/// a task scheduler.
pub(super) fn check_select_target_gate(ctx: &mut LowerCtx, span: &Span) {
    reject_unsupported_target(
        ctx,
        "select expr",
        span,
        "structured concurrency `select` requires the actor/task runtime ABI \
         (unsupported target)",
    );
}

/// Reject a structured-concurrency `race` expression on architectures without
/// a task scheduler.
pub(super) fn check_race_target_gate(ctx: &mut LowerCtx, span: &Span) {
    reject_unsupported_target(
        ctx,
        "race expr",
        span,
        "structured concurrency `race` requires the actor/task runtime ABI \
         (unsupported target)",
    );
}

/// Reject a generator expression or `gen fn` on architectures without
/// coroutine runtime support.
pub(super) fn check_gen_target_gate(ctx: &mut LowerCtx, span: &Span) {
    reject_unsupported_target(
        ctx,
        "gen expr",
        span,
        "generator coroutines require the coroutine runtime ABI (unsupported target)",
    );
}

/// Reject a lambda actor spawn on architectures without an actor runtime ABI.
pub(super) fn check_lambda_actor_target_gate(ctx: &mut LowerCtx, span: &Span) {
    reject_unsupported_target(
        ctx,
        "lambda actor",
        span,
        "lambda actor spawn requires the actor runtime ABI (unsupported target)",
    );
}
