//! Target and coroutine admission.

use super::*;

// ── Target architecture gates ────────────────────────────────────────────────

/// Reject actor-runtime constructs on architectures without a runtime.
/// Native workers and the WASI cooperative driver share the checked actor and
/// supervisor ABI. The source-order pass repeats these checks after resetting
/// diagnostics from its type pre-pass.
pub(super) fn check_target_gates(ctx: &mut LowerCtx, program: &Program) {
    if matches!(ctx.target_arch, TargetArch::Other) {
        check_coroutine_gate(ctx, program);
    }
}

/// Check actor/supervisor usage on an architecture without the runtime ABI.
/// Called only for unsupported target architectures.
pub(super) fn check_coroutine_gate(ctx: &mut LowerCtx, program: &Program) {
    let target_name = match ctx.target_arch {
        TargetArch::Wasm32 => "wasm32",
        TargetArch::Other => "unsupported",
        // x86_64/aarch64 shouldn't reach here (guarded by caller)
        _ => "other",
    };

    for (item, span) in &program.items {
        match item {
            Item::Actor(actor_decl) => {
                // Actors are admitted on wasm32: the cooperative scheduler
                // (`hew-runtime/src/scheduler_wasm.rs`) provides the full
                // actor ABI. Only reject for genuinely unsupported targets.
                if matches!(ctx.target_arch, TargetArch::Other) {
                    ctx.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::TargetCoroutineUnsupported {
                            target_arch: target_name.to_string(),
                            construct: "actor decl".to_string(),
                        },
                        span.clone(),
                        format!(
                            "actor `{}` cannot be compiled for target `{}`: \
                             the actor runtime ABI (scheduler, mailbox) is not \
                             available on this target",
                            actor_decl.name, target_name
                        ),
                    ));
                }
            }
            Item::Supervisor(supervisor_decl) => {
                // The unsupported target has no supervisor restart runtime.
                ctx.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::TargetCoroutineUnsupported {
                        target_arch: target_name.to_string(),
                        construct: "supervisor decl".to_string(),
                    },
                    span.clone(),
                    format!(
                        "supervisor `{}` cannot be compiled for target `{}`: \
                         this target has no supervisor restart runtime",
                        supervisor_decl.name, target_name
                    ),
                ));
            }
            // TODO: When `scope{}` and `fork` are surface syntax, add checks here
            _ => {}
        }
    }

    // TODO: Add checks for:
    // - `scope{}` blocks (when they exist in surface AST)
    // - `fork` statements (when they exist in surface AST)
    // - `await` expressions (when they appear outside of existing actor/scope)
    // For now, actors and supervisors are the only coroutine entry points.
}
