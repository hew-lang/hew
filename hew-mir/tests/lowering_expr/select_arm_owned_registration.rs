//! Owned-local registration coverage for `select` arm bindings (#1875).
//!
//! Actor replies and awaited task results share one registration site. The
//! checker currently exposes actor asks but no source-level `Task<T>` select
//! carrier, so the task tests stamp a synthetic HIR operand with the exact
//! builtin task type before MIR lowering.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, MirStatement, SelectArmKind, Terminator};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

/// Pipe source through parser → checker → HIR → MIR. Asserts no parser,
/// checker, or HIR diagnostics; per-test assertions inspect the MIR output.
fn lower_source(src: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(src);
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
    lower_hir_module(&output.module)
}

fn lower_synthetic_task_await(src: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut output = lower_program(
        &parsed.program,
        &checker_output(&parsed.program),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    for item in &mut output.module.items {
        let hew_hir::HirItem::Function(function) = item else {
            continue;
        };
        for statement in &mut function.body.statements {
            let hew_hir::HirStmtKind::Expr(expr) = &mut statement.kind else {
                continue;
            };
            let hew_hir::HirExprKind::Select(select) = &mut expr.kind else {
                continue;
            };
            for arm in &mut select.arms {
                if let hew_hir::HirSelectArmKind::TaskAwait { task } = &mut arm.kind {
                    task.ty = ResolvedTy::Task(Box::new(ResolvedTy::String));
                    arm.binding_name = Some("t".to_string());
                    arm.binding_id = Some(hew_hir::BindingId(u32::MAX - 80));
                }
            }
        }
    }
    lower_hir_module(&output.module)
}

/// The `Drop` entry `main`'s elaborated statement stream carries for the
/// binding named `binding_name`, if any — the observable artefact of
/// `register_owned_local` at the shared body-block site.
fn main_drop_ty(pipeline: &hew_mir::IrPipeline, binding_name: &str) -> Option<ResolvedTy> {
    pipeline
        .elaborated_mir
        .iter()
        .filter(|f| f.name == "main")
        .flat_map(|f| f.statements.iter())
        .find_map(|stmt| match stmt {
            MirStatement::Drop { name, ty, .. } if name == binding_name => Some(ty.clone()),
            _ => None,
        })
}

const ASK_STRING_BINDING: &str = r#"
actor Svc {
    receive fn get() -> string {
        "owned-reply".to_upper()
    }
}

fn main() {
    let svc = spawn Svc;
    select {
        r from svc.get() => println("won"),
        after 1s => println("timeout"),
    };
}
"#;

const AWAIT_STRING_BINDING: &str = r#"
fn main() {
    select {
        t from await 1 => println("won"),
        after 1s => println("timeout"),
    };
}
"#;

/// The `ActorAsk` arm binding enters `owned_locals` with its resolved reply
/// type (baseline for the shared site — the compiled leak oracle pins the
/// end-to-end behaviour).
#[test]
fn actor_ask_arm_string_binding_registers_owned_drop() {
    let pipeline = lower_source(ASK_STRING_BINDING);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );
    assert_eq!(
        main_drop_ty(&pipeline, "r"),
        Some(ResolvedTy::String),
        "ActorAsk select-arm binding `r` must register a string-typed \
         owned-local Drop entry via the shared body-block site in lower_select"
    );
}

/// The `TaskAwait` arm binding enters `owned_locals` through the same shared
/// site and retains the task's concrete string result type.
#[test]
fn task_await_arm_string_binding_registers_owned_drop() {
    let pipeline = lower_synthetic_task_await(AWAIT_STRING_BINDING);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );
    assert_eq!(
        main_drop_ty(&pipeline, "t"),
        Some(ResolvedTy::String),
        "TaskAwait select-arm binding `t` must register an owned-local Drop \
         entry via the shared body-block site in lower_select"
    );
}

/// Guard the arm-kind discriminator: `t from await task` must lower to
/// a `TaskAwait` MIR arm, not an `ActorAsk` — otherwise the test above would
/// silently stop covering the `TaskAwait` branch of the shared site.
#[test]
fn await_form_lowers_to_task_await_arm() {
    let pipeline = lower_synthetic_task_await(AWAIT_STRING_BINDING);
    let has_task_await_arm = pipeline
        .raw_mir
        .iter()
        .filter(|f| f.name == "main")
        .flat_map(|f| f.blocks.iter())
        .any(|block| match &block.terminator {
            Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => arms
                .iter()
                .any(|arm| matches!(arm.kind, SelectArmKind::TaskAwait { .. })),
            _ => false,
        });
    assert!(
        has_task_await_arm,
        "`t from await task` must produce a SelectArmKind::TaskAwait arm \
         on main's select terminator"
    );
}

/// Type-check `program` so HIR lowering sees the checker's declaration
/// identities.
///
/// These harnesses assert MIR shape below the checker, but HIR resolves every
/// item through `TypeCheckOutput::identity` and fails closed when that view is
/// empty, so the checker still has to run to mint the identities.
fn checker_output(program: &hew_parser::ast::Program) -> hew_types::TypeCheckOutput {
    hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(Vec::new()))
        .check_program(program)
}
