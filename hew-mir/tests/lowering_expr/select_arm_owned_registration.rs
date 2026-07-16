//! Owned-local registration coverage for `select` arm bindings (#1875).
//!
//! The registration site in `lower_select`'s body-block loop is shared by
//! every value-bearing arm kind, but only two kinds are exercisable by a
//! compiled leak oracle (`hew-cli/tests/select_arm_owned_binding_leak_oracle.rs`):
//!
//! - **`ActorAsk`** and **`ChannelRecv`** compile to native and are pinned there.
//! - **`TaskAwait`** (`t from await actor.method()`) passes the checker and
//!   lowers to a real `SelectArmKind::TaskAwait` MIR arm, but codegen fails
//!   closed with `E_NOT_YET_IMPLEMENTED` (the task-await winner edge reads a
//!   `Place::MachineTag` off a non-machine local), so no native binary — and
//!   therefore no leak-slope or poisoned-allocator oracle — can exist for it
//!   yet. The MIR-level assertions here pin the registration half of the seam;
//!   the runtime half (the task-winner slot drop vs `hew_task_free`) gets its
//!   compiled oracle when the codegen surface lands.
//! - **`StreamNext`** is not source-reachable at all: the checker rejects both
//!   `next(<stream>)` and `stream.recv()` arm sources
//!   (`synthesize_actor_concurrency_source`, hew-types/src/check/calls.rs —
//!   "select arm source must be actor.method(args)"; reject fixtures
//!   `tests/vertical-slice/reject/select_arm_{stream_recv,await_task}_dropped.hew`),
//!   so it cannot reach MIR from source and is covered by the shared
//!   registration site by construction.
//!
//! These tests prove the shared site registers the binding for the arm kinds
//! that reach MIR: the elaborated statement stream carries the binding's
//! owned-local `Drop` entry, and (for the await form) the raw terminator
//! really is a `TaskAwait` arm, so the coverage cannot silently degrade to
//! `ActorAsk` if the HIR sealed-form recognition changes.

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
actor Svc {
    receive fn get() -> string {
        "owned-reply".to_upper()
    }
}

fn main() {
    let svc = spawn Svc;
    select {
        t from await svc.get() => println("won"),
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
/// site. Codegen-NYI beyond MIR (see module docs), so this MIR-level pin is
/// the deepest coverage available for the arm kind today.
///
/// The registered type is currently `Unit`, NOT the reply type: the
/// `TaskAwait` arm-setup derives `await_ty` from a `ResolvedTy::Task(inner)`
/// task expression and falls back to `Unit` for this `await actor.method()`
/// form (`lower_select`'s `HirSelectArmKind::TaskAwait` arm). The codegen
/// NYI keeps that placeholder unreachable in native code; when the
/// task-await winner edge lands, the type derivation must resolve the real
/// reply type and the compiled leak oracle takes over from this pin — do
/// not weaken this assertion, replace it with the typed one.
#[test]
fn task_await_arm_string_binding_registers_owned_drop() {
    let pipeline = lower_source(AWAIT_STRING_BINDING);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );
    assert!(
        main_drop_ty(&pipeline, "t").is_some(),
        "TaskAwait select-arm binding `t` must register an owned-local Drop \
         entry via the shared body-block site in lower_select"
    );
}

/// Guard the arm-kind discriminator: `t from await svc.get()` must lower to
/// a `TaskAwait` MIR arm, not an `ActorAsk` — otherwise the test above would
/// silently stop covering the `TaskAwait` branch of the shared site.
#[test]
fn await_form_lowers_to_task_await_arm() {
    let pipeline = lower_source(AWAIT_STRING_BINDING);
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
        "`t from await svc.get()` must produce a SelectArmKind::TaskAwait arm \
         on main's select terminator"
    );
}
