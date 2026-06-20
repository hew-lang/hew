//! Lifecycle-under-supervision (Slice 1 carrier probe).
//!
//! The supervised-spawn lifecycle fix carries an `init` / `#[on(start)]`
//! firing intent from MIR to codegen via
//! `SupervisorChildLayout.lifecycle_symbol`. Codegen's wrapper emission is a
//! no-op if this carrier is not populated for an init-bearing child, so this
//! probe pins the post-loop pass that derives it (LESSONS:
//! verify-ast-carries-discriminator-before-codegen-fix).
//!
//! The symbol value itself is documentary — codegen re-derives the wrapper
//! name from `child.actor_name`. The load-bearing fact is `Some` vs `None`:
//! `Some` exactly when the child's actor declares an `init` or a start hook,
//! `None` when it declares neither (the supervised spawn then fires no
//! wrapper, matching direct-spawn's no-hook early-return).

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

fn child_lifecycle_symbol(p: &IrPipeline, sup: &str, child: &str) -> Option<String> {
    let layout = p
        .supervisor_layouts
        .iter()
        .find(|s| s.name == sup)
        .unwrap_or_else(|| panic!("supervisor `{sup}` must be present in supervisor_layouts"));
    layout
        .children
        .iter()
        .find(|c| c.name == child)
        .unwrap_or_else(|| panic!("supervisor `{sup}` child `{child}` must be present"))
        .lifecycle_symbol
        .clone()
}

/// An init-bearing supervised child carries `Some(lifecycle_symbol)` after the
/// post-loop pass — the carrier codegen reads to emit the lifecycle wrapper.
#[test]
fn init_bearing_child_carries_lifecycle_symbol() {
    let src = r"
        actor Counter {
            var value: i64;
            init() {
                value = 110;
            }
            receive fn get() -> i64 { value }
        }
        supervisor Tree {
            strategy: one_for_one;
            intensity: 5 within 60s;
            child counter: Counter;
        }
        fn main() -> i64 { 0 }
    ";
    let p = pipeline_with_tc(src);
    let sym = child_lifecycle_symbol(&p, "Tree", "counter");
    assert!(
        sym.is_some(),
        "init-bearing child must carry a lifecycle_symbol, got None"
    );
    // The carrier mangles the actor name through the same scheme codegen uses
    // for the wrapper body, so the recorded intent matches the emitted symbol.
    assert_eq!(
        sym.as_deref(),
        Some("__hew_lifecycle_Counter"),
        "lifecycle_symbol must mangle the actor name to the wrapper symbol"
    );
}

/// A start-hook-only supervised child (no `init`) still carries the symbol:
/// the wrapper runs the `#[on(start)]` hook under supervision.
#[test]
fn start_hook_only_child_carries_lifecycle_symbol() {
    let src = r"
        actor Booted {
            var ready: i64;
            #[on(start)]
            fn boot() {
                ready = 1;
            }
            receive fn get() -> i64 { ready }
        }
        supervisor Tree {
            strategy: one_for_one;
            intensity: 5 within 60s;
            child b: Booted;
        }
        fn main() -> i64 { 0 }
    ";
    let p = pipeline_with_tc(src);
    assert!(
        child_lifecycle_symbol(&p, "Tree", "b").is_some(),
        "start-hook-only child must carry a lifecycle_symbol"
    );
}

/// An actor with neither `init` nor a start hook carries `None`: the
/// supervised spawn fires no lifecycle wrapper (matching direct-spawn's
/// no-hook case), and codegen leaves the `HewChildSpec.lifecycle_fn` null.
#[test]
fn no_hook_child_carries_no_lifecycle_symbol() {
    let src = r"
        actor Plain {
            var value: i64;
            receive fn get() -> i64 { value }
        }
        supervisor Tree {
            strategy: one_for_one;
            intensity: 5 within 60s;
            child p: Plain;
        }
        fn main() -> i64 { 0 }
    ";
    let p = pipeline_with_tc(src);
    assert_eq!(
        child_lifecycle_symbol(&p, "Tree", "p"),
        None,
        "no-hook child must carry no lifecycle_symbol"
    );
}
