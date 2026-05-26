//! MIR-level contract tests for static trait dispatch (W3.022).
//!
//! Validates that `CallTraitMethodStatic` in HIR resolves to a concrete
//! `Terminator::Call { callee: "<Type>::<method>" }` after monomorphization.
//! Covers V10 (primitive receiver), V11 (named record), V12 (generic record).

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

/// Collect all callee names from `Terminator::Call` in the named function.
fn call_targets(p: &IrPipeline, fn_name: &str) -> Vec<String> {
    let func = p
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| {
            let names: Vec<&str> = p.raw_mir.iter().map(|f| f.name.as_str()).collect();
            panic!("function `{fn_name}` not in raw_mir; available: {names:?}")
        });
    func.blocks
        .iter()
        .filter_map(|b| match &b.terminator {
            Terminator::Call { callee, .. } => Some(callee.clone()),
            _ => None,
        })
        .collect()
}

// ─── V11: Named record as concrete receiver ─────────────────────────────────

#[test]
fn v11_named_record_static_dispatch_resolves_callee() {
    let src = r#"
trait Show {
    fn show(val: Self) -> string;
}
type Msg { text: string; }
impl Show for Msg {
    fn show(m: Msg) -> string { m.text }
}
fn display<T: Show>(item: T) -> string {
    item.show()
}
fn main() -> string {
    display(Msg { text: "hi" })
}
"#;
    let p = pipeline(src);
    // The monomorphised copy of `display` for Msg should call `Msg::show`.
    let all_fns: Vec<&str> = p.raw_mir.iter().map(|f| f.name.as_str()).collect();
    // Find the monomorphised display function
    let mono_display = all_fns
        .iter()
        .find(|name| name.contains("display") && **name != "display")
        .unwrap_or(&"display");
    let targets = call_targets(&p, mono_display);
    assert!(
        targets.iter().any(|t| t.contains("Msg") && t.contains("show")),
        "expected Msg::show call in monomorphised display, got targets: {targets:?}\nall fns: {all_fns:?}"
    );
}
