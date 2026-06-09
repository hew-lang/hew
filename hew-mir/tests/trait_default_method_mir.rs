//! MIR-level contract tests for trait default method bodies.
//!
//! Validates that a default method body lowered for a concrete impl
//! (`Person::greet` from `Greeter::greet`) resolves `self.name()` to a
//! concrete `Terminator::Call { callee: "Person::name" }` rather than
//! the generic `CallTraitMethodStatic` that would leave `Self` unresolved
//! at MIR level.

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
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    lower_hir_module(&output.module)
}

/// Collect all `Terminator::Call` callee names from the named function.
fn call_targets(p: &IrPipeline, fn_name: &str) -> Vec<String> {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| {
            let names: Vec<&str> = p.raw_mir.iter().map(|f| f.name.as_str()).collect();
            panic!("function `{fn_name}` not in raw_mir; available: {names:?}")
        })
        .blocks
        .iter()
        .filter_map(|b| match &b.terminator {
            Terminator::Call { callee, .. } => Some(callee.clone()),
            _ => None,
        })
        .collect()
}

/// The default body of `Greeter::greet` lowered for `Person` must call
/// `Person::name` concretely — not `CallTraitMethodStatic { Self }`.
#[test]
fn default_method_body_resolves_to_concrete_callee() {
    let p = pipeline(
        r#"
        trait Greeter {
            fn name(self) -> string;
            fn greet(self) -> string {
                "Hi " + self.name()
            }
        }

        type Person { name: string }

        impl Greeter for Person {
            fn name(p: Person) -> string {
                p.name
            }
        }

        fn main() {
            let p = Person { name: "Alice" };
            let s = p.greet();
        }
        "#,
    );
    // `Person::greet` must exist in MIR as a concrete function.
    let targets = call_targets(&p, "Person::greet");
    assert!(
        targets.iter().any(|t| t == "Person::name"),
        "expected `Person::name` in Person::greet call targets; got: {targets:?}"
    );
}

/// A chain of default methods (describe → label → kind) all resolves to
/// concrete callees in MIR.
#[test]
fn default_method_chain_resolves_concrete_callees() {
    let p = pipeline(
        r#"
        trait Describable {
            fn kind(self) -> string;
            fn label(self) -> string {
                "[" + self.kind() + "]"
            }
            fn describe(self) -> string {
                "Type: " + self.label()
            }
        }

        type Widget { id: i64 }

        impl Describable for Widget {
            fn kind(w: Widget) -> string { "widget" }
        }

        fn main() {
            let w = Widget { id: 1 };
            let s = w.describe();
        }
        "#,
    );
    let label_targets = call_targets(&p, "Widget::label");
    assert!(
        label_targets.iter().any(|t| t == "Widget::kind"),
        "expected `Widget::kind` in Widget::label call targets; got: {label_targets:?}"
    );
    let describe_targets = call_targets(&p, "Widget::describe");
    assert!(
        describe_targets.iter().any(|t| t == "Widget::label"),
        "expected `Widget::label` in Widget::describe call targets; got: {describe_targets:?}"
    );
}
