//! Probe (G-1.b prerequisite): inspect `MethodCallRewrite` shape for a
//! trait-method call inside a generic free-fn body, after typecheck.
//!
//! Question: when `fn describe<T: Describable>(item: T) { item.describe() }`
//! typechecks, does the checker record a `MethodCallRewrite` for the
//! inner `item.describe()` call? If so, what is the `c_symbol`:
//! the concrete impl (e.g. `Label_describe`) or a symbolic `T::describe`?
//!
//! Answer determines whether G-1.b can rely on the existing
//! `method_call_rewrites` table verbatim (resolved-concrete case) or
//! must build a per-monomorphisation rewrite table (symbolic case).

use hew_hir::{lower_program, HirItem, HirStmtKind, ResolutionCtx};
use hew_parser::ast::Item;
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, SpanKey};

#[test]
fn probe_trait_method_inside_generic_free_fn_rewrite() {
    let source = r#"
        pub trait Describable {
            fn describe(val: Self) -> string;
        }

        pub type Label {
            text: string;
        }

        impl Describable for Label {
            fn describe(label: Label) -> string {
                label.text
            }
        }

        pub fn describe<T: Describable>(item: T) -> string {
            item.describe()
        }

        fn main() -> int {
            let s: string = describe(Label { text: "hello" });
            0
        }
    "#;

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    // Locate the span of the inner `item.describe()` method-call expression
    // inside `pub fn describe<T>`. This is the span we'll look up in
    // `method_call_rewrites`.
    let mut inner_call_span = None;
    for (item, _) in &parsed.program.items {
        if let Item::Function(fd) = item {
            if fd.name == "describe" && fd.type_params.is_some() {
                if let Some(boxed) = fd.body.trailing_expr.as_ref() {
                    let (expr, span) = &**boxed;
                    if let hew_parser::ast::Expr::MethodCall { .. } = expr {
                        inner_call_span = Some(span.clone());
                    }
                }
            }
        }
    }
    let inner_call_span =
        inner_call_span.expect("could not locate inner item.describe() call span");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);

    // Document the typecheck result. We don't fail the test on errors —
    // probes only document behaviour.
    eprintln!("=== TYPECHECK ERRORS ===");
    for e in &tco.errors {
        eprintln!("  {e:?}");
    }

    let key = SpanKey::from(&inner_call_span);
    eprintln!("\n=== INNER item.describe() rewrite ===");
    eprintln!("span: {inner_call_span:?}");
    eprintln!("rewrite: {:?}", tco.method_call_rewrites.get(&key));

    eprintln!(
        "\n=== ALL method_call_rewrites (count {}) ===",
        tco.method_call_rewrites.len()
    );
    for (k, v) in &tco.method_call_rewrites {
        eprintln!("  {k:?} -> {v:?}");
    }

    eprintln!(
        "\n=== call_type_args (count {}) ===",
        tco.call_type_args.len()
    );
    for (k, v) in &tco.call_type_args {
        eprintln!("  {k:?} -> {v:?}");
    }

    // trait_impls_set is not exposed on TypeCheckOutput (pub(super)); skip.

    // Now lower and report what HIR shape the inner call gets.
    let out = lower_program(&parsed.program, &tco, &ResolutionCtx);
    eprintln!("\n=== HIR diagnostics ===");
    for d in &out.diagnostics {
        eprintln!("  {d:?}");
    }
    eprintln!("\n=== monomorphisations ===");
    for m in &out.module.monomorphisations {
        eprintln!("  {m:?}");
    }

    eprintln!("\n=== HIR describe<T> body ===");
    for item in &out.module.items {
        if let HirItem::Function(f) = item {
            if f.name == "describe" && !f.type_params.is_empty() {
                if let Some(tail) = &f.body.tail {
                    eprintln!("  tail expr kind: {:#?}", tail.kind);
                }
                for st in &f.body.statements {
                    if let HirStmtKind::Expr(e) = &st.kind {
                        eprintln!("  stmt expr kind: {:#?}", e.kind);
                    }
                }
            }
        }
    }
}
