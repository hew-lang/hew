//! Probe (G-1.b prerequisite): does the checker record `call_type_args`
//! for an inner generic-fn call inside a generic body, where the type
//! arg is the outer fn's symbolic type parameter `T`?
//!
//! If YES → MIR can iterate the side-table and substitute. If NO → MIR
//! must walk the body and reconstruct inner-call args from fn signatures.

use hew_parser::ast::Item;
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, SpanKey};

#[test]
fn probe_inner_generic_call_type_args_recorded() {
    let source = r"
        pub fn id<T>(x: T) -> T {
            x
        }

        pub fn outer<U>(y: U) -> U {
            id(y)
        }

        fn main() -> i64 {
            let a: i64 = outer(42);
            0
        }
    ";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty());

    let mut inner_span = None;
    let mut outer_span = None;
    for (item, _) in &parsed.program.items {
        if let Item::Function(fd) = item {
            if fd.name == "outer" {
                if let Some(boxed) = fd.body.trailing_expr.as_ref() {
                    let (_, sp) = &**boxed;
                    inner_span = Some(sp.clone());
                }
            }
            if fd.name == "main" {
                // The statement `let a: i64 = outer(42);`
                for st in &fd.body.stmts {
                    if let hew_parser::ast::Stmt::Let {
                        value: Some((_, sp)),
                        ..
                    } = &st.0
                    {
                        outer_span = Some(sp.clone());
                    }
                }
            }
        }
    }

    let inner = inner_span.expect("inner call span");
    let outer = outer_span.expect("outer call span");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);

    eprintln!("typecheck errors: {:#?}", tco.errors);
    eprintln!(
        "inner id(y) call_type_args entry: {:?}",
        tco.call_type_args.get(&SpanKey::from(&inner))
    );
    eprintln!(
        "outer outer(42) call_type_args entry: {:?}",
        tco.call_type_args.get(&SpanKey::from(&outer))
    );
    eprintln!("all call_type_args:");
    for (k, v) in &tco.call_type_args {
        eprintln!("  {k:?} -> {v:?}");
    }
}
