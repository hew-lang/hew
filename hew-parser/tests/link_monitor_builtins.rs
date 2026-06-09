//! Parser tests for `link(handle)` and `monitor(handle)` actor lifecycle builtins.
//!
//! Both parse as `Expr::Call { function: Identifier("link"/"monitor"), ... }`.
//! The type checker registers them as builtins and enforces arity and the
//! wasm32 rejection (via `WasmUnsupportedFeature::LinkMonitor`); no parser-level
//! target awareness is needed or correct. See `hew-types`
//! `wasm_rejects_link_monitor_calls` for the checker-side gate test.

use hew_parser::ast::{CallArg, Expr, Item, Stmt};

// ── helpers ──────────────────────────────────────────────────────────────────

/// Walk the first function's body statements and return the expression from
/// the first `let _ = <expr>` statement.
fn first_let_value_expr(source: &str) -> Expr {
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    for (item, _) in &result.program.items {
        if let Item::Function(f) = item {
            for (stmt, _) in &f.body.stmts {
                if let Stmt::Let {
                    value: Some((expr, _)),
                    ..
                } = stmt
                {
                    return expr.clone();
                }
            }
        }
    }
    panic!("no let-binding value expression found in first function body");
}

// ── link builtin ─────────────────────────────────────────────────────────────

#[test]
fn link_builtin_parses_without_errors() {
    let src = r"
fn main() {
    let _ = link(actor_handle);
}
";
    let result = hew_parser::parse(src);
    assert!(
        result.errors.is_empty(),
        "link(handle) should parse without errors; got: {:?}",
        result.errors
    );
}

#[test]
fn link_builtin_produces_call_on_identifier() {
    let src = r"
fn main() {
    let _ = link(actor_handle);
}
";
    let expr = first_let_value_expr(src);
    match &expr {
        Expr::Call {
            function,
            args,
            type_args,
            ..
        } => {
            assert!(
                matches!(&function.0, Expr::Identifier(name) if name == "link"),
                "expected Identifier(\"link\") as function; got {:?}",
                &function.0
            );
            assert_eq!(args.len(), 1, "link takes exactly one argument");
            assert!(
                type_args.is_none() || type_args.as_ref().unwrap().is_empty(),
                "link takes no type arguments"
            );
            // Verify the single positional argument is our handle identifier.
            let arg_expr = match &args[0] {
                CallArg::Positional((e, _)) => e,
                CallArg::Named { name, .. } => {
                    panic!("expected positional arg; got named arg `{name}`")
                }
            };
            assert!(
                matches!(arg_expr, Expr::Identifier(n) if n == "actor_handle"),
                "expected Identifier(\"actor_handle\"); got {arg_expr:?}"
            );
        }
        other => panic!("expected Expr::Call; got {other:?}"),
    }
}

// ── monitor builtin ───────────────────────────────────────────────────────────

#[test]
fn monitor_builtin_parses_without_errors() {
    let src = r"
fn main() {
    let _ref = monitor(actor_handle);
}
";
    let result = hew_parser::parse(src);
    assert!(
        result.errors.is_empty(),
        "monitor(handle) should parse without errors; got: {:?}",
        result.errors
    );
}

#[test]
fn monitor_builtin_produces_call_on_identifier() {
    let src = r"
fn main() {
    let _ref = monitor(actor_handle);
}
";
    let expr = first_let_value_expr(src);
    match &expr {
        Expr::Call {
            function,
            args,
            type_args,
            ..
        } => {
            assert!(
                matches!(&function.0, Expr::Identifier(name) if name == "monitor"),
                "expected Identifier(\"monitor\") as function; got {:?}",
                &function.0
            );
            assert_eq!(args.len(), 1, "monitor takes exactly one argument");
            assert!(
                type_args.is_none() || type_args.as_ref().unwrap().is_empty(),
                "monitor takes no type arguments"
            );
            let arg_expr = match &args[0] {
                CallArg::Positional((e, _)) => e,
                CallArg::Named { name, .. } => {
                    panic!("expected positional arg; got named arg `{name}`")
                }
            };
            assert!(
                matches!(arg_expr, Expr::Identifier(n) if n == "actor_handle"),
                "expected Identifier(\"actor_handle\"); got {arg_expr:?}"
            );
        }
        other => panic!("expected Expr::Call; got {other:?}"),
    }
}

// ── combined use in actor context ─────────────────────────────────────────────

#[test]
fn link_and_monitor_parse_together_in_actor_fn() {
    // Verify that both builtins parse cleanly in a realistic actor-body context.
    // This mirrors the source used in the hew-types wasm_rejects_link_monitor_calls
    // checker test, giving the parser layer a concrete regression anchor.
    let src = r"
actor Worker {
    receive fn ping() {}
}

fn main() {
    let worker = spawn Worker;
    let _m = monitor(worker);
    link(worker);
}
";
    let result = hew_parser::parse(src);
    assert!(
        result.errors.is_empty(),
        "link + monitor should parse without errors in actor context; got: {:?}",
        result.errors
    );
}
