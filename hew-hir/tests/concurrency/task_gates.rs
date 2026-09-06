//! Checked HIR coverage for explicit child tasks and lexical deadlines.

use crate::support;

fn lower(source: &str) -> hew_hir::LowerOutput {
    let (parsed, checked) = support::checker_pipeline::typecheck_source(source);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let output =
        hew_hir::lower_program_host_target(&parsed.program, &checked, &hew_hir::ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let diagnostics = hew_hir::verify::verify_hir(&output.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    output
}

#[test]
fn fork_accepts_direct_indirect_and_parameterized_closure_calls() {
    lower(
        r"
        fn worker(value: i64) -> i64 { value + 1 }
        fn main() {
            let callable = worker;
            scope {
                let direct = fork worker(40);
                let other = fork callable(41);
                let closure = fork (|value: i64| value + 1)(42);
                let results: (i64, i64, i64) = (await direct, await other, await closure);
            }
        }
    ",
    );
}

#[test]
fn fork_accepts_empty_multi_statement_and_nested_bodies() {
    lower(
        r"
        fn worker(value: i64) -> i64 { value + 1 }
        fn main() {
            scope {
                let empty = fork {};
                let unit: () = await empty;
                let nested = fork {
                    let first = worker(40);
                    let child = fork worker(first);
                    return await child;
                };
                let result: i64 = await nested;
                let arithmetic: i64 = await fork (result + 1);
            }
        }
    ",
    );
}

#[test]
fn deadline_scopes_accept_empty_and_value_producing_bodies() {
    lower(
        r"
        fn main() {
            scope within 1ms {};
            let value: i64 = scope within 5s {
                let task = fork { 42 };
                await task
            };
        }
    ",
    );
}

#[test]
fn await_accepts_task_operands_from_blocks_and_branches() {
    lower(
        r"
        fn main() {
            let block: i64 = await { let task = fork { 41 }; task };
            let branch: i64 = await (if true { let chosen = fork { block + 1 }; chosen } else { let chosen = fork { 0 }; chosen });
        }
    ",
    );
}

#[test]
fn synthetic_qualified_fork_cannot_recover_a_local_function() {
    let (_, checked) = support::checker_pipeline::typecheck_source(
        r"
        fn worker() {}
        fn main() { let task = fork missing.worker(); }
    ",
    );
    assert!(
        checked
            .errors
            .iter()
            .any(|error| error.message.contains("missing")),
        "{:?}",
        checked.errors
    );
}

#[test]
fn checker_resolved_qualified_fork_is_accepted() {
    let program = support::checker_pipeline::program_with_imported_module(
        "pub fn worker() -> i64 { 42 }",
        "import m; fn main() { let task = fork m.worker(); let value: i64 = await task; }",
    );
    let checked = hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]))
        .check_program(&program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let output = hew_hir::lower_program_host_target(&program, &checked, &hew_hir::ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let diagnostics = hew_hir::verify::verify_hir(&output.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn ordinary_fork_and_await_expressions_lower_through_checked_hir() {
    lower(
        r"
        fn calculate(input: i64) -> i64 { input + 1 }
        fn main() {
            let task = fork calculate(41);
            let first: i64 = await task;
            let second: i64 = await fork calculate(first);
            let third: i64 = await fork { return calculate(second); };
            let fourth: i64 = await fork { calculate(third) };
            let _ = (first, second, third, fourth);
        }
    ",
    );
}

#[test]
fn ordinary_fork_batches_keep_checked_aggregate_result_shapes() {
    lower(
        r#"
        fn number() -> i64 { 42 }
        fn text() -> string { "hello" }
        fn main() {
            let numbers = await fork [number(), number()];
            let mixed = await fork (text(), number());
            let _ = (numbers, mixed);
        }
    "#,
    );
}
