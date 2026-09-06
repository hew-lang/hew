use crate::support::checker_pipeline;

#[test]
fn checked_calls_and_task_values_compose_in_expression_positions() {
    for source in [
        "fn work() -> i64 { await sleep(1ms); 42 } fn main() { let value = await work(); println(value); }",
        "fn work() {} fn main() { let task = fork work(); let value: () = await task; }",
        "fn work() -> i64 { 42 } fn run() -> i64 { scope { let task = fork work(); return await task; } } fn main() {}",
        "fn main() { let value = await (if true { fork { 41 } } else { fork { 0 } }); println(value); }",
        "fn main() { let offset = 1; let child = fork (|value: i64| value + offset)(41); let result = await child; println(result); }",
    ] {
        let (parsed, checked) = checker_pipeline::typecheck_source(source);
        assert!(checked.errors.is_empty(), "{source}: {:?}", checked.errors);
        let output = hew_hir::lower_program_host_target(&parsed.program, &checked, &hew_hir::ResolutionCtx);
        assert!(output.diagnostics.is_empty(), "{source}: {:?}", output.diagnostics);
        let diagnostics = hew_hir::verify_hir(&output.module);
        assert!(diagnostics.is_empty(), "{source}: {diagnostics:?}");
    }
}
