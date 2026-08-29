#![allow(
    dead_code,
    reason = "this support module is shared by separate emission and structural test roots"
)]

use hew_codegen_rs::{cleanup_capabilities_for_target, CleanupUnwindStrategy};
use std::path::Path;

pub(crate) fn read_llvm_ir(path: &Path) -> String {
    std::fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("read emitted LLVM IR at {}: {error}", path.display()))
        // LLVMPrintModuleToFile writes CRLF on Windows. Normalize at this read
        // boundary so participating block/function extractors use one LF grammar.
        .replace("\r\n", "\n")
}

pub(crate) fn cleanup_strategy(ir: &str) -> CleanupUnwindStrategy {
    let triple = ir
        .lines()
        .find_map(|line| {
            line.strip_prefix("target triple = \"")
                .and_then(|value| value.strip_suffix('"'))
        })
        .unwrap_or_else(|| panic!("emitted LLVM IR must declare its target triple:\n{ir}"));
    cleanup_capabilities_for_target(triple).unwind_strategy
}

pub(crate) fn call_pattern(strategy: CleanupUnwindStrategy, signature: &str) -> String {
    let opcode = match strategy {
        CleanupUnwindStrategy::StructuredLlvm => "invoke",
        CleanupUnwindStrategy::CrashOwnerRegistry => "call",
    };
    format!("{opcode} {signature}")
}

pub(crate) fn assert_target_call(
    ir: &str,
    strategy: CleanupUnwindStrategy,
    signature: &str,
    context: &str,
) {
    let expected = call_pattern(strategy, signature);
    assert!(
        ir.contains(&expected),
        "{context} must emit target-appropriate `{expected}`:\n{ir}"
    );
}

pub(crate) fn target_call_count(
    ir: &str,
    strategy: CleanupUnwindStrategy,
    signature: &str,
) -> usize {
    ir.matches(&call_pattern(strategy, signature)).count()
}

fn function_ir<'a>(ir: &'a str, name: &str) -> &'a str {
    let symbol = format!("@{name}(");
    let header = ir
        .lines()
        .find(|line| line.starts_with("define ") && line.contains(&symbol))
        .unwrap_or_else(|| panic!("LLVM IR must define `{name}`:\n{ir}"));
    let start = ir.find(header).expect("selected LLVM line has an offset");
    let tail = &ir[start..];
    let end = tail
        .find("\n}")
        .unwrap_or_else(|| panic!("LLVM definition for `{name}` must close:\n{tail}"));
    &tail[..end + 2]
}

fn block_ir<'a>(function_ir: &'a str, label: &str) -> &'a str {
    let marker = format!("\n{label}:");
    let start = function_ir
        .find(&marker)
        .unwrap_or_else(|| panic!("LLVM function must contain block `{label}`:\n{function_ir}"))
        + 1;
    let tail = &function_ir[start..];
    let end = tail.find("\n\n").unwrap_or(tail.len());
    &tail[..end]
}

fn referenced_cleanup_thunks<'a>(module_ir: &'a str, body: &str) -> Vec<&'a str> {
    let mut symbols = Vec::new();
    for symbol in body
        .lines()
        .filter(|line| line.contains("call i64 @hew_cont_crash_cleanup_arm"))
        .filter_map(|line| {
            let tail = line.split_once("ptr @__hew_frame_cleanup_")?.1;
            let suffix = tail
                .split(|character: char| {
                    character == ',' || character == ')' || character.is_whitespace()
                })
                .next()?;
            Some(format!("__hew_frame_cleanup_{suffix}"))
        })
    {
        if !symbols.contains(&symbol) {
            symbols.push(symbol);
        }
    }
    symbols
        .iter()
        .map(|symbol| function_ir(module_ir, symbol))
        .collect()
}

pub(crate) fn assert_consumed_string_result_cleanup(
    module_ir: &str,
    body: &str,
    caller: &str,
    producer: &str,
    expected_fallback_arms: usize,
    expected_fallback_body_drops: usize,
) {
    let strategy = cleanup_strategy(module_ir);
    assert_target_call(
        body,
        strategy,
        &format!("ptr @{producer}("),
        &format!("{caller}'s canonical string producer"),
    );

    match strategy {
        CleanupUnwindStrategy::StructuredLlvm => {
            let release_blocks = body
                .split("\n\n")
                .filter(|block| block.contains("call void @hew_string_drop("))
                .collect::<Vec<_>>();
            assert_eq!(
                release_blocks.len(),
                2,
                "{caller}: structured cleanup needs one normal and one unwind release block:\n{body}"
            );
            assert_eq!(
                release_blocks
                    .iter()
                    .filter(|block| block.contains("ret i64 "))
                    .count(),
                1,
                "{caller}: exactly one release block must return normally:\n{body}"
            );
            assert_eq!(
                release_blocks
                    .iter()
                    .filter(|block| block.contains("resume "))
                    .count(),
                1,
                "{caller}: exactly one release block must resume the unwind:\n{body}"
            );
            for block in release_blocks {
                assert_eq!(
                    block.matches("call void @hew_string_drop(").count(),
                    1,
                    "{caller}: each executable cleanup path must release exactly once:\n{block}"
                );
                assert!(
                    block.contains("store ptr null"),
                    "{caller}: each release path must neutralize the temporary slot:\n{block}"
                );
            }
            let producer_unwind = block_ir(body, "invoke.cleanup");
            assert_eq!(
                producer_unwind
                    .matches("call void @hew_string_drop(")
                    .count(),
                0,
                "{caller}: producer unwind precedes result materialization and must not release:\n\
                 {producer_unwind}"
            );
        }
        CleanupUnwindStrategy::CrashOwnerRegistry => {
            assert_eq!(
                body.lines()
                    .filter(|line| {
                        line.contains("%helper_crash_cleanup_token_")
                            && line.contains(" = alloca i64")
                    })
                    .count(),
                1,
                "{caller}: fallback cleanup must allocate exactly one owner token:\n{body}"
            );
            assert_eq!(
                body.matches("call i64 @hew_cont_crash_cleanup_arm").count(),
                expected_fallback_arms,
                "{caller}: fallback owner publications must have exactly \
                 {expected_fallback_arms} arm operations:\n{body}"
            );
            assert_eq!(
                body.matches("call i1 @hew_cont_crash_cleanup_deactivate")
                    .count(),
                expected_fallback_arms,
                "{caller}: each fallback rearm must first deactivate the prior token:\n{body}"
            );
            assert!(
                body.contains("call i1 @hew_cont_crash_cleanup_retire"),
                "{caller}: normal cleanup must retire fallback authority before release:\n{body}"
            );
            assert_eq!(
                body.matches("call void @hew_string_drop(").count(),
                expected_fallback_body_drops,
                "{caller}: guarded fallback exits must contain exactly \
                 {expected_fallback_body_drops} string release sites:\n{body}"
            );
            let release_blocks = body
                .split("\n\n")
                .filter(|block| block.contains("call void @hew_string_drop("))
                .collect::<Vec<_>>();
            assert!(
                release_blocks.iter().all(|block| {
                    block.matches("call void @hew_string_drop(").count() == 1
                        && block.contains("store ptr null")
                }),
                "{caller}: every guarded fallback release must drop once and neutralize:\n{body}"
            );
            if expected_fallback_body_drops > 1 {
                assert!(
                    body.contains("helper_crash_cleanup_return_active_value_")
                        && body.contains("helper_crash_cleanup_return_drop_"),
                    "{caller}: additional return-sweep release sites must remain \
                     active-token guarded:\n{body}"
                );
            }
            let thunks = referenced_cleanup_thunks(module_ir, body);
            assert_eq!(
                thunks.len(),
                1,
                "{caller}: its single registry arm must reference one typed thunk:\n{body}"
            );
            assert_eq!(
                thunks[0].matches("call void @hew_string_drop(").count(),
                1,
                "{caller}: the body-referenced fallback thunk must release exactly once:\n{}",
                thunks[0]
            );
            assert!(
                thunks[0].contains("store ptr null"),
                "{caller}: the fallback thunk must neutralize its string slot:\n{}",
                thunks[0]
            );
        }
    }
}
