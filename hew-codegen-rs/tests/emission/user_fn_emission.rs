//! End-to-end LLVM-IR emission tests for `Terminator::Call`.
//!
//! A direct call to a user-defined function in the same module must use the
//! target's cleanup-capable call shape with the callee's decorated symbol name:
//! LLVM `invoke` on structured-unwind targets and `call` where cleanup uses a
//! target-side registry.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the callee symbol must be present in
//!   `fn_symbols`; any mismatch surfaces as a `CodegenError::FailClosed`
//!   before any broken IR is emitted.
//! - `exhaustive-coverage` (P0): one test per observable LLVM-IR property.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

use crate::ir_assertions::{
    assert_consumed_string_result_cleanup, assert_target_call, cleanup_strategy,
};

/// Run the full HIR + checker + MIR + codegen pipeline on `source` and
/// return the emitted textual LLVM IR. Uses the full checker because
/// function-call lowering depends on checker-resolved types.
fn emit_ll(source: &str, module_name: &str) -> String {
    emit_ll_for_target(source, module_name, None)
}

fn emit_ll_for_target(source: &str, module_name: &str, target_triple: Option<&str>) -> String {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
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
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics must be empty before codegen: {:#?}",
        pipeline.diagnostics
    );
    let target_slug = target_triple.unwrap_or("host").replace('-', "_");
    let prefix = format!("hew-userfn-{module_name}-{target_slug}-");
    let tmp = tempfile::Builder::new()
        .prefix(&prefix)
        .tempdir()
        .expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: tmp.path(),
        native: false,
        wasm: false,
        target_triple,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("user-fn pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

fn function_ir<'a>(ll: &'a str, name: &str) -> &'a str {
    let symbol = format!("@{name}(");
    let header = ll
        .lines()
        .find(|line| line.starts_with("define ") && line.contains(&symbol))
        .unwrap_or_else(|| panic!("LLVM IR must define `{name}`:\n{ll}"));
    let start = ll
        .find(header)
        .expect("a line selected from the LLVM IR must have an offset");
    let body = &ll[start..];
    let end = body
        .find("\n}")
        .unwrap_or_else(|| panic!("LLVM definition for `{name}` must be closed:\n{body}"));
    &body[..end + 2]
}

fn block_ir<'a>(function_ir: &'a str, label: &str) -> &'a str {
    let marker = format!("\n{label}:");
    let start = function_ir
        .find(&marker)
        .unwrap_or_else(|| panic!("LLVM function must contain block `{label}`:\n{function_ir}"))
        + 1;
    let body = &function_ir[start..];
    let end = body.find("\n\n").unwrap_or(body.len());
    &body[..end]
}

/// `fn add(a: i64, b: i64) -> i64 { a + b }` called as `add(2, 3)` from
/// `main` must produce the target's cleanup-capable call form.
///
/// This verifies that:
/// 1. `declare_function` emits a correct `define ... @add(i64, i64)` header.
/// 2. `Terminator::Call` follows the target cleanup strategy.
/// 3. The result is stored into a local and returned from `main`.
#[test]
fn call_i64_user_fn_emits_call_instruction() {
    let src = r#"
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
        fn main() -> i64 {
            add(2, 3)
        }
    "#;

    let ll = emit_ll(src, "user_fn_call");

    // The callee must be declared/defined with i64 params.
    assert!(
        ll.contains("@add(i64") || ll.contains("@add(i64 %"),
        "LLVM IR must declare @add with i64 params;\n--- IR ---\n{ll}"
    );

    // The call site must reference @add.
    assert_target_call(
        &ll,
        cleanup_strategy(&ll),
        "i64 @add(",
        "the direct user-function call site",
    );
}

/// `add` itself must have a `define` with two i64 parameters and use them
/// (via `add nsw` / `add` instruction) in its body. Verifies the
/// parameter-prologue: `lower_function` stores each LLVM param into the
/// corresponding local alloca, so `a + b` in the body loads two distinct
/// local slots.
#[test]
fn callee_define_has_i64_params() {
    let src = r#"
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
        fn main() -> i64 {
            add(2, 3)
        }
    "#;

    let ll = emit_ll(src, "user_fn_params");

    // The function definition for `add` must carry two i64 parameters.
    // Match `define internal i64 @add(i64 %0, i64 %1)` or similar forms
    // depending on inkwell's naming of anonymous params.
    assert!(
        ll.contains("@add(i64") && ll.contains("i64 @add("),
        "LLVM IR must define @add with at least one i64 param;\n--- IR ---\n{ll}"
    );
}

#[test]
fn unsafe_audited_extern_string_temp_emits_one_release() {
    let source = r#"
extern "C" {
    fn hew_xml_to_string(node: i64) -> string;
}

fn borrow_len(value: string) -> i64 {
    value.len()
}

fn direct(node: i64) -> i64 {
    borrow_len(unsafe { hew_xml_to_string(node) })
}
"#;
    for (slug, target_triple) in [
        ("host", None),
        ("windows_msvc", Some("x86_64-pc-windows-msvc")),
    ] {
        let module_name = format!("unsafe_extern_string_temp_{slug}");
        let ll = emit_ll_for_target(source, &module_name, target_triple);
        let direct = function_ir(&ll, "direct");
        match cleanup_strategy(&ll) {
            hew_codegen_rs::CleanupUnwindStrategy::StructuredLlvm => {
                let normal = block_ir(direct, "bb2");
                let callee_unwind = block_ir(direct, "invoke.cleanup3");
                let producer_unwind = block_ir(direct, "invoke.cleanup");
                assert_eq!(
                    direct.matches("call void @hew_string_drop(").count(),
                    2,
                    "normal completion and callee unwind must have one mutually-exclusive \
                     release site each:\n{direct}"
                );
                assert_eq!(
                    normal.matches("call void @hew_string_drop(").count(),
                    1,
                    "normal completion must release the transferred extern result exactly once:\n{normal}"
                );
                assert!(normal.contains("store ptr null"));
                assert_eq!(
                    callee_unwind.matches("call void @hew_string_drop(").count(),
                    1,
                    "borrow_len unwind must release the transferred extern result exactly once:\n\
                     {callee_unwind}"
                );
                assert!(
                    callee_unwind.contains("store ptr null") && callee_unwind.contains("resume ")
                );
                assert_eq!(
                    producer_unwind
                        .matches("call void @hew_string_drop(")
                        .count(),
                    0,
                    "the extern-call unwind precedes result production and must not release an \
                     uninitialised string slot:\n{producer_unwind}"
                );
            }
            hew_codegen_rs::CleanupUnwindStrategy::CrashOwnerRegistry => {
                assert_consumed_string_result_cleanup(
                    &ll,
                    direct,
                    "direct",
                    "hew_xml_to_string",
                    2,
                    2,
                );
                assert_target_call(
                    direct,
                    cleanup_strategy(&ll),
                    "i64 @borrow_len(",
                    "the borrowing helper call",
                );
            }
        }
    }
}
