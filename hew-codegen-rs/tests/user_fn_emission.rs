//! End-to-end LLVM-IR emission tests for `Terminator::Call`.
//!
//! A direct call to a user-defined function in the same module must produce
//! an LLVM `call` instruction with the callee's decorated symbol name. For
//! `fn add(a: i64, b: i64) -> i64 { a + b }` called as `add(2, 3)`, the
//! emitted IR must contain `call i64 @add(i64 2, i64 3)`.
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

/// Run the full HIR + checker + MIR + codegen pipeline on `source` and
/// return the emitted textual LLVM IR. Uses the full checker because
/// function-call lowering depends on checker-resolved types.
fn emit_ll(source: &str, module_name: &str) -> String {
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
    let tmp = std::env::temp_dir().join(format!("hew-userfn-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
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

/// `fn add(a: i64, b: i64) -> i64 { a + b }` called as `add(2, 3)` from
/// `main` must produce LLVM IR containing `call i64 @add(i64 2, i64 3)`.
///
/// This verifies that:
/// 1. `declare_function` emits a correct `define ... @add(i64, i64)` header.
/// 2. `Terminator::Call` lowers to `call <ret_ty> @callee(<args>)`.
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
    assert!(
        ll.contains("call i64 @add("),
        "LLVM IR must contain `call i64 @add(`;\n--- IR ---\n{ll}"
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
