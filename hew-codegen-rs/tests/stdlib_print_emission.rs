use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

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
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics must be empty before MIR: {:#?}",
        output.diagnostics
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics must be empty before codegen: {:#?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-stdlib-print-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("stdlib print pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

#[test]
fn println_string_lowers_to_print_value_intercept() {
    let ll = emit_ll(
        r#"
        fn main() -> i64 {
            println("hello");
            0
        }
        "#,
        "stdlib_println_str",
    );

    assert!(
        ll.contains("declare void @hew_print_value(i8, i64, i1)"),
        "LLVM IR must predeclare hew_print_value with the print ABI:\n{ll}"
    );
    assert!(
        ll.contains("call void @hew_print_value(i8 4,"),
        "println(\"hello\") must pass the runtime Str tag (4):\n{ll}"
    );
    assert!(
        ll.contains("i1 true"),
        "println must pass newline=true to hew_print_value:\n{ll}"
    );
    assert!(
        !ll.contains("@println_str"),
        "print intercept must not declare or call a monomorphic println_str symbol:\n{ll}"
    );
}
