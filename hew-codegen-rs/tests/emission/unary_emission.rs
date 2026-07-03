use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tco,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let verify = hew_hir::verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir: {:?} verify: {:?}",
        output.diagnostics,
        verify
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-unary-{module_name}"));
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
    let artefacts = emit_module(&pipeline, &options).expect("unary pipeline must emit");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

#[test]
fn unary_bool_not_emits_logical_not_compare() {
    let ll = emit_ll("fn main() -> bool { !false }", "unary_bool_not");
    assert!(
        ll.contains("icmp eq"),
        "bool ! must emit a logical-not comparison; got:\n{ll}"
    );
}

#[test]
fn unary_integer_negate_of_literal_emits_no_checked_sub() {
    let ll = emit_ll("fn main() -> i64 { -5 }", "unary_int_neg_literal");
    assert!(
        !ll.contains("llvm.ssub.with.overflow"),
        "literal unary - must fold at compile time, not emit a runtime \
         checked-subtract; got:\n{ll}"
    );
}

#[test]
fn unary_integer_negate_of_param_emits_checked_sub_overflow() {
    let ll = emit_ll("fn main(n: i64) -> i64 { -n }", "unary_int_neg_param");
    assert!(
        ll.contains("llvm.ssub.with.overflow.i64"),
        "integer unary - on a non-literal operand must still emit signed \
         checked subtraction; got:\n{ll}"
    );
}

#[test]
fn unary_float_negate_emits_float_sub_from_negative_zero() {
    let ll = emit_ll(
        "fn main() -> i64 { let x: f64 = -1.5; 0 }",
        "unary_float_neg",
    );
    assert!(
        ll.contains("fsub double"),
        "float unary - must emit floating-point negation; got:\n{ll}"
    );
}

#[test]
fn unary_integer_bitwise_not_emits_xor() {
    let ll = emit_ll("fn main() -> i64 { ~0 }", "unary_bitnot");
    assert!(
        ll.contains("xor i64"),
        "integer ~ must emit LLVM xor/not; got:\n{ll}"
    );
}
