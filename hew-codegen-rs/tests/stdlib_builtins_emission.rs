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
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
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
    let tmp = std::env::temp_dir().join(format!("hew-stdlib-builtins-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("stdlib builtin pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

#[test]
fn builtins_assert_family_declares_runtime_symbols() {
    let ll = emit_ll(
        r#"
        fn main() -> i64 {
            assert(true);
            let actual: i64 = 2 + 2;
            assert_eq(actual, 4);
            assert_ne(actual, 5);
            0
        }
        "#,
        "assert_family",
    );

    assert!(
        ll.contains("declare void @hew_assert(i8)"),
        "assert(bool) must use the i8 bool ABI:\n{ll}"
    );
    assert!(
        ll.contains("declare void @hew_assert_eq_i64(i64, i64)"),
        "assert_eq(i64, i64) must declare hew_assert_eq_i64:\n{ll}"
    );
    assert!(
        ll.contains("declare void @hew_assert_ne_i64(i64, i64)"),
        "assert_ne(i64, i64) must declare hew_assert_ne_i64:\n{ll}"
    );
    assert!(
        ll.contains("call void @hew_assert(i8 "),
        "assert(true) must call hew_assert through the i8 bool ABI:\n{ll}"
    );
}

#[test]
fn builtins_exit_declares_runtime_symbol() {
    let ll = emit_ll(
        r#"
        fn main() -> i64 {
            exit(42);
            0
        }
        "#,
        "exit_42",
    );

    assert!(
        ll.contains("declare void @hew_exit(i64)"),
        "exit(i64) must declare hew_exit with the catalog ABI:\n{ll}"
    );
    assert!(
        ll.contains("call void @hew_exit(i64 "),
        "exit(42) must call hew_exit:\n{ll}"
    );
}

#[test]
fn builtins_sleep_ms_declares_i64_runtime_symbol() {
    let ll = emit_ll(
        r#"
        fn main() -> i64 {
            sleep_ms(0);
            0
        }
        "#,
        "sleep_ms",
    );

    assert!(
        ll.contains("declare void @hew_sleep_ms(i64)"),
        "sleep_ms(i64) must declare hew_sleep_ms with the catalog ABI:\n{ll}"
    );
    assert!(
        ll.contains("call void @hew_sleep_ms(i64 "),
        "sleep_ms(0) must call hew_sleep_ms:\n{ll}"
    );
}

#[test]
fn builtins_print_overloads_use_print_value_tags() {
    let ll = emit_ll(
        r#"
        fn main() -> i64 {
            print(7);
            print(true);
            let value: f64 = 1.5;
            print(value);
            0
        }
        "#,
        "print_overloads",
    );

    assert!(
        ll.contains("declare void @hew_print_value(i8, i64, i1)"),
        "print overloads must use the generic print-value ABI:\n{ll}"
    );
    assert!(
        ll.contains("call void @hew_print_value(i8 1,"),
        "print(i64) must pass the I64 tag:\n{ll}"
    );
    assert!(
        ll.contains("call void @hew_print_value(i8 3,"),
        "print(bool) must pass the Bool tag:\n{ll}"
    );
    assert!(
        ll.contains("call void @hew_print_value(i8 2,"),
        "print(f64) must pass the F64 tag:\n{ll}"
    );
    assert!(
        ll.contains("i1 false"),
        "print must pass newline=false:\n{ll}"
    );
}

#[test]
fn free_math_builtins_emit_llvm_intrinsics() {
    let ll = emit_ll(
        r#"
        fn main() -> i64 {
            let a: f64 = sqrt(9.0);
            let b: i64 = abs(-7);
            let c: i64 = min(4, 9);
            let d: i64 = max(4, 9);
            let e: f64 = math.abs(-1.5);
            let f: f64 = math.min(1.5, 2.5);
            let g: f64 = math.max(1.5, 2.5);
            let h: f64 = pow(2, 10);
            let i: f64 = floor(2.75);
            let j: f64 = ceil(2.25);
            let k: f64 = round(2.5);
            b + c + d
        }
        "#,
        "free_math_intrinsics",
    );

    for intrinsic in [
        "declare double @llvm.sqrt.f64(double)",
        "declare i64 @llvm.abs.i64(i64, i1 immarg)",
        "declare i64 @llvm.smin.i64(i64, i64)",
        "declare i64 @llvm.smax.i64(i64, i64)",
        "declare double @llvm.fabs.f64(double)",
        "declare double @llvm.minnum.f64(double, double)",
        "declare double @llvm.maxnum.f64(double, double)",
        "declare double @llvm.pow.f64(double, double)",
        "declare double @llvm.floor.f64(double)",
        "declare double @llvm.ceil.f64(double)",
        "declare double @llvm.round.f64(double)",
    ] {
        assert!(ll.contains(intrinsic), "missing `{intrinsic}`:\n{ll}");
    }

    assert!(
        !ll.contains("declare double @sqrt("),
        "free sqrt must not be emitted as an external user function:\n{ll}"
    );
    assert!(
        !ll.contains("declare i64 @abs("),
        "free abs must not be emitted as an external user function:\n{ll}"
    );
}
