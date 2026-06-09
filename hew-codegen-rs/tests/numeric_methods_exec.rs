#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::OptimizationLevel;

fn compile_to_ll(source: &str, module_name: &str) -> std::path::PathBuf {
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
        "type errors: {:?}",
        tc_output.errors
    );

    let output = lower_program(&parsed.program, &tc_output, &ResolutionCtx);
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify: {verify:?}");

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let tmp = std::env::temp_dir().join(format!("hew-numeric-methods-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create temp dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(&pipeline, &options).unwrap_or_else(|e| panic!("emit_module: {e}"));
    artefacts
        .ll_path
        .expect("emit_module must populate ll_path")
}

extern "C" fn mock_cooperate() -> i32 {
    0
}

extern "C" fn mock_trap(code: i32) {
    panic!("unexpected trap in numeric-method test: code={code}");
}

fn jit_run_main(ll_path: &Path) -> i64 {
    Target::initialize_native(&InitializationConfig::default())
        .expect("native target initialisation must succeed");

    let ctx = Context::create();
    let buf = MemoryBuffer::create_from_file(ll_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", ll_path.display()));
    let module = ctx
        .create_module_from_ir(buf)
        .unwrap_or_else(|e| panic!("parse IR {}: {e}", ll_path.display()));

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("create_jit_execution_engine must succeed");

    if let Some(f) = module.get_function("hew_actor_cooperate") {
        ee.add_global_mapping(&f, mock_cooperate as *const () as usize);
    }
    if let Some(f) = module.get_function("hew_trap_with_code") {
        ee.add_global_mapping(&f, mock_trap as *const () as usize);
    }

    let jit_main = unsafe {
        ee.get_function::<unsafe extern "C" fn() -> i64>("main")
            .expect("main must be present in the JIT module")
    };
    unsafe { jit_main.call() }
}

const OPTION_DECL: &str = "enum Option<T> { Some(T); None; }";

#[test]
fn checked_add_signed_no_overflow_returns_some_result() {
    let src = format!(
        r#"
        {OPTION_DECL}
        fn main() -> i64 {{
            let a: i64 = 40;
            let b: i64 = 2;
            let x: Option<i64> = a.checked_add(b);
            match x {{
                Option::Some(v) => v,
                Option::None => 0,
            }}
        }}
        "#
    );
    let ll = compile_to_ll(&src, "checked_add_signed_some");
    assert_eq!(jit_run_main(&ll), 42);
}

#[test]
fn checked_add_signed_overflow_returns_none() {
    let src = format!(
        r#"
        {OPTION_DECL}
        fn main() -> i64 {{
            let a: i64 = 9223372036854775807;
            let b: i64 = 1;
            let x: Option<i64> = a.checked_add(b);
            match x {{
                Option::Some(v) => v,
                Option::None => 1,
            }}
        }}
        "#
    );
    let ll = compile_to_ll(&src, "checked_add_signed_none");
    assert_eq!(jit_run_main(&ll), 1);
}

#[test]
fn checked_add_unsigned_width_overflow_returns_none() {
    let src = format!(
        r#"
        {OPTION_DECL}
        fn main() -> i64 {{
            let a: u8 = 255;
            let b: u8 = 1;
            let x: Option<u8> = a.checked_add(b);
            match x {{
                Option::Some(v) => 0,
                Option::None => 1,
            }}
        }}
        "#
    );
    let ll = compile_to_ll(&src, "checked_add_u8_none");
    assert_eq!(jit_run_main(&ll), 1);
}

#[test]
fn saturating_add_clamps_to_signed_max() {
    let src = r#"
        fn main() -> i64 {
            let a: i64 = 9223372036854775807;
            let b: i64 = 1;
            a.saturating_add(b)
        }
    "#;
    let ll = compile_to_ll(src, "saturating_add_i64_max");
    assert_eq!(jit_run_main(&ll), i64::MAX);
}

#[test]
fn saturating_sub_clamps_to_signed_min() {
    let src = r#"
        fn main() -> i64 {
            let zero: i8 = 0;
            let max: i8 = 127;
            let one: i8 = 1;
            let near_min: i8 = zero.saturating_sub(max);
            let min: i8 = near_min.saturating_sub(one);
            let clamped: i8 = min.saturating_sub(one);
            if clamped == min { 1 } else { 0 }
        }
    "#;
    let ll = compile_to_ll(src, "saturating_sub_i64_min");
    assert_eq!(jit_run_main(&ll), 1);
}
