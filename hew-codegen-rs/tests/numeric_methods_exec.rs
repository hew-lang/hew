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

    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
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

extern "C" fn mock_string_drop(_ptr: *mut std::ffi::c_void) {}

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
    if let Some(f) = module.get_function("hew_string_drop") {
        ee.add_global_mapping(&f, mock_string_drop as *const () as usize);
    }

    let jit_main = unsafe {
        ee.get_function::<unsafe extern "C" fn() -> i64>("main")
            .expect("main must be present in the JIT module")
    };
    unsafe { jit_main.call() }
}

const OPTION_DECL: &str = "enum Option<T> { Some(T); None; }";

#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn option_predicates_read_tags_for_i64_and_string_payloads() {
    let src = r#"
        fn main() -> i64 {
            let a: Option<i64> = Some(5);
            let b: Option<i64> = None;

            if a.is_some() == false { return 1; }
            if a.is_none() == true { return 2; }
            if b.is_none() == false { return 3; }
            if b.is_some() == true { return 4; }

            let s: Option<string> = Some("hello");
            let n: Option<string> = None;

            if s.is_some() == false { return 5; }
            if s.is_none() == true { return 6; }
            if n.is_none() == false { return 7; }
            if n.is_some() == true { return 8; }

            0
        }
        "#;
    let ll = compile_to_ll(src, "option_predicates_tags");
    assert_eq!(jit_run_main(&ll), 0);
}

#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
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
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
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
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
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
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
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
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
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

#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn numeric_cast_i64_to_i32_truncates_real_bits() {
    let src = r"
        fn main() -> i64 {
            let x: i64 = 4294967296 + 5;
            let y: i32 = x as i32;
            y as i64
        }
    ";
    let ll = compile_to_ll(src, "numeric_cast_i64_i32_trunc");
    assert_eq!(jit_run_main(&ll), 5);
}

#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn numeric_cast_i32_to_i64_widens() {
    let src = r"
        fn main() -> i64 {
            let x: i32 = 123;
            x as i64
        }
    ";
    let ll = compile_to_ll(src, "numeric_cast_i32_i64_widen");
    assert_eq!(jit_run_main(&ll), 123);
}

#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn numeric_cast_i64_to_f64_and_back_executes() {
    let src = r"
        fn main() -> i64 {
            let x: i64 = 7;
            let f: f64 = x as f64;
            f as i64
        }
    ";
    let ll = compile_to_ll(src, "numeric_cast_i64_f64_roundtrip");
    assert_eq!(jit_run_main(&ll), 7);
}

#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn numeric_cast_bool_to_int_and_back_executes() {
    let src = r"
        fn main() -> i64 {
            let n: u16 = true as u16;
            let b: bool = n as bool;
            if b { n as i64 } else { 0 }
        }
    ";
    let ll = compile_to_ll(src, "numeric_cast_bool_int_roundtrip");
    assert_eq!(jit_run_main(&ll), 1);
}

#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn free_math_builtins_execute_via_llvm_intrinsics() {
    let src = r"
        fn distance(x1: f64, y1: f64, x2: f64, y2: f64) -> f64 {
            let dx: f64 = x2 - x1;
            let dy: f64 = y2 - y1;
            sqrt(pow(dx, 2) + pow(dy, 2))
        }

        fn main() -> i64 {
            if sqrt(9.0) != 3.0 { return 1; }
            if pow(2, 10) != 1024.0 { return 2; }
            if floor(2.75) != 2.0 { return 3; }
            if ceil(2.25) != 3.0 { return 4; }
            if round(2.5) != 3.0 { return 5; }
            if round(-2.5) != -3.0 { return 6; }
            if abs(-7) != 7 { return 7; }
            if min(4, 9) != 4 { return 8; }
            if max(4, 9) != 9 { return 9; }
            if math.abs(-1.5) != 1.5 { return 10; }
            if math.min(1.5, 2.5) != 1.5 { return 11; }
            if math.max(1.5, 2.5) != 2.5 { return 12; }
            if distance(0.0, 0.0, 3.0, 4.0) != 5.0 { return 13; }
            0
        }
    ";
    let ll = compile_to_ll(src, "free_math_intrinsics_exec");
    assert_eq!(jit_run_main(&ll), 0);
}

#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn user_math_name_shadowing_is_not_hijacked_by_intrinsic_intercept() {
    let src = r"
        fn min(a: i64, b: i64) -> i64 {
            a + b
        }

        fn sqrt(x: f64) -> f64 {
            x + 1.0
        }

        fn main() -> i64 {
            if min(4, 9) != 13 { return 1; }
            if sqrt(9.0) != 10.0 { return 2; }
            0
        }
    ";
    let ll = compile_to_ll(src, "math_shadowing_not_hijacked");
    assert_eq!(jit_run_main(&ll), 0);
}
