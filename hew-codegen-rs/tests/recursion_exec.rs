//! JIT execution tests for recursive functions.
//!
//! Verifies that the full Hew pipeline (parser → HIR → MIR → LLVM codegen)
//! produces correct machine code for recursive and mutually-recursive
//! functions.  The regression target is a P0 bug where `if` and `match`
//! expressions in tail position were parsed as `Stmt::If`/`Stmt::Match`
//! rather than as trailing expressions, leaving the function's return slot
//! uninitialised.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::OptimizationLevel;

// ---------------------------------------------------------------------------
// Pipeline helper
// ---------------------------------------------------------------------------

/// Compile `source` through the full Hew pipeline and return the path of the
/// emitted `.ll` file.  Panics with a descriptive message at the first stage
/// that fails.
fn compile_to_ll(source: &str, module_name: &str) -> std::path::PathBuf {
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
        "HIR diagnostics: {:?}",
        output.diagnostics
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let tmp = std::env::temp_dir().join(format!("hew-recursion-exec-{module_name}"));
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

// ---------------------------------------------------------------------------
// JIT runner
// ---------------------------------------------------------------------------

/// Mock `hew_actor_cooperate` — always returns 0 (continue, never cancel).
extern "C" fn mock_cooperate() -> i32 {
    0
}

/// Mock `hew_trap_with_code` — panics, because well-formed recursive
/// functions must not trigger arithmetic traps.
extern "C" fn mock_trap(code: i32) {
    panic!("unexpected arithmetic trap in recursive function: code={code}");
}

/// JIT-execute the `main` function from `ll_path` with mock runtime stubs
/// registered for `hew_actor_cooperate` and `hew_trap_with_code`.
///
/// Returns the `i64` value returned by `main`.
fn jit_run_main(ll_path: &Path) -> i64 {
    Target::initialize_native(&InitializationConfig::default())
        .expect("native target initialisation must succeed");

    let ctx = Context::create();
    let buf = MemoryBuffer::create_from_file(ll_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", ll_path.display()));
    let module = ctx
        .create_module_from_ir(buf)
        .unwrap_or_else(|e| panic!("parse IR {}: {e}", ll_path.display()));

    let cooperate_fn = module.get_function("hew_actor_cooperate");
    let trap_fn = module.get_function("hew_trap_with_code");

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("create_jit_execution_engine must succeed");

    if let Some(f) = cooperate_fn {
        ee.add_global_mapping(&f, mock_cooperate as *const () as usize);
    }
    if let Some(f) = trap_fn {
        ee.add_global_mapping(&f, mock_trap as *const () as usize);
    }

    // SAFETY: `main` is `fn() -> i64` matching the HIR function signature.
    let jit_main = unsafe {
        ee.get_function::<unsafe extern "C" fn() -> i64>("main")
            .expect("main must be present in the JIT module")
    };

    unsafe { jit_main.call() }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// Recursive Fibonacci: the canonical regression for the if-as-trailing-expr
/// parser fix.  The key invariant is that `fn fib(n: i64) -> i64 { if ... }`
/// lowers the `if` as the function return value, not as a discarded statement.
#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn recursive_fib_returns_correct_value() {
    let src = r#"
        fn fib(n: i64) -> i64 {
            if n <= 1 {
                n
            } else {
                fib(n - 1) + fib(n - 2)
            }
        }
        fn main() -> i64 {
            fib(10)
        }
    "#;
    let ll = compile_to_ll(src, "fib_recursive");
    assert_eq!(jit_run_main(&ll), 55, "fib(10) must equal 55");
}

#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn recursive_fib_base_cases() {
    let src_0 = r#"
        fn fib(n: i64) -> i64 {
            if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
        }
        fn main() -> i64 { fib(0) }
    "#;
    let ll0 = compile_to_ll(src_0, "fib_base0");
    assert_eq!(jit_run_main(&ll0), 0, "fib(0) must equal 0");

    let src_1 = r#"
        fn fib(n: i64) -> i64 {
            if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
        }
        fn main() -> i64 { fib(1) }
    "#;
    let ll1 = compile_to_ll(src_1, "fib_base1");
    assert_eq!(jit_run_main(&ll1), 1, "fib(1) must equal 1");

    let src_5 = r#"
        fn fib(n: i64) -> i64 {
            if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
        }
        fn main() -> i64 { fib(5) }
    "#;
    let ll5 = compile_to_ll(src_5, "fib_base5");
    assert_eq!(jit_run_main(&ll5), 5, "fib(5) must equal 5");
}

/// Mutually-recursive is_even / is_odd: exercises cross-function call chains
/// and verifies that the parser fix applies to all recursive call patterns,
/// not just self-recursion.
#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn mutually_recursive_is_even_is_odd() {
    let src = r#"
        fn is_even(n: i64) -> i64 {
            if n == 0 { 1 } else { is_odd(n - 1) }
        }
        fn is_odd(n: i64) -> i64 {
            if n == 0 { 0 } else { is_even(n - 1) }
        }
        fn main() -> i64 {
            is_even(4)
        }
    "#;
    let ll = compile_to_ll(src, "mutual_recursion");
    assert_eq!(jit_run_main(&ll), 1, "is_even(4) must return 1 (true)");
}
