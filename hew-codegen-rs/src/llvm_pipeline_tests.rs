//! Post-split reachability keeps dynamic choices and explicitly retained roots.

use super::*;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use std::ffi::CString;

#[test]
fn cleanup_selectors_preserve_dynamic_and_retained_roots_at_o0_o2() {
    for level in [OptLevel::O0, OptLevel::O2] {
        for (condition, root, destroy_live, cleanup_live) in [
            ("true", "", true, false),
            ("false", "", false, true),
            ("%condition", "", true, true),
            (
                "true",
                "@exported = constant [2 x ptr] [ptr @task.destroy, ptr @task.cleanup]",
                true,
                true,
            ),
            (
                "true",
                "@llvm.used = appending global [1 x ptr] [ptr @task.cleanup], section \"llvm.metadata\"",
                true,
                true,
            ),
        ] {
            let context = Context::create();
            let ir = format!(
                r"
                {root}
                @unused = private constant [2 x ptr] [ptr @task.destroy, ptr @task.cleanup]
                define internal i64 @task.destroy() {{ ret i64 11 }}
                define internal i64 @task.cleanup() {{ ret i64 22 }}
                define ptr @choose(i1 %condition) {{
                    %selected = select i1 {condition}, ptr @task.destroy, ptr @task.cleanup
                    ret ptr %selected
                }}
                "
            );
            let ir = CString::new(ir).unwrap();
            let module = context
                .create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
                    ir.as_bytes_with_nul(),
                    "cleanup_selector",
                ))
                .unwrap();
            let machine = target_machine_for_triple_with_opt_level(&native_emission_triple(), level)
                .unwrap();
            module.set_triple(&machine.get_triple());
            module.set_data_layout(&machine.get_target_data().get_data_layout());
            run_module_pipeline(&module, &machine, level).unwrap();
            assert_eq!(
                module.get_function("task.destroy").is_some(),
                destroy_live,
                "{level:?}: {condition}, {root}"
            );
            assert_eq!(
                module.get_function("task.cleanup").is_some(),
                cleanup_live,
                "{level:?}: {condition}, {root}"
            );
            assert!(module.get_global("unused").is_none());
            let engine = module
                .create_jit_execution_engine(inkwell::OptimizationLevel::None)
                .unwrap();
            type Selected = unsafe extern "C" fn() -> i64;
            type Choose = unsafe extern "C" fn(bool) -> Selected;
            // SAFETY: the fixture declares choose(i1) -> ptr to one of the
            // two no-argument i64 functions kept alive by this engine.
            let choose = unsafe { engine.get_function::<Choose>("choose").unwrap() };
            for input in [false, true] {
                let expected = if condition == "true" || (condition == "%condition" && input) {
                    11
                } else {
                    22
                };
                // SAFETY: choose returns a live function with Selected's ABI;
                // its engine and module outlive both calls.
                let actual = unsafe { choose.call(input)() };
                assert_eq!(actual, expected, "{level:?}: {condition}, input={input}");
            }
        }
    }
}
