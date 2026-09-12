use super::*;

#[path = "../../hew-sir/tests/support/select.rs"]
mod fixture;

/// A selection opens over its waker, registers one source per arm, and arms
/// the timer only when the source carries one.
#[test]
fn task_select_registers_one_source_per_arm() {
    for timed in [false, true] {
        let semantic = fixture::module(2, timed);
        let target = physical_target_for_inventory(
            &native_emission_triple(),
            &hew_mir::physical::physical_type_inventory(&semantic),
        )
        .unwrap();
        let physical = hew_mir::lower_physical_module(&semantic, target)
            .unwrap()
            .into_unverified();
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        llvm.verify().unwrap();
        let params = llvm
            .get_function("hew_checked_task_select_new")
            .unwrap()
            .get_type()
            .get_param_types();
        assert_eq!(params.len(), 1, "the selection opens over its waker alone");
        let mut registrations = 0;
        for block in llvm
            .get_functions()
            .filter(|function| function.get_first_basic_block().is_some())
            .flat_map(inkwell::values::FunctionValue::get_basic_blocks)
        {
            let mut instruction = block.get_first_instruction();
            while let Some(current) = instruction {
                if let Ok(call) = inkwell::values::CallSiteValue::try_from(current) {
                    if call.get_called_fn_value().is_some_and(|callee| {
                        callee.get_name().to_bytes() == b"hew_checked_task_select_add_task"
                    }) {
                        registrations += 1;
                    }
                }
                instruction = current.get_next_instruction();
            }
        }
        assert_eq!(
            registrations, 2,
            "the fixture registers one observation per task arm"
        );
        assert!(llvm
            .get_function("hew_checked_task_select_add_channel")
            .is_none());
        assert_eq!(
            llvm.get_function("hew_checked_task_select_arm_timer")
                .is_some(),
            timed,
            "the timer is armed exactly when the selection has one"
        );
    }
}

#[test]
fn task_select_native_timer_abi() {
    let semantic = fixture::module(0, true);
    assert!(hew_sir::verify_module(&semantic).is_empty());
    let target = physical_target_for_inventory(
        &native_emission_triple(),
        &hew_mir::physical::physical_type_inventory(&semantic),
    )
    .unwrap();
    let physical = hew_mir::lower_physical_module(&semantic, target)
        .unwrap()
        .into_unverified();
    let name = emitted_symbol(&physical, &physical.callables[0]);
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        llvm.get_function(&name)
            .unwrap()
            .set_linkage(Linkage::External);
        let engine = engine(&llvm, optimized);
        use hew_runtime::task_scope::checked::{
            hew_checked_task_select_arm_timer, hew_checked_task_select_fault,
            hew_checked_task_select_free, hew_checked_task_select_new,
            hew_checked_task_select_poll, hew_checked_task_select_set_context,
        };
        for (symbol, address) in [
            (
                "hew_checked_task_select_new",
                hew_checked_task_select_new as *const () as usize,
            ),
            (
                "hew_checked_task_select_set_context",
                hew_checked_task_select_set_context as *const () as usize,
            ),
            (
                "hew_checked_task_select_fault",
                hew_checked_task_select_fault as *const () as usize,
            ),
            (
                "hew_checked_task_select_arm_timer",
                hew_checked_task_select_arm_timer as *const () as usize,
            ),
            (
                "hew_checked_task_select_poll",
                hew_checked_task_select_poll as *const () as usize,
            ),
            (
                "hew_checked_task_select_free",
                hew_checked_task_select_free as *const () as usize,
            ),
            (
                "hew_coro_state_cancel_code",
                hew_runtime::coro_state::hew_coro_state_cancel_code as *const () as usize,
            ),
        ] {
            if let Some(function) = llvm.get_function(symbol) {
                engine.add_global_mapping(&function, address);
            }
        }
        type Probe = unsafe extern "C" fn(i64, *mut i64, *mut *mut c_void) -> i32;
        for duration in [0, 1_000_000] {
            let mut result = -1;
            let mut fault = std::ptr::null_mut();
            // SAFETY: the verified callable takes a duration and distinct result/fault slots.
            let status = unsafe {
                engine.get_function::<Probe>(&name).unwrap().call(
                    duration,
                    &raw mut result,
                    &raw mut fault,
                )
            };
            assert_eq!(status, 0, "optimized={optimized}, duration={duration}");
            assert!(fault.is_null());
            assert_eq!(result, 0);
        }
    }
}
