use super::*;

#[path = "../../hew-sir/tests/support/select.rs"]
mod fixture;

#[test]
fn task_select_emits_borrowed_task_arrays() {
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
        let target = TargetData::create(&physical.target.data_layout);
        assert_eq!(params[1], ctx.ptr_sized_int_type(&target, None).into());
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
            hew_checked_task_select_free, hew_checked_task_select_new, hew_checked_task_select_poll,
        };
        for (symbol, address) in [
            (
                "hew_checked_task_select_new",
                hew_checked_task_select_new as *const () as usize,
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
