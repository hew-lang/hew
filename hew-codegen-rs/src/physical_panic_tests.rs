use super::*;

#[path = "../../hew-sir/tests/support/panic.rs"]
mod fixture;

fn module(owned: bool, triple: &str) -> PhysicalModule {
    let semantic = fixture::module(owned);
    let target = physical_target_for_inventory(
        triple,
        &hew_mir::physical::physical_type_inventory(&semantic),
    )
    .unwrap();
    hew_mir::lower_physical_module(&semantic, target)
        .unwrap()
        .into_unverified()
}

#[test]
fn panic_constructor_uses_the_same_pointer_abi_across_native_targets() {
    for triple in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let physical = module(true, triple);
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let ptr = ctx.ptr_type(AddressSpace::default());
        assert_eq!(
            llvm.get_function("hew_fault_new_panic").unwrap().get_type(),
            ptr.fn_type(&[ptr.into()], false)
        );
        llvm.verify().unwrap();
    }
}

#[test]
fn panic_propagates_one_fault_without_initializing_the_result_at_o0_o2() {
    type Probe = unsafe extern "C" fn(*mut c_void, *mut i64, *mut *mut c_void) -> i32;
    for owned in [false, true] {
        let physical = module(owned, &native_emission_triple());
        let name = emitted_symbol(
            &physical,
            physical
                .callables
                .iter()
                .find(|c| c.symbol == "panic_probe")
                .unwrap(),
        );
        for optimized in [false, true] {
            let ctx = Context::create();
            let llvm = llvm(&ctx, &physical);
            llvm.get_function(&name)
                .unwrap()
                .set_linkage(Linkage::External);
            let engine = engine(&llvm, optimized);
            for text in ["", "before\0é 🦀\nafter"] {
                let mut input = std::ptr::null_mut();
                // SAFETY: the literal contains valid UTF-8 over the complete byte length.
                unsafe {
                    hew_runtime::string::hew_string_literal_new(
                        text.as_ptr(),
                        u32::try_from(text.len()).unwrap(),
                        &raw mut input,
                    )
                };
                let mut result = 0x1234_5678;
                let mut fault = std::ptr::null_mut();
                // SAFETY: the verified body has a string pointer and private result/fault outputs.
                let status = unsafe {
                    engine.get_function::<Probe>(&name).unwrap().call(
                        input.cast(),
                        &raw mut result,
                        &raw mut fault,
                    )
                };
                assert_eq!(status, HEW_TRAP_USER_PANIC);
                assert_eq!(result, 0x1234_5678);
                assert!(!fault.is_null());
                // SAFETY: the caller retains a borrowed input; a consuming body released it.
                if !owned {
                    unsafe { hew_runtime::string::hew_string_drop(input) };
                }
                // SAFETY: failure transferred exactly one opaque fault owner to this caller.
                unsafe { hew_runtime::fault::hew_fault_drop(fault.cast()) };
            }
        }
    }
}
