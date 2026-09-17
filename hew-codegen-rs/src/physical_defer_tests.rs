//! Execute verified inline defer CFG through the unchanged native callable ABI.

use super::*;

#[path = "../../hew-sir/tests/support/defer.rs"]
mod fixture;

#[test]
fn native_defer_abi_child() {
    let Ok(case) = std::env::var("HEW_DEFER_ABI_CASE") else {
        return;
    };
    let failing = case != "success";
    let nested = case.starts_with("nested-");
    let existing = case == "body" || nested;
    let optimized = std::env::var("HEW_DEFER_ABI_O2").unwrap() == "1";
    let mut semantic = if nested {
        fixture::nested(failing)
    } else {
        fixture::module(failing)
    };
    if case == "checked" {
        fixture::probe(&mut semantic).blocks[0].ops[2].kind =
            hew_sir::SemOpKind::ConstInteger(i128::from(i64::MAX));
    }
    let diagnostics = hew_sir::verify_module(&semantic);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let target = physical_target_for_inventory(
        &native_emission_triple(),
        &hew_mir::physical::physical_type_inventory(&semantic),
    )
    .unwrap();
    let physical = hew_mir::lower_physical_module(&semantic, target)
        .unwrap()
        .into_unverified();
    let name = emitted_symbol(
        &physical,
        physical
            .callables
            .iter()
            .find(|c| c.declaration.full_path() == "probe")
            .unwrap(),
    );
    let ctx = Context::create();
    let llvm = llvm(&ctx, &physical);
    llvm.get_function(&name)
        .unwrap()
        .set_linkage(Linkage::External);
    let engine = engine(&llvm, optimized);
    if let Some(print) = llvm.get_function("hew_print_value") {
        engine.add_global_mapping(
            &print,
            hew_runtime::print::hew_print_value as *const () as usize,
        );
    }
    let newer = match case.as_str() {
        "nested-empty" => "",
        "nested-nul" => "nested\0é 🦀",
        _ => "D2",
    };
    let strings = ["body", "D1", newer].map(|text| {
        let mut string = std::ptr::null_mut();
        // SAFETY: valid UTF-8 literals and a writable unique output slot.
        unsafe {
            hew_runtime::string::hew_string_literal_new(
                text.as_ptr(),
                u32::try_from(text.len()).unwrap(),
                &raw mut string,
            )
        };
        string
    });
    type Probe = unsafe extern "C" fn(
        u8,
        *mut c_void,
        *mut c_void,
        *mut c_void,
        *mut i64,
        *mut *mut c_void,
    ) -> i32;
    let mut result = 0x1234_5678;
    let mut fault = std::ptr::null_mut();
    // SAFETY: the verified signature has borrowed strings and distinct writable
    // result/fault slots. The JIT owns each fault until its ABI transfers it out.
    let status = unsafe {
        engine.get_function::<Probe>(&name).unwrap().call(
            u8::from(existing),
            strings[0].cast(),
            strings[1].cast(),
            strings[2].cast(),
            &raw mut result,
            &raw mut fault,
        )
    };
    for string in strings {
        // SAFETY: the caller retains each borrowed input owner.
        unsafe { hew_runtime::string::hew_string_drop(string) };
    }
    if failing {
        assert_eq!(
            status,
            if case == "checked" {
                HEW_TRAP_INTEGER_OVERFLOW
            } else {
                HEW_TRAP_USER_PANIC
            }
        );
        assert_eq!(result, 0x1234_5678, "fault must not initialize result-out");
        assert!(!fault.is_null());
        // SAFETY: the failing ABI transferred one live fault to this caller.
        unsafe {
            assert_eq!(hew_runtime::fault::hew_fault_report(fault.cast()), 0);
            hew_runtime::fault::hew_fault_drop(fault.cast());
        }
    } else {
        assert_eq!(status, 0);
        assert!(fault.is_null());
        assert_eq!(
            result, 4,
            "return expression was saved before deferred mutation"
        );
    }
}

#[test]
fn deferred_mutations_and_fault_order_execute_at_o0_o2() {
    let child = "physical::key::tests::defer::native_defer_abi_child";
    for optimized in [false, true] {
        for case in [
            "success",
            "normal",
            "body",
            "checked",
            "nested-empty",
            "nested-nul",
        ] {
            let output = std::process::Command::new(std::env::current_exe().unwrap())
                .args(["--exact", child, "--nocapture"])
                .env("HEW_DEFER_ABI_CASE", case)
                .env("HEW_DEFER_ABI_O2", if optimized { "1" } else { "0" })
                .output()
                .unwrap();
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                output.status.success(),
                "{case} O2={optimized}\n{stdout}\n{stderr}"
            );
            let observed = if case == "checked" {
                i64::MAX.to_string()
            } else {
                "5".into()
            };
            assert!(
                stdout.lines().any(|line| line == observed),
                "defer must observe committed mutation: {stdout}"
            );
            let code = HEW_TRAP_USER_PANIC;
            let expected = match case {
                "body" => format!("hew: failure: UserPanic ({code}): body\nhew: secondary failure: UserPanic ({code}): D2\nhew: secondary failure: UserPanic ({code}): D1\n"),
                "nested-empty" => format!("hew: failure: UserPanic ({code}): body\nhew: secondary failure: UserPanic ({code}): \nhew: secondary failure: UserPanic ({code}): D1\n"),
                "nested-nul" => format!("hew: failure: UserPanic ({code}): body\nhew: secondary failure: UserPanic ({code}): nested\0é 🦀\nhew: secondary failure: UserPanic ({code}): D1\n"),
                "checked" => format!("hew: failure: IntegerOverflow ({})\nhew: secondary failure: UserPanic ({code}): D1\n", HEW_TRAP_INTEGER_OVERFLOW),
                "normal" => format!("hew: failure: UserPanic ({code}): D2\nhew: secondary failure: UserPanic ({code}): D1\n"),
                _ => String::new(),
            };
            assert_eq!(stderr, expected, "{case} O2={optimized}");
        }
    }
}

#[test]
fn defer_fault_combine_has_one_pointer_abi_on_native_targets() {
    let semantic = fixture::nested(true);
    for triple in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let target = physical_target_for_inventory(
            triple,
            &hew_mir::physical::physical_type_inventory(&semantic),
        )
        .unwrap();
        let physical = hew_mir::lower_physical_module(&semantic, target)
            .unwrap()
            .into_unverified();
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let ptr = ctx.ptr_type(AddressSpace::default());
        assert_eq!(
            llvm.get_function("hew_fault_combine").unwrap().get_type(),
            ptr.fn_type(&[ptr.into(), ptr.into()], false)
        );
        llvm.verify().unwrap();
    }
}
