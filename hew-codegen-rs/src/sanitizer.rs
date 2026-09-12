//! LLVM AddressSanitizer instrumentation for generated native modules.

use inkwell::{
    attributes::{Attribute, AttributeLoc},
    module::Module,
    passes::PassBuilderOptions,
    targets::TargetMachine,
};

/// Mark each generated function for AddressSanitizer and run LLVM's ASan module
/// pass. The caller invokes this after its normal optimization pipeline, then
/// verifies the resulting module before emission.
///
/// Runtime declarations deliberately remain unmarked: the generated module
/// owns only its defined bodies, while the linked ASan runtime provides the
/// reporting hooks the pass introduces.
pub(crate) fn instrument_address_sanitizer(
    module: &Module<'_>,
    machine: &TargetMachine,
) -> Result<(), String> {
    let module_target = module.get_triple();
    let module_triple = module_target
        .as_str()
        .to_str()
        .map_err(|error| format!("module target triple is not UTF-8: {error}"))?;
    let machine_target = machine.get_triple();
    let machine_triple = machine_target
        .as_str()
        .to_str()
        .map_err(|error| format!("target machine triple is not UTF-8: {error}"))?;
    if module_triple.is_empty() {
        return Err(
            "AddressSanitizer requires a target triple on the generated module".to_string(),
        );
    }
    if module_triple != machine_triple {
        return Err(format!(
            "AddressSanitizer module target `{module_triple}` does not match target machine `{machine_triple}`"
        ));
    }
    let attribute_kind = Attribute::get_named_enum_kind_id("sanitize_address");
    if attribute_kind == 0 {
        return Err("LLVM does not expose the `sanitize_address` function attribute".to_string());
    }
    let attribute = module
        .get_context()
        .create_enum_attribute(attribute_kind, 0);

    let mut function = module.get_first_function();
    while let Some(current) = function {
        function = current.get_next_function();
        if current.get_first_basic_block().is_some() {
            current.add_attribute(AttributeLoc::Function, attribute);
        }
    }

    module
        .run_passes("asan", machine, PassBuilderOptions::create())
        .map_err(|error| format!("LLVM AddressSanitizer pass failed: {error}"))?;
    module
        .verify()
        .map_err(|error| format!("LLVM rejected the AddressSanitizer-instrumented module: {error}"))
}

#[cfg(test)]
mod tests {
    use super::instrument_address_sanitizer;
    use inkwell::{
        attributes::{Attribute, AttributeLoc},
        context::Context,
        module::Module,
        targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
        AddressSpace, OptimizationLevel,
    };
    use std::{path::PathBuf, process::Command, sync::OnceLock};

    fn initialize_targets() {
        static INIT: OnceLock<()> = OnceLock::new();
        INIT.get_or_init(|| Target::initialize_all(&InitializationConfig::default()));
    }

    fn host_machine() -> TargetMachine {
        initialize_targets();
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).expect("host target");
        target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .expect("host target machine")
    }

    fn allocation_access_module<'ctx>(
        ctx: &'ctx Context,
        out_of_bounds: bool,
    ) -> (Module<'ctx>, TargetMachine) {
        let module = ctx.create_module("asan_sentinel");
        let machine = host_machine();
        module.set_triple(&machine.get_triple());
        module.set_data_layout(&machine.get_target_data().get_data_layout());

        let ptr = ctx.ptr_type(AddressSpace::default());
        let i8_ty = ctx.i8_type();
        let i64_ty = ctx.i64_type();
        let malloc = module.add_function("malloc", ptr.fn_type(&[i64_ty.into()], false), None);
        let free = module.add_function("free", ctx.void_type().fn_type(&[ptr.into()], false), None);
        // Keep this fixture's allocator opaque to the O2 pipeline. A generated
        // runtime allocation may be externally implemented, and retaining the
        // access lets the test prove ASan rather than optimizer dead-code
        // removal. The linked libc symbols still provide the implementation.
        let no_builtin = Attribute::get_named_enum_kind_id("nobuiltin");
        assert_ne!(no_builtin, 0, "LLVM must expose the `nobuiltin` attribute");
        let no_builtin = ctx.create_enum_attribute(no_builtin, 0);
        malloc.add_attribute(AttributeLoc::Function, no_builtin);
        free.add_attribute(AttributeLoc::Function, no_builtin);
        let main = module.add_function("main", ctx.i32_type().fn_type(&[], false), None);
        let entry = ctx.append_basic_block(main, "entry");
        let builder = ctx.create_builder();
        builder.position_at_end(entry);
        let allocation = builder
            .build_call(malloc, &[i64_ty.const_int(1, false).into()], "allocation")
            .expect("malloc call")
            .try_as_basic_value()
            .expect_basic("malloc pointer")
            .into_pointer_value();
        let offset = u64::from(out_of_bounds);
        let access = unsafe {
            builder
                .build_in_bounds_gep(
                    i8_ty,
                    allocation,
                    &[i64_ty.const_int(offset, false)],
                    "access",
                )
                .expect("out-of-bounds address")
        };
        builder
            .build_store(allocation, i8_ty.const_zero())
            .expect("initial store");
        let byte = builder
            .build_load(i8_ty, allocation, "loaded_byte")
            .expect("load from allocation");
        builder
            .build_store(access, byte)
            .expect("out-of-bounds store");
        builder
            .build_call(free, &[allocation.into()], "free")
            .expect("free call");
        builder
            .build_return(Some(&ctx.i32_type().const_zero()))
            .expect("return");
        (module, machine)
    }

    #[test]
    fn defined_memory_accesses_receive_address_sanitizer_checks() {
        let ctx = Context::create();
        let (module, machine) = allocation_access_module(&ctx, true);

        instrument_address_sanitizer(&module, &machine).expect("ASan instrumentation");
        module.verify().expect("instrumented module verifies");
        let ir = module.print_to_string().to_string();

        assert!(
            ir.contains("sanitize_address"),
            "generated definition must carry the LLVM ASan attribute:\n{ir}"
        );
        assert!(
            ir.contains("__asan_report_load1"),
            "generated load must receive a concrete ASan failure check:\n{ir}"
        );
        assert!(
            ir.contains("__asan_report_store1"),
            "generated store must receive a concrete ASan failure check:\n{ir}"
        );
        assert!(
            ir.contains("@asan.module_ctor"),
            "the ASan module pass must install its runtime initializer:\n{ir}"
        );
        assert!(
            module
                .get_function("main")
                .expect("generated main")
                .get_enum_attribute(
                    AttributeLoc::Function,
                    Attribute::get_named_enum_kind_id("sanitize_address")
                )
                .is_some(),
            "generated main must retain the ASan attribute after instrumentation"
        );
        assert!(
            module
                .get_function("malloc")
                .expect("runtime allocation declaration")
                .get_enum_attribute(
                    AttributeLoc::Function,
                    Attribute::get_named_enum_kind_id("sanitize_address")
                )
                .is_none(),
            "runtime declarations are inputs, not generated bodies to instrument"
        );
    }

    #[test]
    fn module_without_target_triple_is_refused_before_instrumentation() {
        let ctx = Context::create();
        let module = ctx.create_module("missing_target");
        let definition =
            module.add_function("generated", ctx.void_type().fn_type(&[], false), None);
        let block = ctx.append_basic_block(definition, "entry");
        let builder = ctx.create_builder();
        builder.position_at_end(block);
        builder.build_return(None).expect("return");

        let machine = host_machine();
        let error = instrument_address_sanitizer(&module, &machine)
            .expect_err("ASan must not guess a target for an unconfigured module");
        assert!(
            error.contains("target triple"),
            "missing target must be a fatal configuration error: {error}"
        );
        assert!(
            definition
                .get_enum_attribute(
                    AttributeLoc::Function,
                    Attribute::get_named_enum_kind_id("sanitize_address")
                )
                .is_none(),
            "a rejected module must not be partially marked for ASan"
        );
    }

    #[test]
    fn mismatched_target_machine_is_refused_before_instrumentation() {
        let ctx = Context::create();
        let module = ctx.create_module("mismatched_target");
        module.set_triple(&inkwell::targets::TargetTriple::create(
            "wasm32-unknown-unknown",
        ));
        let definition =
            module.add_function("generated", ctx.void_type().fn_type(&[], false), None);
        let block = ctx.append_basic_block(definition, "entry");
        let builder = ctx.create_builder();
        builder.position_at_end(block);
        builder.build_return(None).expect("return");

        let error = instrument_address_sanitizer(&module, &host_machine())
            .expect_err("ASan must not pair a module with a different target machine");
        assert!(
            error.contains("does not match"),
            "mismatched target must be a fatal configuration error: {error}"
        );
        assert!(
            definition
                .get_enum_attribute(
                    AttributeLoc::Function,
                    Attribute::get_named_enum_kind_id("sanitize_address")
                )
                .is_none(),
            "a mismatched module must not be partially marked for ASan"
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn instrumented_out_of_bounds_store_reports_address_sanitizer_at_o0_and_o2() {
        for (name, out_of_bounds, pipeline) in [
            ("invalid-o0", true, None),
            ("invalid-o2", true, Some("default<O2>")),
            ("clean-o0", false, None),
            ("clean-o2", false, Some("default<O2>")),
        ] {
            let ctx = Context::create();
            let (module, machine) = allocation_access_module(&ctx, out_of_bounds);
            if let Some(pipeline) = pipeline {
                module
                    .run_passes(
                        pipeline,
                        &machine,
                        inkwell::passes::PassBuilderOptions::create(),
                    )
                    .unwrap_or_else(|err| panic!("{name} optimization pipeline failed: {err}"));
            }
            instrument_address_sanitizer(&module, &machine)
                .unwrap_or_else(|err| panic!("{name} ASan instrumentation failed: {err}"));
            module
                .verify()
                .unwrap_or_else(|err| panic!("{name} instrumented module failed verify: {err}"));

            let dir = tempfile::tempdir().expect("temporary ASan artefact directory");
            let object = dir.path().join(format!("sentinel-{name}.o"));
            machine
                .write_to_file(&module, FileType::Object, &object)
                .unwrap_or_else(|err| panic!("write {name} object: {err}"));
            let executable = dir.path().join(format!("sentinel-{name}"));
            let link = Command::new(llvm_clang())
                .args(["-fsanitize=address", "-fno-omit-frame-pointer"])
                .arg(&object)
                .arg("-o")
                .arg(&executable)
                .output()
                .expect("run LLVM 22 clang to link ASan sentinel");
            assert!(
                link.status.success(),
                "link {name} ASan sentinel failed:\n{}",
                String::from_utf8_lossy(&link.stderr)
            );

            let run = Command::new(&executable)
                .output()
                .unwrap_or_else(|err| panic!("run {name} ASan sentinel: {err}"));
            let report = String::from_utf8_lossy(&run.stderr);
            if out_of_bounds {
                assert!(
                    !run.status.success()
                        && report.contains("ERROR: AddressSanitizer: heap-buffer-overflow")
                        && report.contains("WRITE of size 1"),
                    "{name} sentinel must require the expected ASan heap write diagnostic; status={:?}, stderr:\n{report}",
                    run.status.code()
                );
            } else {
                assert!(
                    run.status.success() && !report.contains("AddressSanitizer"),
                    "{name} in-bounds control must exit cleanly without an ASan report; status={:?}, stderr:\n{report}",
                    run.status.code()
                );
            }
        }
    }

    #[cfg(target_os = "linux")]
    fn llvm_clang() -> PathBuf {
        let path = PathBuf::from(env!("HEW_LLVM_BINDIR")).join("clang");
        assert!(
            path.is_file(),
            "LLVM 22 clang resolved by the build's llvm-config is required for the ASan execution proof: {}",
            path.display()
        );
        path
    }
}
