use std::path::PathBuf;

use hew_codegen_rs::CodegenError;
use hew_types::module_registry::stdlib_search_paths;

fn module_search_paths() -> Vec<PathBuf> {
    let paths = stdlib_search_paths();
    assert!(
        !paths.is_empty(),
        "fuzz harness found no std root; set HEW_STD to a std/ directory"
    );
    paths
}

/// Run admitted source through semantic, physical and LLVM lowering.
///
/// User-facing rejection at any stage is ordinary data. Panics, sanitizer
/// failures and LLVM verifier failures still terminate the fuzz iteration.
pub fn exercise(source: &str, strict_codegen: bool) {
    let source = source.to_owned();
    std::thread::Builder::new()
        .stack_size(hew_compile::COMPILER_STACK_SIZE)
        .spawn(move || exercise_on_compiler_stack(&source, strict_codegen))
        .expect("spawn compiler fuzz thread")
        .join()
        .unwrap_or_else(|panic| std::panic::resume_unwind(panic));
}

fn exercise_on_compiler_stack(source: &str, strict_codegen: bool) {
    let options = hew_compile::FrontendOptions {
        module_search_paths: Some(module_search_paths()),
        ..hew_compile::FrontendOptions::default()
    };
    let state = hew_compile::run_source_frontend(source, "hew-fuzz-input.hew", &options);
    if let Some(failure) = state.stopped {
        assert!(
            !strict_codegen,
            "generated program was not admitted: {failure}"
        );
        return;
    }
    let Some(checked) = state
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
    else {
        assert!(!strict_codegen, "generated program produced no type facts");
        return;
    };
    let lowering = hew_compile::Session::new(
        hew_compile::SessionTarget::native(),
        hew_compile::DiagnosticPolicy::default(),
    )
    .lower_program(&state.program, checked);
    let output = match lowering {
        Ok(output) => output,
        Err(error) => {
            assert!(
                !strict_codegen,
                "generated program failed semantic lowering: {error}"
            );
            return;
        }
    };
    let triple = hew_codegen_rs::native_emission_triple();
    let inventory = hew_mir::physical::physical_type_inventory(&output.semantics().module);
    let target = match hew_codegen_rs::physical::physical_target_for_inventory(&triple, &inventory)
    {
        Ok(target) => target,
        Err(error) => {
            assert!(
                !strict_codegen,
                "generated program has no physical target: {error}"
            );
            return;
        }
    };
    // Source rejection has already happened at the frontend, semantic
    // lowering and target-selection boundaries above. Once those admit a
    // program, physical lowering and its storage/CFG verifier must succeed.
    let physical = output
        .lower_physical(target)
        .unwrap_or_else(|error| panic!("physical lowering rejected admitted code: {error}"));
    let result = hew_codegen_rs::validate_physical_codegen(&physical, "hew_fuzz_codegen");
    match result {
        Ok(()) => {}
        Err(error @ CodegenError::LlvmVerify(_)) => {
            panic!("LLVM rejected admitted physical code: {error}");
        }
        Err(error) if strict_codegen => {
            panic!("generated admitted encoding program failed codegen: {error}");
        }
        Err(_) => {}
    }
}
