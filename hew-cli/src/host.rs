//! Explicit experimental native C export command.

use std::path::Path;

use hew_codegen_rs::physical::{emit_host_object, HostExport, PhysicalEmitOptions};
use hew_types::error::DiagChannel;

use crate::{
    compile, diagnostic, emit_semantic_error, hir_target_arch, lower_session_to_physical, target,
};

pub(super) fn emit(
    input: &Path,
    target: &target::TargetSpec,
    options: &compile::CompileOptions,
    selection: &str,
    output: &Path,
    opt_level: hew_codegen_rs::OptLevel,
    emit_llvm: bool,
) -> Result<(), DiagChannel> {
    let fail = |message: &str| emit_semantic_error("E_HOST_EXPORT", message, DiagChannel::User);
    if target.is_wasm() {
        return Err(fail("C exports require a native target"));
    }
    let (source_name, symbol) = selection
        .split_once('=')
        .filter(|(source, symbol)| !source.is_empty() && !symbol.is_empty())
        .ok_or_else(|| fail("--export-c expects SOURCE=SYMBOL"))?;
    let input_name = input.display().to_string();
    let frontend = compile::frontend_options(target, options);
    let state =
        hew_compile::run_file_frontend_to_typecheck(&input_name, &frontend).map_err(|failure| {
            compile::render_frontend_diagnostics(&failure.diagnostics)
                .unwrap_or_else(|| fail(&failure.message))
        })?;
    compile::render_frontend_diagnostics(&state.diagnostics);
    let checked = state
        .typecheck_result
        .tco
        .as_ref()
        .ok_or_else(|| fail("C export requires a type-checked program"))?;
    let session = hew_compile::Session::from_frontend_options(
        hew_compile::SessionTarget {
            hir_arch: hir_target_arch(target),
            ..hew_compile::SessionTarget::native()
        },
        &frontend,
    );
    let semantic = session
        .lower_host_program(&state.program, checked, source_name)
        .map_err(|error| fail(&error.to_string()))?;
    let [selected] = semantic.compiled_roots() else {
        return Err(fail("C export must resolve exactly one compilation root"));
    };
    let physical = lower_session_to_physical(&semantic, target)?;
    let export =
        HostExport::new(&physical, *selected, symbol).map_err(|error| fail(&error.to_string()))?;
    let out_dir = output
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let stem = output
        .file_stem()
        .and_then(|stem| stem.to_str())
        .ok_or_else(|| fail("C export output requires a UTF-8 file stem"))?;
    let triple = target.linker_triple();
    let artefacts = emit_host_object(
        &export,
        &PhysicalEmitOptions {
            module_name: stem,
            out_dir,
            target_triple: Some(&triple),
            opt_level,
            emit_llvm,
            address_sanitizer: crate::link::address_sanitizer_requested(),
        },
    )
    .map_err(|error| {
        diagnostic::render_codegen_emit_error(&error, None);
        diagnostic::codegen_channel(&error)
    })?;
    let produced = artefacts
        .native_obj_path
        .ok_or_else(|| fail("C export produced no object"))?;
    if produced != output {
        std::fs::rename(&produced, output).map_err(|error| fail(&error.to_string()))?;
    }
    std::fs::write(output.with_extension("h"), export.header())
        .map_err(|error| fail(&error.to_string()))?;
    Ok(())
}
