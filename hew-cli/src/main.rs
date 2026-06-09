//! `hew` — the Hew programming language compiler driver.
//!
//! ```text
//! hew compile file.hew [--emit-dir DIR] [--dump-mir raw|elab] [--target wasm32-unknown-unknown]
//!                                  # Run the v0.5 IR ladder and emit native or WASM
//! hew run file.hew [-- args...]    # Compile through v0.5 native codegen and execute
//! hew debug file.hew [-- args...]  # Compile through v0.5 native codegen and launch a debugger
//! hew check file.hew               # Parse + typecheck only
//! hew watch file_or_dir [options]  # Watch for changes and re-check
//! hew eval                         # Interactive eval through the v0.5 native path
//! hew eval "<expression>"          # Evaluate through the v0.5 native path
//! hew eval -f file.hew             # Evaluate a file through the v0.5 native path
//! hew wire check file.hew --against baseline.hew
//!                                  # Validate wire compatibility
//! hew fmt file.hew                 # Format source file in-place
//! hew fmt --stdin < file.hew       # Format source from stdin to stdout
//! hew fmt --check file.hew         # Check formatting (CI mode)
//! hew init [name]                  # Scaffold main.hew + README.md only (no hew.toml)
//! hew playground verify            # Verify runnable playground examples
//! hew completions <shell>          # Print shell completion script
//! hew version                      # Print version info
//! ```

mod args;
mod compile;
mod diagnostic;
mod doc;
mod eval;
mod explain_cow;
mod host_death;
mod jit;
mod link;
mod machine;
mod platform;
mod playground;
mod process;
mod router;
#[cfg(unix)]
mod signal;
mod target;
mod test_runner;
mod util;
mod wasi_runner;
mod watch;
mod wire;

use std::io::{Read, Write};
use std::path::Path;

use args::Cli;

fn main() {
    // Spawn the real entry point on a thread with a large stack so deeply
    // nested ASTs (e.g. thousands of chained binary operators) don't cause
    // a stack overflow in the parser, type checker, or serializer.
    const STACK_SIZE: usize = 64 * 1024 * 1024; // 64 MiB
    let builder = std::thread::Builder::new()
        .name("hew-main".into())
        .stack_size(STACK_SIZE);
    let handler = builder
        .spawn(hew_main)
        .expect("failed to spawn main thread");
    if let Err(e) = handler.join() {
        std::panic::resume_unwind(e);
    }
}

fn hew_main() {
    let cli = router::parse_cli_or_exit();
    let mut dispatcher = router::MainCommandDispatcher;
    router::dispatch_command(cli.command.as_ref(), &mut dispatcher);
}

// ---------------------------------------------------------------------------
// Sub-commands
// ---------------------------------------------------------------------------

/// Surface the diagnostic family in the CLI prefix so users see what
/// gate tripped at a glance. `MirCheck` findings (move/init/aliasing
/// legality) are a distinct family from spine-subset rejections; the
/// kind in the debug payload disambiguates further.
fn diagnostic_prefix(kind: &hew_mir::MirDiagnosticKind) -> &'static str {
    diagnostic::mir_diagnostic_prefix(kind)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompileEmitTarget {
    Native,
    Wasm,
}

fn resolve_compile_emit_target(requested: Option<&str>) -> CompileEmitTarget {
    match requested {
        None => CompileEmitTarget::Native,
        Some("wasm32-unknown-unknown") => CompileEmitTarget::Wasm,
        Some(other) => {
            eprintln!(
                "Error: unsupported --target `{other}` for `hew compile`; \
                 supported targets: wasm32-unknown-unknown"
            );
            std::process::exit(2);
        }
    }
}

fn lower_file_to_mir(
    input_path: &Path,
    requested_target: Option<&str>,
) -> Result<hew_mir::IrPipeline, ()> {
    let input = input_path.display().to_string();
    let target = target::TargetSpec::from_requested(requested_target).map_err(|e| {
        eprintln!("Error: {e}");
    })?;
    let fopts = compile::frontend_options(&target, &compile::CompileOptions::default());

    let state = hew_compile::run_file_frontend_to_typecheck(&input, &fopts).map_err(|failure| {
        compile::render_frontend_diagnostics(&failure.diagnostics);
        if failure.diagnostics.is_empty() {
            eprintln!("Error: {}", failure.message);
        }
    })?;

    compile::render_frontend_diagnostics(&state.diagnostics);

    let tco = state.typecheck_result.tco.ok_or_else(|| {
        eprintln!(
            "error: hew compile requires a type-checked program; \
             this path should be unreachable (no_typecheck = false)"
        );
    })?;

    let lower_output = hew_hir::lower_program(
        &state.program,
        &tco,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let mut hir_diagnostics = lower_output.diagnostics;
    // Defense-in-depth: the verifier may emit a second `NotYetImplemented`
    // for any `Unsupported` placeholder that lacked a prior lowerer
    // diagnostic. Dedup by (kind, span) so the user sees each problem
    // once, not twice.
    let verifier_diags = hew_hir::verify_hir(&lower_output.module);
    for diag in verifier_diags {
        let already_present = hir_diagnostics
            .iter()
            .any(|d| d.kind == diag.kind && d.span == diag.span);
        if !already_present {
            hir_diagnostics.push(diag);
        }
    }
    if !hir_diagnostics.is_empty() {
        let frontend_diagnostics = hew_compile::hir_diagnostics_to_frontend(
            &state.program,
            &state.source,
            &input,
            hir_diagnostics,
        );
        compile::render_frontend_diagnostics(&frontend_diagnostics);
        return Err(());
    }

    let mut pipeline = hew_mir::lower_hir_module(&lower_output.module);
    // Route checker-authored layout facts onto the pipeline.
    pipeline.attach_lowering_facts(&tco);
    if !pipeline.diagnostics.is_empty() {
        for diagnostic in &pipeline.diagnostics {
            eprintln!("{} {diagnostic:?}", diagnostic_prefix(&diagnostic.kind));
        }
        return Err(());
    }

    Ok(pipeline)
}

fn hir_target_arch(target: &target::TargetSpec) -> hew_hir::TargetArch {
    match target.arch() {
        target::TargetArch::Aarch64 => hew_hir::TargetArch::Aarch64,
        target::TargetArch::X86_64 => hew_hir::TargetArch::X86_64,
        target::TargetArch::Wasm32 => hew_hir::TargetArch::Wasm32,
    }
}

fn run_check_deep_gates(
    input: &str,
    target: &target::TargetSpec,
    state: &hew_compile::FileFrontendState,
) -> Result<(), ()> {
    let Some(tco) = state.typecheck_result.tco.as_ref() else {
        return Ok(());
    };

    let lower_output = hew_hir::lower_program(
        &state.program,
        tco,
        &hew_hir::ResolutionCtx,
        hir_target_arch(target),
    );
    let mut hir_diagnostics = lower_output.diagnostics;
    let verifier_diags = hew_hir::verify_hir(&lower_output.module);
    for diag in verifier_diags {
        let already_present = hir_diagnostics
            .iter()
            .any(|d| d.kind == diag.kind && d.span == diag.span);
        if !already_present {
            hir_diagnostics.push(diag);
        }
    }
    if !hir_diagnostics.is_empty() {
        let frontend_diagnostics = hew_compile::hir_diagnostics_to_frontend(
            &state.program,
            &state.source,
            input,
            hir_diagnostics,
        );
        compile::render_frontend_diagnostics(&frontend_diagnostics);
        return Err(());
    }

    let mut pipeline = hew_mir::lower_hir_module(&lower_output.module);
    // Clone checker-authored layout-fact lifecycle into the pipeline
    // (see the matching invocation in `check_command`).
    pipeline.attach_lowering_facts(tco);
    if !pipeline.diagnostics.is_empty() {
        let module_source_map = diagnostic::build_module_source_map(&state.program);
        let site_spans = hew_hir::collect_site_spans(&lower_output.module);
        diagnostic::render_mir_diagnostics(
            &state.source,
            input,
            &module_source_map,
            &site_spans,
            &pipeline.diagnostics,
        );
        return Err(());
    }

    if let Err(error) = hew_codegen_rs::validate_codegen_front(&pipeline) {
        diagnostic::render_codegen_front_diagnostic(&error);
        return Err(());
    }

    Ok(())
}

fn emit_module(
    pipeline: &hew_mir::IrPipeline,
    module_name: &str,
    emit_dir: &Path,
    emit_target: CompileEmitTarget,
) -> Result<hew_codegen_rs::EmitArtefacts, ()> {
    let options = hew_codegen_rs::EmitOptions {
        module_name,
        out_dir: emit_dir,
        native: emit_target == CompileEmitTarget::Native,
        wasm: emit_target == CompileEmitTarget::Wasm,
    };
    match hew_codegen_rs::emit_module(pipeline, &options) {
        Ok(artefacts) => Ok(artefacts),
        Err(hew_codegen_rs::CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            // WASM-TODO(#1451): hew_duplex_* symbols are excluded from wasm32
            // builds via `hew-runtime/src/duplex.rs:54`. Surface a structured
            // diagnostic so the user knows to omit the WASM target instead of
            // seeing a raw `wasm-ld: undefined symbol` linker error.
            eprintln!(
                "error: WASM target does not support the duplex concurrency \
                 substrate (symbol: {symbol}; WASM-TODO(#1451))\n\
                 hint: remove `--target wasm32-unknown-unknown` to emit a \
                 native binary instead"
            );
            Err(())
        }
        Err(e) => {
            eprintln!("E_NOT_YET_IMPLEMENTED: {e}");
            Err(())
        }
    }
}

fn link_native_object(obj: &Path, bin_path: &Path) -> Result<(), ()> {
    let target = target::TargetSpec::from_requested(None).map_err(|e| {
        eprintln!("Error: cannot determine host target: {e}");
    })?;
    let obj_str = obj.to_str().ok_or_else(|| {
        eprintln!("Error: object path is not valid UTF-8");
    })?;
    let bin_str = bin_path.to_str().ok_or_else(|| {
        eprintln!("Error: output path is not valid UTF-8");
    })?;
    crate::link::link_executable(obj_str, bin_str, &target, &[], false).map_err(|e| {
        eprintln!("{e}");
    })
}

pub(crate) fn compile_native_binary(input: &Path, bin_path: &Path) -> Result<(), ()> {
    let pipeline = lower_file_to_mir(input, None)?;
    let emit_dir = bin_path.parent().unwrap_or_else(|| Path::new("."));
    let module_name = bin_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");
    let artefacts = emit_module(&pipeline, module_name, emit_dir, CompileEmitTarget::Native)?;
    let obj = artefacts.native_obj_path.as_deref().ok_or_else(|| {
        eprintln!("E_NOT_YET_IMPLEMENTED: native codegen did not produce an object");
    })?;
    link_native_object(obj, bin_path)
}

pub(crate) fn compile_native_from_program(
    program: hew_parser::ast::Program,
    source: &str,
    source_label: &str,
    output_path: &Path,
    options: &compile::CompileOptions,
) -> Result<(), ()> {
    let target = target::TargetSpec::from_requested(options.target.as_deref()).map_err(|e| {
        eprintln!("Error: {e}");
    })?;
    let frontend_options = compile::frontend_options(&target, options);
    let state = hew_compile::run_program_frontend_to_typecheck(
        program,
        source,
        source_label,
        &frontend_options,
    )
    .map_err(|failure| {
        compile::render_frontend_diagnostics(&failure.diagnostics);
        if failure.diagnostics.is_empty() {
            eprintln!("Error: {}", failure.message);
        }
    })?;
    compile::render_frontend_diagnostics(&state.diagnostics);

    let tco = state.typecheck_result.tco.ok_or_else(|| {
        eprintln!(
            "error: eval compilation requires a type-checked program; \
             this path should be unreachable (no_typecheck = false)"
        );
    })?;

    let lower_output = hew_hir::lower_program(
        &state.program,
        &tco,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let mut hir_diagnostics = lower_output.diagnostics;
    let verifier_diags = hew_hir::verify_hir(&lower_output.module);
    for diag in verifier_diags {
        let already_present = hir_diagnostics
            .iter()
            .any(|d| d.kind == diag.kind && d.span == diag.span);
        if !already_present {
            hir_diagnostics.push(diag);
        }
    }
    if !hir_diagnostics.is_empty() {
        let frontend_diagnostics = hew_compile::hir_diagnostics_to_frontend(
            &state.program,
            &state.source,
            source_label,
            hir_diagnostics,
        );
        compile::render_frontend_diagnostics(&frontend_diagnostics);
        return Err(());
    }

    let mut pipeline = hew_mir::lower_hir_module(&lower_output.module);
    // Route checker-authored layout facts onto the pipeline.
    pipeline.attach_lowering_facts(&tco);
    if !pipeline.diagnostics.is_empty() {
        for diagnostic in &pipeline.diagnostics {
            eprintln!("{} {diagnostic:?}", diagnostic_prefix(&diagnostic.kind));
        }
        return Err(());
    }

    let emit_dir = output_path.parent().unwrap_or_else(|| Path::new("."));
    let module_name = output_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");
    let emit_target = if target.is_wasm() {
        CompileEmitTarget::Wasm
    } else {
        if !target.can_link_with_host_tools() {
            eprintln!("{}", target.unsupported_native_link_error());
            return Err(());
        }
        CompileEmitTarget::Native
    };
    let artefacts = emit_module(&pipeline, module_name, emit_dir, emit_target)?;

    match emit_target {
        CompileEmitTarget::Native => {
            let obj = artefacts.native_obj_path.as_deref().ok_or_else(|| {
                eprintln!("E_NOT_YET_IMPLEMENTED: native codegen did not produce an object");
            })?;
            link_native_object(obj, output_path)
        }
        CompileEmitTarget::Wasm => {
            if artefacts.wasm_path.is_some() {
                Ok(())
            } else {
                eprintln!("E_NOT_YET_IMPLEMENTED: WASM codegen did not produce a module");
                Err(())
            }
        }
    }
}

fn cmd_compile(a: &args::CompileArgs) {
    let pipeline = lower_file_to_mir(&a.input, a.target.as_deref()).unwrap_or_else(|()| {
        std::process::exit(1);
    });

    // Dump path: print the requested MIR stage and exit. Useful for
    // spot-checking the lowering during development.
    if let Some(stage) = a.dump_mir.as_deref() {
        match stage {
            "raw" => {
                for func in &pipeline.raw_mir {
                    println!("{func:#?}");
                }
            }
            "checked" => {
                // The Checked MIR dump includes the `MirCheck` findings
                // list. On a function that passes, `checks` is empty —
                // that emptiness is the load-bearing signal the CLI
                // rejection path keys off of.
                for func in &pipeline.checked_mir {
                    println!("{func:#?}");
                }
            }
            "elab" => {
                for func in &pipeline.elaborated_mir {
                    println!("{func:#?}");
                }
            }
            other => {
                eprintln!("Error: unknown --dump-mir stage `{other}`");
                std::process::exit(2);
            }
        }
        return;
    }

    // Default emit dir: `.tmp/compile-out` under the cwd. `.tmp/` is
    // gitignored across the workspace.
    let default_dir = std::path::PathBuf::from(".tmp/compile-out");
    let emit_dir = a.emit_dir.as_ref().unwrap_or(&default_dir);
    let module_name = a
        .input
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");

    let emit_target = resolve_compile_emit_target(a.target.as_deref());
    let artefacts =
        emit_module(&pipeline, module_name, emit_dir, emit_target).unwrap_or_else(|()| {
            std::process::exit(1);
        });

    // Link the native object into an executable using the shared
    // `link::link_executable` path. This resolves `libhew.a` (the
    // combined runtime + stdlib staticlib built by `make stdlib` / `cargo
    // build -p hew-lib`) via `find_hew_lib`, applies the per-platform
    // link plan (dead-strip, strip, system libs including `-lpthread
    // -ldl -lm -lrt` on Linux, CoreFoundation + Security on macOS), and
    // routes through lld when available, so compile binaries use the same
    // linker behaviour as release builds.
    if let Some(obj) = &artefacts.native_obj_path {
        let bin_path = emit_dir.join(module_name);
        link_native_object(obj, &bin_path).unwrap_or_else(|()| {
            std::process::exit(1);
        });
        println!("native: {}", bin_path.display());
    }
    if let Some(wasm) = &artefacts.wasm_path {
        println!("wasm:   {}", wasm.display());
    }
}

struct CompiledTempExecutable {
    path: std::path::PathBuf,
    _cleanup: TempExecutableCleanup,
}

enum TempExecutableCleanup {
    TempDir { _temp_dir: tempfile::TempDir },
}

impl CompiledTempExecutable {
    fn path(&self) -> &Path {
        &self.path
    }
}

fn compile_temp_debug_artifact(
    input: &str,
    options: &compile::CompileOptions,
    target: &target::ExecutionTarget,
) -> CompiledTempExecutable {
    compile_temp_artifact(input, create_debug_temp_artifact(target), options)
}

fn compile_temp_run_artifact(
    input: &str,
    options: &compile::CompileOptions,
    target: &target::ExecutionTarget,
) -> CompiledTempExecutable {
    compile_temp_artifact(input, create_run_temp_artifact(target), options)
}

fn compile_temp_artifact(
    input: &str,
    artifact: CompiledTempExecutable,
    _options: &compile::CompileOptions,
) -> CompiledTempExecutable {
    if let Ok(()) = compile_native_binary(Path::new(input), artifact.path()) {
        artifact
    } else {
        drop(artifact);
        // Exit 125 = compile failure (sentinel used by the playground to
        // distinguish compile errors from program exit codes).
        std::process::exit(125);
    }
}

fn create_debug_temp_artifact(target: &target::ExecutionTarget) -> CompiledTempExecutable {
    let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
        eprintln!("Error: cannot create temp dir: {e}");
        std::process::exit(1);
    });
    let path = tmp_dir
        .path()
        .join(format!("hew_debug_bin{}", target.executable_suffix()));

    CompiledTempExecutable {
        path,
        _cleanup: TempExecutableCleanup::TempDir { _temp_dir: tmp_dir },
    }
}

fn create_run_temp_artifact(target: &target::ExecutionTarget) -> CompiledTempExecutable {
    let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
        eprintln!("Error: cannot create temp dir: {e}");
        std::process::exit(1);
    });
    let path = tmp_dir
        .path()
        .join(format!("hew_run_bin{}", target.executable_suffix()));

    CompiledTempExecutable {
        path,
        _cleanup: TempExecutableCleanup::TempDir { _temp_dir: tmp_dir },
    }
}

fn cmd_run(a: &args::RunArgs) {
    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    let target = resolve_run_target(options.target.as_deref());
    let timeout = a.timeout.as_deref().map(|raw| {
        crate::util::parse_timeout(raw).unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            std::process::exit(1);
        })
    });
    let artifact = compile_temp_run_artifact(&input, &options, &target);

    let mut command = std::process::Command::new(artifact.path());
    command.args(&a.program_args);
    if a.profile && std::env::var_os("HEW_PPROF").is_none() {
        command.env("HEW_PPROF", default_profile_endpoint());
    }

    let mut child = match crate::process::BoundedChild::spawn(&mut command) {
        Ok(child) => child,
        Err(e) => {
            drop(artifact);
            eprintln!("Error: cannot launch {input}: {e}");
            std::process::exit(1);
        }
    };

    let status = match timeout {
        Some(timeout) => match child.wait_with_timeout(timeout) {
            Ok(crate::process::ChildWaitOutcome::Exited(status)) => status,
            Ok(crate::process::ChildWaitOutcome::Timeout) => {
                drop(artifact);
                eprintln!(
                    "Error: program timed out after {}",
                    crate::process::format_timeout(timeout)
                );
                std::process::exit(124);
            }
            Err(e) => {
                drop(artifact);
                eprintln!("Error: cannot wait for {input}: {e}");
                std::process::exit(1);
            }
        },
        None => match child.wait_unbounded() {
            Ok(status) => status,
            Err(e) => {
                drop(artifact);
                eprintln!("Error: cannot wait for {input}: {e}");
                std::process::exit(1);
            }
        },
    };

    drop(artifact);
    std::process::exit(status.code().unwrap_or(1));
}

fn default_profile_endpoint() -> &'static str {
    #[cfg(unix)]
    {
        "auto"
    }
    #[cfg(not(unix))]
    {
        ":6060"
    }
}

fn resolve_run_target(requested: Option<&str>) -> target::ExecutionTarget {
    let target = target::ExecutionTarget::from_requested(requested).unwrap_or_else(|e| {
        eprintln!("{e}");
        std::process::exit(1);
    });

    if !target.can_run_on_host() {
        eprintln!("{}", target.cross_target_run_error("run"));
        std::process::exit(1);
    }

    target
}

fn cmd_check(a: &args::CheckArgs) {
    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    let target =
        target::TargetSpec::from_requested(options.target.as_deref()).unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            std::process::exit(1);
        });
    let frontend_options = compile::frontend_options(&target, &options);

    let (result, state) = match hew_compile::check_file_with_state(&input, &frontend_options) {
        Ok(result) => result,
        Err(failure) => {
            compile::render_frontend_diagnostics(&failure.diagnostics);
            eprintln!("{}", failure.message);
            std::process::exit(1);
        }
    };

    compile::render_frontend_diagnostics(&result.diagnostics);
    if a.show_stack_hints {
        diagnostic::print_stack_hints(&result.source, &input, &result.stack_hints);
    } else if a.explain_cow {
        explain_cow::render_explain_cow(
            &result.actor_send_aliasing,
            &result.source,
            &input,
            &mut std::io::stdout(),
        );
    }

    if run_check_deep_gates(&input, &target, &state).is_err() {
        std::process::exit(1);
    }

    eprintln!("{input}: OK");
}

fn cmd_debug(a: &args::DebugArgs) {
    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    let target = resolve_debug_target(options.target.as_deref());
    let artifact = compile_temp_debug_artifact(&input, &options, &target);
    let (debugger, debugger_args) =
        match resolve_debugger_invocation(artifact.path(), &a.program_args) {
            Ok(invocation) => invocation,
            Err(message) => {
                drop(artifact);
                eprintln!("{message}");
                std::process::exit(1);
            }
        };

    eprintln!("Launching {debugger} with debug build of {input}...");
    exit_after_debugger_run(&debugger, &debugger_args, artifact);
}

fn resolve_debug_target(requested: Option<&str>) -> target::ExecutionTarget {
    let target = target::ExecutionTarget::from_requested(requested).unwrap_or_else(|e| {
        eprintln!("{e}");
        std::process::exit(1);
    });

    if !target.can_run_on_host() {
        eprintln!("{}", target.cross_target_run_error("debug"));
        std::process::exit(1);
    }

    target
}

fn resolve_debugger_invocation(
    program_path: &Path,
    program_args: &[String],
) -> Result<(String, Vec<String>), String> {
    let program = program_path.display().to_string();

    if which_exists("gdb") {
        // Load the Hew GDB helper script if it exists
        let gdb_script = find_debug_script("hew-gdb.py");
        let mut gdb_args = Vec::new();
        if let Some(script) = &gdb_script {
            gdb_args.push("-x".to_string());
            gdb_args.push(script.clone());
        }
        gdb_args.push("--args".to_string());
        gdb_args.push(program);
        gdb_args.extend(program_args.iter().cloned());
        Ok(("gdb".to_string(), gdb_args))
    } else if which_exists("lldb") {
        let lldb_script = find_debug_script("hew_lldb.py");
        let mut lldb_args = Vec::new();
        if let Some(script) = &lldb_script {
            lldb_args.push("-o".to_string());
            lldb_args.push(format!("command script import {script}"));
        }
        lldb_args.push("--".to_string());
        lldb_args.push(program);
        lldb_args.extend(program_args.iter().cloned());
        Ok(("lldb".to_string(), lldb_args))
    } else {
        Err("Error: no debugger found. Install gdb or lldb.".to_string())
    }
}

fn exit_after_debugger_run(
    debugger: &str,
    debugger_args: &[String],
    artifact: CompiledTempExecutable,
) -> ! {
    let status = std::process::Command::new(debugger)
        .args(debugger_args)
        .status();
    drop(artifact);

    match status {
        Ok(status) => std::process::exit(status.code().unwrap_or(1)),
        Err(e) => {
            eprintln!("Error: cannot launch {debugger}: {e}");
            std::process::exit(1);
        }
    }
}

fn which_exists(name: &str) -> bool {
    std::process::Command::new(name)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok_and(|s| s.success())
}

fn find_debug_script(name: &str) -> Option<String> {
    // Check next to the hew binary first, then the repo scripts/ dir
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            let candidate = dir.join(format!("../share/hew/{name}"));
            if candidate.exists() {
                return candidate
                    .canonicalize()
                    .ok()
                    .map(|p| p.display().to_string());
            }
            // Development layout
            let candidate = dir.join(format!("../../scripts/debug/{name}"));
            if candidate.exists() {
                return candidate
                    .canonicalize()
                    .ok()
                    .map(|p| p.display().to_string());
            }
        }
    }
    None
}

fn cmd_fmt(a: &args::FmtArgs) {
    if a.stdin {
        let mut source = String::new();
        if let Err(e) = std::io::stdin().read_to_string(&mut source) {
            eprintln!("Error: cannot read stdin: {e}");
            std::process::exit(1);
        }

        let formatted = format_for_display("<stdin>", &source).unwrap_or_else(|| {
            std::process::exit(1);
        });

        if a.check {
            if formatted != source {
                eprintln!("<stdin>: needs formatting");
                std::process::exit(1);
            }
            return;
        }

        if let Err(e) = std::io::stdout().write_all(formatted.as_bytes()) {
            eprintln!("Error: cannot write output: {e}");
            std::process::exit(1);
        }
        return;
    }

    if a.files.is_empty() {
        eprintln!("Usage: hew fmt [--check] (--stdin | <file.hew>...)");
        std::process::exit(1);
    }

    let mut had_errors = false;
    let mut needs_formatting = false;

    for file_path in &a.files {
        let file = file_path.display().to_string();
        let source = match std::fs::read_to_string(file_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: cannot read {file}: {e}");
                had_errors = true;
                continue;
            }
        };

        let Some(formatted) = format_for_display(&file, &source) else {
            had_errors = true;
            continue;
        };

        if a.check {
            if formatted != source {
                eprintln!("{file}: needs formatting");
                needs_formatting = true;
            }
        } else if formatted != source {
            if let Err(e) = std::fs::write(file_path, &formatted) {
                eprintln!("Error: cannot write {file}: {e}");
                had_errors = true;
            } else {
                eprintln!("Formatted {file}");
            }
        }
    }

    if had_errors || needs_formatting {
        std::process::exit(1);
    }
}

fn format_for_display(input_name: &str, source: &str) -> Option<String> {
    let result = hew_parser::parse(source);
    let is_fatal = result
        .errors
        .iter()
        .any(|e| matches!(e.severity, hew_parser::Severity::Error));
    if is_fatal {
        diagnostic::render_parse_diagnostics(source, input_name, &result.errors);
        return None;
    }

    Some(hew_parser::fmt::format_source(source, &result.program))
}

fn cmd_init(a: &args::InitArgs) {
    let (project_name, project_dir) = if let Some(ref name) = a.name {
        let dir = std::path::PathBuf::from(name);
        let pname = dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("hew-project")
            .to_string();
        if dir.exists() && !a.force {
            eprintln!(
                "Error: directory '{}' already exists (use --force to overwrite)",
                dir.display()
            );
            std::process::exit(1);
        }
        if let Err(e) = std::fs::create_dir_all(&dir) {
            eprintln!("Error: cannot create directory '{}': {e}", dir.display());
            std::process::exit(1);
        }
        (pname, dir)
    } else {
        // No name given — use current directory name as project name.
        let cwd = std::env::current_dir().unwrap_or_else(|e| {
            eprintln!("Error: cannot determine current directory: {e}");
            std::process::exit(1);
        });
        let pname = cwd
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("hew-project")
            .to_string();
        (pname, cwd)
    };

    let main_hew = project_dir.join("main.hew");
    let readme = project_dir.join("README.md");

    // Guard against overwriting existing files unless --force is given.
    if !a.force {
        for path in [&main_hew, &readme] {
            if path.exists() {
                eprintln!(
                    "Error: '{}' already exists (use --force to overwrite)",
                    path.display()
                );
                std::process::exit(1);
            }
        }
    }

    let main_content = "\
fn main() {
    println(\"Hello, world!\");
}
";

    let readme_content = format!(
        "\
# {project_name}

A source-only [Hew](https://hew.sh) scaffold.

`hew init` created `main.hew` and this README. It does not create `hew.toml`;
use `adze init` for the manifest-first bootstrap flow.

## Next steps

```sh
hew check main.hew
hew run main.hew
```
"
    );

    if let Err(e) = std::fs::write(&main_hew, main_content) {
        eprintln!("Error: cannot write {}: {e}", main_hew.display());
        std::process::exit(1);
    }
    if let Err(e) = std::fs::write(&readme, &readme_content) {
        eprintln!("Error: cannot write {}: {e}", readme.display());
        std::process::exit(1);
    }

    println!("Created source-only project \"{project_name}\" with main.hew and README.md");
    println!("No hew.toml was created; use `adze init` for manifest-first bootstrap.");
}

fn cmd_completions(a: &args::CompletionsArgs) {
    use clap::CommandFactory;
    use clap_complete::{generate, Shell};

    let mut cmd = Cli::command();
    let shell = match a.shell {
        args::ShellChoice::Bash => Shell::Bash,
        args::ShellChoice::Zsh => Shell::Zsh,
        args::ShellChoice::Fish => Shell::Fish,
        args::ShellChoice::PowerShell => Shell::PowerShell,
    };
    generate(shell, &mut cmd, "hew", &mut std::io::stdout());
}

fn cmd_version() {
    let version = env!("CARGO_PKG_VERSION");
    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    let git_hash = option_env!("HEW_GIT_HASH").unwrap_or("");
    let dirty = match option_env!("HEW_GIT_DIRTY") {
        Some("true") => "-dirty",
        Some("unknown") => "-unknown",
        _ => "",
    };
    if git_hash.is_empty() {
        println!("hew {version} ({profile})");
    } else {
        println!("hew {version} ({profile}, {git_hash}{dirty})");
    }
}

// ---------------------------------------------------------------------------
// Helper utilities
// ---------------------------------------------------------------------------
