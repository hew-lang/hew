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
//! hew observe [args...]             # Launch the TUI actor observer (delegates to hew-observe)
//! hew lsp [args...]                 # Start the language server (delegates to hew-lsp)
//! ```

mod args;
mod compile;
mod diagnostic;
mod diagnostic_json;
mod doc;
mod eval;
mod explain_cow;
mod host_death;
mod jit;
mod link;
mod machine;
mod native_link;
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

/// Render the MIR pipeline diagnostics through the source-attributed renderer.
///
/// Shared by every native-lowering entry point so a MIR gate failure is
/// reported identically (and never as a raw `MirDiagnostic { .. }` Debug
/// payload) regardless of the command. Returns `true` when diagnostics were
/// emitted and the caller should fail.
fn render_pipeline_mir_diagnostics(
    program: &hew_parser::ast::Program,
    source: &str,
    label: &str,
    module: &hew_hir::HirModule,
    diagnostics: &[hew_mir::MirDiagnostic],
) -> bool {
    if diagnostics.is_empty() {
        return false;
    }
    let module_source_map = diagnostic::build_module_source_map(program);
    let site_spans = hew_hir::collect_site_spans(module);
    diagnostic::render_mir_diagnostics(source, label, &module_source_map, &site_spans, diagnostics);
    true
}

/// Surface the MIR-stage lint warnings recorded on `pipeline`, applying the
/// user's lint levels and in-source `// hew:allow(...)` directives. Returns
/// `true` if any lint was promoted to an error (`--deny`), which the caller must
/// turn into a build failure.
///
/// MIR lints are level-configurable warnings, so they render through the
/// warning / error diagnostic path — never the hard `E_MIR_CHECK` family that
/// [`render_pipeline_mir_diagnostics`] uses for move/init check failures.
///
/// Surfaced through the CLI front end only: the LSP and wasm front ends stop at
/// HIR and never lower to MIR, so they never reach this path. Editor / web
/// surfacing of MIR lints is tracked in issue #2176.
fn render_pipeline_mir_lints(
    source: &str,
    label: &str,
    pipeline: &hew_mir::IrPipeline,
    levels: &hew_types::LintLevels,
) -> bool {
    let mut had_error = false;
    for warning in &pipeline.lint_warnings {
        let span_start = warning.span.0 as usize;
        let span_end = warning.span.1 as usize;
        // The MIR lint span is a raw byte offset carrying no module identity
        // (post-flatten HIR spans are module-anonymous). For the single-root
        // case it indexes the root source directly; a span past the end is from
        // an imported module we cannot faithfully render here, so skip it rather
        // than point at the wrong line. Precise multi-module MIR-lint surfacing
        // rides the same editor/web track as the front ends (issue #2176).
        if span_end > source.len() {
            continue;
        }
        // An in-source `// hew:allow(lint)` wins over any flag level — the same
        // precedence the HIR-stage lints honour.
        if hew_types::directive_suppresses(source, span_start, warning.lint) {
            continue;
        }
        let range = span_start..span_end;
        match levels.level(warning.lint) {
            hew_types::LintLevel::Allow => {}
            hew_types::LintLevel::Warn => {
                if diagnostic_json::json_output_active() {
                    diagnostic_json::push_json_diagnostic(diagnostic_json::from_mir_lint(
                        source,
                        label,
                        &range,
                        warning.lint.as_str(),
                        &warning.message,
                        false,
                    ));
                } else {
                    diagnostic::render_warning_with_raw_notes(
                        source,
                        label,
                        &range,
                        &warning.message,
                        &[],
                        &[],
                    );
                }
            }
            hew_types::LintLevel::Deny => {
                if diagnostic_json::json_output_active() {
                    diagnostic_json::push_json_diagnostic(diagnostic_json::from_mir_lint(
                        source,
                        label,
                        &range,
                        warning.lint.as_str(),
                        &warning.message,
                        true,
                    ));
                } else {
                    diagnostic::render_diagnostic_with_raw_notes(
                        source,
                        label,
                        &range,
                        &warning.message,
                        &[],
                        &[],
                    );
                }
                had_error = true;
            }
        }
    }
    had_error
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
        hir_target_arch(&target),
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

    let mut pipeline = hew_mir::lower_hir_module_with_facts(
        &lower_output.module,
        &tco.actor_send_aliasing,
        mir_pointer_width(&target),
    );
    // Route checker-authored layout facts onto the pipeline.
    pipeline.attach_lowering_facts(&tco);
    if render_pipeline_mir_diagnostics(
        &state.program,
        &state.source,
        &input,
        &lower_output.module,
        &pipeline.diagnostics,
    ) {
        return Err(());
    }

    // `hew compile` exposes no `-A/-W/-D` flags, so MIR lints surface at their
    // default levels (`// hew:allow(...)` still suppresses).
    if render_pipeline_mir_lints(
        &state.source,
        &input,
        &pipeline,
        &hew_types::LintLevels::default(),
    ) {
        return Err(());
    }

    Ok(pipeline)
}

/// Lower a source file to MIR for an explicit, already-resolved target.
///
/// Unlike `lower_file_to_mir` (which hardcodes `TargetArch::host()`), this
/// lowers HIR with the requested target's architecture via `hir_target_arch`,
/// so cross-target builds tag layout facts with the correct arch.
fn lower_file_to_mir_for_target(
    input_path: &Path,
    target: &target::TargetSpec,
    options: &compile::CompileOptions,
) -> Result<(hew_mir::IrPipeline, Vec<std::path::PathBuf>), ()> {
    let input = input_path.display().to_string();
    let fopts = compile::frontend_options(target, options);

    let state = hew_compile::run_file_frontend_to_typecheck(&input, &fopts).map_err(|failure| {
        compile::render_frontend_diagnostics(&failure.diagnostics);
        if failure.diagnostics.is_empty() {
            eprintln!("Error: {}", failure.message);
        }
    })?;

    compile::render_frontend_diagnostics(&state.diagnostics);

    let tco = state.typecheck_result.tco.ok_or_else(|| {
        eprintln!(
            "error: hew build requires a type-checked program; \
             this path should be unreachable (no_typecheck = false)"
        );
    })?;

    let lower_output = hew_hir::lower_program(
        &state.program,
        &tco,
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
            &input,
            hir_diagnostics,
        );
        compile::render_frontend_diagnostics(&frontend_diagnostics);
        return Err(());
    }

    let mut pipeline = hew_mir::lower_hir_module_with_facts(
        &lower_output.module,
        &tco.actor_send_aliasing,
        mir_pointer_width(target),
    );
    pipeline.attach_lowering_facts(&tco);
    if render_pipeline_mir_diagnostics(
        &state.program,
        &state.source,
        &input,
        &lower_output.module,
        &pipeline.diagnostics,
    ) {
        return Err(());
    }

    if render_pipeline_mir_lints(&state.source, &input, &pipeline, &options.lint_levels) {
        return Err(());
    }

    let native_pkg_dirs = native_link::collect_import_pkg_dirs(&state.program);
    Ok((pipeline, native_pkg_dirs))
}

fn hir_target_arch(target: &target::TargetSpec) -> hew_hir::TargetArch {
    match target.arch() {
        target::TargetArch::Aarch64 => hew_hir::TargetArch::Aarch64,
        target::TargetArch::X86_64 => hew_hir::TargetArch::X86_64,
        target::TargetArch::Wasm32 => hew_hir::TargetArch::Wasm32,
    }
}

/// Map the compile target to the MIR pointer width so the `isize`/`usize`
/// div/rem and shift trap guards emit the correct per-target constant.
///
/// Derived from the requested `--target` (`Wasm32` is the only 32-bit target;
/// `Aarch64`/`X86_64` are 64-bit), NOT from a host `cfg!` — a cross-compile to
/// wasm32 on a 64-bit host must emit width-32 guards, not the host's width.
fn mir_pointer_width(target: &target::TargetSpec) -> hew_mir::PointerWidth {
    match target.arch() {
        target::TargetArch::Wasm32 => hew_mir::PointerWidth::Bits32,
        target::TargetArch::Aarch64 | target::TargetArch::X86_64 => hew_mir::PointerWidth::Bits64,
    }
}

/// `true` when `input` is the compiler's own `std/builtins.hew` substrate
/// inside a Hew source checkout. Recognised by canonical path shape plus an
/// enclosing checkout root (so an unrelated external `std/builtins.hew` is not
/// matched). The substrate is pre-registered via `include_str!` and never
/// compiled standalone, so `hew check` skips its HIR/MIR deep gates.
fn is_embedded_stdlib_builtins(input: &str) -> bool {
    let Ok(path) = std::path::Path::new(input).canonicalize() else {
        return false;
    };
    if !path.ends_with("std/builtins.hew") {
        return false;
    }
    hew_types::module_registry::find_enclosing_hew_root(&path).is_some()
}

fn run_check_deep_gates(
    input: &str,
    target: &target::TargetSpec,
    state: &hew_compile::FileFrontendState,
    levels: &hew_types::LintLevels,
) -> Result<(), ()> {
    // `std/builtins.hew` is the compiler's embedded builtin-surface substrate:
    // it is consumed by the checker's builtins pre-registration (`include_str!`)
    // and is never compiled as a standalone program. It carries inherent impls
    // on builtin pid types (`LocalPid`/`RemotePid`) and `Display` blanket impls
    // that route through the compiler-magic `to_string` — constructs the
    // user-facing HIR/MIR deep gates correctly reject in user position. The
    // type-check above already validated its declarations; the substrate has no
    // standalone lowering to gate, so stop here.
    if is_embedded_stdlib_builtins(input) {
        return Ok(());
    }
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

    let mut pipeline = hew_mir::lower_hir_module_with_facts(
        &lower_output.module,
        &tco.actor_send_aliasing,
        mir_pointer_width(target),
    );
    // Clone checker-authored layout-fact lifecycle into the pipeline
    // (see the matching invocation in `check_command`).
    pipeline.attach_lowering_facts(tco);
    if render_pipeline_mir_diagnostics(
        &state.program,
        &state.source,
        input,
        &lower_output.module,
        &pipeline.diagnostics,
    ) {
        return Err(());
    }

    // Surface MIR lint warnings before the codegen-front gate so they read in
    // source order ahead of any hard error. A `--deny` lint fails the build, but
    // only after the codegen-front gate has had its say.
    let lint_denied = render_pipeline_mir_lints(&state.source, input, &pipeline, levels);

    if let Err(error) = hew_codegen_rs::validate_codegen_front(&pipeline) {
        diagnostic::render_codegen_front_diagnostic(&error, &state.source, input);
        return Err(());
    }

    if lint_denied {
        return Err(());
    }

    Ok(())
}

fn emit_module(
    pipeline: &hew_mir::IrPipeline,
    module_name: &str,
    emit_dir: &Path,
    emit_target: CompileEmitTarget,
    link_freestanding_wasm: bool,
    opt_level: hew_codegen_rs::OptLevel,
    source_path: Option<&Path>,
) -> Result<hew_codegen_rs::EmitArtefacts, ()> {
    emit_module_with_triple(
        pipeline,
        module_name,
        emit_dir,
        emit_target,
        None,
        link_freestanding_wasm,
        false,
        opt_level,
        source_path,
    )
}

/// Codegen-emit `pipeline`, optionally for an explicit native target triple.
///
/// `target_triple = None` preserves the host-only behaviour (the codegen
/// `native_emission_triple()` default). `Some(triple)` emits the native object
/// for an explicit, clang-compatible triple — on Darwin the deployment-target
/// form — enabling cross-arch object/binary emission.
#[allow(
    clippy::too_many_arguments,
    reason = "all args are direct fields of EmitOptions; no grouping improves clarity"
)]
fn emit_module_with_triple(
    pipeline: &hew_mir::IrPipeline,
    module_name: &str,
    emit_dir: &Path,
    emit_target: CompileEmitTarget,
    target_triple: Option<&str>,
    link_freestanding_wasm: bool,
    debug: bool,
    opt_level: hew_codegen_rs::OptLevel,
    source_path: Option<&Path>,
) -> Result<hew_codegen_rs::EmitArtefacts, ()> {
    let options = hew_codegen_rs::EmitOptions {
        module_name,
        out_dir: emit_dir,
        native: emit_target == CompileEmitTarget::Native,
        wasm: emit_target == CompileEmitTarget::Wasm,
        target_triple,
        debug,
        opt_level,
        source_path,
    };
    let result = if emit_target == CompileEmitTarget::Wasm && !link_freestanding_wasm {
        hew_codegen_rs::emit_module_objects(pipeline, &options)
    } else {
        hew_codegen_rs::emit_module(pipeline, &options)
    };
    match result {
        Ok(artefacts) => Ok(artefacts),
        Err(e) => {
            // #2091: render through the source-attributed diagnostic path so a
            // fail-closed codegen error points at the user's code instead of a
            // bare, locationless line. The failing function's span rides on the
            // error (attached at the MIR body-lowering boundary); resolve the
            // source text from the same path codegen emitted against. No path,
            // or unreadable/spanless source, degrades to the historical bare
            // line inside `render_codegen_emit_error`.
            let (source, filename) = source_path
                .map(|p| {
                    (
                        std::fs::read_to_string(p).unwrap_or_default(),
                        p.display().to_string(),
                    )
                })
                .unwrap_or_default();
            diagnostic::render_codegen_emit_error(&e, &source, &filename);
            Err(())
        }
    }
}

/// Emit artefacts for an explicit target, threading the target triple into
/// codegen so cross-arch object/binary emission produces the foreign arch.
///
/// For native targets the codegen triple is `target.linker_triple()` — the
/// deployment-target form on Darwin (`<arch>-apple-macosx<version>`) so the
/// emitted object's minimum-OS tag matches the link step and no
/// "newer macOS version" warning fires — and the normalized triple elsewhere.
/// Wasm emission does not use a native triple (codegen targets
/// `wasm32-unknown-unknown` directly), so `None` is passed.
#[allow(
    clippy::too_many_arguments,
    reason = "all args are direct fields of EmitOptions; no grouping improves clarity"
)]
fn emit_module_for_target(
    pipeline: &hew_mir::IrPipeline,
    module_name: &str,
    emit_dir: &Path,
    emit_target: CompileEmitTarget,
    target: &target::TargetSpec,
    link_freestanding_wasm: bool,
    debug: bool,
    opt_level: hew_codegen_rs::OptLevel,
    source_path: Option<&Path>,
) -> Result<hew_codegen_rs::EmitArtefacts, ()> {
    let codegen_triple = match emit_target {
        CompileEmitTarget::Native => Some(target.linker_triple()),
        CompileEmitTarget::Wasm => None,
    };
    emit_module_with_triple(
        pipeline,
        module_name,
        emit_dir,
        emit_target,
        codegen_triple.as_deref(),
        link_freestanding_wasm,
        debug,
        opt_level,
        source_path,
    )
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
    let artefacts = emit_module(
        &pipeline,
        module_name,
        emit_dir,
        CompileEmitTarget::Native,
        true,
        hew_codegen_rs::OptLevel::O0,
        Some(input),
    )?;
    let obj = artefacts.native_obj_path.as_deref().ok_or_else(|| {
        eprintln!("E_NOT_YET_IMPLEMENTED: native codegen did not produce an object");
    })?;
    link_native_object(obj, bin_path)
}

#[allow(
    clippy::too_many_lines,
    reason = "cohesive frontend->MIR->emit->link pipeline already at the line boundary; \
              threading opt_level tipped it over — splitting it would obscure the linear flow"
)]
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
        hir_target_arch(&target),
    );
    let mut hir_diagnostics = lower_output.diagnostics;
    for diag in hew_hir::verify_hir(&lower_output.module) {
        if !hir_diagnostics
            .iter()
            .any(|d| d.kind == diag.kind && d.span == diag.span)
        {
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

    let mut pipeline = hew_mir::lower_hir_module_with_facts(
        &lower_output.module,
        &tco.actor_send_aliasing,
        mir_pointer_width(&target),
    );
    // Route checker-authored layout facts onto the pipeline.
    pipeline.attach_lowering_facts(&tco);
    if render_pipeline_mir_diagnostics(
        &state.program,
        &state.source,
        source_label,
        &lower_output.module,
        &pipeline.diagnostics,
    ) {
        return Err(());
    }

    if render_pipeline_mir_lints(&state.source, source_label, &pipeline, &options.lint_levels) {
        return Err(());
    }

    let emit_dir = output_path.parent().unwrap_or_else(|| Path::new("."));
    let module_name = output_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");
    let emit_target = if target.is_wasm() {
        CompileEmitTarget::Wasm
    } else if target.can_link_with_host_tools() {
        CompileEmitTarget::Native
    } else {
        eprintln!("{}", target.unsupported_native_link_error());
        return Err(());
    };
    let artefacts = emit_module(
        &pipeline,
        module_name,
        emit_dir,
        emit_target,
        false,
        hew_codegen_rs::OptLevel::O0,
        // `source_label` is a diagnostic label (REPL/eval snippet), not a
        // guaranteed on-disk path — reading it could render against unrelated
        // file content, so pass no source path. A codegen error here keeps the
        // historical bare line (no regression); the file-based `build`/`run`/
        // `check` paths carry a real path and render with a source span (#2091).
        None,
    )?;

    match emit_target {
        CompileEmitTarget::Native => {
            let obj = artefacts.native_obj_path.as_deref().ok_or_else(|| {
                eprintln!("E_NOT_YET_IMPLEMENTED: native codegen did not produce an object");
            })?;
            link_native_object(obj, output_path)
        }
        CompileEmitTarget::Wasm => {
            let obj = artefacts.wasm_obj_path.as_deref().ok_or_else(|| {
                eprintln!("E_NOT_YET_IMPLEMENTED: WASM codegen did not produce an object");
            })?;
            let obj_str = obj.to_str().ok_or_else(|| {
                eprintln!("Error: WASM object path is not valid UTF-8");
            })?;
            let output_str = output_path.to_str().ok_or_else(|| {
                eprintln!("Error: WASM output path is not valid UTF-8");
            })?;
            crate::link::link_executable(obj_str, output_str, &target, &[], false).map_err(|e| {
                eprintln!("{e}");
            })
        }
    }
}

/// Resolve the output binary path for `hew build` using go-build naming.
///
/// With `-o`, the path is used verbatim. Otherwise the default is
/// `./<input-stem><target.executable_suffix()>` in the current directory —
/// `""` on Unix targets, `.exe` on Windows targets, `.wasm` on wasm targets.
/// The suffix is target-driven, not host-driven, so a Windows cross-build names
/// `foo.exe` even on a Unix host.
fn resolve_build_output_path(
    a: &args::BuildArgs,
    target: &target::TargetSpec,
) -> std::path::PathBuf {
    if let Some(output) = &a.output {
        return output.clone();
    }
    let stem = a
        .input
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("a.out");
    std::path::PathBuf::from(format!("{stem}{}", target.executable_suffix()))
}

/// Link a native object into a binary for an explicit target.
///
/// Unlike `link_native_object` (which hardcodes the host target via
/// `from_requested(None)`), this passes the resolved `&target` so the link plan,
/// `linker_triple()`, and cross `libhew.a` resolution are correct for cross-arch
/// and same-arch explicit-target builds.
fn link_native_object_for_target(
    obj: &Path,
    bin_path: &Path,
    target: &target::TargetSpec,
    debug: bool,
    extra_libs: &[String],
) -> Result<(), ()> {
    let obj_str = obj.to_str().ok_or_else(|| {
        eprintln!("Error: object path is not valid UTF-8");
    })?;
    let bin_str = bin_path.to_str().ok_or_else(|| {
        eprintln!("Error: output path is not valid UTF-8");
    })?;
    crate::link::link_executable(obj_str, bin_str, target, extra_libs, debug).map_err(|e| {
        eprintln!("{e}");
    })
}

/// Build a native (or wasm) binary for an explicit target, writing it to
/// `output_path`. Reuses the front-end → MIR → emit → link chain.
#[allow(
    clippy::too_many_arguments,
    reason = "the build path threads each emit/link knob explicitly; grouping obscures the flow"
)]
fn compile_build_binary(
    input: &Path,
    output_path: &Path,
    target: &target::TargetSpec,
    debug: bool,
    opt_level: hew_codegen_rs::OptLevel,
    extra_libs: &[String],
    options: &compile::CompileOptions,
) -> Result<(), ()> {
    let (pipeline, native_pkg_dirs) = lower_file_to_mir_for_target(input, target, options)?;
    let emit_dir = output_path.parent().unwrap_or_else(|| Path::new("."));
    let module_name = output_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");
    let emit_target = if target.is_wasm() {
        CompileEmitTarget::Wasm
    } else {
        CompileEmitTarget::Native
    };
    let artefacts = emit_module_for_target(
        &pipeline,
        module_name,
        emit_dir,
        emit_target,
        target,
        // Link freestanding wasm so `hew build --target wasm32-unknown-unknown`
        // yields a runnable `.wasm`; the native branch links separately below.
        emit_target == CompileEmitTarget::Wasm,
        debug,
        opt_level,
        Some(input),
    )?;

    match emit_target {
        CompileEmitTarget::Native => {
            let obj = artefacts.native_obj_path.as_deref().ok_or_else(|| {
                eprintln!("E_NOT_YET_IMPLEMENTED: native codegen did not produce an object");
            })?;
            // Auto-link native (FFI) staticlibs declared by imported packages
            // (`[native]` in their `hew.toml`), built on demand, in addition to
            // any explicit `--link-lib` archives.
            let auto_libs = native_link::build_native_link_libs(&native_pkg_dirs).map_err(|e| {
                eprintln!("Error: {e}");
            })?;
            let all_libs: Vec<String> = extra_libs.iter().cloned().chain(auto_libs).collect();
            link_native_object_for_target(obj, output_path, target, debug, &all_libs)
        }
        CompileEmitTarget::Wasm => {
            let wasm = artefacts.wasm_path.as_deref().ok_or_else(|| {
                eprintln!("E_NOT_YET_IMPLEMENTED: WASM codegen did not produce a module");
            })?;
            // `link_wasm_module` writes `<dir>/<name>.wasm`; rename to the
            // requested output path when they differ.
            if wasm != output_path {
                std::fs::rename(wasm, output_path).map_err(|e| {
                    eprintln!(
                        "Error: cannot move wasm output to {}: {e}",
                        output_path.display()
                    );
                })?;
            }
            Ok(())
        }
    }
}

/// Emit a single relocatable object for `hew build --emit-obj`, skipping the
/// link step entirely. Writes `<cwd>/<stem><.o|.obj>` and exits 0 — even for
/// foreign-OS targets that cannot be linked on this host (the whole point of
/// object-only emission). The object format/arch are driven by the target
/// triple, threaded into codegen.
fn emit_obj_only(
    input: &Path,
    target: &target::TargetSpec,
    debug: bool,
    opt_level: hew_codegen_rs::OptLevel,
    options: &compile::CompileOptions,
) -> Result<(), ()> {
    let (pipeline, _native_pkg_dirs) = lower_file_to_mir_for_target(input, target, options)?;
    let stem = input
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");
    let out_dir = Path::new(".");
    let emit_target = if target.is_wasm() {
        CompileEmitTarget::Wasm
    } else {
        CompileEmitTarget::Native
    };
    // Emit objects only — never link (no freestanding wasm link either).
    // `-g` threads debug info AND the source path so the object carries a
    // DWARF line table + variable/type DIEs, matching the linked-binary path.
    let artefacts = emit_module_for_target(
        &pipeline,
        stem,
        out_dir,
        emit_target,
        target,
        false,
        debug,
        opt_level,
        if debug { Some(input) } else { None },
    )?;
    let produced = match emit_target {
        CompileEmitTarget::Native => artefacts.native_obj_path,
        CompileEmitTarget::Wasm => artefacts.wasm_obj_path,
    }
    .ok_or_else(|| {
        eprintln!("E_NOT_YET_IMPLEMENTED: codegen did not produce an object");
    })?;

    // Codegen writes `<dir>/<stem>.o` (or `.wasm.o`); rename to the
    // target-driven object extension (`.o`/`.obj`) when it differs.
    let final_path = out_dir.join(format!("{stem}{}", target.object_suffix()));
    if produced != final_path {
        std::fs::rename(&produced, &final_path).map_err(|e| {
            eprintln!("Error: cannot move object to {}: {e}", final_path.display());
        })?;
    }
    Ok(())
}

fn cmd_build(a: &args::BuildArgs) {
    let json = a.format == args::DiagnosticFormat::Json;
    diagnostic_json::set_output_format(a.format.into());

    // A target parse error is a usage error before any compilation — exit 2.
    let target = target::TargetSpec::from_requested(a.target.as_deref()).unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        std::process::exit(2);
    });
    let options = a.to_compile_options();

    // The clap `value_parser` already constrains `--opt-level` to {"0","2"}, so
    // `from_cli_str` cannot return `None` here; fail closed regardless rather than
    // silently defaulting if the parser contract ever drifts.
    let opt_level = hew_codegen_rs::OptLevel::from_cli_str(&a.opt_level).unwrap_or_else(|| {
        eprintln!(
            "Error: invalid --opt-level `{}` (expected 0 or 2)",
            a.opt_level
        );
        std::process::exit(2);
    });

    if a.emit_obj {
        emit_obj_only(&a.input, &target, a.debug, opt_level, &options).unwrap_or_else(|()| {
            if json {
                diagnostic_json::flush_json_diagnostics();
            }
            std::process::exit(1);
        });
        if json {
            diagnostic_json::flush_json_diagnostics();
        }
        return;
    }

    // Linked-binary path. Foreign-OS targets that cannot be linked on this host
    // are rejected fail-closed with a clear message pointing at `--emit-obj`.
    if !target.is_wasm() && !target.can_link_with_host_tools() {
        eprintln!("{}", target.unsupported_native_link_error());
        if json {
            diagnostic_json::flush_json_diagnostics();
        }
        std::process::exit(1);
    }

    let output_path = resolve_build_output_path(a, &target);
    compile_build_binary(
        &a.input,
        &output_path,
        &target,
        a.debug,
        opt_level,
        &a.link_libs,
        &options,
    )
    .unwrap_or_else(|()| {
        if json {
            diagnostic_json::flush_json_diagnostics();
        }
        std::process::exit(1);
    });
    if json {
        diagnostic_json::flush_json_diagnostics();
    }
}

fn cmd_compile(a: &args::CompileArgs) {
    let json = a.format == args::DiagnosticFormat::Json;
    diagnostic_json::set_output_format(a.format.into());

    let pipeline = lower_file_to_mir(&a.input, a.target.as_deref()).unwrap_or_else(|()| {
        if json {
            diagnostic_json::flush_json_diagnostics();
        }
        std::process::exit(1);
    });

    // Dump path: print the requested MIR stage and exit. Useful for
    // spot-checking the lowering during development.
    if let Some(stage) = a.dump_mir.as_deref() {
        let dump_stage = match stage {
            "raw" => hew_mir::DumpStage::Raw,
            "checked" => hew_mir::DumpStage::Checked,
            "elab" => hew_mir::DumpStage::Elab,
            other => {
                eprintln!("Error: unknown --dump-mir stage `{other}`");
                std::process::exit(2);
            }
        };
        print!("{}", hew_mir::dump_mir(&pipeline, dump_stage));
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

    // The clap `value_parser` constrains `--opt-level` to {"0","2"}; fail closed
    // if that contract ever drifts rather than silently defaulting.
    let opt_level = hew_codegen_rs::OptLevel::from_cli_str(&a.opt_level).unwrap_or_else(|| {
        eprintln!(
            "Error: invalid --opt-level `{}` (expected 0 or 2)",
            a.opt_level
        );
        std::process::exit(2);
    });

    let emit_target = resolve_compile_emit_target(a.target.as_deref());
    let artefacts = emit_module(
        &pipeline,
        module_name,
        emit_dir,
        emit_target,
        true,
        opt_level,
        Some(a.input.as_path()),
    )
    .unwrap_or_else(|()| {
        if json {
            diagnostic_json::flush_json_diagnostics();
        }
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
            if json {
                diagnostic_json::flush_json_diagnostics();
            }
            std::process::exit(1);
        });
        if !json {
            println!("native: {}", bin_path.display());
        }
    }
    if let Some(wasm) = &artefacts.wasm_path {
        if !json {
            println!("wasm:   {}", wasm.display());
        }
    }
    // Clean compile under JSON: emit the (empty) diagnostic array on stdout so
    // the contract holds — `[]` and exit 0.
    if json {
        diagnostic_json::flush_json_diagnostics();
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
) -> Result<CompiledTempExecutable, ()> {
    // `hew debug` launches a native debugger on the artifact, so it must carry
    // DWARF debug info — emit it even though the shared `hew run` path does not.
    compile_temp_artifact(input, create_debug_temp_artifact(target), options, true)
}

fn compile_temp_run_artifact(
    input: &str,
    options: &compile::CompileOptions,
    target: &target::ExecutionTarget,
) -> Result<CompiledTempExecutable, ()> {
    // `hew run` is the fast-exec path; no debug info.
    compile_temp_artifact(input, create_run_temp_artifact(target), options, false)
}

/// Compile a `.hew` source file to a temporary `.wasm` module for WASI execution.
///
/// Mirrors the `hew eval --target wasm32-wasi` compilation path:
///   1. Front-end → MIR (`lower_file_to_mir_for_target`)
///   2. Codegen emits a wasm object only (no freestanding link)
///   3. `link::link_executable` links against `libhew_runtime.a` for wasip1
///
fn compile_temp_wasi_module(
    input: &str,
    options: &compile::CompileOptions,
    target: &target::ExecutionTarget,
) -> Result<CompiledTempExecutable, ()> {
    // Compute the target spec before allocating temp files so that an early
    // `process::exit` does not leak a temp directory.
    let target_spec =
        target::TargetSpec::from_requested(options.target.as_deref()).unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            std::process::exit(1);
        });

    let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
        eprintln!("Error: cannot create temp dir: {e}");
        std::process::exit(1);
    });
    let stem = Path::new(input)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("hew_wasi_run");
    let wasm_path = tmp_dir
        .path()
        .join(format!("{stem}{}", target.executable_suffix()));

    let artifact = CompiledTempExecutable {
        path: wasm_path.clone(),
        _cleanup: TempExecutableCleanup::TempDir { _temp_dir: tmp_dir },
    };

    let result = (|| -> Result<(), ()> {
        let (pipeline, _native_pkg_dirs) =
            lower_file_to_mir_for_target(Path::new(input), &target_spec, options)?;
        let emit_dir = tmp_dir_of_path(&wasm_path);
        // Emit the wasm object only — the WASI runtime link happens in
        // `link::link_executable` below, which links against libhew_runtime.a.
        let artefacts = emit_module_for_target(
            &pipeline,
            stem,
            emit_dir,
            CompileEmitTarget::Wasm,
            &target_spec,
            false,
            false,
            // The WASI `run`/`eval` path is debug-default O0; the `HEW_OPT_LEVEL`
            // env floor lifts the whole corpus to O2 for the differential gate.
            hew_codegen_rs::OptLevel::O0,
            None,
        )?;
        let obj = artefacts.wasm_obj_path.as_deref().ok_or_else(|| {
            eprintln!("E_NOT_YET_IMPLEMENTED: WASM codegen did not produce an object");
        })?;
        let obj_str = obj.to_str().ok_or_else(|| {
            eprintln!("Error: WASM object path is not valid UTF-8");
        })?;
        let out_str = wasm_path.to_str().ok_or_else(|| {
            eprintln!("Error: WASM output path is not valid UTF-8");
        })?;
        crate::link::link_executable(obj_str, out_str, &target_spec, &[], false).map_err(|e| {
            eprintln!("{e}");
        })
    })();

    if let Err(()) = result {
        drop(artifact);
        if diagnostic_json::json_output_active() {
            diagnostic_json::flush_json_diagnostics();
        }
        return Err(());
    }

    Ok(artifact)
}

fn tmp_dir_of_path(path: &Path) -> &Path {
    path.parent().unwrap_or_else(|| Path::new("."))
}

fn compile_temp_artifact(
    input: &str,
    artifact: CompiledTempExecutable,
    options: &compile::CompileOptions,
    debug: bool,
) -> Result<CompiledTempExecutable, ()> {
    // Route through the same path as `hew build` so `hew run` honours
    // `--pkg-path` (resolving `hew::<pkg>` imports) and auto-links the native
    // (FFI) staticlibs declared by imported packages.
    let target = match target::TargetSpec::from_requested(options.target.as_deref()) {
        Ok(target) => target,
        Err(e) => {
            eprintln!("Error: {e}");
            drop(artifact);
            std::process::exit(1);
        }
    };
    if compile_build_binary(
        Path::new(input),
        artifact.path(),
        &target,
        debug,
        // `hew run` is debug-default O0; the `HEW_OPT_LEVEL` env floor lifts the
        // whole corpus to O2 for the differential-exec parity gate.
        hew_codegen_rs::OptLevel::O0,
        &[],
        options,
    )
    .is_ok()
    {
        Ok(artifact)
    } else {
        drop(artifact);
        // Flush any collected JSON diagnostics before the caller exits.
        // No-op when JSON output is not active (e.g. `hew debug`).
        if diagnostic_json::json_output_active() {
            diagnostic_json::flush_json_diagnostics();
        }
        Err(())
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
    // `--format=json` governs compile-time diagnostics. On a compile failure the
    // collected JSON array is flushed (in `compile_temp_artifact`); on a clean
    // compile the program runs normally and its own stdout is preserved — no
    // empty array is emitted so program output is never corrupted.
    diagnostic_json::set_output_format(a.format.into());

    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    let target = resolve_run_target(options.target.as_deref());
    let timeout = a.timeout.as_deref().map(|raw| {
        crate::util::parse_timeout(raw).unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            std::process::exit(1);
        })
    });

    // When --show-stack-hints is set, run the frontend early to collect and
    // print the escape-analysis hints before the program executes. Suppressed
    // under --format=json so program output is not corrupted. The full compile
    // still runs below; this is an intentional double-frontend so the run path
    // does not need to thread hints through the compilation pipeline.
    let json = a.format == args::DiagnosticFormat::Json;
    if a.show_stack_hints && !json {
        let target_spec = target::TargetSpec::from_requested(options.target.as_deref())
            .unwrap_or_else(|e| {
                eprintln!("Error: {e}");
                std::process::exit(1);
            });
        let frontend_options = compile::frontend_options(&target_spec, &options);
        if let Ok((result, _)) = hew_compile::check_file_with_state(&input, &frontend_options) {
            diagnostic::print_stack_hints(&result.source, &input, &result.stack_hints);
        }
        // A frontend failure here is silently swallowed: the full compile below
        // will surface the same error with proper diagnostic attribution.
    }

    if target.is_wasi() {
        cmd_run_wasi(a, &input, &options, &target, timeout);
    } else {
        cmd_run_native(a, &input, &options, &target, timeout);
    }
}

fn cmd_run_wasi(
    a: &args::RunArgs,
    input: &str,
    options: &compile::CompileOptions,
    target: &target::ExecutionTarget,
    timeout: Option<std::time::Duration>,
) -> ! {
    let artifact =
        compile_temp_wasi_module(input, options, target).unwrap_or_else(|()| std::process::exit(1));

    match wasi_runner::run_module(artifact.path(), &a.program_args, timeout) {
        Ok(wasi_runner::WasiRunOutcome::Exited(status)) => {
            drop(artifact);
            std::process::exit(status.code().unwrap_or(1));
        }
        Ok(wasi_runner::WasiRunOutcome::Timeout) => {
            drop(artifact);
            let formatted =
                timeout.map_or_else(|| "unknown".to_string(), crate::process::format_timeout);
            eprintln!("Error: program timed out after {formatted}");
            std::process::exit(1);
        }
        Err(e) => {
            drop(artifact);
            eprintln!("Error: cannot run {input} under WASI: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_run_native(
    a: &args::RunArgs,
    input: &str,
    options: &compile::CompileOptions,
    target: &target::ExecutionTarget,
    timeout: Option<std::time::Duration>,
) -> ! {
    let artifact = compile_temp_run_artifact(input, options, target)
        .unwrap_or_else(|()| std::process::exit(1));

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

    // WASI targets execute via wasmtime rather than directly on the host, so
    // they are allowed even though `can_run_on_host` returns false for them.
    if !target.is_wasi() && !target.can_run_on_host() {
        eprintln!("{}", target.cross_target_run_error("run"));
        std::process::exit(1);
    }

    target
}

fn cmd_check(a: &args::CheckArgs) {
    let json = a.format == args::DiagnosticFormat::Json;
    diagnostic_json::set_output_format(a.format.into());

    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    let target =
        target::TargetSpec::from_requested(options.target.as_deref()).unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            // Usage/configuration error before any diagnostics — exit 2.
            std::process::exit(2);
        });
    let frontend_options = compile::frontend_options(&target, &options);

    let (result, state) = match hew_compile::check_file_with_state(&input, &frontend_options) {
        Ok(result) => result,
        Err(failure) => {
            compile::render_frontend_diagnostics(&failure.diagnostics);
            if json {
                diagnostic_json::flush_json_diagnostics();
            } else {
                eprintln!("{}", failure.message);
            }
            std::process::exit(1);
        }
    };

    compile::render_frontend_diagnostics(&result.diagnostics);
    // Stack hints and explain-cow are human-only diagnostic surfaces; suppress
    // them under JSON so stdout carries only the diagnostic array.
    if !json {
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
    }

    if run_check_deep_gates(&input, &target, &state, &options.lint_levels).is_err() {
        if json {
            diagnostic_json::flush_json_diagnostics();
        }
        std::process::exit(1);
    }

    if json {
        // Clean program (warnings may still be present in the array): emit the
        // collected diagnostics — `[]` when none — and exit 0.
        diagnostic_json::flush_json_diagnostics();
    } else {
        eprintln!("{input}: OK");
    }
}

fn cmd_debug(a: &args::DebugArgs) {
    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    let target = resolve_debug_target(options.target.as_deref());
    let artifact = compile_temp_debug_artifact(&input, &options, &target)
        .unwrap_or_else(|()| std::process::exit(1));
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
// Observe / LSP — sibling binary delegation
// ---------------------------------------------------------------------------

/// Launch `hew-observe` as a subprocess, forwarding `extra_args`.
///
/// Looks for `hew-observe` in the same directory as the running `hew` binary
/// first, then falls back to PATH resolution. This keeps `hew observe` usable
/// both from an install prefix and from a Cargo workspace `target/` tree.
fn cmd_observe(a: &args::ObserveArgs) {
    exec_sibling_binary("hew-observe", &a.args);
}

/// Launch `hew-lsp` as a subprocess, forwarding `extra_args`.
///
/// Looks for `hew-lsp` in the same directory as the running `hew` binary
/// first, then falls back to PATH resolution.
fn cmd_lsp(a: &args::LspArgs) {
    exec_sibling_binary("hew-lsp", &a.args);
}

/// Run the bundled package manager (`adze`) in-process as `hew package …`.
///
/// `hew` ships a single binary: the package-manager command surface lives in
/// the `adze-cli` library and is invoked directly here (no sibling `adze`
/// process). The leading argument is the display name used in usage/help text.
fn cmd_package(a: &args::PackageArgs) {
    let mut argv: Vec<String> = Vec::with_capacity(a.args.len() + 1);
    argv.push("hew package".to_string());
    argv.extend(a.args.iter().cloned());
    adze_cli::cli::run_from_args(argv);
}

/// Replace the current process with `binary_name [extra_args]`.
///
/// Resolution order:
///   1. `<dir of current executable>/<binary_name>[.exe]`
///   2. PATH lookup (via `std::process::Command` which inherits `PATH`)
///
/// On Unix the process image is replaced via `execvp`; on other platforms a
/// child process is spawned, waited for, and the exit code is forwarded.
fn exec_sibling_binary(binary_name: &str, extra_args: &[String]) -> ! {
    // Prefer a sibling binary next to the running `hew` executable.
    let sibling = std::env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|dir| dir.join(binary_name)));

    let binary_path: std::path::PathBuf = match sibling {
        Some(ref candidate) if candidate.exists() => candidate.clone(),
        _ => std::path::PathBuf::from(binary_name),
    };

    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt as _;
        let mut cmd = std::process::Command::new(&binary_path);
        cmd.args(extra_args);
        let error = cmd.exec();
        // exec() only returns on failure.
        eprintln!(
            "error: failed to launch `{}`: {error}",
            binary_path.display()
        );
        std::process::exit(1);
    }

    #[cfg(not(unix))]
    {
        let status = std::process::Command::new(&binary_path)
            .args(extra_args)
            .status();
        match status {
            Ok(s) => std::process::exit(s.code().unwrap_or(1)),
            Err(e) => {
                eprintln!("error: failed to launch `{}`: {e}", binary_path.display());
                std::process::exit(1);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Helper utilities
// ---------------------------------------------------------------------------
