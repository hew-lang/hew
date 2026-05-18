//! `hew` — the Hew programming language compiler driver.
//!
//! ```text
//! hew build file.hew [-o output]   # Compile to executable
//! hew compile-v05 file.hew [--emit-dir DIR] [--dump-mir raw|elab] [--no-wasm]
//!                                  # Run the v0.5 IR ladder and emit native + wasm binaries
//! hew run file.hew [-- args...]    # Compile and run
//! hew debug file.hew [-- args...]  # Build with debug info + launch gdb/lldb
//! hew check file.hew               # Parse + typecheck only
//! hew watch file_or_dir [options]  # Watch for changes and re-check
//! hew eval                         # Interactive REPL
//! hew eval "<expression>"          # Evaluate expression
//! hew eval -f file.hew             # Execute file in REPL context
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

// Force Cargo to include hew-runtime's rlib archive members in the final link
// step.  Without this, Cargo excludes the rlib because hew-cli has no
// direct Rust-level call sites into hew-runtime — `extern "C"` declarations
// alone do not count as usage.  `extern crate X as _` is the standard Rust
// idiom for pulling in a dep's rlib without binding its name.
//
// The anchor static below (from runtime_export.rs) then holds a reference to
// every kStableJitHostSymbols address, preventing LTO/DCE from discarding the
// archive members after lazy resolution.
#[cfg(hew_embedded_codegen)]
extern crate hew_runtime as _;

// Include the build-time generated anchor module that keeps every
// kStableJitHostSymbols entry alive through LTO so dlsym can resolve them at
// JIT session startup.  Only emitted when the embedded LLVM/MLIR codegen
// backend is present (hew_embedded_codegen cfg).
#[cfg(hew_embedded_codegen)]
include!(concat!(env!("OUT_DIR"), "/runtime_export.rs"));

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

// The dispatcher short-circuits `hew build` and `hew run` with a cutover
// error before reaching the bodies below; each `#[allow(dead_code, ...)]`
// keeps the body compiled while it is dormant. The attributes come off when
// the C++ codegen subtree is removed in a later stage of the v0.5 cutover.

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn cmd_build(a: &args::BuildArgs) {
    let input = a.input.display().to_string();
    let output = a.output.as_ref().map(|p| p.display().to_string());
    let options = a.to_compile_options();
    match compile::compile(&input, output.as_deref(), false, &options) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

/// Surface the diagnostic family in the CLI prefix so users see what
/// gate tripped at a glance. `MirCheck` findings (move/init/aliasing
/// legality) are a distinct family from spine-subset rejections; the
/// kind in the debug payload disambiguates further.
fn diagnostic_prefix(kind: &hew_mir::MirDiagnosticKind) -> &'static str {
    match kind {
        hew_mir::MirDiagnosticKind::UseAfterConsume { .. }
        | hew_mir::MirDiagnosticKind::InitialisedBeforeUse { .. }
        | hew_mir::MirDiagnosticKind::DecisionMapTotal { .. }
        | hew_mir::MirDiagnosticKind::MustConsume { .. }
        | hew_mir::MirDiagnosticKind::DropPlanUndetermined { .. } => "E_MIR_CHECK",
        hew_mir::MirDiagnosticKind::CutoverUnsupported { .. } => "E_CUTOVER_UNSUPPORTED",
        hew_mir::MirDiagnosticKind::UnknownType { .. }
        | hew_mir::MirDiagnosticKind::UnsupportedNode { .. }
        | hew_mir::MirDiagnosticKind::UnresolvedPlace { .. }
        | hew_mir::MirDiagnosticKind::CannotMaterializeClosureCapture { .. } => "E_MIR",
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "linear CLI entry point: parse → typecheck → lower → MIR; \
              splitting on line count would scatter a sequential pipeline"
)]
fn cmd_compile_v05(a: &args::CompileV05Args) {
    let input = a.input.display().to_string();
    let source = std::fs::read_to_string(&a.input).unwrap_or_else(|e| {
        eprintln!("Error: cannot read {input}: {e}");
        std::process::exit(1);
    });

    let parsed = hew_parser::parse(&source);
    if !parsed.errors.is_empty() {
        for error in parsed.errors {
            eprintln!("{error:?}");
        }
        std::process::exit(1);
    }

    // Type-check the program and collect the side-table before HIR lowering.
    // `method_call_rewrites` and other checker-produced metadata flow into the
    // HIR bridge via `TypeCheckOutput`; without this call the bridge is empty
    // and every method call fails closed in `lower_program`.
    let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
    let mut checker = hew_types::Checker::new(registry);
    let tc_output = checker.check_program(&parsed.program);
    if !tc_output.errors.is_empty() {
        for error in &tc_output.errors {
            eprintln!("E_TYPE: {error:?}");
        }
        std::process::exit(1);
    }

    let lower_output = hew_hir::lower_program(&parsed.program, &tc_output, &hew_hir::ResolutionCtx);
    let mut diagnostics = lower_output.diagnostics;
    diagnostics.extend(hew_hir::verify_hir(&lower_output.module));
    if !diagnostics.is_empty() {
        for diagnostic in diagnostics {
            eprintln!("{diagnostic:?}");
        }
        std::process::exit(1);
    }

    let pipeline = hew_mir::lower_hir_module(&lower_output.module);
    if !pipeline.diagnostics.is_empty() {
        for diagnostic in &pipeline.diagnostics {
            eprintln!("{} {diagnostic:?}", diagnostic_prefix(&diagnostic.kind));
        }
        std::process::exit(1);
    }

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

    // Default emit dir: `.tmp/compile-v05-out` under the cwd. `.tmp/` is
    // gitignored across the workspace.
    let default_dir = std::path::PathBuf::from(".tmp/compile-v05-out");
    let emit_dir = a.emit_dir.as_ref().unwrap_or(&default_dir);
    let module_name = a
        .input
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");

    let options = hew_codegen_rs::EmitOptions {
        module_name,
        out_dir: emit_dir,
        native: true,
        wasm: !a.no_wasm,
    };
    let artefacts = match hew_codegen_rs::emit_module(&pipeline, &options) {
        Ok(a) => a,
        Err(hew_codegen_rs::CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            // WASM-TODO(#1451): hew_duplex_* symbols are excluded from wasm32
            // builds via `hew-runtime/src/duplex.rs:54`. Surface a structured
            // diagnostic so the user knows to pass `--no-wasm` instead of
            // seeing a raw `wasm-ld: undefined symbol` linker error.
            eprintln!(
                "error: WASM target does not support the duplex concurrency \
                 substrate (symbol: {symbol}; WASM-TODO(#1451))\n\
                 hint: pass `--no-wasm` to skip WASM emission and produce a \
                 native binary only"
            );
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("E_CUTOVER_UNSUPPORTED: {e}");
            std::process::exit(1);
        }
    };

    // Link the native object into an executable using the shared
    // `link::link_executable` path. This resolves `libhew.a` (the
    // combined runtime + stdlib staticlib built by `make stdlib` / `cargo
    // build -p hew-lib`) via `find_hew_lib`, applies the per-platform
    // link plan (dead-strip, strip, system libs including `-lpthread
    // -ldl -lm -lrt` on Linux, CoreFoundation + Security on macOS), and
    // routes through lld when available. The same path is exercised by
    // `hew build`, so compile-v05 binaries now have the same linker
    // behaviour as release builds.
    if let Some(obj) = &artefacts.native_obj_path {
        let bin_path = emit_dir.join(module_name);
        // compile-v05 always targets the host — no cross-compilation yet.
        let target = target::TargetSpec::from_requested(None).unwrap_or_else(|e| {
            eprintln!("Error: cannot determine host target: {e}");
            std::process::exit(1);
        });
        let obj_str = obj.to_str().unwrap_or_else(|| {
            eprintln!("Error: object path is not valid UTF-8");
            std::process::exit(1);
        });
        let bin_str = bin_path.to_str().unwrap_or_else(|| {
            eprintln!("Error: output path is not valid UTF-8");
            std::process::exit(1);
        });
        if let Err(e) = link::link_executable(obj_str, bin_str, &target, &[], false) {
            eprintln!("{e}");
            std::process::exit(1);
        }
        println!("native: {}", bin_path.display());
    }
    if let Some(wasm) = &artefacts.wasm_path {
        println!("wasm:   {}", wasm.display());
    }
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn cmd_run(a: &args::RunArgs) {
    let input = a.input.display().to_string();
    let timeout = resolve_optional_timeout(a.timeout.as_deref());
    let options = a.to_compile_options();
    let target = resolve_run_target(options.target.as_deref(), a.profile);
    if a.show_stack_hints {
        // Print stack hints before invoking codegen so the user sees them in
        // the same stderr stream regardless of whether compile/run later
        // exits non-zero. Failures here are best-effort: if the frontend
        // rejects the program we let the subsequent `compile_temp_run_artifact`
        // call surface the canonical diagnostics.
        let frontend_options = compile::frontend_options_for_check(&options);
        if let Ok(out) = hew_compile::check_file(&input, &frontend_options) {
            diagnostic::print_stack_hints(&out.source, &input, &out.stack_hints);
        }
    }
    let artifact = compile_temp_run_artifact(&input, &options, &target);

    if target.is_wasi() {
        exit_after_wasi_run(artifact, &a.program_args, timeout);
    }

    exit_after_native_run(artifact, &a.program_args, timeout, a.profile);
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn resolve_optional_timeout(raw: Option<&str>) -> Option<std::time::Duration> {
    raw.map(crate::util::parse_timeout)
        .transpose()
        .unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            std::process::exit(1);
        })
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn resolve_run_target(requested: Option<&str>, profile: bool) -> target::ExecutionTarget {
    let target = target::ExecutionTarget::from_requested(requested).unwrap_or_else(|e| {
        eprintln!("{e}");
        std::process::exit(1);
    });

    if target.is_wasi() && profile {
        eprintln!("Error: `hew run --profile` is not supported for wasm32-wasi targets yet");
        std::process::exit(1);
    }

    if target.is_native() && !target.can_run_on_host() {
        eprintln!("{}", target.cross_target_run_error("run"));
        std::process::exit(1);
    }

    target
}

struct CompiledTempExecutable {
    path: std::path::PathBuf,
    _cleanup: TempExecutableCleanup,
}

enum TempExecutableCleanup {
    #[allow(dead_code, reason = "dormant during v0.5 cutover")]
    TempPath {
        _temp_path: tempfile::TempPath,
    },
    TempDir {
        _temp_dir: tempfile::TempDir,
    },
}

impl CompiledTempExecutable {
    fn path(&self) -> &Path {
        &self.path
    }

    fn path_string(&self) -> String {
        self.path.display().to_string()
    }
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn compile_temp_run_artifact(
    input: &str,
    options: &compile::CompileOptions,
    target: &target::ExecutionTarget,
) -> CompiledTempExecutable {
    compile_temp_artifact(input, create_run_temp_artifact(target), options)
}

fn compile_temp_debug_artifact(
    input: &str,
    options: &compile::CompileOptions,
    target: &target::ExecutionTarget,
) -> CompiledTempExecutable {
    compile_temp_artifact(input, create_debug_temp_artifact(target), options)
}

fn compile_temp_artifact(
    input: &str,
    artifact: CompiledTempExecutable,
    options: &compile::CompileOptions,
) -> CompiledTempExecutable {
    let output = artifact.path_string();

    match compile::compile(input, Some(&output), false, options) {
        Ok(_) => artifact,
        Err(e) => {
            eprintln!("{e}");
            drop(artifact);
            // Exit 125 = compile failure (sentinel used by the playground to
            // distinguish compile errors from program exit codes).
            std::process::exit(125);
        }
    }
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn create_run_temp_artifact(target: &target::ExecutionTarget) -> CompiledTempExecutable {
    let temp_path = tempfile::Builder::new()
        .prefix("hew_run_")
        .suffix(target.executable_suffix())
        .tempfile()
        .unwrap_or_else(|e| {
            eprintln!("Error: cannot create temp file: {e}");
            std::process::exit(1);
        })
        .into_temp_path();
    let path = temp_path.to_path_buf();

    CompiledTempExecutable {
        path,
        _cleanup: TempExecutableCleanup::TempPath {
            _temp_path: temp_path,
        },
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

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn exit_after_native_run(
    artifact: CompiledTempExecutable,
    program_args: &[String],
    timeout: Option<std::time::Duration>,
    profile: bool,
) -> ! {
    let mut cmd = std::process::Command::new(artifact.path());
    cmd.args(program_args);
    configure_profiler_env(&mut cmd, profile);

    let status = run_native_binary(&mut cmd, timeout);
    drop(artifact);

    match (timeout, status) {
        (_, Ok(crate::process::ChildWaitOutcome::Exited(status))) => {
            std::process::exit(status.code().unwrap_or(1))
        }
        (Some(timeout), Ok(crate::process::ChildWaitOutcome::Timeout)) => {
            eprintln!(
                "Error: program timed out after {}",
                crate::process::format_timeout(timeout)
            );
            std::process::exit(1);
        }
        (None, Ok(crate::process::ChildWaitOutcome::Timeout)) => {
            unreachable!("timeout outcome requires an explicit timeout")
        }
        (_, Err(e)) => {
            eprintln!("Error: {e}");
            std::process::exit(1);
        }
    }
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn run_native_binary(
    cmd: &mut std::process::Command,
    timeout: Option<std::time::Duration>,
) -> Result<crate::process::ChildWaitOutcome, String> {
    match timeout {
        None => {
            let mut child = cmd
                .spawn()
                .map_err(|e| format!("cannot run compiled binary: {e}"))?;
            // Forward SIGTERM/SIGINT to the child so that signals sent directly
            // to the wrapper also terminate the compiled program.
            #[cfg(unix)]
            signal::forward_signals_to_child(child.id());
            child
                .wait()
                .map(crate::process::ChildWaitOutcome::Exited)
                .map_err(|e| format!("cannot wait for child process: {e}"))
        }
        Some(timeout) => {
            let mut bounded = crate::process::BoundedChild::spawn(cmd)
                .map_err(|e| format!("cannot run compiled binary: {e}"))?;
            // Forward signals to the child PID; the process-group kill on
            // timeout handles the broader tree teardown.
            #[cfg(unix)]
            signal::forward_signals_to_child(bounded.id());
            bounded.wait_with_timeout(timeout)
        }
    }
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn configure_profiler_env(cmd: &mut std::process::Command, profile: bool) {
    if !profile || std::env::var_os("HEW_PPROF").is_some() {
        return;
    }

    // --profile: enable the built-in runtime profiler by setting HEW_PPROF on
    // the child process. "auto" on Unix → per-user unix socket +
    // auto-discovery for hew-observe. ":6060" on non-Unix → TCP listener.
    #[cfg(unix)]
    {
        cmd.env("HEW_PPROF", "auto");
        eprintln!("[hew] profiler enabled (unix socket) — run `hew-observe` to attach");
    }
    #[cfg(not(unix))]
    {
        cmd.env("HEW_PPROF", ":6060");
        eprintln!(
            "[hew] profiler enabled on :6060 — run `hew-observe --addr localhost:6060` to attach"
        );
    }
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn exit_after_wasi_run(
    artifact: CompiledTempExecutable,
    program_args: &[String],
    timeout: Option<std::time::Duration>,
) -> ! {
    let status = wasi_runner::run_module(artifact.path(), program_args, timeout);
    drop(artifact);

    match (timeout, status) {
        (_, Ok(wasi_runner::WasiRunOutcome::Exited(status))) => {
            std::process::exit(status.code().unwrap_or(1))
        }
        (Some(timeout), Ok(wasi_runner::WasiRunOutcome::Timeout)) => {
            eprintln!(
                "Error: program timed out after {}",
                crate::process::format_timeout(timeout)
            );
            std::process::exit(1);
        }
        (None, Ok(wasi_runner::WasiRunOutcome::Timeout)) => {
            unreachable!("timeout outcome requires an explicit timeout")
        }
        (_, Err(e)) => {
            eprintln!("Error: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_check(a: &args::CheckArgs) {
    let input = a.input.display().to_string();
    let options = a.to_compile_options();

    if a.show_stack_hints {
        // Re-run the frontend through `check_file` so we can surface the
        // typechecker's stack-allocation hints. The plain `compile::compile`
        // path discards the source string and TCO, both of which the printer
        // needs for file:line:col attribution. The hints render on stderr
        // before the OK/error verdict so the user sees them in declaration
        // order independent of the verdict's exit-code outcome.
        let frontend_options = compile::frontend_options_for_check(&options);
        match hew_compile::check_file(&input, &frontend_options) {
            Ok(out) => {
                diagnostic::print_stack_hints(&out.source, &input, &out.stack_hints);
                eprintln!("{input}: OK");
            }
            Err(failure) => {
                // Surface the source-span diagnostics and error message in the
                // same shape as the no-flag path: span-attributed lines first,
                // then the failure summary. `render_frontend_diagnostics` is
                // reused so the user sees identical output to the no-flag path.
                compile::render_frontend_diagnostics(&failure.diagnostics);
                eprintln!("{}", failure.message);
                std::process::exit(1);
            }
        }
        return;
    }

    if a.explain_cow {
        // Use check_file_with_explain_cow so we can access actor_send_aliasing.
        match compile::check_explain_cow(&input, &options) {
            Ok(result) => {
                compile::render_frontend_diagnostics_pub(&result.diagnostics);
                explain_cow::render_explain_cow(
                    &result.actor_send_aliasing,
                    &result.source,
                    &input,
                    &mut std::io::stdout(),
                );
                eprintln!("{input}: OK");
            }
            Err(e) => {
                eprintln!("{e}");
                std::process::exit(1);
            }
        }
        return;
    }

    match compile::compile(&input, None, true, &options) {
        Ok(_) => {
            eprintln!("{input}: OK");
        }
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
fn cmd_debug(a: &args::DebugArgs) {
    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    let target = resolve_debug_target(options.target.as_deref());
    let artifact = compile_temp_debug_artifact(&input, &options, &target);
    let (debugger, debugger_args) = resolve_debugger_invocation(artifact.path(), &a.program_args);

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
) -> (String, Vec<String>) {
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
        ("gdb".to_string(), gdb_args)
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
        ("lldb".to_string(), lldb_args)
    } else {
        eprintln!("Error: no debugger found. Install gdb or lldb.");
        std::process::exit(1);
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
