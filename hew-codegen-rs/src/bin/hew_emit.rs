//! `hew-emit` — out-of-process LLVM object emitter for the backend.
//!
//! ## Why a separate process
//!
//! The `hew` binary builds LLVM IR in-process via the Rust `inkwell` /
//! `llvm-sys` binding. LLVM's `TargetMachine::write_to_file` still flows
//! through the legacy PassManager codepath in `LLVMTargetMachineEmitToFile`;
//! once earlier in-process LLVM setup has pre-touched the global
//! `PassRegistry`, the legacy AArch64 pass scheduler can no longer find one
//! of its required analyses and aborts via
//! `addLowerLevelRequiredPass`'s built-in trap (`brk #0x1` on aarch64).
//!
//! Standalone — in a process that starts fresh with only the helper's LLVM
//! state — the same call sequence works without complaint. This binary is
//! that standalone process: the CLI lowers MIR to textual LLVM IR in-process
//! and shells out to this helper for the object-emission step. The CLI hands
//! over a `.ll` file and a target triple; the helper parses, re-attaches the
//! data layout for the requested triple, and writes the relocatable object.
//!
//! The shape mirrors `rustc-driver`'s front-half/back-half split: the front
//! half stays in-process where it can share state with the caller, the back
//! half lives in its own address space where the global LLVM state is clean.
//!
//! ## Protocol
//!
//! ```text
//! hew-emit --triple <triple> --in <input.ll> --out <output.o>
//! ```
//!
//! Exit 0 on success. On failure: a one-line error to stderr and a non-zero
//! exit code. The CLI surfaces stderr verbatim under its
//! `E_CUTOVER_UNSUPPORTED` channel so the original LLVM message reaches the
//! user.

use std::path::PathBuf;
use std::process::ExitCode;

use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;

fn parse_args() -> Result<(String, PathBuf, PathBuf), String> {
    let mut triple: Option<String> = None;
    let mut in_path: Option<PathBuf> = None;
    let mut out_path: Option<PathBuf> = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--triple" => triple = Some(args.next().ok_or("missing value for --triple")?),
            "--in" => in_path = Some(PathBuf::from(args.next().ok_or("missing value for --in")?)),
            "--out" => {
                out_path = Some(PathBuf::from(args.next().ok_or("missing value for --out")?))
            }
            "-h" | "--help" => {
                eprintln!("usage: hew-emit --triple <T> --in <path.ll> --out <path.o>");
                std::process::exit(0);
            }
            other => return Err(format!("unknown argument: {other}")),
        }
    }
    let triple = triple.ok_or("missing required --triple")?;
    let in_path = in_path.ok_or("missing required --in")?;
    let out_path = out_path.ok_or("missing required --out")?;
    Ok((triple, in_path, out_path))
}

fn run() -> Result<(), String> {
    let (triple, in_path, out_path) = parse_args()?;

    let ctx = Context::create();
    let buf = MemoryBuffer::create_from_file(&in_path)
        .map_err(|e| format!("read {}: {e}", in_path.display()))?;
    let module = ctx
        .create_module_from_ir(buf)
        .map_err(|e| format!("parse {}: {e}", in_path.display()))?;

    if triple.starts_with("wasm32") {
        Target::initialize_webassembly(&InitializationConfig::default());
    } else {
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("initialize_native: {e}"))?;
    }
    let target_triple = TargetTriple::create(&triple);
    let target =
        Target::from_triple(&target_triple).map_err(|e| format!("from_triple({triple}): {e:?}"))?;
    let machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .ok_or_else(|| format!("create_target_machine for {triple}"))?;

    // The textual IR is emitted triple-less by the CLI; pin the triple and
    // data layout to the requested target before lowering to machine code.
    module.set_triple(&target_triple);
    module.set_data_layout(&machine.get_target_data().get_data_layout());

    machine
        .write_to_file(&module, FileType::Object, &out_path)
        .map_err(|e| format!("write_to_file {}: {e:?}", out_path.display()))?;
    Ok(())
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("hew-emit: {e}");
            ExitCode::FAILURE
        }
    }
}
