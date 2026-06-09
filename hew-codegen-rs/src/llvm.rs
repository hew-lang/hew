//! Inkwell direct LLVM IR emitter for the v0.5 backend.
//!
//! Adopted from the §10 backend probe (Track C) at HEAD `e6c83faa` on
//! `experiment/c1-backend-shootout-2026-05-13`. Both cross-ecosystem reviewers
//! signed accept on the probe; this module is the in-tree evolution of
//! `experiments/c1-backend-shootout/src/bin/probe_c.rs` against the real MIR
//! shapes in `hew-mir`.
//!
//! ## Shape
//!
//! - Two-pass lowering per module: every non-coroutine function is declared
//!   into the LLVM module before any body is lowered, populating
//!   `FnSymbolMap` so `Terminator::Call` can resolve forward references
//!   without ordering constraints in the source. Cluster 1 never constructs
//!   `Call`, but the discipline survives intact for Cluster 2/3.
//! - `place_pointer` resolves every `Place` (`Local(N)` / `ReturnSlot`) to
//!   a stack slot allocated in the function's entry block; loads and stores
//!   go through the slot. Fail-closed on unallocated locals — the emitter
//!   does not silently allocate on first use.
//! - `Module::verify()` runs after every emit; verification failure is a
//!   `CodegenError::LlvmVerify`. The CLI surfaces this as a non-zero exit.
//!
//! ## Targets
//!
//! `emit_module` emits both the host triple and `wasm32-unknown-unknown`.
//! Native emission writes a relocatable object; `cc` links it into a binary
//! at the CLI layer. WASM emission writes a relocatable `.wasm.o` and then
//! runs `wasm-ld --no-entry --export=main` (falling back to
//! `rust-lld -flavor wasm`) to produce a standalone module that
//! `wasmtime --invoke main` can instantiate directly.
//!
//! ## Two-stage emission (front-half / back-half split)
//!
//! IR construction runs in-process via inkwell — that path is safe even
//! when the caller binary has `libMLIR.dylib` loaded (as the `hew` binary
//! does via the embedded C++ codegen). Object emission via
//! `TargetMachine::write_to_file` is **not** safe under that dual-load:
//! LLVM's legacy PassManager scheduler (which the C codegen API still
//! routes through) hits an `addLowerLevelRequiredPass` trap when libMLIR
//! has pre-touched the global `PassRegistry`. The fix is structural —
//! the back half runs in its own process. `emit_module` writes the
//! textual `.ll` in-process, then spawns the sibling `hew-emit-v05`
//! helper binary to compile each requested triple to a relocatable
//! object. The helper's process loads only `libLLVM.dylib`, so the
//! legacy PM scheduler finds its analyses and `write_to_file` succeeds.
//! See `src/bin/hew_emit_v05.rs` for the helper's own module docs.
//!
//! ## Side-table audit
//!
//! No `HashMap` in this module is keyed by `SiteId`, `ExprId`, `SpanId`, or
//! `Name` with ownership-flavoured values. `FnSymbolMap` is keyed by the
//! function name (a regular module-level identity, not a checker
//! derivative); `FnCtx::locals` is keyed by `u32` local-register id (an MIR
//! identity, also not a checker derivative). The probe's audit
//! (LESSONS `audit-completeness-via-multiple-greps`) carries forward.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

use hew_mir::{
    CmpPred, ElabDrop, ElaboratedMirFunction, ExitPath, Instr, IntArithOp, IntSignedness,
    IrPipeline, Place, RawMirFunction, Terminator, TrapKind,
};
use hew_types::ResolvedTy;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::TargetMachine;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::IntPredicate;

// ---------------------------------------------------------------------------
// Error model
// ---------------------------------------------------------------------------

/// Errors raised by the LLVM IR emitter. Every variant carries enough text
/// for the CLI diagnostic surface to render a human-readable message; the
/// CLI maps each one onto a non-zero exit code (LESSONS
/// `error-count-exit-code`).
#[derive(Debug)]
pub enum CodegenError {
    /// An inkwell builder call returned an error.
    Llvm(String),
    /// The construct is outside Cluster 1's spine subset and Cluster 2/3/4
    /// will provide the proper lowering; until then the emitter rejects it
    /// loudly (LESSONS `boundary-fail-closed`).
    Unsupported(&'static str),
    /// An MIR invariant the emitter assumed was violated (e.g. a `Place`
    /// referencing a local that was never allocated).
    FailClosed(String),
    /// `Module::verify()` rejected the emitted module.
    LlvmVerify(String),
    /// `wasm-ld` / `rust-lld` invocation failed.
    Link(String),
    /// File I/O error.
    Io(std::io::Error),
    /// The program uses a runtime substrate symbol that is excluded from
    /// the wasm32 build (`hew-runtime/src/duplex.rs:54` gates the entire
    /// duplex module out via `#![cfg(not(target_arch = "wasm32"))]`).
    /// WASM-TODO(#1451): duplex WASM parity is tracked in issue #1451.
    /// Pass `--no-wasm` to skip WASM emission and produce a native binary.
    WasmUnsupportedSubstrate { symbol: String },
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Llvm(s) => write!(f, "llvm: {s}"),
            Self::Unsupported(s) => write!(f, "unsupported construct: {s}"),
            Self::FailClosed(s) => write!(f, "fail-closed: {s}"),
            Self::LlvmVerify(s) => write!(f, "llvm verify rejected module: {s}"),
            Self::Link(s) => write!(f, "link: {s}"),
            Self::Io(e) => write!(f, "io: {e}"),
            Self::WasmUnsupportedSubstrate { symbol } => write!(
                f,
                "WASM target does not support the duplex concurrency substrate \
                 (symbol: {symbol}; WASM-TODO(#1451)); pass `--no-wasm` to skip \
                 WASM emission and produce a native binary only"
            ),
        }
    }
}

impl std::error::Error for CodegenError {}

impl From<std::io::Error> for CodegenError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

type CodegenResult<T> = Result<T, CodegenError>;

// ---------------------------------------------------------------------------
// Public surface
// ---------------------------------------------------------------------------

/// Where to write the artefacts. The CLI passes a directory; the emitter
/// writes `<name>.ll`, `<name>.o`, `<name>.wasm.o`, and `<name>.wasm` into
/// it. The CLI links the native `.o` into an executable separately.
#[derive(Debug, Clone)]
pub struct EmitOptions<'a> {
    /// Module name (used as the file stem). Cluster 1 derives this from the
    /// input source path.
    pub module_name: &'a str,
    /// Output directory; must exist before calling `emit_module`.
    pub out_dir: &'a Path,
    /// Whether to emit a native object alongside the wasm artefact.
    pub native: bool,
    /// Whether to emit a wasm module (and link it with `wasm-ld`).
    pub wasm: bool,
}

/// Result of an emit: the paths of every produced artefact, for the CLI to
/// hand off to the linker / runner.
#[derive(Debug, Default, Clone)]
pub struct EmitArtefacts {
    pub ll_path: Option<std::path::PathBuf>,
    pub native_obj_path: Option<std::path::PathBuf>,
    pub wasm_obj_path: Option<std::path::PathBuf>,
    pub wasm_path: Option<std::path::PathBuf>,
}

/// Emit native + wasm artefacts for `pipeline`'s raw MIR. Returns the paths
/// of every artefact produced. Fail-closed on any verification failure.
///
/// The textual LLVM IR (`<name>.ll`) is built in-process — the
/// IR-construction path is safe under the dual `libMLIR` / `libLLVM` load
/// state that the `hew` binary runs under. Object emission shells out to
/// the `hew-emit-v05` helper (see the helper's module docs for why), so the
/// caller's binary must ship `hew-emit-v05` alongside `hew` in the same
/// directory (the workspace `cargo build` produces both into `target/<profile>/`).
///
/// # Errors
///
/// Returns `CodegenError` if any function declares an unsupported construct,
/// `Module::verify()` rejects the emitted IR, the helper binary cannot be
/// located, the helper exits non-zero, or `wasm-ld` fails.
pub fn emit_module(
    pipeline: &IrPipeline,
    options: &EmitOptions<'_>,
) -> CodegenResult<EmitArtefacts> {
    std::fs::create_dir_all(options.out_dir)?;
    let mut artefacts = EmitArtefacts::default();

    // Build the textual IR once and reuse it for both targets. `.ll` is also
    // the cheapest forensic artefact — `opt -passes=verify` runs against it
    // directly without re-deriving anything.
    let ll_path = options.out_dir.join(format!("{}.ll", options.module_name));
    emit_textual(pipeline, options.module_name, &ll_path)?;
    artefacts.ll_path = Some(ll_path.clone());

    if options.native {
        let obj_path = options.out_dir.join(format!("{}.o", options.module_name));
        let host_triple = TargetMachine::get_default_triple();
        let triple_str = host_triple.as_str().to_string_lossy().into_owned();
        run_emit_helper(&ll_path, &triple_str, &obj_path)?;
        artefacts.native_obj_path = Some(obj_path);
    }

    if options.wasm {
        // Fail-closed before invoking `wasm-ld`: detect any reference to
        // duplex substrate symbols.  `hew-runtime/src/duplex.rs:54` gates
        // the entire duplex module out of wasm32 builds via
        // `#![cfg(not(target_arch = "wasm32"))]`, so `wasm-ld` would fail
        // with `undefined symbol: hew_duplex_*`.  Surface a structured
        // diagnostic with a `--no-wasm` pointer rather than a raw linker
        // error.  WASM-TODO(#1451): duplex WASM parity is tracked there.
        // LESSONS: boundary-fail-closed (P0), user-surface-correctness (P0).
        if let Some(symbol) = uses_wasm_excluded_symbol(pipeline) {
            return Err(CodegenError::WasmUnsupportedSubstrate { symbol });
        }

        let wasm_obj_path = options
            .out_dir
            .join(format!("{}.wasm.o", options.module_name));
        run_emit_helper(&ll_path, "wasm32-unknown-unknown", &wasm_obj_path)?;
        let wasm_path = options
            .out_dir
            .join(format!("{}.wasm", options.module_name));
        link_wasm_module(&wasm_obj_path, &wasm_path)?;
        artefacts.wasm_obj_path = Some(wasm_obj_path);
        artefacts.wasm_path = Some(wasm_path);
    }

    Ok(artefacts)
}

/// Return the first duplex substrate symbol found in `pipeline`'s instruction
/// stream, or `None` if no such symbol is present.
///
/// Duplex symbols (`hew_duplex_*`) are excluded from wasm32 builds via
/// `hew-runtime/src/duplex.rs:54` (`#![cfg(not(target_arch = "wasm32"))]`).
/// This scan detects them in the MIR before the `wasm-ld` step so the caller
/// can return `CodegenError::WasmUnsupportedSubstrate` instead of a confusing
/// linker error.  WASM-TODO(#1451).
///
/// The scan covers:
/// - `Instr::CallRuntimeAbi` with a symbol that starts with `"hew_duplex_"`.
/// - `Instr::Drop { drop_fn: Some(fn_name), .. }` in raw_mir where `fn_name`
///   starts with `"hew_duplex_"` (e.g. `hew_duplex_close`).
/// - `ElabDrop { drop_fn: Some(name), .. }` in `elaborated_mir.drop_plans`
///   where the resolved C-ABI symbol starts with `"hew_duplex_"`. This covers
///   elaborator-produced `"Duplex::close"` strings that resolve to
///   `hew_duplex_close` at codegen time.
fn uses_wasm_excluded_symbol(pipeline: &IrPipeline) -> Option<String> {
    for func in &pipeline.raw_mir {
        for block in &func.blocks {
            for instr in &block.instructions {
                let excluded = match instr {
                    Instr::CallRuntimeAbi(call) => {
                        let sym = call.symbol();
                        sym.starts_with("hew_duplex_").then(|| sym.to_string())
                    }
                    Instr::Drop {
                        drop_fn: Some(fn_name),
                        ..
                    } => fn_name.starts_with("hew_duplex_").then(|| fn_name.clone()),
                    _ => None,
                };
                if let Some(sym) = excluded {
                    return Some(sym);
                }
            }
        }
    }
    // Also scan elaborated_mir drop_plans: the real close path goes through
    // here now that codegen consumes drop_plans. Resolve the drop_fn string
    // to its C-ABI symbol and check for hew_duplex_ exclusion.
    for elab_func in &pipeline.elaborated_mir {
        for (_, plan) in &elab_func.drop_plans {
            for drop in &plan.drops {
                if let Some(ref drop_fn) = drop.drop_fn {
                    // Use resolve_drop_fn_to_symbol to get the actual C-ABI
                    // symbol; skip on FailClosed (unknown names are not wasm
                    // exclusion candidates — they'll fail at codegen instead).
                    if let Ok(sym) = resolve_drop_fn_to_symbol(drop_fn) {
                        if sym.starts_with("hew_duplex_") {
                            return Some(sym.to_string());
                        }
                    }
                }
            }
        }
    }
    None
}

/// Locate the `hew-emit-v05` helper binary sibling to the currently running
/// executable, then invoke it to compile `ll_path` for `triple` into
/// `out_path`.
///
/// The rustc-driver-style discovery (sibling of `current_exe`) handles both
/// the dev layout (`target/debug/hew` next to `target/debug/hew-emit-v05`)
/// and any future install layout where the two binaries share a `bin/`.
fn run_emit_helper(ll_path: &Path, triple: &str, out_path: &Path) -> CodegenResult<()> {
    let helper = locate_emit_helper()?;
    let output = Command::new(&helper)
        .arg("--triple")
        .arg(triple)
        .arg("--in")
        .arg(ll_path)
        .arg("--out")
        .arg(out_path)
        .output()
        .map_err(|e| CodegenError::Llvm(format!("spawn {}: {e}", helper.display())))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(CodegenError::Llvm(format!(
            "hew-emit-v05 ({:?}) failed for triple={triple} out={}: {}",
            output.status,
            out_path.display(),
            stderr.trim()
        )));
    }
    Ok(())
}

fn locate_emit_helper() -> CodegenResult<std::path::PathBuf> {
    let exe =
        std::env::current_exe().map_err(|e| CodegenError::Llvm(format!("current_exe: {e}")))?;
    let dir = exe.parent().ok_or_else(|| {
        CodegenError::Llvm(format!(
            "current exe `{}` has no parent directory",
            exe.display()
        ))
    })?;
    let name = if cfg!(windows) {
        "hew-emit-v05.exe"
    } else {
        "hew-emit-v05"
    };
    let candidate = dir.join(name);
    if !candidate.exists() {
        return Err(CodegenError::Llvm(format!(
            "helper binary `{}` not found next to `{}`; \
             ensure `cargo build -p hew-codegen-rs --bin hew-emit-v05` has run",
            candidate.display(),
            exe.display()
        )));
    }
    Ok(candidate)
}

// ---------------------------------------------------------------------------
// Per-function lowering state
// ---------------------------------------------------------------------------

struct FnCtx<'a, 'ctx> {
    ctx: &'ctx Context,
    llvm_mod: &'a LlvmModule<'ctx>,
    builder: Builder<'ctx>,
    return_slot: PointerValue<'ctx>,
    return_ty: BasicTypeEnum<'ctx>,
    /// Local-register id → (stack slot, slot's LLVM type). Keyed by the
    /// `Place::Local(N)` index — an MIR identity, not a checker derivative.
    locals: HashMap<u32, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
    /// Block id → LLVM `BasicBlock`. Populated up front so terminators can
    /// name forward targets.
    blocks: HashMap<u32, inkwell::basic_block::BasicBlock<'ctx>>,
    /// Module-wide runtime-ABI extern declarations interned on first use.
    /// Carried in a `RefCell` so the per-instruction lowering (which only
    /// borrows `FnCtx` immutably) can register new declarations lazily
    /// from inside `Instr::CallRuntimeAbi` and `Instr::Drop` arms.
    runtime_decls: RefCell<RuntimeDeclMap<'ctx>>,
}

/// Module-level symbol table populated by the declaration pass. Keyed by
/// function name (a normal module-level identity); the value carries the
/// LLVM function value plus its return type so `Terminator::Call` can
/// resolve forward without re-deriving anything from the MIR shape.
type FnSymbolMap<'ctx> = HashMap<String, (FunctionValue<'ctx>, BasicTypeEnum<'ctx>)>;

/// Lazily-interned C-ABI extern function declarations for runtime symbols
/// referenced by `Instr::CallRuntimeAbi` and `Instr::Drop`. Keyed by the
/// C-ABI symbol name (already validated by `RuntimeCall::new`'s allowlist
/// check). One declaration per symbol per module — repeat references at
/// multiple call sites share the same LLVM `declare` line.
type RuntimeDeclMap<'ctx> = HashMap<String, FunctionValue<'ctx>>;

/// Intern a runtime-ABI function declaration for `symbol` in `llvm_mod`.
///
/// Returns the cached `FunctionValue` on repeat lookups so multiple
/// `Instr::CallRuntimeAbi` sites share one `declare` line. The signature
/// table is hard-coded from `hew-runtime/src/duplex.rs` and pinned by the
/// E4 plan §D1-D3 (revised 2026-05-15). All three signatures use 64-bit
/// integer widths for `usize`-typed args; the spine subset is 64-bit-only
/// by design and wasm32 surfaces a width mismatch handled at the E5c
/// `--no-wasm` seam, not here.
///
/// Unknown symbols return `FailClosed`. The `RuntimeCall::new` allowlist
/// guard ensures construction-time rejection of unknown names, so reaching
/// this arm for an unknown symbol is an internal-consistency violation,
/// not a user error — fail loudly. LESSONS: boundary-fail-closed,
/// exhaustive-coverage.
fn intern_runtime_decl<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    decls: &mut RuntimeDeclMap<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(fv) = decls.get(symbol) {
        return Ok(*fv);
    }
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let fn_ty = match symbol {
        // hew_duplex_pair(s_cap: usize, r_cap: usize,
        //                 out_a: *mut *mut HewDuplexHandle,
        //                 out_b: *mut *mut HewDuplexHandle) -> i32
        // (`hew-runtime/src/duplex.rs:644-649`). usize ≡ i64 on 64-bit.
        "hew_duplex_pair" => i32_ty.fn_type(
            &[i64_ty.into(), i64_ty.into(), ptr_ty.into(), ptr_ty.into()],
            false,
        ),
        // hew_duplex_send(d: *mut HewDuplexHandle, msg: *const u8,
        //                 len: usize) -> i32
        // (`hew-runtime/src/duplex.rs:689-693`).
        "hew_duplex_send" => i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), i64_ty.into()], false),
        // hew_duplex_close(d: *mut HewDuplexHandle) -> i32
        // (`hew-runtime/src/duplex.rs:992`). Result discarded at the
        // Drop call site; the runtime's AtomicBool double-close guard
        // makes re-entry safe.
        "hew_duplex_close" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // hew_lambda_actor_release(actor: *mut HewLambdaActorHandle) -> i32
        // (`hew-runtime/src/lambda_actor.rs:411`). Same signature shape
        // as hew_duplex_close — one ptr arg, i32 result discarded.
        "hew_lambda_actor_release" => i32_ty.fn_type(&[ptr_ty.into()], false),
        other => {
            return Err(CodegenError::FailClosed(format!(
                "intern_runtime_decl: codegen has no LLVM signature for runtime \
                 symbol {other:?}; the symbol is on the M2_RUNTIME_SYMBOLS allowlist \
                 but no codegen arm wires it — extend the signature table or leave \
                 the producer fail-closed"
            )));
        }
    };
    let fv = llvm_mod.add_function(symbol, fn_ty, Some(Linkage::External));
    decls.insert(symbol.to_string(), fv);
    Ok(fv)
}

// ---------------------------------------------------------------------------
// Type mapping
// ---------------------------------------------------------------------------

fn primitive_to_llvm<'ctx>(
    ctx: &'ctx Context,
    ty: &ResolvedTy,
) -> CodegenResult<BasicTypeEnum<'ctx>> {
    match ty {
        ResolvedTy::I8 | ResolvedTy::U8 => Ok(ctx.i8_type().into()),
        ResolvedTy::I16 | ResolvedTy::U16 => Ok(ctx.i16_type().into()),
        ResolvedTy::I32 | ResolvedTy::U32 => Ok(ctx.i32_type().into()),
        ResolvedTy::I64 | ResolvedTy::U64 => Ok(ctx.i64_type().into()),
        // Platform-sized integers: 32-bit on WASM32, 64-bit on native
        // (Q42 ratification; B-D1).  The spine currently targets native
        // only (see llvm.rs `intern_runtime_decl` comment at line ~394);
        // WASM32 lowers separately via the `hew_emit_v05` binary's
        // `--target wasm32-unknown-unknown` path, which calls back into
        // this same function.  The target data layout recorded on the
        // LLVM module by `hew_emit_v05.rs:107` ensures `i32` is
        // pointer-sized on wasm32 and `i64` on native.
        //
        // SHIM: pointer-width selection here is compile-time via the Rust
        // usize width.  Replace with target-triple inspection once a
        // multi-target codegen path (E5c) lands and `primitive_to_llvm`
        // receives target information.
        // WHY: Rust's usize matches the host pointer size; the LLVM
        //      backend then maps to the correct machine width.
        // WHEN: obsolete once E5c supplies explicit target info.
        // WHAT: query `module.get_target_data().get_pointer_byte_size(None)`
        //       and branch on `== 4`.
        #[cfg(target_pointer_width = "32")]
        ResolvedTy::Isize | ResolvedTy::Usize => Ok(ctx.i32_type().into()),
        #[cfg(not(target_pointer_width = "32"))]
        ResolvedTy::Isize | ResolvedTy::Usize => Ok(ctx.i64_type().into()),
        ResolvedTy::Bool => Ok(ctx.i8_type().into()),
        // `Unit` lowers to a zero-sized stand-in (i8); the binary that
        // consumes it never observes the value (returning unit on Unix
        // simply discards the call result).
        ResolvedTy::Unit => Ok(ctx.i8_type().into()),
        ResolvedTy::F32 => Ok(ctx.f32_type().into()),
        ResolvedTy::F64 => Ok(ctx.f64_type().into()),
        // Cluster 1's spine subset is integer-only; heap and composite
        // types belong to Cluster 2/3/4. Fail closed so an unexpected
        // shape doesn't silently produce a malformed binary
        // (LESSONS `boundary-fail-closed`).
        ResolvedTy::String => Err(CodegenError::Unsupported(
            "String type — Cluster 2 will add owned-string lowering",
        )),
        ResolvedTy::Char => Err(CodegenError::Unsupported(
            "Char type — Cluster 2 lowering pending",
        )),
        ResolvedTy::Bytes => Err(CodegenError::Unsupported(
            "Bytes type — Cluster 2 lowering pending",
        )),
        ResolvedTy::Duration => Err(CodegenError::Unsupported(
            "Duration type — Cluster 2 lowering pending",
        )),
        ResolvedTy::Never => Err(CodegenError::Unsupported(
            "Never type cannot occur in a value-bearing position",
        )),
        ResolvedTy::Tuple(_) => Err(CodegenError::Unsupported(
            "Tuple type — composite lowering is Cluster 2",
        )),
        ResolvedTy::Array(_, _) => Err(CodegenError::Unsupported(
            "Array type — composite lowering is Cluster 2",
        )),
        ResolvedTy::Slice(_) => Err(CodegenError::Unsupported(
            "Slice type — composite lowering is Cluster 2",
        )),
        ResolvedTy::Named { name, .. } if name == "Duplex" => {
            // M2 substrate duplex handle. The producer (`hew-mir/src/lower.rs`
            // `lower_duplex_pair` + `lower_duplex_send`) allocates a
            // `ResolvedTy::Named { name: "Duplex", .. }` local for every
            // duplex handle; codegen materialises that slot as a raw opaque
            // `ptr` (an `*mut HewDuplexHandle` in the runtime C-ABI). The
            // `hew_duplex_pair` out-param protocol writes through this
            // alloca; `hew_duplex_send` loads from it. LESSONS:
            // exhaustive-traversal-and-lowering, boundary-fail-closed.
            Ok(ctx.ptr_type(AddressSpace::default()).into())
        }
        ResolvedTy::Named { name, .. } => Err(CodegenError::FailClosed(format!(
            "D10 violation: Named/user type `{name}` reached the LLVM emitter; \
             the MIR D10 gate should have rejected this earlier"
        ))),
        ResolvedTy::Function { .. } => Err(CodegenError::Unsupported(
            "Function type — first-class fn values are Cluster 4 (closure cluster)",
        )),
        ResolvedTy::Closure { .. } => Err(CodegenError::Unsupported(
            "Closure type — Cluster 4 lowering",
        )),
        ResolvedTy::Pointer { .. } => Err(CodegenError::Unsupported(
            "Pointer type — explicit pointers are out of the spine subset",
        )),
        ResolvedTy::TraitObject { .. } => Err(CodegenError::Unsupported(
            "TraitObject type — Cluster 4 lowering",
        )),
        // Task<T> lowers to a `*mut HewTask` pointer (i64* on the spine).
        // Full task-spawn ABI lowering lands in a later slice (MIR/codegen
        // glue: hew_task_new + hew_task_spawn_thread + hew_task_free). Until
        // that slice lands, reaching this arm is a codegen error — a
        // Task<T>-typed value cannot be emitted by the spine subset.
        ResolvedTy::Task(_) => Err(CodegenError::Unsupported(
            "Task<T> type — task-spawn lowering lands in a later slice",
        )),
    }
}

// ---------------------------------------------------------------------------
// Per-instruction lowering
// ---------------------------------------------------------------------------

fn place_pointer<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    place: Place,
) -> CodegenResult<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
    match place {
        Place::Local(id) => fn_ctx.locals.get(&id).copied().ok_or_else(|| {
            CodegenError::FailClosed(format!("local {id} not allocated before use"))
        }),
        Place::ReturnSlot => Ok((fn_ctx.return_slot, fn_ctx.return_ty)),
        // M2 substrate: `Place::DuplexHandle(N)` addresses the same
        // backing alloca as `Place::Local(N)`. The producer
        // (`hew-mir/src/lower.rs::lower_duplex_pair` and
        // `lower_duplex_send`) allocates the underlying local with
        // `ResolvedTy::Named { name: "Duplex", .. }` and then re-tags
        // the place as `DuplexHandle(N)` using the same `N` so codegen
        // can disambiguate the C-ABI shape (`hew_duplex_pair` writes
        // through `&alloca`, `hew_duplex_send` loads from it) without a
        // second alloca. The alloca's LLVM type is `ptr` (opaque) per
        // `primitive_to_llvm`'s Duplex arm. LESSONS:
        // exhaustive-traversal-and-lowering, dedup-semantic-boundary.
        Place::DuplexHandle(id) => fn_ctx.locals.get(&id).copied().ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "Place::DuplexHandle({id}) references local {id} which was not allocated; \
                 the producer must allocate a ResolvedTy::Named{{name:\"Duplex\",..}} local before \
                 re-tagging it as a DuplexHandle"
            ))
        }),
        // The remaining M2 substrate Place variants are declared in
        // the model but have no construction surface in this lane.
        // Wiring lands in follow-on slices (Duplex::recv vertebra for
        // SendHalf/RecvHalf; lambda-actor lane for LambdaActorHandle).
        // Fail closed here so any reach-through surfaces immediately
        // rather than emitting a binary with a misrouted load/store.
        // LESSONS: boundary-fail-closed, parity-or-tracked-gap.
        Place::LambdaActorHandle(_) | Place::SendHalf(_) | Place::RecvHalf(_) => {
            Err(CodegenError::FailClosed(
                "M2 SendHalf / RecvHalf / LambdaActorHandle Place lowering is not yet wired; \
                 follow-on Duplex::recv + lambda-actor lanes wire the remaining C-ABI surface"
                    .to_string(),
            ))
        }
    }
}

fn lower_instruction(fn_ctx: &FnCtx<'_, '_>, instr: &Instr) -> CodegenResult<()> {
    let ctx = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent().map(|f| f.get_type().get_context()));
    // The builder is positioned inside a basic block before this is called,
    // so the unwrap can't fire; the parent function always exists.
    let ctx = ctx.ok_or_else(|| {
        CodegenError::FailClosed("builder is not positioned in a basic block".into())
    })?;

    match instr {
        Instr::ConstI64 { dest, value } => {
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let int_ty = match dest_ty {
                BasicTypeEnum::IntType(i) => i,
                _ => {
                    return Err(CodegenError::FailClosed(format!(
                        "ConstI64 dest is not an int: dest_ty={dest_ty:?}"
                    )))
                }
            };
            // `value` is i64 in MIR. Truncate/extend at the LLVM level to
            // whatever the dest local actually is — Cluster 1's lowering
            // currently always sizes locals to the front-half's int type
            // (`i64` for `int`), so this is a no-op for the spine.
            #[allow(clippy::cast_sign_loss)]
            let v = int_ty.const_int(*value as u64, true);
            fn_ctx
                .builder
                .build_store(dest_ptr, v)
                .map_err(|e| CodegenError::Llvm(format!("store i64: {e:?}")))?;
        }
        Instr::IntAdd { dest, lhs, rhs }
        | Instr::IntSub { dest, lhs, rhs }
        | Instr::IntMul { dest, lhs, rhs } => {
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let lhs_int = match lhs_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => return Err(CodegenError::FailClosed("IntAdd lhs is not an int".into())),
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "Int arithmetic operands and dest must share the same int type".into(),
                ));
            }
            let lhs_v = fn_ctx
                .builder
                .build_load(lhs_int, lhs_ptr, "arith_lhs")
                .map_err(|e| CodegenError::Llvm(format!("arith lhs load: {e:?}")))?
                .into_int_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_int, rhs_ptr, "arith_rhs")
                .map_err(|e| CodegenError::Llvm(format!("arith rhs load: {e:?}")))?
                .into_int_value();
            let result = match instr {
                Instr::IntAdd { .. } => fn_ctx.builder.build_int_add(lhs_v, rhs_v, "arith_add"),
                Instr::IntSub { .. } => fn_ctx.builder.build_int_sub(lhs_v, rhs_v, "arith_sub"),
                Instr::IntMul { .. } => fn_ctx.builder.build_int_mul(lhs_v, rhs_v, "arith_mul"),
                _ => unreachable!("matched on three int-arith variants above"),
            }
            .map_err(|e| CodegenError::Llvm(format!("int arith: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result)
                .map_err(|e| CodegenError::Llvm(format!("arith store: {e:?}")))?;
        }
        // B-5: division and remainder. These are the post-check instructions —
        // the divisor-zero and signed-MIN/-1 guard branches are emitted by
        // the MIR producer (`lower_div_rem`); by the time execution reaches
        // here the divisor is proven non-zero and non-MIN/-1.
        Instr::IntDiv {
            signed,
            dest,
            lhs,
            rhs,
        }
        | Instr::IntRem {
            signed,
            dest,
            lhs,
            rhs,
        } => {
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let lhs_int = match lhs_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IntDiv/IntRem lhs is not an int".into(),
                    ))
                }
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntDiv/IntRem operands and dest must share the same int type".into(),
                ));
            }
            let lhs_v = fn_ctx
                .builder
                .build_load(lhs_int, lhs_ptr, "div_lhs")
                .map_err(|e| CodegenError::Llvm(format!("div lhs load: {e:?}")))?
                .into_int_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_int, rhs_ptr, "div_rhs")
                .map_err(|e| CodegenError::Llvm(format!("div rhs load: {e:?}")))?
                .into_int_value();
            let result = match (instr, signed) {
                (Instr::IntDiv { .. }, IntSignedness::Signed) => {
                    fn_ctx.builder.build_int_signed_div(lhs_v, rhs_v, "sdiv")
                }
                (Instr::IntDiv { .. }, IntSignedness::Unsigned) => {
                    fn_ctx.builder.build_int_unsigned_div(lhs_v, rhs_v, "udiv")
                }
                (Instr::IntRem { .. }, IntSignedness::Signed) => {
                    fn_ctx.builder.build_int_signed_rem(lhs_v, rhs_v, "srem")
                }
                (Instr::IntRem { .. }, IntSignedness::Unsigned) => {
                    fn_ctx.builder.build_int_unsigned_rem(lhs_v, rhs_v, "urem")
                }
                _ => unreachable!("matched on IntDiv / IntRem above"),
            }
            .map_err(|e| CodegenError::Llvm(format!("div/rem: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result)
                .map_err(|e| CodegenError::Llvm(format!("div/rem store: {e:?}")))?;
        }
        // B-5: shift instructions. The out-of-range guard is emitted by the
        // MIR producer (`lower_shift`); by the time execution reaches here the
        // shift count is proven to be in `[0, width)`.
        Instr::IntShl { dest, lhs, rhs } => {
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let lhs_int = match lhs_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => return Err(CodegenError::FailClosed("IntShl lhs is not an int".into())),
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntShl operands and dest must share the same int type".into(),
                ));
            }
            let lhs_v = fn_ctx
                .builder
                .build_load(lhs_int, lhs_ptr, "shl_lhs")
                .map_err(|e| CodegenError::Llvm(format!("shl lhs load: {e:?}")))?
                .into_int_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_int, rhs_ptr, "shl_rhs")
                .map_err(|e| CodegenError::Llvm(format!("shl rhs load: {e:?}")))?
                .into_int_value();
            let result = fn_ctx
                .builder
                .build_left_shift(lhs_v, rhs_v, "shl")
                .map_err(|e| CodegenError::Llvm(format!("shl: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result)
                .map_err(|e| CodegenError::Llvm(format!("shl store: {e:?}")))?;
        }
        Instr::IntShr {
            signed,
            dest,
            lhs,
            rhs,
        } => {
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let lhs_int = match lhs_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => return Err(CodegenError::FailClosed("IntShr lhs is not an int".into())),
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntShr operands and dest must share the same int type".into(),
                ));
            }
            let lhs_v = fn_ctx
                .builder
                .build_load(lhs_int, lhs_ptr, "shr_lhs")
                .map_err(|e| CodegenError::Llvm(format!("shr lhs load: {e:?}")))?
                .into_int_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_int, rhs_ptr, "shr_rhs")
                .map_err(|e| CodegenError::Llvm(format!("shr rhs load: {e:?}")))?
                .into_int_value();
            // Arithmetic right shift for signed types (preserves sign),
            // logical right shift for unsigned types (zero-fills).
            let result = match signed {
                IntSignedness::Signed => {
                    fn_ctx.builder.build_right_shift(lhs_v, rhs_v, true, "ashr")
                }
                IntSignedness::Unsigned => fn_ctx
                    .builder
                    .build_right_shift(lhs_v, rhs_v, false, "lshr"),
            }
            .map_err(|e| CodegenError::Llvm(format!("shr: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result)
                .map_err(|e| CodegenError::Llvm(format!("shr store: {e:?}")))?;
        }
        Instr::IntArithChecked {
            op,
            signed,
            dest,
            lhs,
            rhs,
            overflow_flag,
        } => {
            // B-2 overflow-trap lowering. Emit:
            //   %r = call {iN, i1} @llvm.{s,u}{add,sub,mul}.with.overflow.iN(%a, %b)
            //   %v = extractvalue {iN, i1} %r, 0    ; store into `dest`
            //   %of = extractvalue {iN, i1} %r, 1   ; widen + store into `overflow_flag`
            //
            // The MIR producer (`lower::lower_binary`) seals the
            // current block with `Terminator::Branch { cond:
            // overflow_flag, then_target: trap_bb, else_target:
            // cont_bb }`; the trap block carries `Terminator::Trap`
            // and the continuation block is where subsequent
            // arithmetic continues. The branch and trap terminators
            // are lowered by the existing `Terminator` arms below;
            // this arm only emits the intrinsic call + extracts.
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let (flag_ptr, flag_ty) = place_pointer(fn_ctx, *overflow_flag)?;
            let lhs_int = match lhs_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IntArithChecked lhs is not an int".into(),
                    ))
                }
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntArithChecked operands and dest must share the same int type".into(),
                ));
            }
            let flag_int = match flag_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IntArithChecked overflow_flag is not an int".into(),
                    ))
                }
            };
            // Choose the intrinsic family by op + signedness. Six
            // intrinsics total — three ops × two signednesses. The
            // overload is per integer width, so `get_declaration`
            // receives the operand int type.
            let intrinsic_name = match (op, signed) {
                (IntArithOp::Add, IntSignedness::Signed) => "llvm.sadd.with.overflow",
                (IntArithOp::Add, IntSignedness::Unsigned) => "llvm.uadd.with.overflow",
                (IntArithOp::Sub, IntSignedness::Signed) => "llvm.ssub.with.overflow",
                (IntArithOp::Sub, IntSignedness::Unsigned) => "llvm.usub.with.overflow",
                (IntArithOp::Mul, IntSignedness::Signed) => "llvm.smul.with.overflow",
                (IntArithOp::Mul, IntSignedness::Unsigned) => "llvm.umul.with.overflow",
            };
            let intrinsic = Intrinsic::find(intrinsic_name).ok_or_else(|| {
                CodegenError::Llvm(format!(
                    "with-overflow intrinsic `{intrinsic_name}` not found in LLVM build"
                ))
            })?;
            let intrinsic_fn = intrinsic
                .get_declaration(fn_ctx.llvm_mod, &[lhs_int.into()])
                .ok_or_else(|| {
                    CodegenError::Llvm(format!(
                        "with-overflow intrinsic `{intrinsic_name}` declaration failed for width {lhs_int:?}"
                    ))
                })?;
            let lhs_v = fn_ctx
                .builder
                .build_load(lhs_int, lhs_ptr, "checked_lhs")
                .map_err(|e| CodegenError::Llvm(format!("checked lhs load: {e:?}")))?
                .into_int_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_int, rhs_ptr, "checked_rhs")
                .map_err(|e| CodegenError::Llvm(format!("checked rhs load: {e:?}")))?
                .into_int_value();
            let call_site = fn_ctx
                .builder
                .build_call(intrinsic_fn, &[lhs_v.into(), rhs_v.into()], "with_overflow")
                .map_err(|e| CodegenError::Llvm(format!("with-overflow call: {e:?}")))?;
            let agg = call_site
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::Llvm(format!(
                        "with-overflow intrinsic `{intrinsic_name}` returned void"
                    ))
                })?
                .into_struct_value();
            let result_v = fn_ctx
                .builder
                .build_extract_value(agg, 0, "checked_result")
                .map_err(|e| CodegenError::Llvm(format!("extractvalue result: {e:?}")))?
                .into_int_value();
            let of_bit = fn_ctx
                .builder
                .build_extract_value(agg, 1, "checked_overflow")
                .map_err(|e| CodegenError::Llvm(format!("extractvalue flag: {e:?}")))?
                .into_int_value();
            // The overflow flag is an i1; the MIR-allocated slot is a
            // `ResolvedTy::Bool` (i8 in LLVM lowering). Widen so the
            // Branch terminator's load reads a non-zero byte when the
            // flag is set.
            let of_widened = fn_ctx
                .builder
                .build_int_z_extend_or_bit_cast(of_bit, flag_int, "checked_overflow_widen")
                .map_err(|e| CodegenError::Llvm(format!("flag zext: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result_v)
                .map_err(|e| CodegenError::Llvm(format!("checked result store: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(flag_ptr, of_widened)
                .map_err(|e| CodegenError::Llvm(format!("checked flag store: {e:?}")))?;
            let _ = ctx;
        }
        Instr::IntCmp {
            dest,
            pred,
            lhs,
            rhs,
        } => {
            // Load both operands at their declared int type, compare with
            // the predicate, zero-extend the i1 result to the dest's
            // stored width. The dest's type is whatever HIR resolved for
            // the comparison expression (today `ResolvedTy::Bool` -> i8
            // per `primitive_to_llvm`); a later real `bool` type just
            // narrows the dest width without changing this lowering.
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let lhs_int = match lhs_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => return Err(CodegenError::FailClosed("IntCmp lhs is not an int".into())),
            };
            if rhs_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntCmp operands must share the same int type".into(),
                ));
            }
            let dest_int = match dest_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => return Err(CodegenError::FailClosed("IntCmp dest is not an int".into())),
            };
            let lhs_v = fn_ctx
                .builder
                .build_load(lhs_int, lhs_ptr, "cmp_lhs")
                .map_err(|e| CodegenError::Llvm(format!("cmp lhs load: {e:?}")))?
                .into_int_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_int, rhs_ptr, "cmp_rhs")
                .map_err(|e| CodegenError::Llvm(format!("cmp rhs load: {e:?}")))?
                .into_int_value();
            let llvm_pred = match pred {
                CmpPred::Eq => IntPredicate::EQ,
                CmpPred::NotEq => IntPredicate::NE,
                CmpPred::SignedLess => IntPredicate::SLT,
                CmpPred::SignedLessEq => IntPredicate::SLE,
                CmpPred::SignedGreater => IntPredicate::SGT,
                CmpPred::SignedGreaterEq => IntPredicate::SGE,
                // B-5: shift-range check reinterprets operands as unsigned.
                // Catches both negative counts (wrap to large unsigned) and
                // counts ≥ bit-width in one compare.
                CmpPred::UnsignedGreaterEq => IntPredicate::UGE,
            };
            let bit = fn_ctx
                .builder
                .build_int_compare(llvm_pred, lhs_v, rhs_v, "cmp_bit")
                .map_err(|e| CodegenError::Llvm(format!("icmp: {e:?}")))?;
            let widened = fn_ctx
                .builder
                .build_int_z_extend_or_bit_cast(bit, dest_int, "cmp_zext")
                .map_err(|e| CodegenError::Llvm(format!("cmp zext: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .map_err(|e| CodegenError::Llvm(format!("cmp store: {e:?}")))?;
            let _ = ctx;
        }
        Instr::Move { dest, src } => {
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let (src_ptr, src_ty) = place_pointer(fn_ctx, *src)?;
            if dest_ty != src_ty {
                return Err(CodegenError::FailClosed(format!(
                    "Move type mismatch: src={src_ty:?} dest={dest_ty:?}"
                )));
            }
            let loaded = fn_ctx
                .builder
                .build_load(src_ty, src_ptr, "move_load")
                .map_err(|e| CodegenError::Llvm(format!("move load: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, loaded)
                .map_err(|e| CodegenError::Llvm(format!("move store: {e:?}")))?;
        }
        Instr::CallRuntimeAbi(call) => {
            // Per-symbol C-ABI lowering. The `RuntimeCall::new`
            // allowlist guard ensures `call.symbol()` is one of the
            // M2 substrate symbols; codegen disambiguates the ABI
            // shape (especially the `Place::DuplexHandle(N)`
            // address-of vs load-pointer choice) by symbol + arg
            // index. Symbols on the allowlist but not yet wired
            // remain fail-closed below — `parity-or-tracked-gap`.
            // LESSONS: boundary-fail-closed, exhaustive-coverage,
            // checker-output-boundary, dedup-semantic-boundary.
            lower_call_runtime_abi(fn_ctx, call)?;
        }
        Instr::Drop {
            place,
            ty: _,
            drop_fn,
        } => {
            // Drop ritual. `drop_fn: None` is a legitimate no-op for
            // `@linear` (move-checker proof elsewhere) and for value
            // classes with no implicit close. `drop_fn: Some(name)`
            // dispatches on the literal close-symbol string.
            //
            // The two emitter-known close protocols today are the
            // M2 substrate's `hew_duplex_close` (direct C-ABI symbol
            // name) and the runtime substrate's per-`@resource`
            // close. The runtime side is wired via the C-ABI symbol
            // string; the per-`@resource` path (drop_fn shaped like
            // `"<TypeName>::close"`, produced by the elaborator at
            // `hew-mir/src/lower.rs::build_lifo_drops` line ~2512)
            // is **not yet reachable from real lowering** because
            // codegen consumes `raw_mir`, not `elaborated_mir`;
            // wiring `drop_plans` is a follow-on seam. Until then,
            // a `"<TypeName>::close"` drop_fn remains fail-closed
            // with a tracked-gap message. LESSONS:
            // boundary-fail-closed, parity-or-tracked-gap,
            // cleanup-all-exits, raii-null-after-move.
            if let Some(name) = drop_fn {
                lower_drop(fn_ctx, *place, name)?;
            }
            let _ = ctx;
        }
    }
    Ok(())
}

/// Lower `Instr::CallRuntimeAbi(call)` to a `LLVMBuildCall` against the
/// interned extern declaration for `call.symbol()`. The per-symbol
/// dispatch encodes the ABI-shape decisions documented in the E4 plan
/// (SHIM(E4) comment in `hew-mir/src/lower.rs` ~1220):
///
/// - `hew_duplex_pair(i64, i64, ptr, ptr) -> i32`: args[0]/args[1] load
///   capacities; args[2]/args[3] are `Place::DuplexHandle(N)` and pass
///   the *address of* local-N's alloca (a `*mut *mut HewDuplexHandle`
///   the runtime writes through). Return discarded.
/// - `hew_duplex_send(ptr, ptr, i64) -> i32`: args[0] is the receiver
///   `Place::DuplexHandle(N)` — load the raw `*mut HewDuplexHandle`
///   from local-N's alloca. args[1] is the message `Place::Local(M)`
///   — spill its value into a fresh i64 stack slot and pass the slot's
///   address as `*const u8`. args[2] is the length `Place::Local(L)`
///   carrying `ConstI64 { value: 8 }` from the producer; load as i64.
///   Return discarded.
///
/// Symbols on the M2 allowlist but not yet wired (e.g.
/// `hew_lambda_actor_release`) fall through to a fail-closed arm so
/// the producer surface and the codegen surface stay in lock-step.
fn lower_call_runtime_abi(
    fn_ctx: &FnCtx<'_, '_>,
    call: &hew_mir::RuntimeCall,
) -> CodegenResult<()> {
    let symbol = call.symbol();
    let args = call.args();
    let dest = call.dest();
    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    match symbol {
        "hew_duplex_pair" => {
            if args.len() != 4 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_duplex_pair): expected 4 args \
                     (s_cap, r_cap, out_a, out_b), got {}",
                    args.len()
                )));
            }
            // arg0 / arg1: capacities. Load from caller-provided
            // alloca; the producer (`lower_duplex_pair`) wires both
            // slots from the same capacity expression so types match.
            let cap0 = load_int_arg(fn_ctx, args[0], i64_ty, "duplex_pair_s_cap")?;
            let cap1 = load_int_arg(fn_ctx, args[1], i64_ty, "duplex_pair_r_cap")?;
            // arg2 / arg3: out-params. Must be Place::DuplexHandle(N)
            // per the E2 producer's SHIM(E4) convention. Pass the
            // *address of* local-N's alloca so the runtime writes the
            // pair of handle pointers through. Reject any other Place
            // kind at this seam (fail-closed; not a silent shim).
            let out_a = duplex_handle_out_addr(fn_ctx, args[2], "hew_duplex_pair arg2")?;
            let out_b = duplex_handle_out_addr(fn_ctx, args[3], "hew_duplex_pair arg3")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let llvm_args: [BasicMetadataValueEnum; 4] =
                [cap0.into(), cap1.into(), out_a.into(), out_b.into()];
            fn_ctx
                .builder
                .build_call(fv, &llvm_args, "hew_duplex_pair_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_duplex_pair call: {e:?}")))?;
            // Producer emits dest: None — i32 return is discarded.
            // Defence in depth: if a future producer ever wires a
            // dest, fail-closed rather than silently dropping it.
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_duplex_pair returns i32 (discarded by the runtime contract); \
                     producer must not supply dest={d:?}"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        "hew_duplex_send" => {
            if args.len() != 3 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_duplex_send): expected 3 args \
                     (handle, msg, len), got {}",
                    args.len()
                )));
            }
            // arg0: receiver handle. Place::DuplexHandle(N) loads the
            // raw `*mut HewDuplexHandle` value from local-N's alloca
            // (non-consuming; alloca persists across the call so the
            // drop ritual can close it at end-of-scope).
            let handle = load_duplex_handle(fn_ctx, args[0], "hew_duplex_send arg0")?;
            // arg1: message. Place::Local(M) holds an i64 today
            // (single-vertebra exemplar). Spill into a fresh i64
            // alloca and pass the slot's address as `*const u8`
            // (opaque-pointer mode bitcasts away the element type).
            let msg_ptr = spill_int_arg_as_ptr(fn_ctx, args[1], i64_ty, "duplex_send_msg")?;
            // arg2: byte length. Producer emits `ConstI64 { value: 8 }`
            // into this local; load it as i64 (== usize on 64-bit).
            let len = load_int_arg(fn_ctx, args[2], i64_ty, "duplex_send_len")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let llvm_args: [BasicMetadataValueEnum; 3] =
                [handle.into(), msg_ptr.into(), len.into()];
            fn_ctx
                .builder
                .build_call(fv, &llvm_args, "hew_duplex_send_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_duplex_send call: {e:?}")))?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_duplex_send returns i32 (discarded by the runtime contract); \
                     producer must not supply dest={d:?}"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        // `hew_duplex_close` is only called from the Drop ritual
        // (`lower_drop`); reaching it via `Instr::CallRuntimeAbi`
        // means a producer mis-routed a destructor through the
        // generic call path. Fail-closed.
        "hew_duplex_close" => {
            return Err(CodegenError::FailClosed(
                "Instr::CallRuntimeAbi(hew_duplex_close): close is the drop \
                 ritual's responsibility — emit Instr::Drop { drop_fn: \
                 Some(\"hew_duplex_close\") } instead so the alloca-zero \
                 ritual fires after the close"
                    .to_string(),
            ));
        }
        other => {
            // Allowlisted but not wired. Names a missing follow-on
            // seam so the next implementer can find the gap quickly
            // (`parity-or-tracked-gap`).
            return Err(CodegenError::FailClosed(format!(
                "Instr::CallRuntimeAbi(symbol={other:?}): codegen has no lowering arm \
                 for this M2 runtime symbol; wire a per-symbol arm or leave the \
                 producer fail-closed until the substrate lane lands"
            )));
        }
    }
    Ok(())
}

/// Map an elaborator-produced `drop_fn` string to its C-ABI runtime symbol.
///
/// The elaborator emits `"<TypeName>::<method>"` strings from
/// `hew-mir/src/lower.rs::build_lifo_drops` via the `TypeClassTable`.
/// The C-ABI names in `hew-runtime/` are not always a predictable
/// transformation of those strings — in particular, `LambdaActorHandle`
/// uses the method name "close" in the type-class seeding but the
/// runtime symbol is `hew_lambda_actor_release` (not
/// `hew_lambda_actor_close`). This table is the authoritative bridge.
///
/// Accepts literal C-ABI symbol names (e.g. `"hew_duplex_close"`)
/// as a backward-compatible pass-through so hand-built test MIR can
/// use either format. Unknown strings fail closed — no wildcard.
///
/// LESSONS: producer-bridge-before-codegen (P1), boundary-fail-closed (P0),
/// lifecycle-symmetry (P0).
fn resolve_drop_fn_to_symbol(drop_fn: &str) -> Result<&'static str, CodegenError> {
    match drop_fn {
        // Elaborator-produced names (from builtin_type_classes.rs seeding).
        // Duplex<S, R> — close-both-directions.
        "Duplex::close" => Ok("hew_duplex_close"),
        // LambdaActorHandle — NB: the type-class seeding calls the method
        // "close" but the runtime C-ABI symbol is "release", not "close".
        // An explicit table entry is required; string-mangling would produce
        // the wrong symbol.
        "LambdaActorHandle::close" => Ok("hew_lambda_actor_release"),
        // Literal C-ABI symbol pass-through (backward compat for hand-built
        // test MIR that pre-dates elaborated-drop-plan consumption).
        "hew_duplex_close" => Ok("hew_duplex_close"),
        "hew_lambda_actor_release" => Ok("hew_lambda_actor_release"),
        // Remaining elaborator-produced names have no MIR producer today —
        // SendHalf/RecvHalf place_pointer is fail-closed, Sink/Stream have
        // no constructor surface. Fail closed here so any future producer
        // that outpaces codegen surfaces immediately rather than leaking.
        other => Err(CodegenError::FailClosed(format!(
            "drop_fn={other:?}: no C-ABI runtime symbol wired for this \
             drop_fn string. Recognised names today: Duplex::close, \
             LambdaActorHandle::close (and their hew_* C-ABI literals). \
             Refusing to silently no-op a resource drop \
             (LESSONS: boundary-fail-closed, lifecycle-symmetry)."
        ))),
    }
}

/// Lower a drop_fn close ritual for `place`: call the named runtime close
/// symbol with `place`'s loaded pointer, then null-out the alloca so a
/// second drop on the same place hits null at both the codegen layer and
/// the runtime's AtomicBool guard (LESSONS `raii-null-after-move`).
///
/// Accepts both elaborator-produced `"<TypeName>::close"` strings and
/// literal C-ABI symbol names via `resolve_drop_fn_to_symbol`. Unknown
/// strings fail closed — no wildcard, no silent no-op.
fn lower_drop(fn_ctx: &FnCtx<'_, '_>, place: Place, drop_fn: &str) -> CodegenResult<()> {
    let symbol = resolve_drop_fn_to_symbol(drop_fn)?;
    // Load the handle pointer from the place's alloca. `load_duplex_handle`
    // resolves both `Place::Local(N)` and `Place::DuplexHandle(N)` to the
    // same ptr-typed alloca, which is what all ptr-arg close symbols expect.
    let label = format!("{symbol} drop");
    let handle = load_duplex_handle(fn_ctx, place, &label)?;
    let fv = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        symbol,
    )?;
    let llvm_args: [BasicMetadataValueEnum; 1] = [handle.into()];
    fn_ctx
        .builder
        .build_call(fv, &llvm_args, &format!("{symbol}_call"))
        .map_err(|e| CodegenError::Llvm(format!("{symbol} call: {e:?}")))?;
    // Zero the alloca: structurally-reachable second drop must hit null
    // at the codegen layer. The runtime AtomicBool guard provides
    // defence-in-depth, but the codegen null-store is required per
    // LESSONS `raii-null-after-move`.
    let (slot, _) = place_pointer(fn_ctx, place)?;
    let null_ptr = fn_ctx.ctx.ptr_type(AddressSpace::default()).const_null();
    fn_ctx
        .builder
        .build_store(slot, null_ptr)
        .map_err(|e| CodegenError::Llvm(format!("post-{symbol} alloca null-store: {e:?}")))?;
    Ok(())
}

/// Emit the LIFO drop ritual for `block_id` from `drop_plans`.
///
/// Walks `drop_plans` for the entry whose `ExitPath` block id matches
/// `block_id`. Each `ElabDrop` with `drop_fn = Some(name)` is lowered
/// via `lower_drop`; trivial drops (`drop_fn = None`) are no-ops.
///
/// The elaborator already emits drops in LIFO order (latest-bound
/// resource drops first); no re-reversal here. Spine-only functions
/// carry an empty `DropPlan`; the loop is a no-op in that case.
///
/// Called immediately before `lower_terminator` for every block so
/// every exit path receives its drops regardless of CFG shape.
/// LESSONS: cleanup-all-exits (P0), lifecycle-symmetry (P0).
fn emit_elab_drops(
    fn_ctx: &FnCtx<'_, '_>,
    block_id: u32,
    drop_plans: &[(ExitPath, hew_mir::DropPlan)],
) -> CodegenResult<()> {
    // Find the plan whose owning block matches. `drop_plans` has one entry
    // per exit terminator; functions with a single block have one entry.
    // Linear scan is correct and cheap for the number of exits a function
    // has in the current spine.
    let plan = drop_plans.iter().find(|(exit, _)| {
        // Every ExitPath variant carries a `block` field. Match by value.
        let exit_block = match exit {
            ExitPath::Return { block }
            | ExitPath::Goto { block, .. }
            | ExitPath::Branch { block, .. }
            | ExitPath::Call { block, .. }
            | ExitPath::Panic { block }
            | ExitPath::Cancel { block }
            | ExitPath::Yield { block, .. }
            | ExitPath::Send { block, .. }
            | ExitPath::Ask { block, .. }
            | ExitPath::Select { block, .. } => *block,
        };
        exit_block == block_id
    });
    let Some((_, plan)) = plan else {
        // No drop plan for this block — spine-only function or a block
        // that has no exit in the elaborated plan. No drops to emit.
        return Ok(());
    };
    for drop in &plan.drops {
        emit_one_elab_drop(fn_ctx, drop)?;
    }
    Ok(())
}

/// Emit a single `ElabDrop` from `elaborated_mir.drop_plans`.
///
/// `drop_fn = None` is a legitimate no-op (trivial drop for a value class
/// with no side-effecting close). `drop_fn = Some(name)` routes through
/// `lower_drop` → `resolve_drop_fn_to_symbol` → `intern_runtime_decl`.
/// Unknown `drop_fn` strings surface `CodegenError::FailClosed` immediately
/// (LESSONS: boundary-fail-closed).
fn emit_one_elab_drop(fn_ctx: &FnCtx<'_, '_>, drop: &ElabDrop) -> CodegenResult<()> {
    let Some(ref drop_fn) = drop.drop_fn else {
        // Trivial drop: value class with no side-effecting close
        // (e.g. a moved-but-not-dropped local in a future surface).
        return Ok(());
    };
    lower_drop(fn_ctx, drop.place, drop_fn)
}

/// Load an integer-typed `Place` and return its value coerced to the
/// requested `expected` int type. Today every M2 substrate integer arg
/// is i64 at the C-ABI; this helper rejects type mismatches loudly.
fn load_int_arg<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    place: Place,
    expected: inkwell::types::IntType<'ctx>,
    label: &str,
) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
    let (ptr, ty) = place_pointer(fn_ctx, place)?;
    let int_ty = match ty {
        BasicTypeEnum::IntType(i) => i,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "{label}: Place {place:?} resolves to non-int type {other:?}; \
                 expected i64-shaped runtime ABI arg"
            )));
        }
    };
    if int_ty != expected {
        return Err(CodegenError::FailClosed(format!(
            "{label}: Place {place:?} has width {int_ty:?}, expected {expected:?} \
             (M2 substrate is 64-bit on native; wasm32 widening is E5c territory)"
        )));
    }
    let v = fn_ctx
        .builder
        .build_load(int_ty, ptr, label)
        .map_err(|e| CodegenError::Llvm(format!("{label} load: {e:?}")))?
        .into_int_value();
    Ok(v)
}

/// Spill an integer-typed `Place` into a fresh stack alloca and return
/// the alloca's pointer (as `ptr`/opaque). The C-ABI seam for
/// `hew_duplex_send`'s message arg requires a pointer to the byte
/// payload; the spine subset's single-vertebra exemplar carries i64
/// payloads, so the spill produces an 8-byte alloca whose address is
/// the `*const u8` the runtime reads.
fn spill_int_arg_as_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    place: Place,
    expected: inkwell::types::IntType<'ctx>,
    label: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    let v = load_int_arg(fn_ctx, place, expected, label)?;
    let slot = fn_ctx
        .builder
        .build_alloca(expected, &format!("{label}_spill"))
        .map_err(|e| CodegenError::Llvm(format!("{label} spill alloca: {e:?}")))?;
    fn_ctx
        .builder
        .build_store(slot, v)
        .map_err(|e| CodegenError::Llvm(format!("{label} spill store: {e:?}")))?;
    Ok(slot)
}

/// Materialise a `Place::DuplexHandle(N)` (or `Place::Local(N)` whose
/// underlying alloca is `ptr`-typed) as a loaded `*mut HewDuplexHandle`.
/// Used by `hew_duplex_send`'s receiver arg and the `hew_duplex_close`
/// drop ritual.
fn load_duplex_handle<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    place: Place,
    label: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    let (slot, ty) = place_pointer(fn_ctx, place)?;
    match ty {
        BasicTypeEnum::PointerType(_) => {}
        other => {
            return Err(CodegenError::FailClosed(format!(
                "{label}: Place {place:?} resolves to non-pointer type {other:?}; \
                 expected `ptr` (the Duplex handle alloca)"
            )));
        }
    }
    let loaded = fn_ctx
        .builder
        .build_load(ty, slot, label)
        .map_err(|e| CodegenError::Llvm(format!("{label} load: {e:?}")))?;
    match loaded {
        BasicValueEnum::PointerValue(p) => Ok(p),
        other => Err(CodegenError::FailClosed(format!(
            "{label}: loaded value is not a pointer: {other:?}"
        ))),
    }
}

/// Return the address of the alloca backing a `Place::DuplexHandle(N)`.
/// Used by `hew_duplex_pair`'s two out-param args, which write through
/// the address into the underlying `ptr` slot. Rejects any non-DuplexHandle
/// place at this seam (Place::Local has no symmetric runtime contract
/// today; gate it loudly).
fn duplex_handle_out_addr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    place: Place,
    label: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    match place {
        Place::DuplexHandle(_) => {
            let (slot, ty) = place_pointer(fn_ctx, place)?;
            match ty {
                BasicTypeEnum::PointerType(_) => Ok(slot),
                other => Err(CodegenError::FailClosed(format!(
                    "{label}: DuplexHandle alloca has non-pointer slot type {other:?}; \
                     primitive_to_llvm must map ResolvedTy::Named{{name:\"Duplex\",..}} \
                     to `ptr`"
                ))),
            }
        }
        other => Err(CodegenError::FailClosed(format!(
            "{label}: out-param argument must be Place::DuplexHandle(N), got {other:?}"
        ))),
    }
}

fn lower_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    fn_symbols: &FnSymbolMap<'ctx>,
    term: &Terminator,
) -> CodegenResult<()> {
    match term {
        Terminator::Return => {
            let loaded = fn_ctx
                .builder
                .build_load(fn_ctx.return_ty, fn_ctx.return_slot, "ret_val")
                .map_err(|e| CodegenError::Llvm(format!("ret load: {e:?}")))?;
            fn_ctx
                .builder
                .build_return(Some(&loaded))
                .map_err(|e| CodegenError::Llvm(format!("ret: {e:?}")))?;
        }
        Terminator::Goto { target } => {
            let bb = fn_ctx.blocks.get(target).ok_or_else(|| {
                CodegenError::FailClosed(format!("goto target bb{target} not found"))
            })?;
            fn_ctx
                .builder
                .build_unconditional_branch(*bb)
                .map_err(|e| CodegenError::Llvm(format!("br: {e:?}")))?;
        }
        Terminator::Branch {
            cond,
            then_target,
            else_target,
        } => {
            let (cond_ptr, cond_ty) = place_pointer(fn_ctx, *cond)?;
            let cond_int_ty = match cond_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "Branch cond is not an integer".into(),
                    ))
                }
            };
            let cond_loaded = fn_ctx
                .builder
                .build_load(cond_int_ty, cond_ptr, "cond_load")
                .map_err(|e| CodegenError::Llvm(format!("cond load: {e:?}")))?
                .into_int_value();
            let zero = cond_int_ty.const_zero();
            let nonzero = fn_ctx
                .builder
                .build_int_compare(IntPredicate::NE, cond_loaded, zero, "cond_nz")
                .map_err(|e| CodegenError::Llvm(format!("icmp ne: {e:?}")))?;
            let then_bb = *fn_ctx
                .blocks
                .get(then_target)
                .ok_or_else(|| CodegenError::FailClosed(format!("then bb{then_target} missing")))?;
            let else_bb = *fn_ctx
                .blocks
                .get(else_target)
                .ok_or_else(|| CodegenError::FailClosed(format!("else bb{else_target} missing")))?;
            fn_ctx
                .builder
                .build_conditional_branch(nonzero, then_bb, else_bb)
                .map_err(|e| CodegenError::Llvm(format!("condbr: {e:?}")))?;
        }
        Terminator::Call {
            callee,
            args,
            dest,
            next,
        } => {
            let (llvm_fn, callee_ret_ty) = *fn_symbols.get(callee).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Call to `{callee}` with no declared symbol (coroutine targets are not callable yet)"
                ))
            })?;
            let mut arg_vals: Vec<inkwell::values::BasicMetadataValueEnum> =
                Vec::with_capacity(args.len());
            for arg in args {
                let (arg_ptr, arg_ty) = place_pointer(fn_ctx, *arg)?;
                let loaded = fn_ctx
                    .builder
                    .build_load(arg_ty, arg_ptr, "call_arg")
                    .map_err(|e| CodegenError::Llvm(format!("call arg load: {e:?}")))?;
                arg_vals.push(match loaded {
                    BasicValueEnum::IntValue(v) => v.into(),
                    BasicValueEnum::FloatValue(v) => v.into(),
                    BasicValueEnum::PointerValue(v) => v.into(),
                    BasicValueEnum::StructValue(v) => v.into(),
                    BasicValueEnum::ArrayValue(v) => v.into(),
                    BasicValueEnum::VectorValue(v) => v.into(),
                    BasicValueEnum::ScalableVectorValue(v) => v.into(),
                });
            }
            let call_site = fn_ctx
                .builder
                .build_call(llvm_fn, &arg_vals, "call_result")
                .map_err(|e| CodegenError::Llvm(format!("build_call: {e:?}")))?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            if dest_ty != callee_ret_ty {
                return Err(CodegenError::FailClosed(format!(
                    "Call dest type {dest_ty:?} does not match callee return {callee_ret_ty:?}"
                )));
            }
            let ret_val = call_site.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed(
                    "Call to void-returning fn is not usable as Terminator::Call dest".into(),
                )
            })?;
            fn_ctx
                .builder
                .build_store(dest_ptr, ret_val)
                .map_err(|e| CodegenError::Llvm(format!("call store: {e:?}")))?;
            let next_bb = *fn_ctx
                .blocks
                .get(next)
                .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
            fn_ctx
                .builder
                .build_unconditional_branch(next_bb)
                .map_err(|e| CodegenError::Llvm(format!("call br: {e:?}")))?;
        }
        Terminator::Trap { kind } => {
            // Emit `llvm.trap` followed by `unreachable`. The `kind`
            // discriminant is carried only in the MIR; at the LLVM IR
            // level all trap causes lower identically to a hard abort
            // (LLVM will remove the block if it proves it unreachable;
            // on the taken path the trap fires before the unreachable).
            //
            // `llvm.trap` is a non-overloaded void intrinsic so
            // `get_declaration` takes an empty type slice.
            let _kind: TrapKind = *kind; // exhaustiveness: future arms may discriminate
            let trap_intrinsic = Intrinsic::find("llvm.trap").ok_or_else(|| {
                CodegenError::Llvm("llvm.trap intrinsic not found in LLVM build".into())
            })?;
            let trap_fn = trap_intrinsic
                .get_declaration(fn_ctx.llvm_mod, &[])
                .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
            fn_ctx
                .builder
                .build_call(trap_fn, &[], "trap")
                .map_err(|e| CodegenError::Llvm(format!("llvm.trap call: {e:?}")))?;
            fn_ctx
                .builder
                .build_unreachable()
                .map_err(|e| CodegenError::Llvm(format!("trap unreachable: {e:?}")))?;
        }
        Terminator::Yield { .. } => {
            // The variant is declared so Checked MIR's borrow-liveness
            // check has a suspension point to look for; the v0.5
            // integer spine never constructs it. Generator lowering
            // arrives with the construction surface in a later release.
            return Err(CodegenError::Unsupported(
                "Terminator::Yield — generator lowering not yet implemented",
            ));
        }
        Terminator::Send { .. } => {
            // Same shape as Yield: declared for the legality check, no
            // construction surface in the v0.5 spine.
            return Err(CodegenError::Unsupported(
                "Terminator::Send — actor lowering not yet implemented",
            ));
        }
        Terminator::Ask { .. } => {
            // Reserved for the non-select actor-ask path. HIR-to-MIR
            // lowers `select{}` ask arms into `Terminator::Select` with
            // `SelectArmKind::ActorAsk`, so this arm is currently
            // unreachable (non-select `actor.method()` is rejected
            // upstream as `CutoverUnsupported`). When the construction
            // surface lands, this arm emits the runtime ABI sequence:
            //   hew_reply_channel_new → hew_actor_ask_with_channel
            //   → hew_reply_wait
            // On the loser / cancel leg:
            //   hew_reply_channel_cancel + hew_reply_channel_free
            // Late replies to a cancelled channel are silently dropped
            // by the receiver.
            return Err(CodegenError::Unsupported(
                "Terminator::Ask — actor-ask lowering not yet implemented",
            ));
        }
        Terminator::Select { arms, .. } => {
            // Sealed `select{}` construct. The MIR terminator's shape is
            // fixed (see hew-mir::model::Terminator::Select) and HIR
            // recognises the four sealed arm forms; the runtime
            // substrate (`hew_select_wait` heterogeneous-arm dispatch,
            // `hew_stream_poll` pending-read, multiplex-await primitive
            // for task-await win-path, and actor-call lowering for the
            // ask arm) is not yet wired. `hew_task_scope_cancel_one`
            // (task-await loss-path) is available. Codegen fails closed
            // per arm kind with a message naming the remaining substrate
            // the future implementation must satisfy.
            //
            // Each arm kind's TODO marker below records the semantic
            // invariants the runtime substrate must observe (lifted
            // from HEW-SPEC-2026 §4.11.1's per-form table). The
            // markers are intentionally semantic — they tell a future
            // implementer the contract, not which lane will land it.
            let first_kind = arms.first().map(|arm| &arm.kind);
            let msg: String = match first_kind {
                Some(hew_mir::SelectArmKind::StreamNext { .. }) => {
                    // The cancellable pending-read primitive on
                    // Stream<T> now exists in hew-runtime
                    // (`hew_stream_poll` + `hew_stream_cancel_pending_read`).
                    // Lowering still fails closed because the
                    // heterogeneous-arm dispatch glue
                    // (`hew_select_wait`) that picks a winner across
                    // arm kinds is not yet wired. When that glue
                    // lands, this arm emits:
                    //   - `hew_stream_poll(stream, callback, userdata)`
                    //     on the winning side, returning a
                    //     PendingReadId stored per-arm.
                    //   - `hew_stream_cancel_pending_read(stream, id)`
                    //     on every losing side, using the stored id.
                    //   - Binding for the winning arm receives the
                    //     malloc'd item pointer (null → None on EOF).
                    "select{} stream-next arm awaits runtime substrate: \
                     hew_select_wait heterogeneous-arm dispatch glue \
                     (the cancellable pending-read primitive on Stream<T> \
                     is wired via hew_stream_poll / \
                     hew_stream_cancel_pending_read; the loser-cleanup \
                     path withdraws the pending read without consuming \
                     the stream)"
                        .to_string()
                }
                Some(hew_mir::SelectArmKind::ActorAsk { .. }) => {
                    // TODO: emit actor-ask lowering for this select arm.
                    // The MIR shape is complete: `Terminator::Ask`
                    // (with distinct `channel` and `reply_dest` Places),
                    // `ExitPath::Ask` (carrying the `channel` Place for
                    // loser-cleanup), and `MirCheck::ActorAskEscape`.
                    // The runtime ABI is present; what remains is
                    // lowering each select arm into a per-arm body block
                    // terminated by `Terminator::Ask`.
                    //
                    // Runtime ABI invariants:
                    // - Win path: hew_reply_channel_new →
                    //     hew_actor_ask_with_channel → hew_reply_wait.
                    // - Lose / cancel path: hew_reply_channel_cancel +
                    //     hew_reply_channel_free against the channel
                    //     slot. The pending ask is withdrawn; the actor
                    //     handler may still deliver a reply after cancel.
                    // - Late replies to a cancelled channel are silently
                    //     dropped by the receiver (OrphanedAsk path);
                    //     the caller must not interpret a false return
                    //     from hew_reply as an error.
                    // - Loser cleanup is best-effort: hew_reply may race
                    //     cancel; the ref-count prevents use-after-free.
                    "select{} actor-ask arm: per-arm body-block lowering \
                     terminated by Terminator::Ask \
                     (win: hew_reply_channel_new → hew_actor_ask_with_channel \
                     → hew_reply_wait; lose: cancel + free the channel)"
                        .to_string()
                }
                Some(hew_mir::SelectArmKind::TaskAwait { .. }) => {
                    // TODO: emit task-await observer registration for
                    // the await arm. The runtime must (a) register a
                    // completion observer on the existing task
                    // handle, (b) park the current coroutine, (c)
                    // resume with the task result on win (only
                    // `Ok(T)` is a winning value; cancellation and
                    // trap outcomes propagate through the select
                    // site), (d) on loss cancel the task at its next
                    // safepoint via `hew_task_scope_cancel_one`
                    // (available; cancel-after-done is a no-op).
                    // The remaining gap is the completion-observer /
                    // park / resume primitive for the win-path.
                    "select{} task-await arm awaits multiplex-await \
                     substrate: hew_task_scope_cancel_one available; \
                     missing completion-observer/park/resume primitive \
                     for winner path"
                        .to_string()
                }
                Some(hew_mir::SelectArmKind::AfterTimer { .. }) => {
                    // TODO: emit timer-arm registration. The runtime
                    // must (a) schedule a one-shot timer on the
                    // current task scope's timer list with the
                    // absolute deadline, (b) park the current
                    // coroutine, (c) resume the after-arm body on
                    // expiry. Loser cleanup: hew_timer_cancel
                    // unregisters the timer from the wheel (the
                    // timer wheel substrate is complete; only the
                    // heterogeneous-arm dispatch glue is missing).
                    "select{} after-timer arm awaits runtime substrate: \
                     heterogeneous-arm dispatch wiring \
                     (hew_select_wait) to the existing timer wheel"
                        .to_string()
                }
                None => {
                    // HIR rejects empty selects with SelectNoArms; a
                    // zero-arm Terminator::Select is structurally
                    // unreachable. Fail closed defensively.
                    "select{} terminator carries zero arms (HIR should \
                     have rejected with SelectNoArms)"
                        .to_string()
                }
            };
            return Err(CodegenError::FailClosed(msg));
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Per-function declaration + body lowering
// ---------------------------------------------------------------------------

fn declare_function<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    func: &RawMirFunction,
) -> CodegenResult<(FunctionValue<'ctx>, BasicTypeEnum<'ctx>)> {
    let linkage = if func.name == "main" {
        Some(Linkage::External)
    } else {
        Some(Linkage::Internal)
    };
    let return_ty_llvm = primitive_to_llvm(ctx, &func.return_ty)?;
    // Cluster 1's spine subset only lowers integer-returning functions.
    // Bool/Unit/F32/F64 all reach `primitive_to_llvm` with valid shapes,
    // but the MIR `Instr` stream cannot populate the return slot for them
    // — the function body would emit `ret` against an uninitialised
    // alloca. Reject the return type at declaration time so the failure
    // is loud and well-located (LESSONS `boundary-fail-closed`).
    if !matches!(return_ty_llvm, BasicTypeEnum::IntType(_)) {
        return Err(CodegenError::Unsupported(
            "Cluster 1 only lowers integer-returning functions; \
             non-integer return types are out of the spine subset",
        ));
    }
    // Cluster 1 functions are zero-arg. When Cluster 2 adds params it
    // grows this loop; the shape is here so the diff is small.
    let param_tys: Vec<BasicMetadataTypeEnum> = Vec::new();
    let fn_ty = match return_ty_llvm {
        BasicTypeEnum::IntType(i) => i.fn_type(&param_tys, false),
        // Other shapes are pre-filtered above; keep the arms exhaustive so
        // the LLVM API surface stays explicit if a future cluster widens the
        // accepted set.
        BasicTypeEnum::FloatType(f) => f.fn_type(&param_tys, false),
        BasicTypeEnum::StructType(s) => s.fn_type(&param_tys, false),
        BasicTypeEnum::PointerType(p) => p.fn_type(&param_tys, false),
        BasicTypeEnum::ArrayType(a) => a.fn_type(&param_tys, false),
        BasicTypeEnum::VectorType(v) => v.fn_type(&param_tys, false),
        BasicTypeEnum::ScalableVectorType(v) => v.fn_type(&param_tys, false),
    };
    let llvm_fn = llvm_mod.add_function(&func.name, fn_ty, linkage);
    Ok((llvm_fn, return_ty_llvm))
}

/// Lower one function from `raw_mir`, consuming `elaborated_mir.drop_plans`
/// to emit LIFO close calls before every exit terminator.
///
/// `elab` is the `ElaboratedMirFunction` whose `name` matches `func.name`.
/// When `pipeline.elaborated_mir` has no matching entry (e.g. in hand-built
/// test pipelines that predate elaboration), `elab` may be `None`; in that
/// case `emit_elab_drops` receives an empty slice and emits nothing — the
/// inline `Instr::Drop` path in `lower_instruction` still fires for any
/// `Instr::Drop` entries baked into `raw_mir.blocks`.
fn lower_function<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    func: &RawMirFunction,
    fn_symbols: &FnSymbolMap<'ctx>,
    elab: Option<&ElaboratedMirFunction>,
) -> CodegenResult<()> {
    let (llvm_fn, return_ty_llvm) = *fn_symbols.get(&func.name).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "function `{}` was not declared before body lowering",
            func.name
        ))
    })?;
    let builder = ctx.create_builder();

    // Build every block up front so terminators can name forward targets.
    let mut blocks = HashMap::new();
    for block in &func.blocks {
        let bb = ctx.append_basic_block(llvm_fn, &format!("bb{}", block.id));
        blocks.insert(block.id, bb);
    }

    // Position in the entry block to emit the alloca prologue.
    let entry_block = func.blocks.first().ok_or_else(|| {
        CodegenError::FailClosed(format!("function `{}` has zero blocks", func.name))
    })?;
    let entry_bb = *blocks.get(&entry_block.id).expect("entry block in map");
    builder.position_at_end(entry_bb);

    let return_slot = builder
        .build_alloca(return_ty_llvm, "return_slot")
        .map_err(|e| CodegenError::Llvm(format!("alloca return_slot: {e:?}")))?;

    let mut locals: HashMap<u32, (PointerValue, BasicTypeEnum)> = HashMap::new();
    for (idx, ty) in func.locals.iter().enumerate() {
        let llvm_ty = primitive_to_llvm(ctx, ty)?;
        let idx_u32 = u32::try_from(idx).map_err(|_| {
            CodegenError::FailClosed("function exceeds u32::MAX locals — impossible".into())
        })?;
        let slot = builder
            .build_alloca(llvm_ty, &format!("local_{idx}"))
            .map_err(|e| CodegenError::Llvm(format!("alloca local {idx}: {e:?}")))?;
        locals.insert(idx_u32, (slot, llvm_ty));
    }

    let fn_ctx = FnCtx {
        ctx,
        llvm_mod,
        builder,
        return_slot,
        return_ty: return_ty_llvm,
        locals,
        blocks: blocks.clone(),
        runtime_decls: RefCell::new(HashMap::new()),
    };

    // Extract drop_plans from the matched elaborated function, or use an
    // empty slice when no elaborated function is available (hand-built test
    // pipelines that inline Instr::Drop instead of using drop_plans).
    let empty_plans: Vec<(ExitPath, hew_mir::DropPlan)> = Vec::new();
    let drop_plans: &[(ExitPath, hew_mir::DropPlan)] = elab
        .map(|e| e.drop_plans.as_slice())
        .unwrap_or(empty_plans.as_slice());

    for block in &func.blocks {
        let bb = *blocks.get(&block.id).expect("block in map");
        fn_ctx.builder.position_at_end(bb);
        for instr in &block.instructions {
            lower_instruction(&fn_ctx, instr)?;
        }
        // Emit LIFO drops from the elaborated drop plan BEFORE the
        // terminator so the alloca null-stores precede the ret/br.
        // LESSONS: cleanup-all-exits (P0), lifecycle-symmetry (P0).
        emit_elab_drops(&fn_ctx, block.id, drop_plans)?;
        lower_terminator(&fn_ctx, fn_symbols, &block.terminator)?;
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Module-level orchestration
// ---------------------------------------------------------------------------

fn build_module<'ctx>(
    ctx: &'ctx Context,
    pipeline: &IrPipeline,
    name: &str,
) -> CodegenResult<LlvmModule<'ctx>> {
    let llvm_mod = ctx.create_module(name);
    let mut fn_symbols: FnSymbolMap<'ctx> = HashMap::new();
    for func in &pipeline.raw_mir {
        let sym = declare_function(ctx, &llvm_mod, func)?;
        fn_symbols.insert(func.name.clone(), sym);
    }
    for func in &pipeline.raw_mir {
        // Match by name: elaborated_mir is parallel to raw_mir when the
        // full pipeline runs. Hand-built test pipelines may leave
        // elaborated_mir empty; `find` returns `None` in that case and
        // `lower_function` falls back to the inline Instr::Drop path.
        let elab = pipeline.elaborated_mir.iter().find(|e| e.name == func.name);
        lower_function(ctx, &llvm_mod, func, &fn_symbols, elab)?;
    }
    llvm_mod
        .verify()
        .map_err(|e| CodegenError::LlvmVerify(e.to_string()))?;
    Ok(llvm_mod)
}

fn emit_textual(pipeline: &IrPipeline, name: &str, out: &Path) -> CodegenResult<()> {
    let ctx = Context::create();
    let llvm_mod = build_module(&ctx, pipeline, name)?;
    llvm_mod
        .print_to_file(out)
        .map_err(|e| CodegenError::Llvm(format!("print_to_file {}: {e:?}", out.display())))?;
    Ok(())
}

/// Link a wasm relocatable object into a standalone wasm module.
///
/// Prefers `wasm-ld` on PATH (Homebrew/lld). Falls back to
/// `rust-lld -flavor wasm` (which ships with rustup). The probe established
/// this fallback order empirically; the Hew dev shell on macOS has both,
/// CI runners on Linux typically have at least `rust-lld` via rustup.
fn link_wasm_module(obj: &Path, out: &Path) -> CodegenResult<()> {
    let candidates: &[(&str, &[&str])] = &[("wasm-ld", &[]), ("rust-lld", &["-flavor", "wasm"])];
    let mut last_err: Option<String> = None;
    for (cmd, prefix_args) in candidates {
        let mut command = Command::new(cmd);
        command.args(*prefix_args);
        command
            .arg("--no-entry")
            .arg("--export=main")
            .arg("-o")
            .arg(out)
            .arg(obj);
        let output = match command.output() {
            Ok(o) => o,
            Err(e) => {
                last_err = Some(format!("spawn {cmd}: {e}"));
                continue;
            }
        };
        if output.status.success() {
            return Ok(());
        }
        last_err = Some(format!(
            "{cmd} failed ({:?}): {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Err(CodegenError::Link(format!(
        "wasm link failed for {}: {} (install `wasm-ld` via Homebrew/llvm or ensure `rust-lld` is on PATH via rustup)",
        obj.display(),
        last_err.unwrap_or_else(|| "no linker available".into())
    )))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use hew_mir::{BasicBlock, DecisionFact, IrPipeline};

    fn empty_pipeline_with_const_42() -> IrPipeline {
        let return_ty = ResolvedTy::I64;
        let main = RawMirFunction {
            name: "main".to_string(),
            return_ty: return_ty.clone(),
            locals: vec![return_ty.clone()],
            blocks: vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![
                    Instr::ConstI64 {
                        dest: Place::Local(0),
                        value: 42,
                    },
                    Instr::Move {
                        dest: Place::ReturnSlot,
                        src: Place::Local(0),
                    },
                ],
                terminator: Terminator::Return,
            }],
            decisions: Vec::<DecisionFact>::new(),
        };
        IrPipeline {
            thir: Vec::new(),
            raw_mir: vec![main],
            checked_mir: Vec::new(),
            elaborated_mir: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    #[test]
    fn const_42_module_verifies() {
        let pipeline = empty_pipeline_with_const_42();
        let ctx = Context::create();
        let m = build_module(&ctx, &pipeline, "const42_test").expect("const-42 module must build");
        assert!(m.verify().is_ok(), "const-42 module must pass LLVM verify");
    }

    #[test]
    fn unsupported_string_return_fails_closed() {
        let return_ty = ResolvedTy::String;
        let main = RawMirFunction {
            name: "main".to_string(),
            return_ty: return_ty.clone(),
            locals: Vec::new(),
            blocks: vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            }],
            decisions: Vec::new(),
        };
        let pipeline = IrPipeline {
            thir: Vec::new(),
            raw_mir: vec![main],
            checked_mir: Vec::new(),
            elaborated_mir: Vec::new(),
            diagnostics: Vec::new(),
        };
        let ctx = Context::create();
        let err = build_module(&ctx, &pipeline, "string_return")
            .expect_err("String return must be rejected");
        assert!(
            matches!(err, CodegenError::Unsupported(_)),
            "String return must surface as Unsupported, got: {err:?}"
        );
    }

    // ── Terminator::Select fail-closed per arm kind ──────────────────
    //
    // The four sealed select arm forms each lower through a distinct
    // codegen match arm that fails closed with a message naming the
    // runtime substrate the future implementation must wire. These
    // tests pin the per-arm-kind diagnostic so a regression that
    // silently swallows a Select terminator is caught immediately.

    fn pipeline_with_select_terminator(arm_kind: hew_mir::SelectArmKind) -> IrPipeline {
        let main = RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            locals: vec![ResolvedTy::I64],
            blocks: vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Select {
                    arms: vec![hew_mir::SelectArm {
                        kind: arm_kind,
                        body_block: 0,
                        binding: None,
                    }],
                    next: 0,
                },
            }],
            decisions: Vec::new(),
        };
        IrPipeline {
            thir: Vec::new(),
            raw_mir: vec![main],
            checked_mir: Vec::new(),
            elaborated_mir: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    #[test]
    fn select_stream_next_arm_fails_closed_with_named_substrate() {
        let pipeline = pipeline_with_select_terminator(hew_mir::SelectArmKind::StreamNext {
            stream: Place::Local(0),
        });
        let ctx = Context::create();
        let err = build_module(&ctx, &pipeline, "select_stream_next")
            .expect_err("Terminator::Select stream-next must fail closed");
        let msg = match err {
            CodegenError::FailClosed(s) => s,
            other => panic!("expected FailClosed, got {other:?}"),
        };
        assert!(
            msg.contains("stream-next")
                && msg.contains("hew_select_wait")
                && msg.contains("hew_stream_poll")
                && msg.contains("hew_stream_cancel_pending_read"),
            "stream-next FailClosed must name the dispatch glue still \
             missing and the substrate primitives that now exist: {msg}"
        );
    }

    #[test]
    fn select_actor_ask_arm_fails_closed_with_named_substrate() {
        let pipeline = pipeline_with_select_terminator(hew_mir::SelectArmKind::ActorAsk {
            actor: Place::Local(0),
            method: "process".to_string(),
            args: Vec::new(),
        });
        let ctx = Context::create();
        let err = build_module(&ctx, &pipeline, "select_actor_ask")
            .expect_err("Terminator::Select actor-ask must fail closed");
        let msg = match err {
            CodegenError::FailClosed(s) => s,
            other => panic!("expected FailClosed, got {other:?}"),
        };
        assert!(
            msg.contains("actor-ask") && msg.contains("Terminator::Ask"),
            "actor-ask FailClosed must name the missing substrate \
             (per-arm body-block construction terminated by \
             Terminator::Ask): {msg}"
        );
    }

    #[test]
    fn select_task_await_arm_fails_closed_with_named_substrate() {
        let pipeline = pipeline_with_select_terminator(hew_mir::SelectArmKind::TaskAwait {
            task: Place::Local(0),
        });
        let ctx = Context::create();
        let err = build_module(&ctx, &pipeline, "select_task_await")
            .expect_err("Terminator::Select task-await must fail closed");
        let msg = match err {
            CodegenError::FailClosed(s) => s,
            other => panic!("expected FailClosed, got {other:?}"),
        };
        assert!(
            msg.contains("task-await")
                && msg.contains("hew_task_scope_cancel_one")
                && msg.contains("multiplex-await"),
            "task-await FailClosed must name the available cancel ABI \
             and the missing multiplex-await substrate: {msg}"
        );
    }

    #[test]
    fn select_after_timer_arm_fails_closed_with_named_substrate() {
        let pipeline = pipeline_with_select_terminator(hew_mir::SelectArmKind::AfterTimer {
            duration: Place::Local(0),
        });
        let ctx = Context::create();
        let err = build_module(&ctx, &pipeline, "select_after_timer")
            .expect_err("Terminator::Select after-timer must fail closed");
        let msg = match err {
            CodegenError::FailClosed(s) => s,
            other => panic!("expected FailClosed, got {other:?}"),
        };
        assert!(
            msg.contains("after-timer") && msg.contains("hew_select_wait"),
            "after-timer FailClosed must name the dispatch wiring: {msg}"
        );
    }
}
