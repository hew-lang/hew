//! Inkwell direct LLVM IR emitter for the out-of-process LLVM backend.
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
//!   without ordering constraints in the source.
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
//! IR construction runs in-process via inkwell. Object emission via
//! `TargetMachine::write_to_file` is **not** safe in that same process:
//! LLVM's legacy PassManager scheduler (which the C codegen API still
//! routes through) can hit an `addLowerLevelRequiredPass` trap after
//! earlier in-process LLVM setup has pre-touched the global
//! `PassRegistry`. The fix is structural — the back half runs in its own
//! process. `emit_module` writes the textual `.ll` in-process, then
//! spawns the sibling `hew-emit` helper binary to compile each
//! requested triple to a relocatable object. The helper process starts
//! with a clean `libLLVM` global-state footprint, so the legacy PM
//! scheduler finds its analyses and `write_to_file` succeeds.
//! See `src/bin/hew_emit.rs` for the helper's own module docs.
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

use hew_hir::stdlib_catalog::{self, BuiltinEntry, BuiltinLinkage, BuiltinTy, PrintKind};
use hew_hir::{mangle, HirRestartPolicy, HirSupervisorStrategy};
use hew_mir::{
    validate_context_markers, ActorLayout, CheckedMirFunction, CmpPred, CooperateKind,
    CooperateSite, ElabDrop, ElaboratedMirFunction, EnumLayout, ExitPath, FieldOffset, FloatWidth,
    FunctionCallConv, Instr, IntArithOp, IntSignedness, IrPipeline, MachineLayout,
    MachineVariantLayout, Place, RawMirFunction, RecordLayout, RegexLiteral, SupervisorChildLayout,
    SupervisorLayout, Terminator, TrapKind,
};
use hew_types::{NumericWidth, ResolvedTy};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue,
};
use inkwell::AddressSpace;
use inkwell::IntPredicate;

// Mirrored from `hew-runtime/src/execution_context.rs`'s HEW_CTX_OFFSET_* ABI
// constants. Runtime asserts those offsets with `offset_of!`; codegen keeps a
// copy so the compiler backend does not depend on the runtime crate.
const HEW_CTX_OFFSET_ACTOR: usize = 0;
const HEW_CTX_OFFSET_ACTOR_ID: usize = 8;
const HEW_CTX_OFFSET_PARENT_SUPERVISOR: usize = 16;
const HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX: usize = 24;
const HEW_CTX_OFFSET_FLAGS: usize = 28;
const HEW_CTX_OFFSET_CANCEL_TOKEN: usize = 32;
const HEW_CTX_OFFSET_TASK_SCOPE: usize = 40;
const HEW_CTX_OFFSET_ARENA: usize = 48;
const HEW_CTX_OFFSET_TRACE: usize = 56;
const HEW_TRACE_OFFSET_SPAN_ID: usize = 16;
const HEW_CTX_OFFSET_TRACE_SPAN: usize = HEW_CTX_OFFSET_TRACE + HEW_TRACE_OFFSET_SPAN_ID;
const HEW_ACTOR_OFFSET_ID: usize = 8;
const HEW_ACTOR_OFFSET_STATE: usize = 24;
const HEW_CTX_OFFSET_PARTITION_POLICY: usize = 96;
const HEW_CTX_OFFSET_PREV_CONTEXT: usize = 104;
const HEW_CTX_OFFSET_LOCK_SEAT: usize = 112;

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
    /// Omit the WASM target to produce a native binary instead.
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
                 (symbol: {symbol}; WASM-TODO(#1451)); omit the WASM target to \
                 produce a native binary instead"
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
/// IR-construction path is safe under the `hew` driver's normal in-process
/// LLVM setup. Object emission shells out to
/// the `hew-emit` helper (see the helper's module docs for why), so the
/// caller's binary must ship `hew-emit` alongside `hew` in the same
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
        // diagnostic with a WASM-target pointer rather than a raw linker
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

/// Return the first WASM-excluded substrate symbol found in `pipeline`'s
/// instruction stream, or `None` if none is present.
///
/// Excluded symbols:
/// - `hew_duplex_*` — excluded from wasm32 builds via
///   `hew-runtime/src/duplex.rs:54` (`#![cfg(not(target_arch = "wasm32"))]`).
///   WASM-TODO(#1451).
/// - `hew_supervisor_*` — requires the native preemptive scheduler's
///   supervisor restart machinery; WASM builds use a cooperative executor that
///   does not support it.  WASM-TODO(#1475).
/// - `hew_tcp_stream_from_conn` — TCP transport is unavailable on wasm32.
///   The runtime stub returns null; codegen surfaces a structured diagnostic
///   when a MIR `CallRuntimeAbi` refers to this symbol on a wasm target.
///   WASM-TODO(#1451): TCP transport gap.
///   Note: calls from the Hew stdlib `extern "C"` block in `std/net/net.hew`
///   bypass this scan (they produce direct LLVM calls, not `CallRuntimeAbi`
///   instructions); the wasm32 runtime stub is the safety net for those.
///
/// This scan detects them in the MIR before the `wasm-ld` step so the caller
/// can return `CodegenError::WasmUnsupportedSubstrate` instead of a confusing
/// linker error.
///
/// The scan covers:
/// - `Instr::CallRuntimeAbi` with an excluded symbol.
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
                        // hew_duplex_* — excluded via hew-runtime/src/duplex.rs:54
                        //   `#![cfg(not(target_arch = "wasm32"))]`.
                        //   WASM-TODO(#1451).
                        // hew_supervisor_* — the supervisor family requires the native
                        //   preemptive scheduler; WASM builds use a cooperative
                        //   single-threaded executor that does not support supervisor
                        //   restart machinery.
                        //   WASM-TODO(#1475): supervisor WASM parity is tracked there.
                        // hew_tcp_stream_from_conn — TCP transport is unavailable on
                        //   wasm32; the runtime stub returns null but codegen surfaces
                        //   a structured diagnostic instead of a silent null.
                        //   WASM-TODO(#1451): TCP transport gap.
                        (sym.starts_with("hew_duplex_")
                            || sym.starts_with("hew_supervisor_")
                            || sym == "hew_tcp_stream_from_conn")
                            .then(|| sym.to_string())
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
            if let Terminator::Select { arms, .. } = &block.terminator {
                if arms
                    .iter()
                    .any(|arm| matches!(arm.kind, hew_mir::SelectArmKind::StreamNext { .. }))
                {
                    return Some("hew_stream_poll".to_string());
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

/// Locate the `hew-emit` helper binary sibling to the currently running
/// executable, then invoke it to compile `ll_path` for `triple` into
/// `out_path`.
///
/// The rustc-driver-style discovery (sibling of `current_exe`) handles both
/// the dev layout (`target/debug/hew` next to `target/debug/hew-emit`)
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
            "hew-emit ({:?}) failed for triple={triple} out={}: {}",
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
        "hew-emit.exe"
    } else {
        "hew-emit"
    };
    let candidate = dir.join(name);
    if !candidate.exists() {
        return Err(CodegenError::Llvm(format!(
            "helper binary `{}` not found next to `{}`; \
             ensure `cargo build -p hew-codegen-rs --bin hew-emit` has run",
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
    return_resolved_ty: ResolvedTy,
    execution_context: Option<PointerValue<'ctx>>,
    execution_context_is_actor_handler: bool,
    actor_state_ty: Option<StructType<'ctx>>,
    /// Local-register id → (stack slot, slot's LLVM type). Keyed by the
    /// `Place::Local(N)` index — an MIR identity, not a checker derivative.
    locals: HashMap<u32, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
    /// Local-register id → resolved Hew type. Kept alongside `locals` for
    /// ABI helpers that must encode a value from its source type, not only its
    /// LLVM storage shape.
    local_tys: HashMap<u32, ResolvedTy>,
    /// Block id → LLVM `BasicBlock`. Populated up front so terminators can
    /// name forward targets.
    blocks: HashMap<u32, inkwell::basic_block::BasicBlock<'ctx>>,
    /// Module-wide runtime-ABI extern declarations interned on first use.
    /// Carried in a `RefCell` so the per-instruction lowering (which only
    /// borrows `FnCtx` immutably) can register new declarations lazily
    /// from inside `Instr::CallRuntimeAbi` and `Instr::Drop` arms.
    runtime_decls: RefCell<RuntimeDeclMap<'ctx>>,
    /// Module-wide record layouts (A-7). Keyed by record name; values are
    /// the LLVM named struct types registered up front in `build_module`
    /// via `register_record_layouts`. `Instr::RecordInit` /
    /// `Instr::RecordFieldLoad` consult this through `record_struct_for`
    /// to resolve a record-typed local's slot to its `StructType` for GEP
    /// indexing. The map is borrowed (no ownership) — the registered
    /// `StructType<'ctx>` values share the codegen context's lifetime.
    record_layouts: &'a RecordLayoutMap<'ctx>,
    /// Module-wide function symbol table. Populated before any body is
    /// lowered by the `declare_function` pass in `build_module`. The borrow
    /// is shared (read-only) — no new declarations are added during body
    /// lowering.
    fn_symbols: &'a FnSymbolMap<'ctx>,
    /// Module-wide actor layouts keyed by `ActorLayout.name` at use sites.
    /// Spawn lowering consumes these layouts to emit the WASM bridge metadata
    /// producer before calling into the runtime spawn ABI.
    actor_layouts: &'a [ActorLayout],
    /// Module-wide machine tagged-union layouts keyed by machine name
    /// (and by `<Name>Event` for event-companion enums). Populated up
    /// front in `build_module` via `register_machine_layouts`.
    /// `Place::MachineTag` / `Place::MachineVariant` / `Instr::EnumTagLoad`
    /// codegen consults this through `machine_layout_for_local` to
    /// resolve a machine-typed local's slot to its outer LLVM struct,
    /// per-variant inner structs, and tag integer width.
    machine_layouts: &'a MachineLayoutMap<'ctx>,
}

/// Module-level symbol table populated by the declaration pass. Keyed by
/// function name (a normal module-level identity); the value carries the
/// LLVM function value plus its return type so `Terminator::Call` can
/// resolve forward without re-deriving anything from the MIR shape.
type FnSymbolMap<'ctx> = HashMap<String, FnSymbol<'ctx>>;

#[derive(Clone, Copy)]
enum FnSymbol<'ctx> {
    Real {
        value: FunctionValue<'ctx>,
        return_ty: BasicTypeEnum<'ctx>,
        returns_unit: bool,
    },
    PrintIntercept {
        runtime_symbol: &'static str,
        kind: PrintKind,
        newline: bool,
    },
}

impl<'ctx> FnSymbol<'ctx> {
    fn real(
        self,
        callee: &str,
        context: &str,
    ) -> CodegenResult<(FunctionValue<'ctx>, BasicTypeEnum<'ctx>, bool)> {
        match self {
            Self::Real {
                value,
                return_ty,
                returns_unit,
            } => Ok((value, return_ty, returns_unit)),
            Self::PrintIntercept { .. } => Err(CodegenError::FailClosed(format!(
                "{context} expected `{callee}` to be a callable LLVM function, \
                 but it is a stdlib print intercept"
            ))),
        }
    }
}

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
/// CLI target-selection seam, not here.
///
/// Unknown symbols return `FailClosed`. The `RuntimeCall::new` allowlist
/// guard ensures construction-time rejection of unknown names, so reaching
/// this arm for an unknown symbol is an internal-consistency violation,
/// not a user error — fail loudly. LESSONS: boundary-fail-closed,
/// exhaustive-coverage.
///
/// If `llvm_mod.get_function(symbol)` finds a pre-existing declaration, this
/// adopts it without signature verification and relies on `Module::verify()` as
/// the downstream safety net. WHEN-OBSOLETE: replace this adoption path with an
/// explicit existing-signature check before caching the declaration.
fn intern_runtime_decl<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    decls: &mut RuntimeDeclMap<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(fv) = decls.get(symbol) {
        return Ok(*fv);
    }
    if let Some(fv) = llvm_mod.get_function(symbol) {
        decls.insert(symbol.to_string(), fv);
        return Ok(fv);
    }
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let fn_ty = match symbol {
        // hew_actor_cooperate() -> c_int
        // (`hew-runtime/src/scheduler.rs`, `scheduler_wasm.rs`). Decrements
        // the current actor's reductions budget and yields when exhausted.
        // The return value is a scheduler signal; cooperate-site injection
        // discards it.
        "hew_actor_ask" => ptr_ty.fn_type(
            &[ptr_ty.into(), i32_ty.into(), ptr_ty.into(), i64_ty.into()],
            false,
        ),
        // hew_actor_ask_with_channel(actor: *mut HewActor, msg_type: i32,
        //                            data: *mut c_void, size: usize,
        //                            ch: *mut HewReplyChannel) -> i32
        // (`hew-runtime/src/actor.rs:3259`). Returns 0 (HewError::Ok) on
        // success; non-zero indicates the ask could not be submitted (the
        // caller-provided channel ref is consumed only on success; on
        // failure the runtime releases the queued retain but the
        // caller-provided creator ref remains live so the select caller
        // can still free it via `hew_reply_channel_free`).
        "hew_actor_ask_with_channel" => i32_ty.fn_type(
            &[
                ptr_ty.into(),
                i32_ty.into(),
                ptr_ty.into(),
                i64_ty.into(),
                ptr_ty.into(),
            ],
            false,
        ),
        // hew_reply_channel_new() -> *mut HewReplyChannel
        // (`hew-runtime/src/reply_channel.rs:78`). Box-allocates a fresh
        // single-shot reply channel with one caller-side reference.
        // The caller must free with `hew_reply_channel_free` after
        // `hew_reply_wait` returns (or after cancel on the loser path).
        "hew_reply_channel_new" => ptr_ty.fn_type(&[], false),
        // hew_reply_wait(ch: *mut HewReplyChannel) -> *mut c_void
        // (`hew-runtime/src/reply_channel.rs:296`). Blocks until a reply
        // is deposited; returns the malloc'd reply pointer (caller frees
        // with `libc::free`) or null on orphaned-ask. The channel ref
        // count is NOT consumed — the caller must still call
        // `hew_reply_channel_free` exactly once.
        "hew_reply_wait" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        // hew_reply_channel_cancel(ch: *mut HewReplyChannel) -> void
        // (`hew-runtime/src/reply_channel.rs:440`). Marks the channel
        // cancelled so a late replier observes the flag and releases
        // its sender-side ref without UAF. MUST be called BEFORE
        // `hew_reply_channel_free` on every loser arm (Risk R4 in the
        // select-actor-ask-race plan; the cancel flag + ref count
        // prevent UAF when a reply races our free).
        "hew_reply_channel_cancel" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_reply_channel_free(ch: *mut HewReplyChannel) -> void
        // (`hew-runtime/src/reply_channel.rs:409`). Releases one
        // reference; frees the channel when the refcount reaches zero.
        "hew_reply_channel_free" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        "hew_reply_channel_retain" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        "hew_reply_channel_signal_ready" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_select_first(channels: *mut *mut HewReplyChannel, count: i32,
        //                  timeout_ms: i32) -> i32
        // (`hew-runtime/src/reply_channel.rs:484`). Returns the winning
        // channel index, or -1 on timeout (-1 timeout means wait
        // indefinitely; any non-negative value enforces a deadline).
        "hew_select_first" => i32_ty.fn_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false),
        "hew_actor_cooperate" => i32_ty.fn_type(&[], false),
        // hew_actor_link(a: *mut HewActor, b: *mut HewActor) -> void
        // (`hew-runtime/src/link.rs:80`). Establishes a bidirectional link.
        // Actor handles are opaque ptrs on the spine. Return is void — the
        // Hew `link()` builtin synthesises Ok(()) in Cluster 2.
        "hew_actor_link" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_actor_monitor(watcher: *mut HewActor, target: *mut HewActor) -> u64
        // (`hew-runtime/src/monitor.rs:157`). Returns a ref_id; 0 only on
        // null inputs. Actor handles are opaque ptrs. The u64 is wrapped into
        // MonitorRef { ref_id } in Cluster 2.
        "hew_actor_monitor" => i64_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        "hew_actor_send_by_id" => i32_ty.fn_type(
            &[i64_ty.into(), i32_ty.into(), ptr_ty.into(), i64_ty.into()],
            false,
        ),
        "hew_actor_state_lock_acquire" => i32_ty.fn_type(&[ptr_ty.into()], false),
        "hew_actor_state_lock_release" => i32_ty.fn_type(&[ptr_ty.into()], false),
        "hew_actor_spawn" => ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), ptr_ty.into()], false),
        // hew_actor_spawn_opts(opts: *const HewActorOpts) -> *mut HewActor
        // (`hew-runtime/src/actor.rs:1754`). Used when `#[max_heap(N)]` is
        // set; routes through the opts struct instead of the 3-arg spawn.
        "hew_actor_spawn_opts" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        // hew_wasm_register_actor_meta(meta: *const HewActorMeta) -> void
        // (`hew-runtime/src/bridge.rs`). WASM/test bridge registration for
        // trace actor-type attribution and host metadata queries. The metadata
        // structs are generated on the spawn path because this backend has no
        // module-init hook.
        "hew_wasm_register_actor_meta" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        "hew_get_reply_channel" => ptr_ty.fn_type(&[], false),
        "hew_reply" => ctx
            .bool_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into(), i64_ty.into()], false),
        "hew_sched_init" => i32_ty.fn_type(&[], false),
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
        // hew_vec_len(v: *mut HewVec) -> i64
        // (`hew-runtime/src/vec.rs:649`). Returns the element count as i64.
        "hew_vec_len" => i64_ty.fn_type(&[ptr_ty.into()], false),
        // hew_vec_get_i32(v: *mut HewVec, index: i64) -> i32
        // (`hew-runtime/src/vec.rs:394`). Bounds-checked by the MIR emitter
        // before this call; the runtime also aborts on OOB as defence-in-depth.
        "hew_vec_get_i32" => i32_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_vec_get_i64(v: *mut HewVec, index: i64) -> i64
        // (`hew-runtime/src/vec.rs:411`).
        "hew_vec_get_i64" => i64_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_vec_get_f64(v: *mut HewVec, index: i64) -> double
        // (`hew-runtime/src/vec.rs:456`).
        "hew_vec_get_f64" => ctx
            .f64_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_vec_get_ptr(v: *mut HewVec, index: i64) -> *mut c_void
        // (`hew-runtime/src/vec.rs:473`). For pointer-shaped elements
        // (Duplex handles, Named heap types). The caller casts the returned
        // opaque pointer to the appropriate concrete pointer type.
        "hew_vec_get_ptr" => ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_vec_get_str(v: *mut HewVec, index: i64) -> *const c_char
        // (`hew-runtime/src/vec.rs:430`). Returns a strdup'd copy; the
        // caller owns and must free the returned string. Allowlisted but
        // not emitted by this slice's MIR producer — Vec<String> indexing
        // deferred to a follow-on slice that wires the drop for the copy.
        "hew_vec_get_str" => ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // C-3 Vec<T> range-slice: hew_vec_slice_range_T(v, start, end) ->
        // *mut HewVec<T>. Caller owns the freshly-allocated result; drop
        // elaboration frees it via the existing hew_vec_free path.
        // (`hew-runtime/src/vec.rs` — added in this slice.)
        "hew_vec_slice_range_i32"
        | "hew_vec_slice_range_i64"
        | "hew_vec_slice_range_f64"
        | "hew_vec_slice_range_ptr"
        | "hew_vec_slice_range_str" => {
            ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false)
        }
        // ── scope{}/spawn/await task ABI (Phase 2, inventory rows 2/3/4) ──────
        //
        // hew_scope_spawn(scope: *mut HewScope, actor: *mut c_void) -> i32
        // (`hew-runtime/src/scope.rs:169`). Adds an actor to the scope's
        // actor list; returns 0 on success, -1 if full. i32 return is a
        // runtime-internal signal — MIR producers discard it (dest: None).
        "hew_scope_spawn" => i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        "hew_rc_new" => ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), ptr_ty.into()], false),
        // hew_supervisor_new(strategy: c_int, max_restarts: c_int, window_secs: c_int)
        //                    -> *mut HewSupervisor
        // (`hew-runtime/src/supervisor.rs:1608`). Box-allocates an empty
        // supervisor. Codegen calls this from the synthesised bootstrap body.
        "hew_supervisor_new" => {
            ptr_ty.fn_type(&[i32_ty.into(), i32_ty.into(), i32_ty.into()], false)
        }
        // hew_supervisor_add_child_spec(sup: *mut HewSupervisor,
        //                               spec: *const HewChildSpec) -> c_int
        // (`hew-runtime/src/supervisor.rs:1655`). Registers one child spec on
        // the supervisor; the runtime deep-copies `name` and `init_state` so
        // the stack-allocated literal is safe to discard after the call.
        // Returns 0 on success or -1 on null/OOM; the bootstrap currently
        // discards the result — restart/diagnostic wiring lands in a follow-on.
        "hew_supervisor_add_child_spec" => i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_supervisor_start(sup: *mut HewSupervisor) -> c_int
        // (`hew-runtime/src/supervisor.rs:1726`). Marks the supervisor running
        // and spawns its self-actor. Returns 0 on success. The bootstrap
        // traps on non-zero (fail-closed) before returning the supervisor ptr.
        "hew_supervisor_start" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // hew_supervisor_stop(sup: *mut HewSupervisor) -> void
        // (`hew-runtime/src/supervisor.rs:1944`). Requests graceful shutdown of
        // the supervisor and all its children. Void return; the Hew builtin
        // `supervisor_stop(sup)` discards the result.
        "hew_supervisor_stop" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_supervisor_child_get(sup: *mut HewSupervisor, key: u32)
        //     -> ChildLookupResult
        // (`hew-runtime/src/supervisor.rs:2795`). Returns the live actor handle
        // for slot `key`, or a tagged Transient/Dead result when the slot is
        // unavailable (mid-restart, circuit-open, budget-exhausted, or the
        // supervisor is shut down). The 16-byte `ChildLookupResult` C-ABI struct
        // On aarch64 (SysV/AAPCS), structs ≤ 16 bytes are returned in x0:x1
        // as a `[2 x i64]` aggregate — NOT via an x8 sret pointer.  The Rust
        // runtime emits `define [2 x i64] @hew_supervisor_child_get(...)`.
        // Declaring the return type as `{ i8, i8, [6 x i8], ptr }` causes LLVM
        // to choose the indirect-return (sret) path, so the caller reads the tag
        // byte from a stale x8 stack slot → spurious non-zero tag → trap 206.
        //
        // Fix: declare the return type as `{ i64, i64 }`.  Field 0 is the
        // packed `(tag: u8, reason: u8, _pad: [u8; 6])` word (tag lives in the
        // low byte); field 1 is the handle integer.  The extractvalue block
        // below truncates field 0 to i8 for the tag check.
        //
        // `key` is `u32` (i32 in LLVM) — the slot index fits in 32 bits.
        //
        // NOTE: `hew_supervisor_nested_get` (hew-runtime/src/supervisor.rs)
        // also returns `ChildLookupResult` by value and will need the same
        // `{ i64, i64 }` ABI declaration when wired into codegen.
        //
        // WASM: supervisor child access requires the native scheduler; see
        // `uses_wasm_excluded_symbol` which gates this symbol at WASM emit time.
        "hew_supervisor_child_get" => {
            let result_ty = ctx.struct_type(&[i64_ty.into(), i64_ty.into()], false);
            result_ty.fn_type(&[ptr_ty.into(), i32_ty.into()], false)
        }
        // hew_task_new() -> *mut HewTask
        // (`hew-runtime/src/task_scope.rs:214`). Box-allocates a HewTask
        // in the Ready state. Producer calls this to obtain a task handle
        // before calling hew_task_spawn_thread.
        "hew_task_new" => ptr_ty.fn_type(&[], false),
        "hew_task_complete_threaded" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        "hew_task_get_error" => i32_ty.fn_type(&[ptr_ty.into()], false),
        "hew_task_get_env" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        "hew_task_scope_new" => ptr_ty.fn_type(&[], false),
        "hew_task_scope_destroy" | "hew_task_scope_join_all" => {
            ctx.void_type().fn_type(&[ptr_ty.into()], false)
        }
        "hew_task_scope_spawn" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        "hew_task_scope_set_current" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        "hew_task_set_env" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        "hew_task_scope_cancel_after_ns" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_task_spawn_thread(task: *mut HewTask, task_fn: TaskFn) -> void
        // (`hew-runtime/src/task_scope.rs:368`). Spawns task_fn(task) on
        // a new OS thread. TaskFn = unsafe extern "C" fn(*mut HewTask).
        // Both args are opaque pointers at the LLVM level — the task handle
        // is ptr-typed; the function pointer is also ptr-typed in opaque-
        // pointer mode (LLVM 15+ ptr is used for all pointee types).
        "hew_task_spawn_thread" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_task_spawn_thread_with_inherited_context(
        //     parent_ctx: *mut HewExecutionContext,
        //     task: *mut HewTask,
        //     task_fn: ContextTaskFn,
        // ) -> i32
        // (`hew-runtime/src/task_scope.rs`). Spawns a codegen-synthesised
        // `void (*)(HewExecutionContext*, HewTask*)` wrapper and installs a
        // child context derived from parent_ctx on the worker thread.
        "hew_task_spawn_thread_with_inherited_context" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_task_await_blocking(task: *mut HewTask) -> *mut c_void
        // (`hew-runtime/src/task_scope.rs:411`). Blocks until the task
        // completes and returns its result pointer (null for void tasks).
        // Producer must supply a dest for the result pointer; callers that
        // ignore the result still need the blocking guarantee.
        "hew_task_await_blocking" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        // hew_task_get_result(task: *mut HewTask) -> *mut c_void
        // (`hew-runtime/src/task_scope.rs:283`). Returns the result pointer
        // if Done, null otherwise. Called after hew_task_await_blocking
        // when the producer needs to read the result separately.
        "hew_task_get_result" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        "hew_task_completion_observe" => i32_ty.fn_type(
            &[ptr_ty.into(), ptr_ty.into(), ptr_ty.into(), ptr_ty.into()],
            false,
        ),
        "hew_task_completion_unobserve" => i32_ty.fn_type(
            &[ptr_ty.into(), ptr_ty.into(), ptr_ty.into(), ptr_ty.into()],
            false,
        ),
        // hew_task_free(task: *mut HewTask) -> void
        // (`hew-runtime/src/task_scope.rs:237`). Frees the Box-allocated
        // HewTask and its result buffer. Called by the scope teardown path
        // after all tasks have been awaited.
        "hew_task_free" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        "hew_stream_poll" => i64_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false),
        "hew_stream_cancel_pending_read" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_require_execution_context() -> *mut HewExecutionContext
        // (`hew-runtime/src/execution_context.rs`). Returns the current
        // per-dispatch execution context. Used by codegen-emitted terminate
        // trampolines to bridge the terminate-fn ABI (`fn(*mut c_void)`) to
        // the ActorHandler ABI (`fn(*mut HewExecutionContext)`).
        "hew_require_execution_context" => ptr_ty.fn_type(&[], false),
        // hew_actor_set_terminate(actor: *mut HewActor,
        //                         terminate_fn: unsafe extern "C" fn(*mut c_void)) -> void
        // (`hew-runtime/src/actor.rs`). Registers the codegen-emitted
        // terminate trampoline on the actor so the runtime calls it at normal
        // actor teardown. Called once at spawn time, after hew_actor_spawn.
        "hew_actor_set_terminate" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // ── Regex literal substrate (codegen-emitted, not user-callable) ──────
        //
        // hew_regex_compile(pattern: *const c_char) -> *mut HewRegex
        // (`std/text/regex/src/lib.rs`). Called once per regex literal from the
        // `hew_module_init_regex` constructor registered in `@llvm.global_ctors`.
        // Stores the compiled handle into the module-level `@hew_regex_handles`
        // global array. Returns null on invalid pattern (should not happen: the
        // type-checker validated the syntax); codegen traps fail-closed if null.
        "hew_regex_compile" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        // hew_regex_match(re: *const HewRegex, text: *const c_char) -> i32
        // (`std/text/regex/src/lib.rs`). Called by compiler-emitted match-arm
        // predicate code. The `re` pointer is loaded from `@hew_regex_handles`
        // by GEP before the call (codegen resolves literal_id → handle inline).
        // Returns 1 on match, 0 on no-match or null `re`. MIR emits an
        // `IntCmp(NotEq, result, 0i32)` immediately after to produce the branch
        // condition; returning i32 rather than i1 avoids C ABI bool-extension
        // hazards on all supported targets.
        "hew_regex_match" => i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_regex_capture(re: *const HewRegex, text: *const c_char,
        //                   capture_idx: i64) -> *mut c_char
        // (`std/text/regex/src/lib.rs`). Called by compiler-emitted capture-
        // extraction code after a successful `hew_regex_match`. `capture_idx`
        // is the 0-based index into the pattern's named-capture list
        // (i64 to match the `ConstI64` local that MIR uses for the index).
        // Returns a malloc-owned NUL-terminated string for the captured group,
        // or null for non-participating optional groups. Callers free with
        // `libc::free`. The null-check in the MIR capture chain drives the
        // "missing capture → try next arm" branch (fail-closed).
        "hew_regex_capture" => {
            ptr_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), i64_ty.into()], false)
        }
        // hew_regex_free_capture(ptr: *mut c_char) -> void
        // Frees a capture string returned by hew_regex_capture (malloc-owned).
        // Emitted by MIR at arm-body exit (success path) and on the partial-failure
        // cleanup paths (captures[0..j] already allocated when capture[j] is null).
        "hew_regex_free_capture" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        other => {
            return Err(CodegenError::FailClosed(format!(
                "intern_runtime_decl: codegen has no LLVM signature for runtime \
                 symbol {other:?}; the symbol is on the MIR_EMITTER_RUNTIME_SYMBOLS allowlist \
                 but no codegen arm wires it — extend the signature table or leave \
                 the producer fail-closed"
            )));
        }
    };
    let fv = llvm_mod.add_function(symbol, fn_ty, Some(Linkage::External));
    decls.insert(symbol.to_string(), fv);
    Ok(fv)
}

fn fn_type_for_return<'ctx>(
    ctx: &'ctx Context,
    return_ty: Option<BasicTypeEnum<'ctx>>,
    params: &[BasicMetadataTypeEnum<'ctx>],
) -> FunctionType<'ctx> {
    match return_ty {
        Some(BasicTypeEnum::IntType(t)) => t.fn_type(params, false),
        Some(BasicTypeEnum::FloatType(t)) => t.fn_type(params, false),
        Some(BasicTypeEnum::StructType(t)) => t.fn_type(params, false),
        Some(BasicTypeEnum::PointerType(t)) => t.fn_type(params, false),
        Some(BasicTypeEnum::ArrayType(t)) => t.fn_type(params, false),
        Some(BasicTypeEnum::VectorType(t)) => t.fn_type(params, false),
        Some(BasicTypeEnum::ScalableVectorType(t)) => t.fn_type(params, false),
        None => ctx.void_type().fn_type(params, false),
    }
}

fn builtin_type_to_llvm<'ctx>(
    ctx: &'ctx Context,
    ty: BuiltinTy,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<BasicTypeEnum<'ctx>> {
    resolve_ty(ctx, &ty.to_resolved(), record_layouts)
}

fn builtin_return_to_llvm<'ctx>(
    ctx: &'ctx Context,
    ty: BuiltinTy,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<Option<BasicTypeEnum<'ctx>>> {
    match ty {
        BuiltinTy::Unit | BuiltinTy::Never => Ok(None),
        other => Ok(Some(builtin_type_to_llvm(ctx, other, record_layouts)?)),
    }
}

fn declare_print_runtime<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    symbol: &str,
) -> FunctionValue<'ctx> {
    if let Some(fv) = llvm_mod.get_function(symbol) {
        return fv;
    }
    let fn_ty = ctx.void_type().fn_type(
        &[
            ctx.i8_type().into(),
            ctx.i64_type().into(),
            ctx.bool_type().into(),
        ],
        false,
    );
    llvm_mod.add_function(symbol, fn_ty, Some(Linkage::External))
}

fn declare_catalog_ffi<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    entry: &BuiltinEntry,
    symbol: &str,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<FnSymbol<'ctx>> {
    let mut param_tys: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(entry.params.len());
    for param in entry.params {
        param_tys.push(metadata_type_from_basic(builtin_type_to_llvm(
            ctx,
            *param,
            record_layouts,
        )?));
    }
    let return_ty = builtin_return_to_llvm(ctx, entry.return_ty, record_layouts)?;
    let fn_ty = fn_type_for_return(ctx, return_ty, &param_tys);
    let value = llvm_mod
        .get_function(symbol)
        .unwrap_or_else(|| llvm_mod.add_function(symbol, fn_ty, Some(Linkage::External)));
    Ok(FnSymbol::Real {
        value,
        return_ty: return_ty.unwrap_or_else(|| ctx.i8_type().into()),
        returns_unit: return_ty.is_none(),
    })
}

fn predeclare_stdlib_catalog<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    fn_symbols: &mut FnSymbolMap<'ctx>,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<()> {
    for entry in stdlib_catalog::entries() {
        match entry.linkage {
            BuiltinLinkage::RuntimeFfiShim { symbol }
            | BuiltinLinkage::ToStringShim { symbol }
            | BuiltinLinkage::StringCloneShim { symbol } => {
                let symbol_entry =
                    declare_catalog_ffi(ctx, llvm_mod, entry, symbol, record_layouts)?;
                fn_symbols.insert(entry.name.to_string(), symbol_entry);
            }
            BuiltinLinkage::PrintIntercept {
                runtime_symbol,
                kind,
                newline,
            } => {
                declare_print_runtime(ctx, llvm_mod, runtime_symbol);
                fn_symbols.insert(
                    entry.name.to_string(),
                    FnSymbol::PrintIntercept {
                        runtime_symbol,
                        kind,
                        newline,
                    },
                );
            }
            BuiltinLinkage::CompilerIntrinsic { .. } => {}
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Type mapping
// ---------------------------------------------------------------------------

/// Per-module map from a `record` type's name to its registered LLVM named
/// struct type. Populated once per `build_module` from
/// `IrPipeline.record_layouts` (A-7 slice). Codegen consults this map in
/// `resolve_ty` before falling to `primitive_to_llvm` so that
/// `ResolvedTy::Named { name, .. }` references to user records resolve to
/// the struct type rather than tripping the D10-violation fail-closed arm.
///
/// The map is keyed by the bare record name (no generic-args mangling)
/// because A-7's surface is monomorphic — generic records are deferred.
type RecordLayoutMap<'ctx> = HashMap<String, StructType<'ctx>>;

/// Register every named-form record from `layouts` as an LLVM named struct
/// type on `ctx`, populating the body with each field's LLVM lowering.
///
/// Two-pass to support records that reference each other by name (forward /
/// mutual references are valid Hew; the struct body resolution must see
/// every record's opaque type before we attempt to set any body):
/// 1. Pass 1: create an opaque named struct for every record so cross-
///    references can resolve.
/// 2. Pass 2: lower each field type and call `set_body` on the opaque
///    struct.
///
/// Returns the populated map. Fails closed if any field type cannot be
/// lowered — e.g. a record field with a `Tuple` or `Array` type (Cluster 2
/// composite lowering pending), or a `Named` type that names neither a
/// registered record nor a built-in handle. The MIR producer + checker
/// would have rejected such a record at HIR-validation time, so reaching
/// the fail-closed arm here is itself a bug.
fn register_record_layouts<'ctx>(
    ctx: &'ctx Context,
    layouts: &[RecordLayout],
) -> CodegenResult<RecordLayoutMap<'ctx>> {
    let mut map: RecordLayoutMap<'ctx> = HashMap::new();
    // Pass 1: opaque named structs.
    for layout in layouts {
        let st = ctx.opaque_struct_type(&layout.name);
        map.insert(layout.name.clone(), st);
    }
    // Pass 2: set body for each. Records-of-records resolve via the map's
    // opaque entries created in pass 1.
    for layout in layouts {
        let st = map
            .get(&layout.name)
            .copied()
            .expect("pass 1 populated every record name");
        let mut field_tys: Vec<BasicTypeEnum<'ctx>> = Vec::with_capacity(layout.field_tys.len());
        for fty in &layout.field_tys {
            field_tys.push(resolve_ty(ctx, fty, &map)?);
        }
        // packed = false: use the target's natural alignment per
        // `RecordLayout` doc (A-6b). LESSONS: parity-or-tracked-gap.
        st.set_body(&field_tys, false);
    }
    Ok(map)
}

// ---------------------------------------------------------------------------
// Machine tagged-union layouts (Slice 5)
// ---------------------------------------------------------------------------
//
// **Layout invariants** (load-bearing — Slice 4b dispatch, Slice 4c
// drop-elaborator, and forthcoming `m.step(ev)` ABI all rely on these):
//
// 1. The outer machine struct is a non-packed LLVM struct
//    `{ tag: iN, payload: [N x iA] }` where:
//      - `tag` is `i8` for up to 256 variants and `i16` for up to
//        65,536 variants. Larger machines fail closed with
//        `CodegenError::Unsupported`.
//      - `tag` is at struct field index 0 (offset 0 bytes — natural
//        alignment for the selected tag integer width).
//      - `payload` is a fixed-size byte array sized to fit the largest
//        per-variant payload struct. The byte size is `TargetData::get_abi_size`
//        of the variant's LLVM struct — ABI-correct, including inter-field
//        padding (see sizing note below).
//
// 2. Each variant has its own anonymous LLVM struct
//    `{ field_0, field_1, ... }` matching declaration order in
//    `HirMachineState.fields`. Variant access bitcasts the payload
//    pointer to the variant struct pointer and indexes via GEP.
//
// 3. `Place::MachineTag(local)` addresses the outer struct's field 0.
//    `Place::MachineVariant { local, variant_idx, field_idx }` addresses
//    `variant_struct.field_idx` through the bitcast payload pointer.
//
// **Sizing**: `build_tagged_union_layout` constructs each variant's LLVM
// struct type first, then calls `host_target_data().get_abi_size(variant_struct)`
// to obtain the ABI-correct byte count including inter-field padding.
// `host_target_data()` initialises the native LLVM target once (via `OnceLock`)
// and builds a `TargetData` from the host data-layout string. The wasm32 emit
// path runs in a separate process (`hew_emit`) with its own data layout set
// after IR generation; sizing at IR-build time uses the host layout, which
// matches the native target. WASM-TODO(#1451): wasm32 variant layout accuracy.
//
// **Alignment**: the outer payload field is `[K x iA]` where `A` is the
// max ABI alignment across all variant payload structs (in bytes) and
// `K = ceil(N / A)`. The element type's natural alignment is `A`, so the
// payload field itself is `A`-byte aligned in the outer struct, and variant
// GEPs (which bitcast the payload pointer to the variant struct type) target
// pointers whose alignment satisfies the variant's most-aligned field. A
// 1-byte-aligned `[N x i8]` payload would be LLVM IR UB: GEP'd loads/stores
// of i32/i64 fields at sub-natural alignment have no contract that LLVM
// emits unaligned-tolerant ops — tier-1 hosts happen to do so today, but the
// spec does not require it. Note that `Module::verify()` does NOT catch
// ABI-alignment violations; the payload-element-type computed here is what
// guarantees natural alignment.

/// Per-module map from a machine type name to its registered tagged-union
/// LLVM layout. Populated once per `build_module` from
/// `IrPipeline.machine_layouts`.
type MachineLayoutMap<'ctx> = HashMap<String, MachineCodegenLayout<'ctx>>;

/// Cached LLVM types for one machine declaration's tagged-union layout.
#[derive(Clone)]
struct MachineCodegenLayout<'ctx> {
    /// Outer named LLVM struct: `{ tag: iW, payload: [N x i8] }`. Used by
    /// `place_resolved_ty(Place::MachineTag(local))` to size the alloca
    /// and by `place_pointer` to GEP into field 0 (tag) or field 1
    /// (payload).
    outer_struct: StructType<'ctx>,
    /// LLVM integer type for the discriminant tag (`iW` where
    /// `i8`/`i16` selected from the declared variant count. Cached so
    /// `Place::MachineTag` loads use
    /// the same type the alloca was declared with.
    tag_int_ty: inkwell::types::IntType<'ctx>,
    /// Per-variant anonymous struct types in declaration order. Index
    /// matches `MachineLayout.variants[i]`. Used by
    /// `place_pointer(Place::MachineVariant { variant_idx, .. })` to
    /// bitcast the payload byte array to the active variant's struct.
    variant_struct_tys: Vec<StructType<'ctx>>,
    /// Resolved per-variant field types in declaration order. Used by
    /// `place_resolved_ty(Place::MachineVariant { ... })` to surface the
    /// `ResolvedTy` of a payload field load/store without going through
    /// the LLVM type back to a Hew type. Index `i` matches
    /// `MachineLayout.variants[i].field_tys`.
    variant_field_tys: Vec<Vec<ResolvedTy>>,
    /// Static `[N x ptr]` table of state-name strings for `Instr::MachineStateName`.
    /// `state_name_table[i]` is a pointer to a private NUL-terminated read-only
    /// global holding `variants[i].name`. Populated only for the state-side
    /// machine layout; the `<Name>Event` companion layout entry has `None`.
    state_name_table: Option<inkwell::values::GlobalValue<'ctx>>,
}

/// Return the native host data-layout string, initialising LLVM's native
/// target exactly once per process (guarded by `OnceLock`).
///
/// `TargetData` wraps a raw pointer and is not `Sync`, so we cache the
/// layout *string* (which is `Sync`) and construct a fresh `TargetData`
/// from it on each call via `TargetData::create`. `TargetData::create` is
/// cheap — it only parses the layout string into target-description tables;
/// it does not allocate LLVM IR or touch the global PassManager.
///
/// The in-process LLVM limitation that motivated the `hew_emit` split
/// applies only to `TargetMachine::write_to_file` (legacy PassManager
/// global). Initialising the native target for layout queries has no
/// conflicting side effects.
///
/// **Multi-target portability**: this function returns the *host* data layout.
/// `emit_module` builds a single target-agnostic textual IR that `hew-emit`
/// re-parses for both native and wasm32 in separate processes — there is no
/// single "current target" at IR-build time. The primitive ABI alignments
/// Hew currently lowers (i8/i16/i32/i64/ptr) agree across native and wasm32
/// data layouts, so querying the host's `TargetData` for variant ABI size and
/// alignment in `build_tagged_union_layout` yields the same answers as
/// querying the wasm32 target would. The wider-integer payload element type
/// computed there makes per-target alignment moot — the IR is naturally
/// aligned under either target's data layout.
fn host_data_layout_string() -> &'static str {
    use std::sync::OnceLock;
    static HOST_DL: OnceLock<String> = OnceLock::new();
    HOST_DL.get_or_init(|| {
        // `initialize_native` is idempotent; `base=true` registers only
        // the target description, not the asm printer/parser.
        Target::initialize_native(&InitializationConfig {
            base: true,
            ..InitializationConfig::default()
        })
        .expect(
            "host_data_layout_string: native LLVM target failed to initialise — \
             the host platform is not supported by this build of LLVM",
        );
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .expect("host_data_layout_string: no LLVM target for host triple");
        // OptimizationLevel / RelocMode / CodeModel do not affect the data
        // layout string; use defaults to minimise setup cost.
        let tm = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                inkwell::OptimizationLevel::None,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("host_data_layout_string: TargetMachine construction failed");
        tm.get_target_data()
            .get_data_layout()
            .as_str()
            .to_string_lossy()
            .into_owned()
    })
}

/// Construct a `TargetData` for the native host from the cached data-layout
/// string. See `host_data_layout_string` for initialisation details.
fn host_target_data() -> TargetData {
    TargetData::create(host_data_layout_string())
}

/// Register every machine from `pipeline.machine_layouts` as a named LLVM
/// tagged-union struct. See the module-level "Layout invariants" block
/// for the struct shape and access pattern.
///
/// Variant payload structs are anonymous; only the outer machine type
/// is named so `Place::MachineTag(local)` slots can be alloca'd by name.
///
/// LESSONS: `feedback_fail_closed_not_pretend` — out-of-range variant or
/// unsupported payload type returns `CodegenError::FailClosed`, never a
/// silent fallback to variant 0 or a zero-sized payload.
fn register_machine_layouts<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    machine_layouts: &[MachineLayout],
    record_layout_map: &mut RecordLayoutMap<'ctx>,
) -> CodegenResult<MachineLayoutMap<'ctx>> {
    let mut map: MachineLayoutMap<'ctx> = HashMap::new();
    for layout in machine_layouts {
        // The state-side machine value: `<Name>` with state-variant payloads.
        let mut machine_cg =
            build_tagged_union_layout(ctx, &layout.name, &layout.variants, record_layout_map)?;
        // Build the per-machine state-name string table. Each entry is a
        // pointer to a private NUL-terminated read-only global; the table
        // itself is a private `[N x ptr]` constant. `Instr::MachineStateName`
        // reads the machine's tag and GEPs into this table.
        machine_cg.state_name_table = Some(build_state_name_table(
            ctx,
            llvm_mod,
            &layout.name,
            &layout.variants,
        )?);
        // Register the outer struct in the shared layout map so
        // `resolve_ty` finds `ResolvedTy::Named { name: "<Name>" }`
        // naturally — the alloca slot for a `self: <Name>` parameter
        // resolves the same way a record-typed local does.
        record_layout_map.insert(layout.name.clone(), machine_cg.outer_struct);
        map.insert(layout.name.clone(), machine_cg);

        // The companion event enum: `<Name>Event` with event-variant
        // payloads. Tag bit width is derived from the event count
        let event_name = format!("{}Event", layout.name);
        let event_cg =
            build_tagged_union_layout(ctx, &event_name, &layout.events, record_layout_map)?;
        record_layout_map.insert(event_name.clone(), event_cg.outer_struct);
        map.insert(event_name, event_cg);
    }
    Ok(map)
}

/// Register every user-defined enum from `pipeline.enum_layouts` as a named
/// LLVM tagged-union struct, inserting into the shared `machine_layouts` map
/// so that `Place::MachineTag` / `Place::MachineVariant` codegen can look up
/// enum-typed locals by their type name.
///
/// User enums share the tagged-union substrate (`{ tag: iW, payload: [N x i8] }`)
/// with machine states and event companions. Monomorphic enums of every
/// variant shape (unit, tuple, struct) lower end-to-end through this
/// substrate — `build_tagged_union_layout` walks per-variant `field_tys` and
/// emits an alignment-correct payload byte array sized to the widest variant.
/// Generic enums (`enum Maybe<T> { ... }`) are fully supported via the
/// `EnumLayoutRegistry` substrate; each instantiation arrives in `enum_layouts`
/// under a mangled name (e.g. `Option$$i64`) and is registered here.
fn register_enum_layouts<'ctx>(
    ctx: &'ctx Context,
    enum_layouts: &[EnumLayout],
    record_layout_map: &mut RecordLayoutMap<'ctx>,
    machine_layout_map: &mut MachineLayoutMap<'ctx>,
) -> CodegenResult<()> {
    for layout in enum_layouts {
        // Convert `EnumLayout.variants` (which are `MachineVariantLayout`)
        // directly — they share the same shape (`name`, `field_tys`).
        let enum_cg =
            build_tagged_union_layout(ctx, &layout.name, &layout.variants, record_layout_map)?;
        // Register the outer struct so `resolve_ty` resolves
        // `ResolvedTy::Named { name: "<EnumName>" }` the same way as a
        // machine-typed or record-typed local.
        record_layout_map.insert(layout.name.clone(), enum_cg.outer_struct);
        machine_layout_map.insert(layout.name.clone(), enum_cg);
    }
    Ok(())
}

/// Shared builder for the machine-value and event-companion tagged-union
/// LLVM types. Both share the `{ tag: iW, payload: [N x i8] }` shape
/// described in the layout-invariants block above.
///
/// Payload byte count is `TargetData::get_abi_size` of the variant's LLVM
/// struct — ABI-correct for the host native target including inter-field
/// padding (see sizing note in the block comment above). `record_layouts`
/// is no longer a parameter; variant LLVM types are resolved through
/// `record_layout_map` (the already-populated LLVM type map), which carries
/// the same type information without needing raw `RecordLayout` access.
fn build_tagged_union_layout<'ctx>(
    ctx: &'ctx Context,
    outer_name: &str,
    variants: &[MachineVariantLayout],
    record_layout_map: &RecordLayoutMap<'ctx>,
) -> CodegenResult<MachineCodegenLayout<'ctx>> {
    // Build each variant's LLVM struct type first, then query its ABI size.
    // This order is required: `get_abi_size` operates on a completed LLVM
    // type; we cannot query sizes incrementally from `ResolvedTy` fields
    // without replicating LLVM's padding rules.
    let mut variant_struct_tys: Vec<StructType<'ctx>> = Vec::with_capacity(variants.len());
    let mut variant_field_tys: Vec<Vec<ResolvedTy>> = Vec::with_capacity(variants.len());
    for variant in variants {
        let field_tys: Vec<BasicTypeEnum<'ctx>> = variant
            .field_tys
            .iter()
            .map(|fty| resolve_ty(ctx, fty, record_layout_map))
            .collect::<CodegenResult<Vec<_>>>()?;
        variant_struct_tys.push(ctx.struct_type(&field_tys, false));
        variant_field_tys.push(variant.field_tys.clone());
    }

    // Compute the max ABI byte count AND max ABI alignment across all
    // variant structs. Empty-variant unions yield 0 for both; we floor the
    // byte count at 1 so field 1 is always a non-empty array and GEP indices
    // stay consistent regardless of payload presence, and floor the alignment
    // at 1 so the payload element type is well-defined.
    //
    // Alignment-aware payload typing: the payload is emitted as
    // `[ceil(payload_bytes / max_align) x i{max_align*8}]`. The element type
    // gives the payload field its natural alignment of `max_align` bytes, so
    // variant-struct GEPs (which bitcast the payload pointer to the variant
    // struct type) target pointers whose alignment meets the variant's
    // most-aligned field. Primitive ABI alignments (i8=1, i16=2, i32=4,
    // i64=8, ptr=8 on the targets Hew currently emits for) are consistent
    // across native and wasm32 data layouts, so this single IR shape is
    // valid on every target Hew lowers to. See the "Alignment" note in the
    // layout-invariants block above.
    let td = host_target_data();
    let mut max_bytes: u64 = 0;
    let mut max_align: u32 = 0;
    for vs in &variant_struct_tys {
        let abi_bytes = td.get_abi_size(vs);
        if abi_bytes > max_bytes {
            max_bytes = abi_bytes;
        }
        let abi_align = td.get_abi_alignment(vs);
        if abi_align > max_align {
            max_align = abi_align;
        }
    }
    let payload_align = max_align.max(1);
    // Round payload byte count up to a multiple of `payload_align` so the
    // array element count `K = payload_bytes / payload_align` is exact.
    let payload_align_u64 = u64::from(payload_align);
    let payload_bytes = {
        let bytes = max_bytes.max(1);
        // `bytes` and `payload_align_u64` are non-zero; round bytes up to a
        // multiple of `payload_align_u64` so the array element count is exact.
        bytes.div_ceil(payload_align_u64) * payload_align_u64
    };
    let element_count_u64 = payload_bytes / payload_align_u64;
    let element_count_u32 = u32::try_from(element_count_u64).map_err(|_| {
        CodegenError::FailClosed(format!(
            "machine `{outer_name}` payload element count {element_count_u64} exceeds u32 — \
             a variant with > 4 GiB payload is unsupported"
        ))
    })?;
    // Element bit width: `payload_align` bytes => `payload_align * 8` bits.
    // Cap at 64 because LLVM's array element types and Hew's primitive set
    // top out at i64; an alignment >8 on a Hew variant would imply a vector
    // or aggregate field type the current substrate does not lower.
    if payload_align > 8 {
        return Err(CodegenError::FailClosed(format!(
            "machine `{outer_name}` requires payload alignment {payload_align} > 8 bytes — \
             Hew's primitive substrate tops out at i64 (8-byte alignment); a variant \
             field with a wider alignment requirement is unsupported"
        )));
    }
    let element_bits = payload_align * 8;
    let element_bits_nz = std::num::NonZeroU32::new(element_bits).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "machine `{outer_name}` derived zero-bit payload element — \
             payload_align computation is invalid"
        ))
    })?;
    let payload_element_int_ty = ctx
        .custom_width_int_type(element_bits_nz)
        .map_err(|e| CodegenError::Llvm(format!("custom_width_int_type({element_bits}): {e}")))?;

    let tag_int_ty = tag_int_type_for_variant_count(ctx, outer_name, variants.len())?;
    let payload_arr_ty = payload_element_int_ty.array_type(element_count_u32);

    let outer_struct = ctx.opaque_struct_type(outer_name);
    outer_struct.set_body(&[tag_int_ty.into(), payload_arr_ty.into()], false);

    Ok(MachineCodegenLayout {
        outer_struct,
        tag_int_ty,
        variant_struct_tys,
        variant_field_tys,
        state_name_table: None,
    })
}

fn tag_int_type_for_variant_count<'ctx>(
    ctx: &'ctx Context,
    _outer_name: &str,
    variant_count: usize,
) -> CodegenResult<inkwell::types::IntType<'ctx>> {
    if variant_count <= 256 {
        Ok(ctx.i8_type())
    } else if variant_count <= 65_536 {
        Ok(ctx.i16_type())
    } else {
        Err(CodegenError::Unsupported(
            "machine tagged-union layout supports at most 65,536 variants",
        ))
    }
}

/// Emit a private `[N x ptr]` LLVM global containing pointers to each
/// state's NUL-terminated read-only name string. The address of this
/// global is what `Instr::MachineStateName` GEPs into using the machine's
/// tag as the array index. Each per-state string is itself a separate
/// private global (so the table holds opaque `ptr` values, matching
/// `ResolvedTy::String`'s ABI in LLVM IR).
fn build_state_name_table<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    machine_name: &str,
    variants: &[MachineVariantLayout],
) -> CodegenResult<inkwell::values::GlobalValue<'ctx>> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let mut entry_ptrs: Vec<inkwell::values::PointerValue<'ctx>> =
        Vec::with_capacity(variants.len());
    for variant in variants {
        // Per-state name string: private, internal-linkage, NUL-terminated,
        // unnamed_addr so LLVM may merge with other identical literals.
        let str_global_name = format!("__hew_state_name__{machine_name}__{}", variant.name);
        let bytes_with_nul: Vec<u8> = variant.name.bytes().chain(std::iter::once(0u8)).collect();
        let i8_ty = ctx.i8_type();
        let arr_ty = i8_ty.array_type(u32::try_from(bytes_with_nul.len()).unwrap_or(u32::MAX));
        let str_global = llvm_mod.add_global(arr_ty, None, &str_global_name);
        let initial: Vec<inkwell::values::IntValue<'ctx>> = bytes_with_nul
            .iter()
            .map(|b| i8_ty.const_int(u64::from(*b), false))
            .collect();
        let arr_init = i8_ty.const_array(&initial);
        str_global.set_initializer(&arr_init);
        str_global.set_linkage(Linkage::Private);
        str_global.set_constant(true);
        str_global.set_unnamed_addr(true);
        entry_ptrs.push(str_global.as_pointer_value());
    }
    // The table itself: `[N x ptr]` private constant.
    let table_ty = ptr_ty.array_type(u32::try_from(entry_ptrs.len()).unwrap_or(u32::MAX));
    let table_global_name = format!("__hew_state_name_table__{machine_name}");
    let table_global = llvm_mod.add_global(table_ty, None, &table_global_name);
    let table_init = ptr_ty.const_array(&entry_ptrs);
    table_global.set_initializer(&table_init);
    table_global.set_linkage(Linkage::Private);
    table_global.set_constant(true);
    Ok(table_global)
}

/// Locate a machine's tagged-union codegen layout for an MIR local known
/// to hold a machine value. Reads the local's resolved type and consults
/// the machine layout map. Fails closed if the local is not a machine
/// type or the machine name is not registered.
fn machine_layout_for_local<'a, 'ctx>(
    fn_ctx: &'a FnCtx<'_, 'ctx>,
    local: u32,
) -> CodegenResult<&'a MachineCodegenLayout<'ctx>> {
    let ty = fn_ctx.local_tys.get(&local).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "Place::MachineTag/MachineVariant references local {local} which has no \
             resolved type — the MIR producer must allocate the machine local before \
             projecting through it"
        ))
    })?;
    let (name, args) = match ty {
        ResolvedTy::Named { name, args } => (name, args),
        other => {
            return Err(CodegenError::FailClosed(format!(
                "Place::MachineTag/MachineVariant references local {local} whose type \
                 is {other:?}, not a named machine type"
            )));
        }
    };
    // Generic-enum monomorphisations are registered under the mangled key
    // (e.g. `"Option$$i64"`) by `register_enum_layouts`. When the local's
    // type carries type args, compute the same mangled key used at
    // registration so the lookup succeeds.
    // WHY: bare-name lookup fails for any instantiated generic enum because
    //   `register_enum_layouts` stores under `hir_layout.mangled_name`, not
    //   the origin enum name. Bare lookup is correct only for monomorphic enums
    //   (no type args) and machine/actor layouts.
    // WHEN-OBSOLETE: if the layout map is ever re-keyed by a richer
    //   identifier (e.g. a `(origin_id, mono_args)` pair), this branch goes away.
    let lookup_key: String = if args.is_empty() {
        name.clone()
    } else {
        let key = mangle(name, args);
        if !fn_ctx.machine_layouts.contains_key(&key) {
            return Err(CodegenError::FailClosed(format!(
                "Place::MachineTag/MachineVariant references generic enum `{name}` \
                 with type args {args:?}: mangled key `{key}` is not in \
                 IrPipeline.machine_layouts — the monomorphisation was not registered \
                 by `register_enum_layouts` (registration-mismatch)"
            )));
        }
        key
    };
    fn_ctx.machine_layouts.get(&lookup_key).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "Place::MachineTag/MachineVariant references machine `{name}` which is not \
             in IrPipeline.machine_layouts — registration mismatch between MIR producer \
             and codegen"
        ))
    })
}

/// Resolve any `ResolvedTy` to its LLVM `BasicTypeEnum`, consulting the
/// record-layout map first for named user records. This is the codegen-side
/// entry point for type lowering — it replaces direct calls to
/// `primitive_to_llvm` at any site that may encounter a record-typed value.
///
/// Tuples lower as anonymous LLVM struct types with fields in declaration order
/// (`packed = false` for natural alignment). The struct type is constructed
/// on demand; anonymous structs are not interned by LLVM and share the same
/// `StructType` ABI as named structs, so GEP field-index addressing works
/// identically to `RecordFieldLoad`. Field types are resolved recursively via
/// `resolve_ty` so records nested inside tuples resolve through the layout map.
///
/// WHY anonymous (not opaque/named): tuple types have no canonical name in
/// Hew source; the positional field list is the entire identity. Constructing
/// an anonymous struct per call is safe — LLVM deduplicates structurally
/// identical anonymous types within the same `Context`.
/// WHEN-OBSOLETE: if Hew adds named tuple types (type aliases with struct
/// semantics) this arm can be updated to look up the map first.
fn resolve_ty<'ctx>(
    ctx: &'ctx Context,
    ty: &ResolvedTy,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<BasicTypeEnum<'ctx>> {
    if let ResolvedTy::Named { name, args } = ty {
        // Generic-enum instantiations are keyed by mangled name (e.g.
        // `"Option$$i64"`) in the record-layout map — the same key produced
        // by `register_enum_layouts`. When type args are present, mangle
        // before lookup so the map entry is found.
        //
        // WHY: bare-name lookup finds nothing for `Named { name: "Option",
        //   args: [I64] }` because the map entry was inserted as `"Option$$i64"`.
        //   Without mangling, `resolve_ty` falls through to `primitive_to_llvm`,
        //   which emits a D10 fail-closed error for unknown Named types.
        //
        // Fall-through when the key is absent is intentional: known
        // pointer-backed handles (Vec, Duplex, HewTask, etc.) carry type
        // args but are handled by `primitive_to_llvm`'s explicit arm for
        // their bare name. `primitive_to_llvm` already emits the D10 error
        // for any Named type that reaches it without a registered layout.
        //
        // WHEN-OBSOLETE: same as `machine_layout_for_local` note above.
        let lookup_key: std::borrow::Cow<str> = if args.is_empty() {
            std::borrow::Cow::Borrowed(name.as_str())
        } else {
            std::borrow::Cow::Owned(mangle(name, args))
        };
        if let Some(st) = record_layouts.get(lookup_key.as_ref()) {
            return Ok((*st).into());
        }
    }
    if let ResolvedTy::Tuple(elems) = ty {
        let mut field_tys: Vec<BasicTypeEnum<'ctx>> = Vec::with_capacity(elems.len());
        for elem in elems {
            field_tys.push(resolve_ty(ctx, elem, record_layouts)?);
        }
        return Ok(ctx.struct_type(&field_tys, false).into());
    }
    primitive_to_llvm(ctx, ty)
}

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
        // WASM32 lowers separately via the `hew_emit` binary's
        // `--target wasm32-unknown-unknown` path, which calls back into
        // this same function.  The target data layout recorded on the
        // LLVM module by `hew_emit.rs:107` ensures `i32` is
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
        // `String` is a null-terminated heap string at the runtime ABI
        // boundary (`*mut c_char`). LLVM represents it as an opaque `ptr`
        // in the alloca — the same representation used for Duplex/Vec/Task
        // handles. `Instr::StringLit` stores the address of an LLVM global
        // constant into this slot; C-string runtime ops (`hew_string_*`) load
        // from it. Matches the C++ codegen's `hew.global_string` →
        // `llvm.mlir.addressof` pattern (codegen.cpp ~line 264).
        ResolvedTy::String => Ok(ctx.ptr_type(AddressSpace::default()).into()),
        // Cluster 1's spine subset. Heap and composite types below belong to
        // Cluster 2/3/4. Fail closed so an unexpected shape doesn't silently
        // produce a malformed binary (LESSONS `boundary-fail-closed`).
        // `Char` is a Unicode scalar value (U+0000..U+10FFFF). Stored as i32
        // (matching the C `int32_t` convention for code points). The upper two
        // billion i32 values are unused — all scalar values fit in 21 bits.
        ResolvedTy::Char => Ok(ctx.i32_type().into()),
        ResolvedTy::Bytes => Err(CodegenError::Unsupported(
            "Bytes type — Cluster 2 lowering pending",
        )),
        // `Duration` is i64 nanoseconds. The `i64` encoding covers ±292 years
        // of nanosecond precision, which is sufficient for all practical use.
        ResolvedTy::Duration => Ok(ctx.i64_type().into()),
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
        ResolvedTy::Named { name, .. }
            if matches!(
                name.as_str(),
                "Duplex" | "LocalPid" | "ActorRef" | "Actor" | "Stream"
            ) =>
        {
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
        ResolvedTy::Named { name, .. } if name == "Vec" => {
            // C-2 Vec<T> handle. A Vec<T> local is a `*mut HewVec` pointer.
            // The producer (`lower_vec_index`) allocates the result local
            // with the element type (i32/i64/f64/ptr); the vec-handle locals
            // themselves are function parameters at this stage (ptr-sized).
            // We represent the Vec handle as an opaque `ptr` in the alloca
            // so `load_ptr_arg` can load the raw pointer from the slot and
            // pass it to `hew_vec_len` / `hew_vec_get_T`. LESSONS:
            // exhaustive-traversal-and-lowering, boundary-fail-closed.
            Ok(ctx.ptr_type(AddressSpace::default()).into())
        }
        ResolvedTy::Named { name, .. } if name == "HewTask" => {
            // Phase 2 task handle. A `HewTask` local holds a `*mut HewTask`
            // opaque pointer — the Box-allocated task struct returned by
            // `hew_task_new` and consumed by `hew_task_free`. The MIR
            // producer for `spawn fn(...)` (inventory row 3) allocates a
            // `ResolvedTy::Named { name: "HewTask", .. }` local for each
            // spawned task and uses `Place::DuplexHandle(N)` to reference
            // it (reusing the duplex-handle tag — the underlying alloca
            // shape is identical: an opaque ptr slot). Codegen materialises
            // this as an opaque `ptr` alloca, same as Duplex and Vec handles.
            // LESSONS: exhaustive-traversal-and-lowering.
            Ok(ctx.ptr_type(AddressSpace::default()).into())
        }
        ResolvedTy::Named { name, .. } if name == "HewScope" => {
            // Phase 2 scope handle. A `HewScope` local holds a `*mut HewScope`
            // opaque pointer — returned by `hew_scope_create` and consumed by
            // `hew_scope_free`. The MIR producer for `scope {}` (inventory row
            // 2) allocates a `ResolvedTy::Named { name: "HewScope", .. }` local
            // and references it via `Place::DuplexHandle(N)`. Codegen emits an
            // opaque `ptr` alloca, same as other runtime handle types.
            // LESSONS: exhaustive-traversal-and-lowering.
            Ok(ctx.ptr_type(AddressSpace::default()).into())
        }
        ResolvedTy::Named { name, .. } if name == "HewTaskScope" => {
            Ok(ctx.ptr_type(AddressSpace::default()).into())
        }
        ResolvedTy::Named { name, .. } => Err(CodegenError::FailClosed(format!(
            "D10 violation: Named/user type `{name}` reached the LLVM emitter; \
             the MIR D10 gate should have rejected this earlier"
        ))),
        ResolvedTy::Function { .. } | ResolvedTy::Closure { .. } => {
            let ptr = ctx.ptr_type(AddressSpace::default()).into();
            Ok(ctx.struct_type(&[ptr, ptr], false).into())
        }
        ResolvedTy::Pointer { .. } => Ok(ctx.ptr_type(AddressSpace::default()).into()),
        ResolvedTy::TraitObject { .. } => Err(CodegenError::Unsupported(
            "TraitObject type — Cluster 4 lowering",
        )),
        ResolvedTy::Task(_) => Ok(ctx.ptr_type(AddressSpace::default()).into()),
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
        Place::DuplexHandle(id) | Place::ActorHandle(id) => {
            fn_ctx.locals.get(&id).copied().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Place::DuplexHandle({id}) references local {id} which was not allocated; \
                 the producer must allocate a ResolvedTy::Named{{name:\"Duplex\",..}} local before \
                 re-tagging it as a DuplexHandle"
                ))
            })
        }
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
        // Tagged-union sub-structure addressing. `MachineTag` GEPs to
        // outer-struct field 0 (the discriminant); `MachineVariant` GEPs
        // to outer-struct field 1 (the payload byte array), then bitcasts
        // to the active variant's struct pointer and GEPs to its field
        // `field_idx`. See the layout-invariants block above
        // `register_machine_layouts` for the load-bearing struct shape.
        //
        // User-declared enums (`Place::EnumTag` / `Place::EnumVariant`)
        // share the same LLVM layout: `register_enum_layouts` installs
        // each `EnumLayout` into `machine_layout_map` keyed by the enum's
        // name, so `machine_layout_for_local` resolves enum-typed locals
        // through the same path. The Place variants are typed-distinct in
        // MIR so future enum-only drop / lifecycle policies have a
        // separate surface to attach to, but the codegen seam is one GEP.
        Place::MachineTag(local) | Place::EnumTag(local) => {
            let layout = machine_layout_for_local(fn_ctx, local)?;
            let (slot, _slot_ty) = fn_ctx.locals.get(&local).copied().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Place::MachineTag({local}) has no alloca slot — local must be \
                     allocated as the machine outer struct before tag projection"
                ))
            })?;
            let tag_ptr = fn_ctx
                .builder
                .build_struct_gep(layout.outer_struct, slot, 0, "machine_tag_ptr")
                .map_err(|e| CodegenError::Llvm(format!("GEP machine tag: {e:?}")))?;
            Ok((tag_ptr, layout.tag_int_ty.into()))
        }
        Place::MachineVariant {
            local,
            variant_idx,
            field_idx,
        }
        | Place::EnumVariant {
            local,
            variant_idx,
            field_idx,
        } => {
            let layout = machine_layout_for_local(fn_ctx, local)?;
            let variant_idx_usize = variant_idx as usize;
            let variant_struct = layout
                .variant_struct_tys
                .get(variant_idx_usize)
                .copied()
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "Place::MachineVariant {{ local: {local}, variant_idx: {variant_idx} }} \
                         is out of range — machine has {} variants. The MIR producer must \
                         project only variants declared in MachineLayout.variants",
                        layout.variant_struct_tys.len()
                    ))
                })?;
            let (slot, _slot_ty) = fn_ctx.locals.get(&local).copied().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Place::MachineVariant local {local} has no alloca slot"
                ))
            })?;
            // GEP to outer struct field 1 (the payload byte array).
            let payload_ptr = fn_ctx
                .builder
                .build_struct_gep(layout.outer_struct, slot, 1, "machine_payload_ptr")
                .map_err(|e| CodegenError::Llvm(format!("GEP machine payload: {e:?}")))?;
            // LLVM opaque pointers: a `ptr` is a `ptr`; the GEP that
            // follows is typed against the variant struct, which is
            // the LLVM idiom for "reinterpret these bytes as this
            // variant's struct layout".
            let field_count = variant_struct.count_fields();
            if field_idx >= field_count {
                return Err(CodegenError::FailClosed(format!(
                    "Place::MachineVariant {{ field_idx: {field_idx} }} is out of range \
                     — variant {variant_idx} has {field_count} fields"
                )));
            }
            let field_ptr = fn_ctx
                .builder
                .build_struct_gep(
                    variant_struct,
                    payload_ptr,
                    field_idx,
                    "machine_variant_field_ptr",
                )
                .map_err(|e| CodegenError::Llvm(format!("GEP machine variant field: {e:?}")))?;
            let field_llvm_ty = variant_struct
                .get_field_type_at_index(field_idx)
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "variant struct has no field at index {field_idx}"
                    ))
                })?;
            Ok((field_ptr, field_llvm_ty))
        }
    }
}

fn place_resolved_ty<'a>(fn_ctx: &'a FnCtx<'_, '_>, place: Place) -> CodegenResult<&'a ResolvedTy> {
    match place {
        Place::Local(id)
        | Place::DuplexHandle(id)
        | Place::LambdaActorHandle(id)
        | Place::ActorHandle(id)
        | Place::SendHalf(id)
        | Place::RecvHalf(id) => fn_ctx.local_tys.get(&id).ok_or_else(|| {
            CodegenError::FailClosed(format!("local {id} has no resolved type before use"))
        }),
        Place::ReturnSlot => Ok(&fn_ctx.return_resolved_ty),
        // Tag projection: the discriminant is an integer whose width is
        // chosen by `MachineLayout.tag_width`. The dispatch tree (Slice
        // 4b) widens to `ResolvedTy::I64` via a separate Move into an
        // I64-typed local; here we surface the narrowest sound type to
        // keep `place_resolved_ty`'s shape contract (the type the slot
        // is declared with). Currently the only consumer that reads
        // this through `place_resolved_ty` is `instr_places`-style
        // walking, not type-driven loads — type-driven loads use
        // `place_pointer`'s second tuple element. We surface a typed
        // integer cached on the FnCtx via a `static` table so the
        // signature stays `&ResolvedTy` (a borrow). For tag widths up
        // through 64 we route to the standard I8/I16/I32/I64 buckets;
        // narrower widths (1, 2, 4, …) round up to the smallest holder.
        Place::MachineTag(_) | Place::EnumTag(_) => {
            // Map to the smallest standard integer that fits any tag.
            // Currently `lower_hir_module` derives `tag_width` from the
            // state count via `next_power_of_two().trailing_zeros()` so
            // the value is small (1..=6 for any realistic machine).
            // Returning I64 matches what the dispatch tree's
            // `state_tag` local is declared as. The actual LLVM tag
            // load uses the `place_pointer` tuple's `BasicTypeEnum`
            // (iW) and the Move arm widens to i64 when dest_ty differs
            // (Move's existing type-mismatch handling).
            //
            // WHY a constant ref: `place_resolved_ty` returns
            // `&ResolvedTy` borrowed from FnCtx fields. The tag is not
            // stored in any FnCtx field, so we return a borrow of a
            // static value with the appropriate width. Long-term, an
            // `i<W>` `ResolvedTy` variant would let this surface the
            // exact width; for v0.5 the I64-shaped reads via Move
            // cover every use site. `EnumTag` shares the same layout as
            // `MachineTag` (see `register_enum_layouts`).
            const TAG_TY: ResolvedTy = ResolvedTy::I64;
            Ok(&TAG_TY)
        }
        Place::MachineVariant {
            local,
            variant_idx,
            field_idx,
        }
        | Place::EnumVariant {
            local,
            variant_idx,
            field_idx,
        } => {
            let layout = fn_ctx
                .machine_layouts
                .get(match fn_ctx.local_tys.get(&local) {
                    Some(ResolvedTy::Named { name, .. }) => name,
                    _ => {
                        return Err(CodegenError::FailClosed(format!(
                            "Place::MachineVariant references local {local} which is not a \
                         machine-typed local"
                        )));
                    }
                })
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "Place::MachineVariant local {local} machine type not registered"
                    ))
                })?;
            let variant = layout
                .variant_field_tys
                .get(variant_idx as usize)
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "Place::MachineVariant variant_idx {variant_idx} out of range"
                    ))
                })?;
            variant.get(field_idx as usize).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Place::MachineVariant field_idx {field_idx} out of range for variant \
                     {variant_idx}"
                ))
            })
        }
    }
}

fn sanitize_symbol(s: &str) -> String {
    s.chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

fn task_wrapper_name(callee_symbol: &str) -> String {
    format!("__hew_task_wrapper_{}", sanitize_symbol(callee_symbol))
}

fn task_closure_wrapper_name(fn_symbol: &str) -> String {
    format!("__hew_task_closure_wrapper_{}", sanitize_symbol(fn_symbol))
}

fn get_or_create_task_wrapper<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    callee_symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    let wrapper_name = task_wrapper_name(callee_symbol);
    if let Some(existing) = fn_ctx.llvm_mod.get_function(&wrapper_name) {
        return Ok(existing);
    }

    let callee_symbol_entry = *fn_ctx.fn_symbols.get(callee_symbol).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SpawnTaskDirect callee `{callee_symbol}` was not declared"
        ))
    })?;
    let (callee_value, callee_return_ty, callee_returns_unit) =
        callee_symbol_entry.real(callee_symbol, "SpawnTaskDirect")?;
    if !callee_returns_unit || !matches!(callee_return_ty, BasicTypeEnum::IntType(_)) {
        return Err(CodegenError::FailClosed(format!(
            "SpawnTaskDirect callee `{callee_symbol}` must be unit-lowered to the i8 \
             stand-in return type; got {:?}",
            callee_return_ty
        )));
    }

    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    if callee_value.get_nth_param(0).is_none() || callee_value.get_nth_param(1).is_some() {
        return Err(CodegenError::FailClosed(format!(
            "SpawnTaskDirect callee `{callee_symbol}` must take a leading \
             HewExecutionContext* and no user parameters"
        )));
    }

    let wrapper_ty = fn_ctx
        .ctx
        .void_type()
        .fn_type(&[ptr_ty.into(), ptr_ty.into()], false);
    let wrapper = fn_ctx
        .llvm_mod
        .add_function(&wrapper_name, wrapper_ty, Some(Linkage::Internal));
    let bb = fn_ctx.ctx.append_basic_block(wrapper, "entry");
    let builder = fn_ctx.ctx.create_builder();
    builder.position_at_end(bb);
    let ctx_param = wrapper.get_nth_param(0).ok_or_else(|| {
        CodegenError::FailClosed("task wrapper missing HewExecutionContext* parameter".into())
    })?;
    let task_param = wrapper.get_nth_param(1).ok_or_else(|| {
        CodegenError::FailClosed("task wrapper missing HewTask* parameter".into())
    })?;
    builder
        .build_call(callee_value, &[ctx_param.into()], "task_body_call")
        .map_err(|e| CodegenError::Llvm(format!("task wrapper body call: {e:?}")))?;

    let complete = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_complete_threaded",
    )?;
    builder
        .build_call(
            complete,
            &[task_param.into()],
            "hew_task_complete_threaded_call",
        )
        .map_err(|e| CodegenError::Llvm(format!("hew_task_complete_threaded call: {e:?}")))?;
    builder
        .build_return(None)
        .map_err(|e| CodegenError::Llvm(format!("task wrapper return: {e:?}")))?;
    Ok(wrapper)
}

fn emit_spawn_task_direct(
    fn_ctx: &FnCtx<'_, '_>,
    task: Place,
    callee_symbol: &str,
) -> CodegenResult<()> {
    let parent_ctx = fn_ctx.execution_context.ok_or_else(|| {
        CodegenError::FailClosed("SpawnTaskDirect spawn site requires an execution context".into())
    })?;
    let task_ptr = load_duplex_handle(fn_ctx, task, "SpawnTaskDirect task")?;
    let wrapper = get_or_create_task_wrapper(fn_ctx, callee_symbol)?;
    let spawn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_spawn_thread_with_inherited_context",
    )?;
    let fn_ptr = wrapper.as_global_value().as_pointer_value();
    fn_ctx
        .builder
        .build_call(
            spawn,
            &[parent_ctx.into(), task_ptr.into(), fn_ptr.into()],
            "hew_task_spawn_thread_with_inherited_context_call",
        )
        .map_err(|e| {
            CodegenError::Llvm(format!(
                "hew_task_spawn_thread_with_inherited_context call: {e:?}"
            ))
        })?;
    Ok(())
}

fn llvm_global_name_fragment(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        "unnamed".to_string()
    } else {
        out
    }
}

fn intern_global_string_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    value: &str,
    name: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    if let Some(global) = fn_ctx.llvm_mod.get_global(name) {
        return Ok(global.as_pointer_value());
    }
    let global = fn_ctx
        .builder
        .build_global_string_ptr(value, name)
        .map_err(|e| CodegenError::Llvm(format!("actor metadata string `{name}`: {e:?}")))?;
    Ok(global.as_pointer_value())
}

fn emit_wasm_actor_metadata_registration<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    actor_name: &str,
) -> CodegenResult<()> {
    let layout = fn_ctx
        .actor_layouts
        .iter()
        .find(|layout| layout.name == actor_name)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "spawn `{actor_name}` has no ActorLayout for WASM trace metadata registration"
            ))
        })?;

    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let i32_ty = fn_ctx.ctx.i32_type();
    let actor_fragment = llvm_global_name_fragment(actor_name);
    let actor_name_ptr = intern_global_string_ptr(
        fn_ctx,
        actor_name,
        &format!("str_actor_meta_name_{actor_fragment}"),
    )?;

    // Mirrors hew-runtime/src/bridge.rs:
    //   HewHandlerMeta { name: *const u8, msg_type: i32, param_count: u32,
    //                    params: *const HewParamMeta, return_type: *const u8,
    //                    return_size: u32 }
    //   HewActorMeta   { name: *const u8, handler_count: u32,
    //                    handlers: *const HewHandlerMeta }
    //
    // Parameter and return metadata require names/layouts that ActorLayout does
    // not own today. Trace attribution consumes the supported fields available
    // here (actor name, handler name, msg_type) and leaves params/return null.
    let handler_ty = fn_ctx.ctx.struct_type(
        &[
            ptr_ty.into(),
            i32_ty.into(),
            i32_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
            i32_ty.into(),
        ],
        false,
    );
    let actor_meta_ty = fn_ctx
        .ctx
        .struct_type(&[ptr_ty.into(), i32_ty.into(), ptr_ty.into()], false);

    let handlers_ptr = if layout.handlers.is_empty() {
        ptr_ty.const_null()
    } else {
        let handler_count = u32::try_from(layout.handlers.len()).map_err(|_| {
            CodegenError::FailClosed(format!(
                "actor `{actor_name}` has more than u32::MAX handlers"
            ))
        })?;
        let handler_array_ty = handler_ty.array_type(handler_count);
        let handler_array_slot = fn_ctx
            .builder
            .build_alloca(
                handler_array_ty,
                &format!("actor_meta_handlers_{actor_fragment}"),
            )
            .map_err(|e| CodegenError::Llvm(format!("actor metadata handler array: {e:?}")))?;
        for (idx, handler) in layout.handlers.iter().enumerate() {
            let handler_idx = u32::try_from(idx).map_err(|_| {
                CodegenError::FailClosed(format!(
                    "actor `{actor_name}` handler index exceeds u32::MAX"
                ))
            })?;
            let handler_name_fragment = llvm_global_name_fragment(&handler.name);
            let handler_name_ptr = intern_global_string_ptr(
                fn_ctx,
                &handler.name,
                &format!(
                    "str_actor_meta_handler_{actor_fragment}_{handler_idx}_{handler_name_fragment}"
                ),
            )?;
            let handler_slot = unsafe {
                fn_ctx
                    .builder
                    .build_gep(
                        handler_array_ty,
                        handler_array_slot,
                        &[
                            i32_ty.const_zero(),
                            i32_ty.const_int(handler_idx as u64, false),
                        ],
                        &format!("actor_meta_handler_{handler_idx}"),
                    )
                    .map_err(|e| CodegenError::Llvm(format!("actor metadata handler GEP: {e:?}")))?
            };
            let fields: [(u32, BasicValueEnum<'ctx>); 6] = [
                (0, handler_name_ptr.into()),
                (1, i32_ty.const_int(handler.msg_type as u64, true).into()),
                (2, i32_ty.const_zero().into()),
                (3, ptr_ty.const_null().into()),
                (4, ptr_ty.const_null().into()),
                (5, i32_ty.const_zero().into()),
            ];
            for (field_idx, value) in fields {
                let field_ptr = fn_ctx
                    .builder
                    .build_struct_gep(
                        handler_ty,
                        handler_slot,
                        field_idx,
                        &format!("actor_meta_handler_{handler_idx}_f{field_idx}"),
                    )
                    .map_err(|e| {
                        CodegenError::Llvm(format!("actor metadata handler field GEP: {e:?}"))
                    })?;
                fn_ctx.builder.build_store(field_ptr, value).map_err(|e| {
                    CodegenError::Llvm(format!("actor metadata handler field store: {e:?}"))
                })?;
            }
        }
        unsafe {
            fn_ctx
                .builder
                .build_gep(
                    handler_array_ty,
                    handler_array_slot,
                    &[i32_ty.const_zero(), i32_ty.const_zero()],
                    "actor_meta_handlers_ptr",
                )
                .map_err(|e| CodegenError::Llvm(format!("actor metadata handlers ptr: {e:?}")))?
        }
    };

    let actor_meta_slot = fn_ctx
        .builder
        .build_alloca(actor_meta_ty, &format!("actor_meta_{actor_fragment}"))
        .map_err(|e| CodegenError::Llvm(format!("actor metadata alloca: {e:?}")))?;
    let handler_count = u32::try_from(layout.handlers.len()).map_err(|_| {
        CodegenError::FailClosed(format!(
            "actor `{actor_name}` has more than u32::MAX handlers"
        ))
    })?;
    let fields: [(u32, BasicValueEnum<'ctx>); 3] = [
        (0, actor_name_ptr.into()),
        (1, i32_ty.const_int(handler_count as u64, false).into()),
        (2, handlers_ptr.into()),
    ];
    for (field_idx, value) in fields {
        let field_ptr = fn_ctx
            .builder
            .build_struct_gep(
                actor_meta_ty,
                actor_meta_slot,
                field_idx,
                &format!("actor_meta_f{field_idx}"),
            )
            .map_err(|e| CodegenError::Llvm(format!("actor metadata field GEP: {e:?}")))?;
        fn_ctx
            .builder
            .build_store(field_ptr, value)
            .map_err(|e| CodegenError::Llvm(format!("actor metadata field store: {e:?}")))?;
    }

    let register = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_wasm_register_actor_meta",
    )?;
    fn_ctx
        .builder
        .build_call(
            register,
            &[actor_meta_slot.into()],
            "hew_wasm_register_actor_meta_call",
        )
        .map_err(|e| CodegenError::Llvm(format!("hew_wasm_register_actor_meta call: {e:?}")))?;
    Ok(())
}

fn emit_spawn_actor(
    fn_ctx: &FnCtx<'_, '_>,
    actor_name: &str,
    state: Option<Place>,
    init_args: &[Place],
    dest: Place,
    max_heap_bytes: Option<u64>,
    cycle_capable: bool,
) -> CodegenResult<()> {
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();
    let (state_ptr, state_size) = if let Some(state_place) = state {
        let (slot, slot_ty) = place_pointer(fn_ctx, state_place)?;
        let size = slot_ty.size_of().ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "spawn `{actor_name}` state type has no statically known size: {slot_ty:?}"
            ))
        })?;
        let size = if size.get_type() == i64_ty {
            size
        } else {
            fn_ctx
                .builder
                .build_int_z_extend(size, i64_ty, "actor_state_size")
                .map_err(|e| CodegenError::Llvm(format!("actor state size zext: {e:?}")))?
        };
        (slot, size)
    } else {
        (ptr_ty.const_null(), i64_ty.const_zero())
    };
    let dispatch_name = format!("__hew_actor_dispatch_{actor_name}");
    let dispatch = fn_ctx
        .llvm_mod
        .get_function(&dispatch_name)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "spawn `{actor_name}` requires dispatch trampoline `{dispatch_name}`"
            ))
        })?;
    let sched_init = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_sched_init",
    )?;
    fn_ctx
        .builder
        .build_call(sched_init, &[], "hew_sched_init_call")
        .map_err(|e| CodegenError::Llvm(format!("hew_sched_init call: {e:?}")))?;
    emit_wasm_actor_metadata_registration(fn_ctx, actor_name)?;

    let spawned = if max_heap_bytes.is_some() || cycle_capable {
        // `#[max_heap(N)]` and/or `cycle_capable` is set — route through
        // `hew_actor_spawn_opts` so the runtime applies all spawn policy bits.
        // The `HewActorOpts` struct is stack-allocated, populated with the
        // minimal fields, and passed by pointer.
        //
        // `HewActorOpts` `#[repr(C)]` field order (hew-runtime/src/actor.rs:1456):
        //   0  init_state:       *mut c_void   → ptr
        //   1  state_size:       usize         → i64
        //   2  dispatch:         Option<fn>    → ptr
        //   3  mailbox_capacity: i32           → i32
        //   4  overflow:         i32           → i32
        //   5  coalesce_key_fn:  Option<fn>    → ptr
        //   6  coalesce_fallback: i32          → i32
        //   7  budget:           i32           → i32
        //   8  arena_cap_bytes:  usize         → i64
        //   9  cycle_capable:    i32           → i32
        let arena_cap = max_heap_bytes.unwrap_or(0);
        let cycle_flag = if cycle_capable { 1 } else { 0 };
        let opts_ty = fn_ctx.ctx.struct_type(
            &[
                ptr_ty.into(), // init_state
                i64_ty.into(), // state_size
                ptr_ty.into(), // dispatch
                i32_ty.into(), // mailbox_capacity
                i32_ty.into(), // overflow
                ptr_ty.into(), // coalesce_key_fn (null)
                i32_ty.into(), // coalesce_fallback
                i32_ty.into(), // budget
                i64_ty.into(), // arena_cap_bytes
                i32_ty.into(), // cycle_capable
            ],
            false,
        );
        let opts_slot = fn_ctx
            .builder
            .build_alloca(opts_ty, "actor_spawn_opts")
            .map_err(|e| CodegenError::Llvm(format!("HewActorOpts alloca: {e:?}")))?;
        let opts_fields: [(u32, BasicValueEnum<'_>); 10] = [
            (0, state_ptr.into()),
            (1, state_size.into()),
            (2, dispatch.as_global_value().as_pointer_value().into()),
            (3, i32_ty.const_zero().into()),
            (4, i32_ty.const_zero().into()),
            (5, ptr_ty.const_null().into()),
            (6, i32_ty.const_zero().into()),
            (7, i32_ty.const_zero().into()),
            (8, i64_ty.const_int(arena_cap, false).into()),
            (9, i32_ty.const_int(cycle_flag, false).into()),
        ];
        for (field_idx, value) in opts_fields {
            let gep = fn_ctx
                .builder
                .build_struct_gep(opts_ty, opts_slot, field_idx, &format!("opts_f{field_idx}"))
                .map_err(|e| {
                    CodegenError::Llvm(format!("HewActorOpts GEP field {field_idx}: {e:?}"))
                })?;
            fn_ctx.builder.build_store(gep, value).map_err(|e| {
                CodegenError::Llvm(format!("HewActorOpts store field {field_idx}: {e:?}"))
            })?;
        }
        let spawn_opts_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_actor_spawn_opts",
        )?;
        fn_ctx
            .builder
            .build_call(
                spawn_opts_fn,
                &[opts_slot.into()],
                "hew_actor_spawn_opts_call",
            )
            .map_err(|e| CodegenError::Llvm(format!("hew_actor_spawn_opts call: {e:?}")))?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_actor_spawn_opts returned void".into()))?
            .into_pointer_value()
    } else {
        // No arena cap — use the lighter 3-arg `hew_actor_spawn` path.
        let spawn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_actor_spawn",
        )?;
        fn_ctx
            .builder
            .build_call(
                spawn,
                &[
                    state_ptr.into(),
                    state_size.into(),
                    dispatch.as_global_value().as_pointer_value().into(),
                ],
                "hew_actor_spawn_call",
            )
            .map_err(|e| CodegenError::Llvm(format!("hew_actor_spawn call: {e:?}")))?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_actor_spawn returned void".into()))?
            .into_pointer_value()
    };

    emit_actor_spawn_lifecycle(fn_ctx, actor_name, spawned, init_args)?;
    let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest)?;
    if !matches!(dest_ty, BasicTypeEnum::PointerType(_)) {
        return Err(CodegenError::FailClosed(format!(
            "SpawnActor destination is not pointer-typed: {dest_ty:?}"
        )));
    }
    fn_ctx
        .builder
        .build_store(dest_ptr, spawned)
        .map_err(|e| CodegenError::Llvm(format!("SpawnActor store: {e:?}")))?;
    Ok(())
}

/// Map a `HirSupervisorStrategy` to the runtime `STRATEGY_*` integer.
///
/// Mirrors the `pub const STRATEGY_*` constants at
/// `hew-runtime/src/supervisor.rs:315-324`. The values are part of the ABI:
/// changing them requires a coordinated runtime + codegen edit.
fn supervisor_strategy_to_int(strategy: HirSupervisorStrategy) -> i32 {
    match strategy {
        HirSupervisorStrategy::OneForOne => 0,
        HirSupervisorStrategy::OneForAll => 1,
        HirSupervisorStrategy::RestForOne => 2,
        HirSupervisorStrategy::SimpleOneForOne => 3,
    }
}

/// Map a `HirRestartPolicy` to the runtime `RESTART_*` integer.
///
/// Mirrors the `pub const RESTART_*` constants at
/// `hew-runtime/src/supervisor.rs:329-331`.
fn restart_policy_to_int(policy: HirRestartPolicy) -> i32 {
    match policy {
        HirRestartPolicy::Permanent => 0,
        HirRestartPolicy::Transient => 1,
        HirRestartPolicy::Temporary => 2,
    }
}

/// Build the LLVM struct type for `HewChildSpec`.
///
/// The struct mirrors the `#[repr(C)]` Rust layout at
/// `hew-runtime/src/supervisor.rs::HewChildSpec` exactly. Field order MUST match;
/// drift here is wrong-code at the FFI boundary.
///
/// Field map (Rust → LLVM):
/// - `name: *const c_char`                  → `ptr`
/// - `init_state: *mut c_void`              → `ptr`
/// - `init_state_size: usize`               → `i64`  (64-bit spine only)
/// - `dispatch: Option<HewDispatchFn>`      → `ptr`  (opaque-pointer mode)
/// - `restart_policy: c_int`                → `i32`
/// - `mailbox_capacity: c_int`              → `i32`
/// - `overflow: c_int`                      → `i32`
/// - `arena_cap_bytes: usize`               → `i64`
/// - `cycle_capable: c_int`                 → `i32`
/// - `on_crash: Option<HewOnCrashFn>`       → `ptr`
///
/// The three consecutive `i32` fields followed by `i64`, and the trailing
/// `cycle_capable: i32` followed by a pointer, produce natural padding under
/// `#[repr(C)]`; LLVM's default (non-packed) struct alignment matches it.
fn hew_child_spec_struct_type<'ctx>(ctx: &'ctx Context) -> StructType<'ctx> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    ctx.struct_type(
        &[
            ptr_ty.into(), // name
            ptr_ty.into(), // init_state
            i64_ty.into(), // init_state_size
            ptr_ty.into(), // dispatch
            i32_ty.into(), // restart_policy
            i32_ty.into(), // mailbox_capacity
            i32_ty.into(), // overflow
            i64_ty.into(), // arena_cap_bytes (alignment introduces 4B pad after `overflow`)
            i32_ty.into(), // cycle_capable
            ptr_ty.into(), // on_crash fn-pointer: null when child's actor has no #[on(crash)]; otherwise pointer to `{actor_name}__on_crash`
        ],
        false,
    )
}

/// Emit the body of a supervisor bootstrap function.
///
/// The bootstrap symbol is declared by `declare_function` like any other
/// `FunctionCallConv::ActorHandler` function (one leading
/// `*mut HewExecutionContext` param the body ignores). This helper substitutes
/// the MIR-side synthesised body wholesale with the canonical
/// `hew_supervisor_*` call sequence:
///
/// 1. `%sup = call hew_supervisor_new(strategy, max_restarts, window_secs)`
/// 2. for each child: alloca `HewChildSpec`, populate fields, call
///    `hew_supervisor_add_child_spec(%sup, &spec)`
/// 3. `%rc = call hew_supervisor_start(%sup)`; trap on non-zero (fail-closed)
/// 4. `ret %sup`
///
/// Fail-closed posture:
/// - Missing per-child dispatch trampoline → `FailClosed` (must be emitted by
///   `emit_actor_dispatch_trampoline` ahead of this helper).
/// - Non-integer `window` literal → `FailClosed`. The fixture uses
///   `window: 60`; the `"60s"` form is deferred to a follow-on slice.
/// - `hew_supervisor_start` non-zero return → `llvm.trap; unreachable`. The
///   supervisor surface is one of Hew's fail-closed boundaries (LESSONS
///   boundary-fail-closed); a start that the runtime rejected is wrong-code
///   territory, not a recoverable error.
fn emit_supervisor_bootstrap_body<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    layout: &SupervisorLayout,
    fn_symbols: &FnSymbolMap<'ctx>,
) -> CodegenResult<()> {
    let symbol = *fn_symbols.get(&layout.bootstrap_symbol).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "supervisor `{}` bootstrap symbol `{}` was not declared before body emission",
            layout.name, layout.bootstrap_symbol
        ))
    })?;
    let (llvm_fn, return_ty_llvm, _returns_unit) =
        symbol.real(&layout.bootstrap_symbol, "emit_supervisor_bootstrap_body")?;
    if !matches!(return_ty_llvm, BasicTypeEnum::PointerType(_)) {
        return Err(CodegenError::FailClosed(format!(
            "supervisor `{}` bootstrap return type must be a pointer (LocalPid<Sup>); got {return_ty_llvm:?}",
            layout.name
        )));
    }

    let builder = ctx.create_builder();
    let entry_bb = ctx.append_basic_block(llvm_fn, "entry");
    builder.position_at_end(entry_bb);

    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    // ── Strategy / max_restarts / window literals ───────────────────────
    //
    // `None` falls back to `0`; the runtime treats `0` for max_restarts as
    // "no automatic restarts" and `0` for window_secs as "policy clamps to a
    // safe minimum". The fixture sets explicit values so the runtime path
    // exercises real numbers.
    let strategy_int = layout.strategy.map(supervisor_strategy_to_int).unwrap_or(0);
    let max_restarts_int: i32 = layout
        .max_restarts
        .and_then(|n| i32::try_from(n).ok())
        .unwrap_or(0);
    // Window parsing: this slice accepts only an integer-seconds literal
    // (e.g. `window: 60`). The `"60s"` duration-literal form is deferred per
    // plan §S-D.3 "Out of scope". If a non-integer string reaches this point
    // codegen fails closed — silently coercing to 0 would hide a parser/MIR
    // drift bug.
    let window_secs_int: i32 = match layout.window.as_deref() {
        None => 0,
        Some(s) => s.trim().parse::<i32>().map_err(|_| {
            CodegenError::FailClosed(format!(
                "supervisor `{}` window literal `{s:?}` is not an integer-seconds value; \
                 the `\"60s\"` duration form is deferred to a follow-on slice",
                layout.name
            ))
        })?,
    };

    let mut runtime_decls = RuntimeDeclMap::new();

    // ── hew_sched_init() ────────────────────────────────────────────────────
    // The plain-actor spawn path calls hew_sched_init before hew_actor_spawn
    // (llvm.rs `emit_spawn_actor`).  The supervisor bootstrap must do the same
    // before hew_supervisor_new, or the runtime panics "scheduler not
    // initialized" (exit 134).
    let sched_init = intern_runtime_decl(ctx, llvm_mod, &mut runtime_decls, "hew_sched_init")?;
    builder
        .build_call(sched_init, &[], "hew_sched_init_call")
        .map_err(|e| {
            CodegenError::Llvm(format!(
                "hew_sched_init call in supervisor bootstrap: {e:?}"
            ))
        })?;

    // ── %sup = call hew_supervisor_new(strategy, max_restarts, window_secs)
    let sup_new = intern_runtime_decl(ctx, llvm_mod, &mut runtime_decls, "hew_supervisor_new")?;
    let sup = builder
        .build_call(
            sup_new,
            &[
                i32_ty.const_int(strategy_int as u64, true).into(),
                i32_ty.const_int(max_restarts_int as u64, true).into(),
                i32_ty.const_int(window_secs_int as u64, true).into(),
            ],
            "hew_supervisor_new_call",
        )
        .map_err(|e| CodegenError::Llvm(format!("hew_supervisor_new call: {e:?}")))?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_supervisor_new returned void".into()))?
        .into_pointer_value();

    // ── For each child: alloca HewChildSpec, populate, register ─────────
    let child_spec_ty = hew_child_spec_struct_type(ctx);
    let add_child_spec = intern_runtime_decl(
        ctx,
        llvm_mod,
        &mut runtime_decls,
        "hew_supervisor_add_child_spec",
    )?;
    for (idx, child) in layout.children.iter().enumerate() {
        emit_supervisor_child_spec_and_register(
            ctx,
            llvm_mod,
            &builder,
            &child_spec_ty,
            sup,
            add_child_spec,
            idx,
            child,
            &layout.name,
        )?;
    }

    // ── %rc = call hew_supervisor_start(%sup); trap on non-zero ─────────
    let sup_start = intern_runtime_decl(ctx, llvm_mod, &mut runtime_decls, "hew_supervisor_start")?;
    let rc = builder
        .build_call(sup_start, &[sup.into()], "hew_supervisor_start_call")
        .map_err(|e| CodegenError::Llvm(format!("hew_supervisor_start call: {e:?}")))?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_supervisor_start returned void".into()))?
        .into_int_value();
    let zero = i32_ty.const_zero();
    let is_ok = builder
        .build_int_compare(IntPredicate::EQ, rc, zero, "sup_start_ok")
        .map_err(|e| CodegenError::Llvm(format!("sup start cmp: {e:?}")))?;
    let ok_bb = ctx.append_basic_block(llvm_fn, "sup_start_ok");
    let trap_bb = ctx.append_basic_block(llvm_fn, "sup_start_trap");
    builder
        .build_conditional_branch(is_ok, ok_bb, trap_bb)
        .map_err(|e| CodegenError::Llvm(format!("sup start cond br: {e:?}")))?;
    builder.position_at_end(trap_bb);
    // llvm.trap; unreachable — fail-closed on supervisor start failure.
    let trap_intrinsic = Intrinsic::find("llvm.trap").ok_or_else(|| {
        CodegenError::FailClosed("llvm.trap intrinsic not available in this LLVM build".into())
    })?;
    let trap_fn = trap_intrinsic
        .get_declaration(llvm_mod, &[])
        .ok_or_else(|| CodegenError::FailClosed("llvm.trap declaration missing".into()))?;
    builder
        .build_call(trap_fn, &[], "sup_start_trap_call")
        .map_err(|e| CodegenError::Llvm(format!("sup start trap call: {e:?}")))?;
    builder
        .build_unreachable()
        .map_err(|e| CodegenError::Llvm(format!("sup start unreachable: {e:?}")))?;

    // ── ret sup ─────────────────────────────────────────────────────────
    builder.position_at_end(ok_bb);
    let _ = ptr_ty;
    let _ = i64_ty;
    builder
        .build_return(Some(&sup))
        .map_err(|e| CodegenError::Llvm(format!("sup bootstrap ret: {e:?}")))?;
    Ok(())
}

/// Emit one `HewChildSpec` literal + `hew_supervisor_add_child_spec` call.
///
/// Per-field semantics:
/// - `name` — global C-string `@.str.child.<sup>.<idx>` set to `child.name`
///   (the slot name, not the actor type; matches the surface name visible
///   to user-level `sup.<name>` access).
/// - `init_state` / `init_state_size` — `null` / `0`. The fixture has no
///   per-child init args; richer init lowering is deferred.
/// - `dispatch` — pointer to `__hew_actor_dispatch_<actor_name>`, declared
///   by `emit_actor_dispatch_trampoline` in `build_module` ahead of this
///   helper. Fail-closed if missing.
/// - `restart_policy` — child's policy via `restart_policy_to_int`; `None`
///   defaults to `RESTART_PERMANENT` (`0`) to match runtime defaults.
/// - `mailbox_capacity` / `overflow` — `0` for both, which the runtime maps
///   to its bounded defaults. Richer per-child overrides are deferred.
/// - `arena_cap_bytes` / `cycle_capable` — mirrored from the child's
///   `ActorLayout` through `SupervisorChildLayout` so restarts preserve the
///   same spawn policy bits as direct spawn.
#[allow(clippy::too_many_arguments)]
fn emit_supervisor_child_spec_and_register<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &Builder<'ctx>,
    child_spec_ty: &StructType<'ctx>,
    sup: PointerValue<'ctx>,
    add_child_spec: FunctionValue<'ctx>,
    idx: usize,
    child: &SupervisorChildLayout,
    sup_name: &str,
) -> CodegenResult<()> {
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    let dispatch_name = format!("__hew_actor_dispatch_{}", child.actor_name);
    let dispatch_fn = llvm_mod.get_function(&dispatch_name).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "supervisor `{sup_name}` child `{}` requires dispatch trampoline `{dispatch_name}`",
            child.name
        ))
    })?;

    // Resolve on_crash fn-pointer: Some(symbol) → pointer to the declared
    // function; None → null. The on_crash function is produced by MIR
    // lowering (as `{actor_name}__on_crash`) and declared by `declare_function`
    // over `raw_mir` before `emit_supervisor_bootstrap_body` runs, so
    // `get_function` must find it if the symbol is populated.
    //
    // Fail-closed: a missing symbol means Slice 2 didn't produce the
    // function body, which is wrong-code territory — diagnose clearly rather
    // than silently falling back to null.
    let on_crash_ptr: BasicValueEnum<'ctx> = match &child.on_crash_symbol {
        Some(symbol) => {
            let on_crash_fn = llvm_mod.get_function(symbol).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "supervisor `{sup_name}` child `{}` has on_crash_symbol `{symbol}` \
                     but no function with that name was declared in the module; \
                     the MIR lowering pass must emit the on_crash function before codegen",
                    child.name
                ))
            })?;
            on_crash_fn.as_global_value().as_pointer_value().into()
        }
        None => ptr_ty.const_null().into(),
    };

    // Per-child stack alloca for the spec struct.
    let spec_slot = builder
        .build_alloca(*child_spec_ty, &format!("child_spec_{idx}"))
        .map_err(|e| CodegenError::Llvm(format!("child spec alloca: {e:?}")))?;

    // name → @.str.child.<sup>.<idx>
    let name_global = builder
        .build_global_string_ptr(&child.name, &format!("str_child_name_{sup_name}_{idx}"))
        .map_err(|e| CodegenError::Llvm(format!("child name global: {e:?}")))?;
    let name_ptr = name_global.as_pointer_value();

    let restart_int = child.restart_policy.map(restart_policy_to_int).unwrap_or(0);
    // arena_cap_bytes: lifted from the child actor's `#[max_heap(N)]`
    // annotation (mirrored into SupervisorChildLayout.max_heap_bytes by the
    // MIR post-loop pass). Zero means unbounded — matches runtime default.
    let arena_cap = child.max_heap_bytes.unwrap_or(0);
    let cycle_flag = if child.cycle_capable { 1 } else { 0 };

    let field_values: [(u32, BasicValueEnum<'ctx>); 10] = [
        (0, name_ptr.into()),
        (1, ptr_ty.const_null().into()),
        (2, i64_ty.const_zero().into()),
        (3, dispatch_fn.as_global_value().as_pointer_value().into()),
        (4, i32_ty.const_int(restart_int as u64, true).into()),
        (5, i32_ty.const_zero().into()),
        (6, i32_ty.const_zero().into()),
        (7, i64_ty.const_int(arena_cap, false).into()), // arena_cap_bytes from #[max_heap(N)]
        (8, i32_ty.const_int(cycle_flag, false).into()), // cycle_capable from checker side-table
        (9, on_crash_ptr), // on_crash: fn-pointer when child's actor has #[on(crash)], null otherwise
    ];
    for (field_idx, value) in field_values {
        let gep = builder
            .build_struct_gep(
                *child_spec_ty,
                spec_slot,
                field_idx,
                &format!("child_spec_{idx}_f{field_idx}"),
            )
            .map_err(|e| CodegenError::Llvm(format!("child spec gep: {e:?}")))?;
        builder
            .build_store(gep, value)
            .map_err(|e| CodegenError::Llvm(format!("child spec store: {e:?}")))?;
    }

    builder
        .build_call(
            add_child_spec,
            &[sup.into(), spec_slot.into()],
            &format!("hew_supervisor_add_child_spec_call_{idx}"),
        )
        .map_err(|e| CodegenError::Llvm(format!("hew_supervisor_add_child_spec call: {e:?}")))?;
    Ok(())
}

fn emit_actor_spawn_lifecycle<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    actor_name: &str,
    spawned: PointerValue<'ctx>,
    init_args: &[Place],
) -> CodegenResult<()> {
    let init_name = format!("{actor_name}__init");
    let start_name = format!("{actor_name}__on_start");
    let terminate_name = format!("__terminate_{actor_name}");
    let init_fn = fn_ctx.llvm_mod.get_function(&init_name);
    let start_fn = fn_ctx.llvm_mod.get_function(&start_name);
    let terminate_fn = fn_ctx.llvm_mod.get_function(&terminate_name);
    if init_fn.is_none() && !init_args.is_empty() {
        return Err(CodegenError::FailClosed(format!(
            "spawn `{actor_name}` carried init args but `{init_name}` was not declared"
        )));
    }
    if init_fn.is_none() && start_fn.is_none() && terminate_fn.is_none() {
        return Ok(());
    }

    let mut init_call_args = Vec::with_capacity(1 + init_args.len());
    for (idx, place) in init_args.iter().enumerate() {
        init_call_args.push(load_place_as_metadata(
            fn_ctx,
            *place,
            &format!("actor_init_arg_{idx}"),
        )?);
    }
    let ctx_arg = build_spawn_lifecycle_context(fn_ctx, spawned)?;
    emit_actor_state_lock_call(fn_ctx, spawned, "hew_actor_state_lock_acquire")?;
    if let Some(init_fn) = init_fn {
        let mut args = Vec::with_capacity(1 + init_call_args.len());
        args.push(ctx_arg.into());
        args.extend(init_call_args);
        fn_ctx
            .builder
            .build_call(init_fn, &args, &format!("call_{init_name}"))
            .map_err(|e| CodegenError::Llvm(format!("actor init call: {e:?}")))?;
    }
    if let Some(start_fn) = start_fn {
        fn_ctx
            .builder
            .build_call(start_fn, &[ctx_arg.into()], &format!("call_{start_name}"))
            .map_err(|e| CodegenError::Llvm(format!("actor on(start) call: {e:?}")))?;
    }
    emit_actor_state_lock_call(fn_ctx, spawned, "hew_actor_state_lock_release")?;

    // Register the terminate trampoline on the actor so the runtime fires it
    // at normal teardown. Done after the init/on(start) block (outside the
    // lock) so that the registration write is not ordered before any state
    // initialisation performed by init.
    if let Some(terminate_fn) = terminate_fn {
        let mut runtime_decls = RuntimeDeclMap::new();
        let set_terminate = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut runtime_decls,
            "hew_actor_set_terminate",
        )?;
        let fn_ptr = terminate_fn.as_global_value().as_pointer_value();
        fn_ctx
            .builder
            .build_call(
                set_terminate,
                &[spawned.into(), fn_ptr.into()],
                "hew_actor_set_terminate_call",
            )
            .map_err(|e| CodegenError::Llvm(format!("hew_actor_set_terminate call: {e:?}")))?;
    }

    Ok(())
}

fn build_spawn_lifecycle_context<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    actor: PointerValue<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    let ctx_ty = fn_ctx.ctx.i8_type().array_type(128);
    let ctx_slot = fn_ctx
        .builder
        .build_alloca(ctx_ty, "actor_spawn_lifecycle_ctx")
        .map_err(|e| CodegenError::Llvm(format!("spawn lifecycle ctx alloca: {e:?}")))?;
    store_context_ptr_field(fn_ctx, ctx_slot, HEW_CTX_OFFSET_ACTOR, actor, "ctx_actor")?;
    let actor_id = load_actor_id(fn_ctx, actor)?;
    store_context_i64_field(
        fn_ctx,
        ctx_slot,
        HEW_CTX_OFFSET_ACTOR_ID,
        actor_id,
        "ctx_actor_id",
    )?;
    Ok(ctx_slot)
}

fn store_context_ptr_field<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    ctx_slot: PointerValue<'ctx>,
    offset: usize,
    value: PointerValue<'ctx>,
    name: &str,
) -> CodegenResult<()> {
    let field_ptr = unsafe {
        fn_ctx
            .builder
            .build_gep(
                fn_ctx.ctx.i8_type(),
                ctx_slot,
                &[fn_ctx.ctx.i64_type().const_int(offset as u64, false)],
                &format!("{name}_slot"),
            )
            .map_err(|e| CodegenError::Llvm(format!("{name} gep: {e:?}")))?
    };
    fn_ctx
        .builder
        .build_store(field_ptr, value)
        .map_err(|e| CodegenError::Llvm(format!("{name} store: {e:?}")))?;
    Ok(())
}

fn store_context_i64_field<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    ctx_slot: PointerValue<'ctx>,
    offset: usize,
    value: IntValue<'ctx>,
    name: &str,
) -> CodegenResult<()> {
    let field_ptr = unsafe {
        fn_ctx
            .builder
            .build_gep(
                fn_ctx.ctx.i8_type(),
                ctx_slot,
                &[fn_ctx.ctx.i64_type().const_int(offset as u64, false)],
                &format!("{name}_slot"),
            )
            .map_err(|e| CodegenError::Llvm(format!("{name} gep: {e:?}")))?
    };
    fn_ctx
        .builder
        .build_store(field_ptr, value)
        .map_err(|e| CodegenError::Llvm(format!("{name} store: {e:?}")))?;
    Ok(())
}

fn load_place_as_metadata<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    place: Place,
    label: &str,
) -> CodegenResult<BasicMetadataValueEnum<'ctx>> {
    let (ptr, ty) = place_pointer(fn_ctx, place)?;
    let loaded = fn_ctx
        .builder
        .build_load(ty, ptr, &format!("{label}_load"))
        .map_err(|e| CodegenError::Llvm(format!("{label} load: {e:?}")))?;
    Ok(metadata_value_from_basic(loaded))
}

fn emit_actor_state_lock_call(
    fn_ctx: &FnCtx<'_, '_>,
    actor: PointerValue<'_>,
    symbol: &str,
) -> CodegenResult<()> {
    let lock_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        symbol,
    )?;
    let rc = fn_ctx
        .builder
        .build_call(lock_fn, &[actor.into()], &format!("{symbol}_call"))
        .map_err(|e| CodegenError::Llvm(format!("{symbol} call: {e:?}")))?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed(format!("{symbol} returned void")))?
        .into_int_value();
    let ok = fn_ctx
        .builder
        .build_int_compare(
            inkwell::IntPredicate::EQ,
            rc,
            fn_ctx.ctx.i32_type().const_zero(),
            &format!("{symbol}_ok"),
        )
        .map_err(|e| CodegenError::Llvm(format!("{symbol} compare: {e:?}")))?;
    let current = fn_ctx
        .builder
        .get_insert_block()
        .ok_or_else(|| CodegenError::FailClosed(format!("{symbol} has no insertion block")))?;
    let parent = current
        .get_parent()
        .ok_or_else(|| CodegenError::FailClosed(format!("{symbol} block has no parent")))?;
    let ok_bb = fn_ctx
        .ctx
        .append_basic_block(parent, &format!("{symbol}_ok_bb"));
    let trap_bb = fn_ctx
        .ctx
        .append_basic_block(parent, &format!("{symbol}_trap_bb"));
    fn_ctx
        .builder
        .build_conditional_branch(ok, ok_bb, trap_bb)
        .map_err(|e| CodegenError::Llvm(format!("{symbol} branch: {e:?}")))?;
    fn_ctx.builder.position_at_end(trap_bb);
    let trap = Intrinsic::find("llvm.trap")
        .and_then(|intrinsic| intrinsic.get_declaration(fn_ctx.llvm_mod, &[]))
        .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
    fn_ctx
        .builder
        .build_call(trap, &[], &format!("{symbol}_trap"))
        .map_err(|e| CodegenError::Llvm(format!("{symbol} trap: {e:?}")))?;
    fn_ctx
        .builder
        .build_unreachable()
        .map_err(|e| CodegenError::Llvm(format!("{symbol} unreachable: {e:?}")))?;
    fn_ctx.builder.position_at_end(ok_bb);
    Ok(())
}

fn get_or_create_task_closure_wrapper<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    fn_symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    let wrapper_name = task_closure_wrapper_name(fn_symbol);
    if let Some(existing) = fn_ctx.llvm_mod.get_function(&wrapper_name) {
        return Ok(existing);
    }

    let closure_symbol = *fn_ctx.fn_symbols.get(fn_symbol).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SpawnTaskClosure closure invoke shim `{fn_symbol}` was not declared"
        ))
    })?;
    let (closure_value, closure_return_ty, closure_returns_unit) =
        closure_symbol.real(fn_symbol, "SpawnTaskClosure")?;
    if !closure_returns_unit || !matches!(closure_return_ty, BasicTypeEnum::IntType(_)) {
        return Err(CodegenError::FailClosed(format!(
            "SpawnTaskClosure shim `{fn_symbol}` must be unit-lowered to the i8 \
             stand-in return type; got {:?}",
            closure_return_ty
        )));
    }

    if closure_value.get_nth_param(0).is_none() || closure_value.get_nth_param(1).is_none() {
        return Err(CodegenError::FailClosed(format!(
            "SpawnTaskClosure shim `{fn_symbol}` must take leading \
             HewExecutionContext* and closure environment parameters"
        )));
    }

    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let wrapper_ty = fn_ctx
        .ctx
        .void_type()
        .fn_type(&[ptr_ty.into(), ptr_ty.into()], false);
    let wrapper = fn_ctx
        .llvm_mod
        .add_function(&wrapper_name, wrapper_ty, Some(Linkage::Internal));
    let bb = fn_ctx.ctx.append_basic_block(wrapper, "entry");
    let builder = fn_ctx.ctx.create_builder();
    builder.position_at_end(bb);
    let ctx_param = wrapper.get_nth_param(0).ok_or_else(|| {
        CodegenError::FailClosed(
            "closure task wrapper missing HewExecutionContext* parameter".into(),
        )
    })?;
    let task_param = wrapper.get_nth_param(1).ok_or_else(|| {
        CodegenError::FailClosed("closure task wrapper missing HewTask* parameter".into())
    })?;
    let get_env = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_get_env",
    )?;
    let env_ptr = builder
        .build_call(get_env, &[task_param.into()], "hew_task_get_env_call")
        .map_err(|e| CodegenError::Llvm(format!("hew_task_get_env call: {e:?}")))?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_task_get_env returned void".into()))?
        .into_pointer_value();
    builder
        .build_call(
            closure_value,
            &[ctx_param.into(), env_ptr.into()],
            "closure_task_body_call",
        )
        .map_err(|e| CodegenError::Llvm(format!("closure task body call: {e:?}")))?;

    let complete = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_complete_threaded",
    )?;
    builder
        .build_call(
            complete,
            &[task_param.into()],
            "hew_task_complete_threaded_call",
        )
        .map_err(|e| CodegenError::Llvm(format!("hew_task_complete_threaded call: {e:?}")))?;
    builder
        .build_return(None)
        .map_err(|e| CodegenError::Llvm(format!("closure task wrapper return: {e:?}")))?;
    Ok(wrapper)
}

fn emit_spawn_task_closure(
    fn_ctx: &FnCtx<'_, '_>,
    task: Place,
    fn_symbol: &str,
    env: Place,
    env_ty: &ResolvedTy,
) -> CodegenResult<()> {
    let parent_ctx = fn_ctx.execution_context.ok_or_else(|| {
        CodegenError::FailClosed("SpawnTaskClosure spawn site requires an execution context".into())
    })?;
    let task_ptr = load_duplex_handle(fn_ctx, task, "SpawnTaskClosure task")?;
    let env_struct = record_struct_for(fn_ctx, env_ty)?;
    let (env_ptr, env_slot_ty) = place_pointer(fn_ctx, env)?;
    if env_slot_ty != BasicTypeEnum::StructType(env_struct) {
        return Err(CodegenError::FailClosed(format!(
            "SpawnTaskClosure env place type {env_slot_ty:?} does not match registered env \
             struct {env_struct:?}"
        )));
    }
    let Some(env_size) = env_struct.size_of() else {
        return Err(CodegenError::FailClosed(
            "SpawnTaskClosure could not compute closure environment byte size".into(),
        ));
    };
    let rc_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_rc_new",
    )?;
    let null_drop = fn_ctx.ctx.ptr_type(AddressSpace::default()).const_null();
    let rc_env = fn_ctx
        .builder
        .build_call(
            rc_new,
            &[env_ptr.into(), env_size.into(), null_drop.into()],
            "hew_closure_env_rc_new",
        )
        .map_err(|e| CodegenError::Llvm(format!("hew_rc_new call: {e:?}")))?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_rc_new returned void".into()))?
        .into_pointer_value();
    let set_env = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_set_env",
    )?;
    fn_ctx
        .builder
        .build_call(
            set_env,
            &[task_ptr.into(), rc_env.into()],
            "hew_task_set_env_call",
        )
        .map_err(|e| CodegenError::Llvm(format!("hew_task_set_env call: {e:?}")))?;

    let wrapper = get_or_create_task_closure_wrapper(fn_ctx, fn_symbol)?;
    let spawn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_spawn_thread_with_inherited_context",
    )?;
    fn_ctx
        .builder
        .build_call(
            spawn,
            &[
                parent_ctx.into(),
                task_ptr.into(),
                wrapper.as_global_value().as_pointer_value().into(),
            ],
            "hew_task_spawn_thread_with_inherited_context_closure_call",
        )
        .map_err(|e| {
            CodegenError::Llvm(format!(
                "hew_task_spawn_thread_with_inherited_context call: {e:?}"
            ))
        })?;
    Ok(())
}

fn overflow_intrinsic_name(op: IntArithOp, signed: IntSignedness) -> &'static str {
    match (op, signed) {
        (IntArithOp::Add, IntSignedness::Signed) => "llvm.sadd.with.overflow",
        (IntArithOp::Add, IntSignedness::Unsigned) => "llvm.uadd.with.overflow",
        (IntArithOp::Sub, IntSignedness::Signed) => "llvm.ssub.with.overflow",
        (IntArithOp::Sub, IntSignedness::Unsigned) => "llvm.usub.with.overflow",
        (IntArithOp::Mul, IntSignedness::Signed) => "llvm.smul.with.overflow",
        (IntArithOp::Mul, IntSignedness::Unsigned) => "llvm.umul.with.overflow",
    }
}

fn validate_numeric_method_width(
    width: NumericWidth,
    int_ty: inkwell::types::IntType<'_>,
    construct: &str,
) -> CodegenResult<()> {
    match width {
        NumericWidth::Bits(bits) if bits == int_ty.get_bit_width() => Ok(()),
        NumericWidth::Bits(bits) => Err(CodegenError::FailClosed(format!(
            "{construct} width side-table mismatch: checker recorded {bits} bits, LLVM operand is {} bits",
            int_ty.get_bit_width()
        ))),
        NumericWidth::Pointer => Err(CodegenError::FailClosed(format!(
            "{construct} on isize/usize requires target pointer-width layout; no target layout was provided to this instruction"
        ))),
    }
}

fn saturating_bound<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    op: IntArithOp,
    signed: IntSignedness,
    int_ty: inkwell::types::IntType<'ctx>,
    lhs_v: IntValue<'ctx>,
    rhs_v: IntValue<'ctx>,
) -> CodegenResult<IntValue<'ctx>> {
    let bits = int_ty.get_bit_width();
    let zero = int_ty.const_zero();
    match signed {
        IntSignedness::Unsigned => Ok(match op {
            IntArithOp::Sub => zero,
            IntArithOp::Add | IntArithOp::Mul => int_ty.const_all_ones(),
        }),
        IntSignedness::Signed => {
            let max = int_ty.const_int(((1u128 << (bits - 1)) - 1) as u64, false);
            let min = int_ty.const_int((1u128 << (bits - 1)) as u64, false);
            let choose_max = match op {
                IntArithOp::Add => fn_ctx
                    .builder
                    .build_int_compare(IntPredicate::SGE, rhs_v, zero, "sat_add_positive")
                    .map_err(|e| {
                        CodegenError::Llvm(format!("saturating add sign compare: {e:?}"))
                    })?,
                IntArithOp::Sub => fn_ctx
                    .builder
                    .build_int_compare(IntPredicate::SLT, rhs_v, zero, "sat_sub_negative")
                    .map_err(|e| {
                        CodegenError::Llvm(format!("saturating sub sign compare: {e:?}"))
                    })?,
                IntArithOp::Mul => {
                    let lhs_neg = fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::SLT, lhs_v, zero, "sat_mul_lhs_neg")
                        .map_err(|e| {
                            CodegenError::Llvm(format!("saturating mul lhs sign compare: {e:?}"))
                        })?;
                    let rhs_neg = fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::SLT, rhs_v, zero, "sat_mul_rhs_neg")
                        .map_err(|e| {
                            CodegenError::Llvm(format!("saturating mul rhs sign compare: {e:?}"))
                        })?;
                    fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::EQ, lhs_neg, rhs_neg, "sat_mul_same_sign")
                        .map_err(|e| {
                            CodegenError::Llvm(format!("saturating mul same-sign compare: {e:?}"))
                        })?
                }
            };
            Ok(fn_ctx
                .builder
                .build_select(choose_max, max, min, "sat_signed_bound")
                .map_err(|e| CodegenError::Llvm(format!("saturating bound select: {e:?}")))?
                .into_int_value())
        }
    }
}

fn lower_instruction(
    fn_ctx: &FnCtx<'_, '_>,
    instr: &Instr,
    block_id: u32,
    drop_plans: &[(ExitPath, hew_mir::DropPlan)],
) -> CodegenResult<()> {
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
        Instr::EnterContext | Instr::ExitContext => {
            if fn_ctx.execution_context.is_none() {
                return Err(CodegenError::FailClosed(
                    "context boundary marker requires an actor-handler execution context".into(),
                ));
            }
        }
        Instr::CheckCancellation => {
            if fn_ctx.execution_context.is_none() {
                return Err(CodegenError::FailClosed(
                    "CheckCancellation requires an actor-handler execution context".into(),
                ));
            }
            emit_cooperate_check(fn_ctx, block_id, drop_plans)?;
        }
        Instr::ContextField { dest, offset } => {
            lower_context_field(fn_ctx, *dest, *offset)?;
        }
        Instr::ConstI64 { dest, value } => {
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let int_ty = match dest_ty {
                BasicTypeEnum::IntType(i) => i,
                _ => {
                    return Err(CodegenError::FailClosed(format!(
                        "ConstI64 dest is not an i64: dest_ty={dest_ty:?}"
                    )));
                }
            };
            // `value` is i64 in MIR. Truncate/extend at the LLVM level to
            // whatever the dest local actually is — Cluster 1's lowering
            // currently always sizes locals to the front-half's i64 type
            // (`i64` for `i64`), so this is a no-op for the spine.
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
                _ => return Err(CodegenError::FailClosed("IntAdd lhs is not an i64".into())),
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "Int arithmetic operands and dest must share the same i64 type".into(),
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
                _ => unreachable!("matched on three i64-arith variants above"),
            }
            .map_err(|e| CodegenError::Llvm(format!("i64 arith: {e:?}")))?;
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
                        "IntDiv/IntRem lhs is not an i64".into(),
                    ));
                }
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntDiv/IntRem operands and dest must share the same i64 type".into(),
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
                _ => return Err(CodegenError::FailClosed("IntShl lhs is not an i64".into())),
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntShl operands and dest must share the same i64 type".into(),
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
                _ => return Err(CodegenError::FailClosed("IntShr lhs is not an i64".into())),
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntShr operands and dest must share the same i64 type".into(),
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
        // Bitwise &, |, ^. Well-defined for all integer widths/signednesses;
        // no traps, no overflow checks. Operands and dest share the same i64
        // type (enforced upstream by the checker).
        Instr::IntBitAnd { dest, lhs, rhs }
        | Instr::IntBitOr { dest, lhs, rhs }
        | Instr::IntBitXor { dest, lhs, rhs } => {
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let lhs_int = match lhs_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IntBitwise lhs is not an i64".into(),
                    ));
                }
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntBitwise operands and dest must share the same i64 type".into(),
                ));
            }
            let lhs_v = fn_ctx
                .builder
                .build_load(lhs_int, lhs_ptr, "bitwise_lhs")
                .map_err(|e| CodegenError::Llvm(format!("bitwise lhs load: {e:?}")))?
                .into_int_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_int, rhs_ptr, "bitwise_rhs")
                .map_err(|e| CodegenError::Llvm(format!("bitwise rhs load: {e:?}")))?
                .into_int_value();
            let result = match instr {
                Instr::IntBitAnd { .. } => fn_ctx.builder.build_and(lhs_v, rhs_v, "bitand"),
                Instr::IntBitOr { .. } => fn_ctx.builder.build_or(lhs_v, rhs_v, "bitor"),
                Instr::IntBitXor { .. } => fn_ctx.builder.build_xor(lhs_v, rhs_v, "bitxor"),
                _ => unreachable!("matched on three bitwise variants above"),
            }
            .map_err(|e| CodegenError::Llvm(format!("bitwise: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result)
                .map_err(|e| CodegenError::Llvm(format!("bitwise store: {e:?}")))?;
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
                        "IntArithChecked lhs is not an i64".into(),
                    ));
                }
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntArithChecked operands and dest must share the same i64 type".into(),
                ));
            }
            let flag_int = match flag_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IntArithChecked overflow_flag is not an i64".into(),
                    ));
                }
            };
            // Choose the intrinsic family by op + signedness. Six
            // intrinsics total — three ops × two signednesses. The
            // overload is per integer width, so `get_declaration`
            // receives the operand i64 type.
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
        Instr::IntArithCheckedOption {
            op,
            signed,
            width,
            dest,
            lhs,
            rhs,
        } => {
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let lhs_int = match lhs_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IntArithCheckedOption lhs is not an integer".into(),
                    ));
                }
            };
            if rhs_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntArithCheckedOption operands must share the same integer type".into(),
                ));
            }
            validate_numeric_method_width(*width, lhs_int, "IntArithCheckedOption")?;
            let Place::Local(dest_local) = dest else {
                return Err(CodegenError::FailClosed(
                    "IntArithCheckedOption destination must be a local enum slot".into(),
                ));
            };
            let (tag_ptr, tag_ty) = place_pointer(fn_ctx, Place::EnumTag(*dest_local))?;
            let tag_int = match tag_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IntArithCheckedOption enum tag is not an integer".into(),
                    ));
                }
            };
            let some_payload = Place::EnumVariant {
                local: *dest_local,
                variant_idx: 0,
                field_idx: 0,
            };
            let (payload_ptr, payload_ty) = place_pointer(fn_ctx, some_payload)?;
            if payload_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntArithCheckedOption Some payload type must match operand type".into(),
                ));
            }
            let intrinsic_name = overflow_intrinsic_name(*op, *signed);
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
                .build_load(lhs_int, lhs_ptr, "checked_option_lhs")
                .map_err(|e| CodegenError::Llvm(format!("checked-option lhs load: {e:?}")))?
                .into_int_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_int, rhs_ptr, "checked_option_rhs")
                .map_err(|e| CodegenError::Llvm(format!("checked-option rhs load: {e:?}")))?
                .into_int_value();
            let call_site = fn_ctx
                .builder
                .build_call(
                    intrinsic_fn,
                    &[lhs_v.into(), rhs_v.into()],
                    "checked_option_with_overflow",
                )
                .map_err(|e| CodegenError::Llvm(format!("checked-option intrinsic call: {e:?}")))?;
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
                .build_extract_value(agg, 0, "checked_option_result")
                .map_err(|e| CodegenError::Llvm(format!("checked-option result extract: {e:?}")))?
                .into_int_value();
            let of_bit = fn_ctx
                .builder
                .build_extract_value(agg, 1, "checked_option_overflow")
                .map_err(|e| CodegenError::Llvm(format!("checked-option overflow extract: {e:?}")))?
                .into_int_value();
            let some_tag = tag_int.const_zero();
            let none_tag = tag_int.const_int(1, false);
            let tag_v = fn_ctx
                .builder
                .build_select(of_bit, none_tag, some_tag, "checked_option_tag")
                .map_err(|e| CodegenError::Llvm(format!("checked-option tag select: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(tag_ptr, tag_v)
                .map_err(|e| CodegenError::Llvm(format!("checked-option tag store: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(payload_ptr, result_v)
                .map_err(|e| CodegenError::Llvm(format!("checked-option payload store: {e:?}")))?;
        }
        Instr::IntArithSaturating {
            op,
            signed,
            width,
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
                        "IntArithSaturating lhs is not an integer".into(),
                    ));
                }
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntArithSaturating operands and dest must share the same integer type".into(),
                ));
            }
            validate_numeric_method_width(*width, lhs_int, "IntArithSaturating")?;
            let intrinsic_name = overflow_intrinsic_name(*op, *signed);
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
                .build_load(lhs_int, lhs_ptr, "saturating_lhs")
                .map_err(|e| CodegenError::Llvm(format!("saturating lhs load: {e:?}")))?
                .into_int_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_int, rhs_ptr, "saturating_rhs")
                .map_err(|e| CodegenError::Llvm(format!("saturating rhs load: {e:?}")))?
                .into_int_value();
            let call_site = fn_ctx
                .builder
                .build_call(
                    intrinsic_fn,
                    &[lhs_v.into(), rhs_v.into()],
                    "saturating_with_overflow",
                )
                .map_err(|e| CodegenError::Llvm(format!("saturating intrinsic call: {e:?}")))?;
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
                .build_extract_value(agg, 0, "saturating_result")
                .map_err(|e| CodegenError::Llvm(format!("saturating result extract: {e:?}")))?
                .into_int_value();
            let of_bit = fn_ctx
                .builder
                .build_extract_value(agg, 1, "saturating_overflow")
                .map_err(|e| CodegenError::Llvm(format!("saturating overflow extract: {e:?}")))?
                .into_int_value();
            let bound = saturating_bound(fn_ctx, *op, *signed, lhs_int, lhs_v, rhs_v)?;
            let final_v = fn_ctx
                .builder
                .build_select(of_bit, bound, result_v, "saturating_select")
                .map_err(|e| CodegenError::Llvm(format!("saturating select: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, final_v)
                .map_err(|e| CodegenError::Llvm(format!("saturating result store: {e:?}")))?;
        }
        Instr::IntCmp {
            dest,
            pred,
            lhs,
            rhs,
        } => {
            // Load both operands at their declared i64 type, compare with
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
                _ => return Err(CodegenError::FailClosed("IntCmp lhs is not an i64".into())),
            };
            if rhs_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "IntCmp operands must share the same i64 type".into(),
                ));
            }
            let dest_int = match dest_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => return Err(CodegenError::FailClosed("IntCmp dest is not an i64".into())),
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
        Instr::IdentityCompare { dest, lhs, rhs } => {
            // Emit pointer/handle identity comparison for `lhs is rhs`.
            //
            // The operands are pointer-shaped allocas (`ptr` LLVM type) —
            // the checker (D-2) has already validated that only identity-
            // bearing types reach this instruction. We load the `ptr`
            // value from each operand's alloca, convert both to `i64` via
            // `ptrtoint`, compare with `icmp eq` to get an `i1`, then
            // zero-extend to the dest width (matching the `IntCmp` path).
            //
            // LESSONS: `checker-authority` (P0) — the type is read from
            // the operand's LLVM type at codegen time; we do not re-derive
            // the identity allowance set here.
            let i64_ty = fn_ctx.ctx.i64_type();
            let ptr_ty = fn_ctx.ctx.ptr_type(inkwell::AddressSpace::default());
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let dest_int = match dest_ty {
                BasicTypeEnum::IntType(t) => t,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IdentityCompare dest is not an i64".into(),
                    ));
                }
            };
            // Load the pointer/handle value from each alloca.
            let lhs_val = fn_ctx
                .builder
                .build_load(lhs_ty, lhs_ptr, "is_lhs")
                .map_err(|e| CodegenError::Llvm(format!("is lhs load: {e:?}")))?;
            let rhs_val = fn_ctx
                .builder
                .build_load(rhs_ty, rhs_ptr, "is_rhs")
                .map_err(|e| CodegenError::Llvm(format!("is rhs load: {e:?}")))?;
            // Convert both operands to i64 integers for comparison.
            // Pointer operands: `ptrtoint ptr to i64`.
            // Integer operands (machine-id, already i64): bitcast or no-op.
            let lhs_int = match lhs_val {
                inkwell::values::BasicValueEnum::PointerValue(p) => fn_ctx
                    .builder
                    .build_ptr_to_int(p, i64_ty, "is_lhs_int")
                    .map_err(|e| CodegenError::Llvm(format!("is lhs ptrtoint: {e:?}")))?,
                inkwell::values::BasicValueEnum::IntValue(v) => {
                    // Machine-id or other integer-shaped handle: extend/truncate
                    // to i64 so the icmp operands are uniform width.
                    fn_ctx
                        .builder
                        .build_int_z_extend_or_bit_cast(v, i64_ty, "is_lhs_i64")
                        .map_err(|e| CodegenError::Llvm(format!("is lhs cast: {e:?}")))?
                }
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IdentityCompare lhs must be a pointer or integer value".into(),
                    ));
                }
            };
            let rhs_int = match rhs_val {
                inkwell::values::BasicValueEnum::PointerValue(p) => fn_ctx
                    .builder
                    .build_ptr_to_int(p, i64_ty, "is_rhs_int")
                    .map_err(|e| CodegenError::Llvm(format!("is rhs ptrtoint: {e:?}")))?,
                inkwell::values::BasicValueEnum::IntValue(v) => fn_ctx
                    .builder
                    .build_int_z_extend_or_bit_cast(v, i64_ty, "is_rhs_i64")
                    .map_err(|e| CodegenError::Llvm(format!("is rhs cast: {e:?}")))?,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "IdentityCompare rhs must be a pointer or integer value".into(),
                    ));
                }
            };
            let _ = (ptr_ty, lhs_ty, rhs_ty);
            let bit = fn_ctx
                .builder
                .build_int_compare(inkwell::IntPredicate::EQ, lhs_int, rhs_int, "is_bit")
                .map_err(|e| CodegenError::Llvm(format!("is icmp: {e:?}")))?;
            let widened = fn_ctx
                .builder
                .build_int_z_extend_or_bit_cast(bit, dest_int, "is_zext")
                .map_err(|e| CodegenError::Llvm(format!("is zext: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .map_err(|e| CodegenError::Llvm(format!("is store: {e:?}")))?;
            let _ = ctx;
        }
        Instr::Move { dest, src } => {
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let (src_ptr, src_ty) = place_pointer(fn_ctx, *src)?;
            if dest_ty != src_ty {
                // Special case: i64 → ptr is the handle-pointer promotion path
                // emitted by `lower_supervisor_child_get`. The MIR stores the
                // runtime handle as a raw `i64` in the `__HewChildLookupResult`
                // struct (field 1); after extraction via `RecordFieldLoad`, the
                // value must be reinterpreted as a `ptr` before being stored
                // into an `ActorHandle`-typed place (which is `LocalPid<T>`,
                // lowered to `ptr`). Emit `inttoptr` instead of failing.
                //
                // WHY this shape: `lower_supervisor_child_get` in hew-mir uses
                // `ResolvedTy::I64` for both struct fields so a single MIR type
                // covers the wire representation; S3 is the responsible layer for
                // the `i64 → ptr` promotion (S2 doc comment, lower.rs:5225–5226).
                //
                // WHEN obsolete: when the supervisor accessor MIR is redesigned
                // to carry a ptr-typed handle field directly.
                use inkwell::types::BasicTypeEnum;
                // Slice 5: integer width mismatch (iW → iN where N >= W)
                // arises when Slice 4b's dispatch tree moves a narrow
                // machine tag (iW from `Place::MachineTag`) into an I64
                // scratch local for comparison. Emit a `zext` load+store
                // so the dispatch keeps its I64 invariant without forcing
                // each tag-width to introduce a new MIR-level narrow type.
                if let (BasicTypeEnum::IntType(src_int), BasicTypeEnum::IntType(dest_int)) =
                    (src_ty, dest_ty)
                {
                    if src_int.get_bit_width() < dest_int.get_bit_width() {
                        let narrow = fn_ctx
                            .builder
                            .build_load(src_ty, src_ptr, "move_iN_load")
                            .map_err(|e| {
                                CodegenError::Llvm(format!("move narrow int load: {e:?}"))
                            })?
                            .into_int_value();
                        let widened = fn_ctx
                            .builder
                            .build_int_z_extend(narrow, dest_int, "move_iN_zext")
                            .map_err(|e| CodegenError::Llvm(format!("move zext: {e:?}")))?;
                        fn_ctx
                            .builder
                            .build_store(dest_ptr, widened)
                            .map_err(|e| CodegenError::Llvm(format!("move zext store: {e:?}")))?;
                        return Ok(());
                    }
                    // Narrowing iN → iW for the symmetric write-back:
                    // `Move { dest: Place::MachineTag(local), src: <I64 local> }`.
                    // The transition body's `MachineVariantCtor` emits an
                    // `Instr::ConstI64` into an I64 local then moves it
                    // into the tag slot.
                    if src_int.get_bit_width() > dest_int.get_bit_width() {
                        let wide = fn_ctx
                            .builder
                            .build_load(src_ty, src_ptr, "move_iN_load_wide")
                            .map_err(|e| CodegenError::Llvm(format!("move wide int load: {e:?}")))?
                            .into_int_value();
                        let narrowed = fn_ctx
                            .builder
                            .build_int_truncate(wide, dest_int, "move_iN_trunc")
                            .map_err(|e| CodegenError::Llvm(format!("move trunc: {e:?}")))?;
                        fn_ctx
                            .builder
                            .build_store(dest_ptr, narrowed)
                            .map_err(|e| CodegenError::Llvm(format!("move trunc store: {e:?}")))?;
                        return Ok(());
                    }
                }
                if matches!(src_ty, BasicTypeEnum::IntType(t) if t.get_bit_width() == 64)
                    && matches!(dest_ty, BasicTypeEnum::PointerType(_))
                    && matches!(*dest, Place::ActorHandle(_))
                {
                    let i64_val = fn_ctx
                        .builder
                        .build_load(src_ty, src_ptr, "move_i64_load")
                        .map_err(|e| CodegenError::Llvm(format!("move i64 load: {e:?}")))?
                        .into_int_value();
                    let ptr_ty = dest_ty.into_pointer_type();
                    let ptr_val = fn_ctx
                        .builder
                        .build_int_to_ptr(i64_val, ptr_ty, "move_inttoptr")
                        .map_err(|e| {
                            CodegenError::Llvm(format!("move inttoptr (handle promotion): {e:?}"))
                        })?;
                    fn_ctx
                        .builder
                        .build_store(dest_ptr, ptr_val)
                        .map_err(|e| CodegenError::Llvm(format!("move ptr store: {e:?}")))?;
                } else {
                    return Err(CodegenError::FailClosed(format!(
                        "Move type mismatch: src={src_ty:?} dest={dest_ty:?}"
                    )));
                }
            } else {
                let loaded = fn_ctx
                    .builder
                    .build_load(src_ty, src_ptr, "move_load")
                    .map_err(|e| CodegenError::Llvm(format!("move load: {e:?}")))?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, loaded)
                    .map_err(|e| CodegenError::Llvm(format!("move store: {e:?}")))?;
            }
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
        Instr::StringLit { bytes, dest } => {
            // Emit an LLVM global constant for the string bytes (NUL-terminated,
            // internal linkage, read-only) and store its address into the `dest`
            // alloca. Matches the C++ codegen's `hew.global_string` →
            // `llvm.mlir.addressof` pattern (codegen.cpp `ConstantOpLowering` /
            // `GlobalStringOpLowering`, ~lines 257-265).
            //
            // The `dest` local must have been allocated with `ResolvedTy::String`,
            // which `primitive_to_llvm` maps to an opaque `ptr`. The pointer to the
            // global is the `String` value at the ABI boundary (`*const c_char`).
            // `hew_string_drop` skips freeing static-segment pointers via its
            // `is_static_string` guard, so no clone or heap allocation is needed.
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            if !matches!(dest_ty, BasicTypeEnum::PointerType(_)) {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::StringLit dest is not a pointer type: dest_ty={dest_ty:?}"
                )));
            }
            // `build_global_string_ptr` emits:
            //   @.str.N = private unnamed_addr constant [len+1 x i8] c"...\00"
            // and returns a GlobalValue whose as_pointer_value() is the address
            // of the first byte (an opaque `ptr` in LLVM 17+ opaque-pointer mode).
            //
            // Safety note: `build_global_string_ptr` takes `&str` and internally
            // calls `to_c_str` which NUL-terminates via CString. Since Hew source
            // strings are UTF-8 and `bytes` carries the parser-decoded sequence,
            // `from_utf8` is infallible for valid programs. The SHIM note below
            // covers the embedded-NUL edge case.
            //
            // SHIM: embedded NUL bytes in the literal byte sequence would be
            // truncated at the first NUL by `to_c_str` → `CStr::from_bytes_until_nul`.
            // WHY: the Hew runtime treats all strings as null-terminated C strings;
            //      embedded NULs are silently truncated by every C-string runtime op.
            // WHEN: obsolete if Hew ever adopts a length-prefixed or fat-pointer
            //       string representation.
            // WHAT: replace `build_global_string_ptr` with a manual `add_global` +
            //       set_initializer using an i8 array to preserve bytes exactly.
            let s = std::str::from_utf8(bytes).map_err(|e| {
                CodegenError::FailClosed(format!("Instr::StringLit bytes are not valid UTF-8: {e}"))
            })?;
            let global = fn_ctx
                .builder
                .build_global_string_ptr(s, "str_lit")
                .map_err(|e| CodegenError::Llvm(format!("build_global_string_ptr: {e:?}")))?;
            let ptr_val = global.as_pointer_value();
            fn_ctx
                .builder
                .build_store(dest_ptr, ptr_val)
                .map_err(|e| CodegenError::Llvm(format!("StringLit store: {e:?}")))?;
            let _ = ctx;
        }
        Instr::RecordInit { ty, fields, dest } => {
            lower_record_init(fn_ctx, ty, fields, *dest)?;
            let _ = ctx;
        }
        Instr::RecordFieldLoad {
            record,
            field_offset,
            dest,
        } => {
            lower_record_field_load(fn_ctx, *record, *field_offset, *dest)?;
            let _ = ctx;
        }
        Instr::ActorStateFieldLoad { field_offset, dest } => {
            lower_actor_state_field_load(fn_ctx, *field_offset, *dest)?;
            let _ = ctx;
        }
        Instr::ActorStateFieldStore { field_offset, src } => {
            lower_actor_state_field_store(fn_ctx, *field_offset, *src)?;
            let _ = ctx;
        }
        Instr::TupleFieldLoad {
            tuple,
            field_index,
            dest,
        } => {
            lower_tuple_field_load(fn_ctx, *tuple, *field_index, *dest)?;
            let _ = ctx;
        }
        Instr::FloatLit {
            dest,
            value_bits,
            width,
        } => {
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let float_ty = match dest_ty {
                BasicTypeEnum::FloatType(f) => f,
                _ => {
                    return Err(CodegenError::FailClosed(format!(
                        "FloatLit dest is not a float type: dest_ty={dest_ty:?}"
                    )));
                }
            };
            // Reconstruct the IEEE 754 value from its stored bit pattern.
            // For F32: lower 32 bits are the f32 pattern; widen to f64 for
            // inkwell's `const_float(f64)` API (exact for all finite values
            // that Hew literal syntax can produce).
            // For F64: all 64 bits are the f64 pattern.
            let value_f64 = match width {
                FloatWidth::F32 => f32::from_bits(*value_bits as u32) as f64,
                FloatWidth::F64 => f64::from_bits(*value_bits),
            };
            let v = float_ty.const_float(value_f64);
            fn_ctx
                .builder
                .build_store(dest_ptr, v)
                .map_err(|e| CodegenError::Llvm(format!("FloatLit store: {e:?}")))?;
            let _ = ctx;
        }
        Instr::FloatAdd { dest, lhs, rhs, .. }
        | Instr::FloatSub { dest, lhs, rhs, .. }
        | Instr::FloatMul { dest, lhs, rhs, .. }
        | Instr::FloatDiv { dest, lhs, rhs, .. }
        | Instr::FloatRem { dest, lhs, rhs, .. } => {
            // IEEE 754 float arithmetic. No `nsw`/`nuw` flags — those are
            // integer-only. Out-of-range results produce ±inf or NaN per
            // IEEE 754; no trap path is emitted.
            let (lhs_ptr, lhs_ty) = place_pointer(fn_ctx, *lhs)?;
            let (rhs_ptr, rhs_ty) = place_pointer(fn_ctx, *rhs)?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let lhs_float = match lhs_ty {
                BasicTypeEnum::FloatType(f) => f,
                _ => {
                    return Err(CodegenError::FailClosed(
                        "float arithmetic lhs is not a float type".into(),
                    ));
                }
            };
            if rhs_ty != lhs_ty || dest_ty != lhs_ty {
                return Err(CodegenError::FailClosed(
                    "float arithmetic operands and dest must share the same float type".into(),
                ));
            }
            let lhs_v = fn_ctx
                .builder
                .build_load(lhs_float, lhs_ptr, "farith_lhs")
                .map_err(|e| CodegenError::Llvm(format!("farith lhs load: {e:?}")))?
                .into_float_value();
            let rhs_v = fn_ctx
                .builder
                .build_load(lhs_float, rhs_ptr, "farith_rhs")
                .map_err(|e| CodegenError::Llvm(format!("farith rhs load: {e:?}")))?
                .into_float_value();
            let result = match instr {
                Instr::FloatAdd { .. } => {
                    fn_ctx.builder.build_float_add(lhs_v, rhs_v, "farith_add")
                }
                Instr::FloatSub { .. } => {
                    fn_ctx.builder.build_float_sub(lhs_v, rhs_v, "farith_sub")
                }
                Instr::FloatMul { .. } => {
                    fn_ctx.builder.build_float_mul(lhs_v, rhs_v, "farith_mul")
                }
                Instr::FloatDiv { .. } => {
                    fn_ctx.builder.build_float_div(lhs_v, rhs_v, "farith_div")
                }
                Instr::FloatRem { .. } => {
                    fn_ctx.builder.build_float_rem(lhs_v, rhs_v, "farith_rem")
                }
                _ => unreachable!("matched on five float-arith variants above"),
            }
            .map_err(|e| CodegenError::Llvm(format!("float arith: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result)
                .map_err(|e| CodegenError::Llvm(format!("farith store: {e:?}")))?;
            let _ = ctx;
        }
        Instr::CharLit { value, dest } => {
            // `char` is i32: the Unicode scalar value U+0000..U+10FFFF.
            // Stored as `u32` in MIR; emit as an i32 constant (sign-extension
            // is harmless — no scalar value exceeds 0x10FFFF, well within i32
            // positive range).
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let int_ty = match dest_ty {
                BasicTypeEnum::IntType(i) => i,
                _ => {
                    return Err(CodegenError::FailClosed(format!(
                        "CharLit dest is not an i64: dest_ty={dest_ty:?}"
                    )));
                }
            };
            let v = int_ty.const_int(*value as u64, false);
            fn_ctx
                .builder
                .build_store(dest_ptr, v)
                .map_err(|e| CodegenError::Llvm(format!("CharLit store: {e:?}")))?;
            let _ = ctx;
        }
        Instr::UnitLit { dest } => {
            // Unit is zero-sized. `primitive_to_llvm(Unit)` maps to i8 as a
            // stand-in. Store 0 to give the slot a well-defined value; the
            // consumer never observes it in well-typed programs (unit bindings
            // are dropped before escaping the codegen boundary).
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let int_ty = match dest_ty {
                BasicTypeEnum::IntType(i) => i,
                _ => {
                    return Err(CodegenError::FailClosed(format!(
                        "UnitLit dest is not an i64 (expected i8 stand-in): dest_ty={dest_ty:?}"
                    )));
                }
            };
            let v = int_ty.const_int(0, false);
            fn_ctx
                .builder
                .build_store(dest_ptr, v)
                .map_err(|e| CodegenError::Llvm(format!("UnitLit store: {e:?}")))?;
            let _ = ctx;
        }
        Instr::DurationLit { nanos, dest } => {
            // Duration is i64 nanoseconds. `nanos` is already the final
            // nanosecond representation — no conversion needed.
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let int_ty = match dest_ty {
                BasicTypeEnum::IntType(i) => i,
                _ => {
                    return Err(CodegenError::FailClosed(format!(
                        "DurationLit dest is not an i64: dest_ty={dest_ty:?}"
                    )));
                }
            };
            #[allow(clippy::cast_sign_loss)]
            let v = int_ty.const_int(*nanos as u64, true);
            fn_ctx
                .builder
                .build_store(dest_ptr, v)
                .map_err(|e| CodegenError::Llvm(format!("DurationLit store: {e:?}")))?;
            let _ = ctx;
        }
        Instr::MakeClosure {
            fn_symbol,
            env,
            dest,
        } => {
            lower_make_closure(fn_ctx, fn_symbol, *env, *dest)?;
            let _ = ctx;
        }
        Instr::ClosureEnvFieldLoad {
            env,
            env_ty,
            field_offset,
            dest,
        } => {
            lower_closure_env_field_load(fn_ctx, *env, env_ty, *field_offset, *dest)?;
            let _ = ctx;
        }
        Instr::CallClosure {
            callee,
            args,
            ret_ty,
            dest,
        } => {
            lower_call_closure(fn_ctx, *callee, args, ret_ty, *dest)?;
            let _ = ctx;
        }
        Instr::SpawnTaskDirect {
            task,
            callee_symbol,
        } => {
            emit_spawn_task_direct(fn_ctx, *task, callee_symbol)?;
            let _ = ctx;
        }
        Instr::SpawnTaskClosure {
            task,
            fn_symbol,
            env,
            env_ty,
        } => {
            emit_spawn_task_closure(fn_ctx, *task, fn_symbol, *env, env_ty)?;
            let _ = ctx;
        }
        Instr::SpawnActor {
            actor_name,
            state,
            init_args,
            dest,
            max_heap_bytes,
            cycle_capable,
        } => {
            emit_spawn_actor(
                fn_ctx,
                actor_name,
                *state,
                init_args,
                *dest,
                *max_heap_bytes,
                *cycle_capable,
            )?;
            let _ = ctx;
        }
        // TO-3 lands the MIR shape (`Instr::CoerceToDynTrait`,
        // `Instr::CallTraitMethod`); TO-4 (runtime-trait-object-abi.md
        // §D-3 / §D-4) adds the LLVM emission (vtable static + GEP /
        // load / call). Fail closed until TO-4 wires the arms — better
        // a deterministic compile error than a silent miscompile.
        Instr::CoerceToDynTrait { trait_name, .. } => {
            return Err(CodegenError::Llvm(format!(
                "Instr::CoerceToDynTrait (trait `{trait_name}`) reached LLVM emission \
                 before the TO-4 vtable-emission slice landed"
            )));
        }
        Instr::CallTraitMethod {
            trait_name,
            method_name,
            ..
        } => {
            return Err(CodegenError::Llvm(format!(
                "Instr::CallTraitMethod `{trait_name}::{method_name}` reached LLVM emission \
                 before the TO-4 vtable-dispatch slice landed"
            )));
        }
        Instr::MachineEmitPlaceholder { event_idx, payload } => {
            // Lower a machine emit expression to a call to `hew_machine_emit_push`.
            //
            // ABI: `hew_machine_emit_push(event_idx: u64, payload_ptr: *const u8,
            //                             payload_len: u64) -> void`
            //
            // The per-thread emit queue (a `thread_local!` `EmitQueue` in
            // `hew-runtime/src/machine_emit.rs`) receives the push. After the
            // enclosing `step()` boundary the scheduler (or test harness) calls
            // `thread_emit_drain` / `hew_machine_emit_drain` to process events.
            //
            // SHIM: only unit events (empty payload) are supported in this slice.
            // Non-unit emits require a serialisation scheme (likely MessagePack)
            // to encode `payload: Vec<Place>` into bytes. Until that lands,
            // non-empty payloads fail closed with a clear diagnostic.
            // WHY: the tcp_handshake.hew fixture only emits unit events
            //      (`emit AckReceive {}`); deferring serialisation avoids
            //      inventing an encoding before the ABI is ratified.
            // WHEN-OBSOLETE: when the emit-payload serialisation slice lands and
            //      defines how field values are packed into the payload buffer.
            // WHAT: replace the null/0 args with a stack-allocated payload struct
            //      encoded via the ratified scheme, and load the ptr/len from it.
            if !payload.is_empty() {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::MachineEmitPlaceholder(event_idx={event_idx}): non-unit emit \
                     (payload len={}) cannot be lowered until the emit-payload serialisation \
                     slice lands; only unit events (no fields) are supported in this slice",
                    payload.len()
                )));
            }
            let i64_ty = fn_ctx.ctx.i64_type();
            let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
            // Intern (or reuse) the extern declaration for `hew_machine_emit_push`.
            // ABI: (u64, *const u8, u64) -> void
            let emit_push_fn = match fn_ctx.llvm_mod.get_function("hew_machine_emit_push") {
                Some(fv) => fv,
                None => {
                    let fn_ty = fn_ctx
                        .ctx
                        .void_type()
                        .fn_type(&[i64_ty.into(), ptr_ty.into(), i64_ty.into()], false);
                    fn_ctx.llvm_mod.add_function(
                        "hew_machine_emit_push",
                        fn_ty,
                        Some(Linkage::External),
                    )
                }
            };
            // event_idx as u64 constant.
            let idx_val = i64_ty.const_int(*event_idx as u64, false);
            // null payload pointer and zero length for unit events.
            let null_ptr = ptr_ty.const_null();
            let zero_len = i64_ty.const_int(0, false);
            fn_ctx
                .builder
                .build_call(
                    emit_push_fn,
                    &[idx_val.into(), null_ptr.into(), zero_len.into()],
                    "machine_emit_push_call",
                )
                .map_err(|e| CodegenError::Llvm(format!("hew_machine_emit_push call: {e:?}")))?;
        }
        Instr::EnumTagLoad { src, dest } => {
            // The `src` local holds a tagged-union enum value (the machine
            // event companion in Slice 4b dispatch trees). The tag is at
            // outer-struct field 0 (per the layout invariants documented
            // at `register_machine_layouts`). We GEP to field 0, load the
            // iW tag, zext to the dest's integer width, and store. Symmetric
            // with `Place::MachineTag` for state tags.
            let Place::Local(src_local) = src else {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::EnumTagLoad src must be a Place::Local; got {src:?}"
                )));
            };
            let src_ty = fn_ctx.local_tys.get(src_local).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Instr::EnumTagLoad src local {src_local} has no resolved type"
                ))
            })?;
            let enum_name = match src_ty {
                ResolvedTy::Named { name, .. } => name.clone(),
                other => {
                    return Err(CodegenError::FailClosed(format!(
                        "Instr::EnumTagLoad src local {src_local} type {other:?} is not a \
                         Named enum/event type"
                    )));
                }
            };
            let layout = fn_ctx.machine_layouts.get(&enum_name).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Instr::EnumTagLoad src type `{enum_name}` is not in the machine \
                     layout map — only machine values and their event companions are \
                     supported by Slice 5"
                ))
            })?;
            let (src_slot, _src_slot_ty) =
                fn_ctx.locals.get(src_local).copied().ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "Instr::EnumTagLoad src local {src_local} has no alloca slot"
                    ))
                })?;
            let tag_ptr = fn_ctx
                .builder
                .build_struct_gep(layout.outer_struct, src_slot, 0, "enum_tag_ptr")
                .map_err(|e| CodegenError::Llvm(format!("GEP enum tag: {e:?}")))?;
            let tag_loaded = fn_ctx
                .builder
                .build_load(layout.tag_int_ty, tag_ptr, "enum_tag_load")
                .map_err(|e| CodegenError::Llvm(format!("load enum tag: {e:?}")))?
                .into_int_value();
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            let dest_int_ty = match dest_ty {
                BasicTypeEnum::IntType(t) => t,
                other => {
                    return Err(CodegenError::FailClosed(format!(
                        "Instr::EnumTagLoad dest must be an integer-typed Place; got {other:?}"
                    )));
                }
            };
            // zext the iW tag to the destination integer width. If widths
            // match (rare — only when dest is also iW), `build_int_z_extend`
            // is a no-op upcast that LLVM folds.
            let widened = if dest_int_ty.get_bit_width() == layout.tag_int_ty.get_bit_width() {
                tag_loaded
            } else {
                fn_ctx
                    .builder
                    .build_int_z_extend(tag_loaded, dest_int_ty, "enum_tag_zext")
                    .map_err(|e| CodegenError::Llvm(format!("zext enum tag: {e:?}")))?
            };
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .map_err(|e| CodegenError::Llvm(format!("store enum tag: {e:?}")))?;
        }
        Instr::MachineStateName {
            machine_name,
            src_local,
            dest,
        } => {
            // 1. Load the machine's discriminant tag from its outer struct
            //    field 0 (same shape as `Place::MachineTag` and
            //    `Instr::EnumTagLoad`).
            // 2. Widen the iW tag to i64 for use as a GEP index.
            // 3. GEP into the per-machine `__hew_state_name_table` global
            //    using `[i32 0, i64 tag]`.
            // 4. Load the pointer entry and store it into `dest`.
            let layout = fn_ctx.machine_layouts.get(machine_name).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Instr::MachineStateName references machine `{machine_name}` which is \
                     not in the layout map — register_machine_layouts must populate it"
                ))
            })?;
            let table_global = layout.state_name_table.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Instr::MachineStateName: machine `{machine_name}` has no state-name \
                     table; only state-side machine layouts carry one (event companion \
                     layouts do not)"
                ))
            })?;
            let (src_slot, _src_slot_ty) =
                fn_ctx.locals.get(src_local).copied().ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "Instr::MachineStateName src local {src_local} has no alloca slot"
                    ))
                })?;
            // GEP field 0 (tag) of the outer struct, then load the iW tag.
            let tag_ptr = fn_ctx
                .builder
                .build_struct_gep(layout.outer_struct, src_slot, 0, "machine_tag_ptr")
                .map_err(|e| CodegenError::Llvm(format!("GEP state-name tag: {e:?}")))?;
            let tag_loaded = fn_ctx
                .builder
                .build_load(layout.tag_int_ty, tag_ptr, "state_name_tag")
                .map_err(|e| CodegenError::Llvm(format!("load state-name tag: {e:?}")))?
                .into_int_value();
            let i64_ty = ctx.i64_type();
            let tag_i64 = if layout.tag_int_ty.get_bit_width() == 64 {
                tag_loaded
            } else {
                fn_ctx
                    .builder
                    .build_int_z_extend(tag_loaded, i64_ty, "state_name_tag_zext")
                    .map_err(|e| CodegenError::Llvm(format!("zext state-name tag: {e:?}")))?
            };
            // GEP into `[N x ptr]` table using [i32 0, i64 tag]. The element
            // type is `ptr` and the load yields the state-name pointer.
            let table_ptr = table_global.as_pointer_value();
            // Reconstruct the table type: `[N x ptr]` where N is the
            // declared variant count. `register_machine_layouts` built the
            // global with this exact shape, and `variant_field_tys.len()`
            // is the same N (one entry per declared state).
            let n_variants = u32::try_from(layout.variant_field_tys.len()).unwrap_or(u32::MAX);
            let table_ty = ctx.ptr_type(AddressSpace::default()).array_type(n_variants);
            let i32_zero = ctx.i32_type().const_zero();
            // SAFETY: build_in_bounds_gep with a constant base + tag index;
            // tag is in-range by construction (machine dispatch + HIR
            // exhaustiveness; an out-of-range tag would already have
            // tripped Trap::MachineDispatchUnreachable on a step).
            let entry_ptr = unsafe {
                fn_ctx
                    .builder
                    .build_in_bounds_gep(
                        table_ty,
                        table_ptr,
                        &[i32_zero, tag_i64],
                        "state_name_entry_ptr",
                    )
                    .map_err(|e| CodegenError::Llvm(format!("GEP state-name entry: {e:?}")))?
            };
            let ptr_ty = ctx.ptr_type(AddressSpace::default());
            let name_ptr = fn_ctx
                .builder
                .build_load(ptr_ty, entry_ptr, "state_name_ptr")
                .map_err(|e| CodegenError::Llvm(format!("load state-name ptr: {e:?}")))?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
            if !matches!(dest_ty, BasicTypeEnum::PointerType(_)) {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::MachineStateName dest is not a pointer type: {dest_ty:?}"
                )));
            }
            fn_ctx
                .builder
                .build_store(dest_ptr, name_ptr)
                .map_err(|e| CodegenError::Llvm(format!("store state-name ptr: {e:?}")))?;
        }
    }
    Ok(())
}

/// Resolve the LLVM `StructType` for a record-typed place by inspecting the
/// place's `ResolvedTy::Named { name, .. }` name against the registered
/// record layout map. Fails closed if `ty` is not a `Named` type or if the
/// named type is not in the layout map — both indicate the MIR producer
/// emitted a `RecordInit` / `RecordFieldLoad` against a Place whose type
/// wasn't a registered record, which is an upstream invariant violation.
fn record_struct_for<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    ty: &ResolvedTy,
) -> CodegenResult<StructType<'ctx>> {
    match ty {
        ResolvedTy::Named { name, .. } => {
            fn_ctx.record_layouts.get(name).copied().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "record codegen: type `{name}` reached RecordInit/RecordFieldLoad \
                 but is not in the registered record-layout map; either the MIR \
                 producer emitted the instruction against a non-record type, or \
                 `IrPipeline.record_layouts` was not populated for this module"
                ))
            })
        }
        other => Err(CodegenError::FailClosed(format!(
            "record codegen: expected a Named record type, got {other:?}"
        ))),
    }
}

/// Lower `Instr::RecordInit { ty, fields, dest }` to per-field GEP+store
/// into the record's destination alloca.
///
/// `dest` is a `Place::Local(N)` whose slot was already allocated by
/// `lower_function`'s prologue with the struct type (via `resolve_ty`). The
/// alloca itself IS the destination record value — no second allocation is
/// needed. We GEP into each field offset and store the source value loaded
/// from its place.
///
/// Field source values may be either scalar (loaded with `build_load`) or
/// composite (a nested record, loaded as a struct value). The LLVM
/// `build_load` call uses the field's declared LLVM type from the parent
/// struct's element-types list, so both shapes go through the same path.
///
/// Functional-update (`R { x: 1, ..base }`) is handled by the MIR producer
/// expanding to per-field `RecordFieldLoad` from the base + `RecordInit`
/// with all field-pairs explicit — A-7 sees only the flat store-each-field
/// shape; no `base` parameter is consumed here.
fn lower_record_init(
    fn_ctx: &FnCtx<'_, '_>,
    ty: &ResolvedTy,
    fields: &[(FieldOffset, Place)],
    dest: Place,
) -> CodegenResult<()> {
    let struct_ty = record_struct_for(fn_ctx, ty)?;
    let (dest_ptr, dest_slot_ty) = place_pointer(fn_ctx, dest)?;
    // Sanity: the destination slot must already be allocated as the struct
    // type. If it's a different type, `lower_function`'s alloca prologue
    // did not see the same `ResolvedTy::Named` for this local — a
    // producer/codegen disagreement.
    if dest_slot_ty != BasicTypeEnum::StructType(struct_ty) {
        return Err(CodegenError::FailClosed(format!(
            "RecordInit dest slot type does not match registered struct: \
             slot={dest_slot_ty:?}, struct={struct_ty:?}"
        )));
    }
    let element_tys = struct_ty.get_field_types();
    for (offset, src_place) in fields {
        let idx = offset.0;
        let idx_usize = usize::try_from(idx).map_err(|_| {
            CodegenError::FailClosed(format!(
                "RecordInit field offset {idx} exceeds usize::MAX — impossible"
            ))
        })?;
        let field_ty = *element_tys.get(idx_usize).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "RecordInit field offset {idx} is out of bounds for struct with \
                 {} fields",
                element_tys.len()
            ))
        })?;
        let (src_ptr, src_slot_ty) = place_pointer(fn_ctx, *src_place)?;
        if src_slot_ty != field_ty {
            return Err(CodegenError::FailClosed(format!(
                "RecordInit field {idx} source slot type does not match struct field \
                 type: src={src_slot_ty:?}, field={field_ty:?}"
            )));
        }
        // GEP to the field within the destination struct alloca.
        let field_ptr = fn_ctx
            .builder
            .build_struct_gep(struct_ty, dest_ptr, idx, &format!("field_{idx}_init_ptr"))
            .map_err(|e| CodegenError::Llvm(format!("RecordInit struct_gep field {idx}: {e:?}")))?;
        // Load the source value (scalar or struct), store it into the field.
        let src_val = fn_ctx
            .builder
            .build_load(field_ty, src_ptr, &format!("field_{idx}_init_src"))
            .map_err(|e| CodegenError::Llvm(format!("RecordInit field {idx} load: {e:?}")))?;
        fn_ctx
            .builder
            .build_store(field_ptr, src_val)
            .map_err(|e| CodegenError::Llvm(format!("RecordInit field {idx} store: {e:?}")))?;
    }
    Ok(())
}

/// Lower `Instr::RecordFieldLoad { record, field_offset, dest }` to a
/// GEP+load on the record's alloca, storing the loaded field value into
/// the dest place.
///
/// The record's `Place` must reference a struct-typed local slot (allocated
/// by `lower_function` from a `ResolvedTy::Named` record-typed local). We
/// recover the struct type from the slot's LLVM type, GEP to the field, and
/// load using the field's declared element type from the parent struct.
fn lower_record_field_load(
    fn_ctx: &FnCtx<'_, '_>,
    record: Place,
    field_offset: FieldOffset,
    dest: Place,
) -> CodegenResult<()> {
    let (record_ptr, record_slot_ty) = place_pointer(fn_ctx, record)?;
    let struct_ty = match record_slot_ty {
        BasicTypeEnum::StructType(st) => st,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "RecordFieldLoad record place has non-struct slot type: {other:?}"
            )));
        }
    };
    let idx = field_offset.0;
    let idx_usize = usize::try_from(idx).map_err(|_| {
        CodegenError::FailClosed(format!(
            "RecordFieldLoad field offset {idx} exceeds usize::MAX — impossible"
        ))
    })?;
    let element_tys = struct_ty.get_field_types();
    let field_ty = *element_tys.get(idx_usize).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "RecordFieldLoad field offset {idx} is out of bounds for struct with \
             {} fields",
            element_tys.len()
        ))
    })?;
    let (dest_ptr, dest_slot_ty) = place_pointer(fn_ctx, dest)?;
    if dest_slot_ty != field_ty {
        return Err(CodegenError::FailClosed(format!(
            "RecordFieldLoad dest slot type does not match field type: \
             dest={dest_slot_ty:?}, field={field_ty:?}"
        )));
    }
    let field_ptr = fn_ctx
        .builder
        .build_struct_gep(struct_ty, record_ptr, idx, &format!("field_{idx}_load_ptr"))
        .map_err(|e| {
            CodegenError::Llvm(format!("RecordFieldLoad struct_gep field {idx}: {e:?}"))
        })?;
    let field_val = fn_ctx
        .builder
        .build_load(field_ty, field_ptr, &format!("field_{idx}_load"))
        .map_err(|e| CodegenError::Llvm(format!("RecordFieldLoad field {idx} load: {e:?}")))?;
    fn_ctx
        .builder
        .build_store(dest_ptr, field_val)
        .map_err(|e| CodegenError::Llvm(format!("RecordFieldLoad field {idx} store: {e:?}")))?;
    Ok(())
}

fn current_actor_state_ptr<'ctx>(fn_ctx: &FnCtx<'_, 'ctx>) -> CodegenResult<PointerValue<'ctx>> {
    let ctx_ptr = fn_ctx.execution_context.ok_or_else(|| {
        CodegenError::FailClosed("actor state access requires an execution context".into())
    })?;
    if !fn_ctx.execution_context_is_actor_handler {
        return Err(CodegenError::FailClosed(
            "actor state access is only legal in ActorHandler functions".into(),
        ));
    }
    let i64_ty = fn_ctx.ctx.i64_type();
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let actor_ptr_slot = unsafe {
        fn_ctx
            .builder
            .build_gep(
                fn_ctx.ctx.i8_type(),
                ctx_ptr,
                &[i64_ty.const_int(HEW_CTX_OFFSET_ACTOR as u64, false)],
                "ctx_actor_ptr_slot",
            )
            .map_err(|e| CodegenError::Llvm(format!("actor state ctx actor gep: {e:?}")))?
    };
    let actor_ptr = fn_ctx
        .builder
        .build_load(ptr_ty, actor_ptr_slot, "ctx_actor_ptr")
        .map_err(|e| CodegenError::Llvm(format!("actor state actor load: {e:?}")))?
        .into_pointer_value();
    let state_slot = unsafe {
        fn_ctx
            .builder
            .build_gep(
                fn_ctx.ctx.i8_type(),
                actor_ptr,
                &[i64_ty.const_int(HEW_ACTOR_OFFSET_STATE as u64, false)],
                "actor_state_slot",
            )
            .map_err(|e| CodegenError::Llvm(format!("actor state gep: {e:?}")))?
    };
    Ok(fn_ctx
        .builder
        .build_load(ptr_ty, state_slot, "actor_state_ptr")
        .map_err(|e| CodegenError::Llvm(format!("actor state ptr load: {e:?}")))?
        .into_pointer_value())
}

fn lower_actor_state_field_load(
    fn_ctx: &FnCtx<'_, '_>,
    field_offset: FieldOffset,
    dest: Place,
) -> CodegenResult<()> {
    let state_ty = fn_ctx.actor_state_ty.ok_or_else(|| {
        CodegenError::FailClosed("ActorStateFieldLoad has no registered actor state type".into())
    })?;
    let state_ptr = current_actor_state_ptr(fn_ctx)?;
    let idx = field_offset.0;
    let element_tys = state_ty.get_field_types();
    let field_ty = *element_tys.get(idx as usize).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "ActorStateFieldLoad field offset {idx} is out of bounds for state with {} fields",
            element_tys.len()
        ))
    })?;
    let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest)?;
    if dest_ty != field_ty {
        return Err(CodegenError::FailClosed(format!(
            "ActorStateFieldLoad dest type mismatch: dest={dest_ty:?}, field={field_ty:?}"
        )));
    }
    let field_ptr = fn_ctx
        .builder
        .build_struct_gep(
            state_ty,
            state_ptr,
            idx,
            &format!("actor_state_field_{idx}_ptr"),
        )
        .map_err(|e| CodegenError::Llvm(format!("ActorStateFieldLoad gep: {e:?}")))?;
    let field_val = fn_ctx
        .builder
        .build_load(field_ty, field_ptr, &format!("actor_state_field_{idx}"))
        .map_err(|e| CodegenError::Llvm(format!("ActorStateFieldLoad load: {e:?}")))?;
    fn_ctx
        .builder
        .build_store(dest_ptr, field_val)
        .map_err(|e| CodegenError::Llvm(format!("ActorStateFieldLoad store: {e:?}")))?;
    Ok(())
}

fn lower_actor_state_field_store(
    fn_ctx: &FnCtx<'_, '_>,
    field_offset: FieldOffset,
    src: Place,
) -> CodegenResult<()> {
    let state_ty = fn_ctx.actor_state_ty.ok_or_else(|| {
        CodegenError::FailClosed("ActorStateFieldStore has no registered actor state type".into())
    })?;
    let state_ptr = current_actor_state_ptr(fn_ctx)?;
    let idx = field_offset.0;
    let element_tys = state_ty.get_field_types();
    let field_ty = *element_tys.get(idx as usize).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "ActorStateFieldStore field offset {idx} is out of bounds for state with {} fields",
            element_tys.len()
        ))
    })?;
    let (src_ptr, src_ty) = place_pointer(fn_ctx, src)?;
    if src_ty != field_ty {
        return Err(CodegenError::FailClosed(format!(
            "ActorStateFieldStore src type mismatch: src={src_ty:?}, field={field_ty:?}"
        )));
    }
    let field_ptr = fn_ctx
        .builder
        .build_struct_gep(
            state_ty,
            state_ptr,
            idx,
            &format!("actor_state_field_{idx}_ptr"),
        )
        .map_err(|e| CodegenError::Llvm(format!("ActorStateFieldStore gep: {e:?}")))?;
    let src_val = fn_ctx
        .builder
        .build_load(field_ty, src_ptr, &format!("actor_state_field_{idx}_src"))
        .map_err(|e| CodegenError::Llvm(format!("ActorStateFieldStore load: {e:?}")))?;
    fn_ctx
        .builder
        .build_store(field_ptr, src_val)
        .map_err(|e| CodegenError::Llvm(format!("ActorStateFieldStore store: {e:?}")))?;
    Ok(())
}

fn lower_make_closure(
    fn_ctx: &FnCtx<'_, '_>,
    fn_symbol: &str,
    env: Place,
    dest: Place,
) -> CodegenResult<()> {
    let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest)?;
    let struct_ty = match dest_ty {
        BasicTypeEnum::StructType(st) if st.count_fields() == 2 => st,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "MakeClosure dest must be the two-pointer closure pair, got {other:?}"
            )));
        }
    };
    let shim = fn_ctx
        .fn_symbols
        .get(fn_symbol)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "MakeClosure references missing closure invoke shim `{fn_symbol}`"
            ))
        })?
        .real(fn_symbol, "MakeClosure")?
        .0;
    let (env_ptr, _) = place_pointer(fn_ctx, env)?;
    let fn_field = fn_ctx
        .builder
        .build_struct_gep(struct_ty, dest_ptr, 0, "closure_fn_ptr")
        .map_err(|e| CodegenError::Llvm(format!("MakeClosure fn gep: {e:?}")))?;
    let env_field = fn_ctx
        .builder
        .build_struct_gep(struct_ty, dest_ptr, 1, "closure_env_ptr")
        .map_err(|e| CodegenError::Llvm(format!("MakeClosure env gep: {e:?}")))?;
    fn_ctx
        .builder
        .build_store(fn_field, shim.as_global_value().as_pointer_value())
        .map_err(|e| CodegenError::Llvm(format!("MakeClosure fn store: {e:?}")))?;
    fn_ctx
        .builder
        .build_store(env_field, env_ptr)
        .map_err(|e| CodegenError::Llvm(format!("MakeClosure env store: {e:?}")))?;
    Ok(())
}

fn context_field_matches_dest<'ctx>(
    ctx: &'ctx Context,
    offset: usize,
    dest_ty: BasicTypeEnum<'ctx>,
) -> bool {
    let ptr_ok = matches!(dest_ty, BasicTypeEnum::PointerType(_));
    let int_bits = match dest_ty {
        BasicTypeEnum::IntType(int_ty) => Some(int_ty.get_bit_width()),
        _ => None,
    };
    match offset {
        HEW_CTX_OFFSET_ACTOR
        | HEW_CTX_OFFSET_PARENT_SUPERVISOR
        | HEW_CTX_OFFSET_CANCEL_TOKEN
        | HEW_CTX_OFFSET_TASK_SCOPE
        | HEW_CTX_OFFSET_ARENA
        | HEW_CTX_OFFSET_PARTITION_POLICY
        | HEW_CTX_OFFSET_PREV_CONTEXT
        | HEW_CTX_OFFSET_LOCK_SEAT => ptr_ok,
        HEW_CTX_OFFSET_ACTOR_ID | HEW_CTX_OFFSET_TRACE_SPAN => {
            int_bits == Some(ctx.i64_type().get_bit_width())
        }
        HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX | HEW_CTX_OFFSET_FLAGS => {
            int_bits == Some(ctx.i32_type().get_bit_width())
        }
        _ => false,
    }
}

fn lower_context_field(fn_ctx: &FnCtx<'_, '_>, dest: Place, offset: usize) -> CodegenResult<()> {
    let ctx_ptr = fn_ctx.execution_context.ok_or_else(|| {
        CodegenError::FailClosed("ContextField requires an actor-handler execution context".into())
    })?;
    if !fn_ctx.execution_context_is_actor_handler
        && matches!(
            offset,
            HEW_CTX_OFFSET_ACTOR
                | HEW_CTX_OFFSET_ACTOR_ID
                | HEW_CTX_OFFSET_ARENA
                | HEW_CTX_OFFSET_LOCK_SEAT
        )
    {
        return Err(CodegenError::FailClosed(format!(
            "ContextField offset {offset} requires an actor-owned execution context"
        )));
    }
    let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest)?;
    if !context_field_matches_dest(fn_ctx.ctx, offset, dest_ty) {
        return Err(CodegenError::FailClosed(format!(
            "ContextField offset {offset} is not valid for destination type {dest_ty:?}"
        )));
    }
    let offset_u64 = u64::try_from(offset).map_err(|_| {
        CodegenError::FailClosed(format!("ContextField offset {offset} exceeds u64::MAX"))
    })?;
    let offset_val = fn_ctx.ctx.i64_type().const_int(offset_u64, false);
    let field_ptr = unsafe {
        fn_ctx
            .builder
            .build_gep(
                fn_ctx.ctx.i8_type(),
                ctx_ptr,
                &[offset_val],
                &format!("ctx_field_{offset}_ptr"),
            )
            .map_err(|e| CodegenError::Llvm(format!("ContextField gep: {e:?}")))?
    };
    let field_val = fn_ctx
        .builder
        .build_load(dest_ty, field_ptr, &format!("ctx_field_{offset}_load"))
        .map_err(|e| CodegenError::Llvm(format!("ContextField load: {e:?}")))?;
    fn_ctx
        .builder
        .build_store(dest_ptr, field_val)
        .map_err(|e| CodegenError::Llvm(format!("ContextField store: {e:?}")))?;
    Ok(())
}

fn lower_closure_env_field_load(
    fn_ctx: &FnCtx<'_, '_>,
    env: Place,
    env_ty: &ResolvedTy,
    field_offset: FieldOffset,
    dest: Place,
) -> CodegenResult<()> {
    let env_struct = record_struct_for(fn_ctx, env_ty)?;
    let (env_slot, env_slot_ty) = place_pointer(fn_ctx, env)?;
    if !matches!(env_slot_ty, BasicTypeEnum::PointerType(_)) {
        return Err(CodegenError::FailClosed(format!(
            "ClosureEnvFieldLoad env slot must hold a pointer, got {env_slot_ty:?}"
        )));
    }
    let env_ptr = fn_ctx
        .builder
        .build_load(env_slot_ty, env_slot, "closure_env_ptr_load")
        .map_err(|e| CodegenError::Llvm(format!("ClosureEnvFieldLoad env load: {e:?}")))?
        .into_pointer_value();
    let idx = field_offset.0;
    let idx_usize = usize::try_from(idx).map_err(|_| {
        CodegenError::FailClosed(format!(
            "ClosureEnvFieldLoad field offset {idx} exceeds usize::MAX — impossible"
        ))
    })?;
    let field_ty = *env_struct.get_field_types().get(idx_usize).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "ClosureEnvFieldLoad field offset {idx} out of bounds"
        ))
    })?;
    let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest)?;
    if dest_ty != field_ty {
        return Err(CodegenError::FailClosed(format!(
            "ClosureEnvFieldLoad dest type {dest_ty:?} does not match field type {field_ty:?}"
        )));
    }
    let field_ptr = fn_ctx
        .builder
        .build_struct_gep(env_struct, env_ptr, idx, "closure_capture_ptr")
        .map_err(|e| CodegenError::Llvm(format!("ClosureEnvFieldLoad gep: {e:?}")))?;
    let value = fn_ctx
        .builder
        .build_load(field_ty, field_ptr, "closure_capture_load")
        .map_err(|e| CodegenError::Llvm(format!("ClosureEnvFieldLoad field load: {e:?}")))?;
    fn_ctx
        .builder
        .build_store(dest_ptr, value)
        .map_err(|e| CodegenError::Llvm(format!("ClosureEnvFieldLoad store: {e:?}")))?;
    Ok(())
}

fn metadata_type_from_basic<'ctx>(ty: BasicTypeEnum<'ctx>) -> BasicMetadataTypeEnum<'ctx> {
    match ty {
        BasicTypeEnum::ArrayType(t) => t.into(),
        BasicTypeEnum::FloatType(t) => t.into(),
        BasicTypeEnum::IntType(t) => t.into(),
        BasicTypeEnum::PointerType(t) => t.into(),
        BasicTypeEnum::StructType(t) => t.into(),
        BasicTypeEnum::VectorType(t) => t.into(),
        BasicTypeEnum::ScalableVectorType(t) => t.into(),
    }
}

fn metadata_value_from_basic<'ctx>(
    value: BasicValueEnum<'ctx>,
) -> inkwell::values::BasicMetadataValueEnum<'ctx> {
    match value {
        BasicValueEnum::IntValue(v) => v.into(),
        BasicValueEnum::FloatValue(v) => v.into(),
        BasicValueEnum::PointerValue(v) => v.into(),
        BasicValueEnum::StructValue(v) => v.into(),
        BasicValueEnum::ArrayValue(v) => v.into(),
        BasicValueEnum::VectorValue(v) => v.into(),
        BasicValueEnum::ScalableVectorValue(v) => v.into(),
    }
}

fn fn_type_from_return<'ctx>(
    ret: BasicTypeEnum<'ctx>,
    params: &[BasicMetadataTypeEnum<'ctx>],
) -> inkwell::types::FunctionType<'ctx> {
    match ret {
        BasicTypeEnum::ArrayType(t) => t.fn_type(params, false),
        BasicTypeEnum::FloatType(t) => t.fn_type(params, false),
        BasicTypeEnum::IntType(t) => t.fn_type(params, false),
        BasicTypeEnum::PointerType(t) => t.fn_type(params, false),
        BasicTypeEnum::StructType(t) => t.fn_type(params, false),
        BasicTypeEnum::VectorType(t) => t.fn_type(params, false),
        BasicTypeEnum::ScalableVectorType(t) => t.fn_type(params, false),
    }
}

fn lower_call_closure(
    fn_ctx: &FnCtx<'_, '_>,
    callee: Place,
    args: &[Place],
    ret_ty: &ResolvedTy,
    dest: Option<Place>,
) -> CodegenResult<()> {
    let ctx_ptr = fn_ctx.execution_context.ok_or_else(|| {
        CodegenError::FailClosed("CallClosure requires an execution context".into())
    })?;
    let (callee_ptr, callee_ty) = place_pointer(fn_ctx, callee)?;
    let pair_ty = match callee_ty {
        BasicTypeEnum::StructType(st) if st.count_fields() == 2 => st,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "CallClosure callee must be the two-pointer closure pair, got {other:?}"
            )));
        }
    };
    let pair = fn_ctx
        .builder
        .build_load(pair_ty, callee_ptr, "closure_pair_load")
        .map_err(|e| CodegenError::Llvm(format!("CallClosure pair load: {e:?}")))?
        .into_struct_value();
    let fn_ptr = fn_ctx
        .builder
        .build_extract_value(pair, 0, "closure_fn_extract")
        .map_err(|e| CodegenError::Llvm(format!("CallClosure fn extract: {e:?}")))?
        .into_pointer_value();
    let env_ptr = fn_ctx
        .builder
        .build_extract_value(pair, 1, "closure_env_extract")
        .map_err(|e| CodegenError::Llvm(format!("CallClosure env extract: {e:?}")))?
        .into_pointer_value();

    let mut param_tys: Vec<BasicMetadataTypeEnum> =
        Vec::with_capacity(args.len().saturating_add(2));
    param_tys.push(fn_ctx.ctx.ptr_type(AddressSpace::default()).into());
    param_tys.push(fn_ctx.ctx.ptr_type(AddressSpace::default()).into());
    let mut arg_vals: Vec<inkwell::values::BasicMetadataValueEnum> =
        Vec::with_capacity(args.len().saturating_add(2));
    arg_vals.push(ctx_ptr.into());
    arg_vals.push(env_ptr.into());
    for arg in args {
        let (arg_ptr, arg_ty) = place_pointer(fn_ctx, *arg)?;
        param_tys.push(metadata_type_from_basic(arg_ty));
        let loaded = fn_ctx
            .builder
            .build_load(arg_ty, arg_ptr, "closure_call_arg")
            .map_err(|e| CodegenError::Llvm(format!("CallClosure arg load: {e:?}")))?;
        arg_vals.push(metadata_value_from_basic(loaded));
    }

    let ret_llvm = resolve_ty(fn_ctx.ctx, ret_ty, fn_ctx.record_layouts)?;
    let fn_ty = fn_type_from_return(ret_llvm, &param_tys);
    let call = fn_ctx
        .builder
        .build_indirect_call(fn_ty, fn_ptr, &arg_vals, "closure_call_result")
        .map_err(|e| CodegenError::Llvm(format!("CallClosure indirect call: {e:?}")))?;
    if let Some(dest_place) = dest {
        let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
        if dest_ty != ret_llvm {
            return Err(CodegenError::FailClosed(format!(
                "CallClosure dest type {dest_ty:?} does not match return type {ret_llvm:?}"
            )));
        }
        let ret_val = call.try_as_basic_value().basic().ok_or_else(|| {
            CodegenError::FailClosed("CallClosure produced void for value-return call".into())
        })?;
        fn_ctx
            .builder
            .build_store(dest_ptr, ret_val)
            .map_err(|e| CodegenError::Llvm(format!("CallClosure result store: {e:?}")))?;
    }
    Ok(())
}

/// Lower `Instr::TupleFieldLoad { tuple, field_index, dest }` to a GEP+load on
/// the tuple's struct alloca, storing the loaded element value into the dest place.
///
/// Tuples are laid out as anonymous LLVM struct types (positional field order,
/// `packed = false`). The GEP+load pattern is identical to `RecordFieldLoad`;
/// the only difference is that the struct type is recovered from the slot's
/// LLVM type rather than from the named record-layout map, since tuple types are
/// anonymous and carry no layout registration entry.
///
/// Fail-closed when:
/// - The tuple place does not reference a struct-typed alloca (i.e. the MIR
///   producer allocated the wrong type for the local — a bug in the lowerer).
/// - `field_index` is out of bounds for the struct's field count.
/// - The dest slot type does not match the element type (type-mismatch in MIR).
fn lower_tuple_field_load(
    fn_ctx: &FnCtx<'_, '_>,
    tuple: Place,
    field_index: u32,
    dest: Place,
) -> CodegenResult<()> {
    let (tuple_ptr, tuple_slot_ty) = place_pointer(fn_ctx, tuple)?;
    let struct_ty = match tuple_slot_ty {
        BasicTypeEnum::StructType(st) => st,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "TupleFieldLoad tuple place has non-struct slot type: {other:?}"
            )));
        }
    };
    let idx = field_index;
    let idx_usize = usize::try_from(idx).map_err(|_| {
        CodegenError::FailClosed(format!(
            "TupleFieldLoad field index {idx} exceeds usize::MAX — impossible"
        ))
    })?;
    let element_tys = struct_ty.get_field_types();
    let field_ty = *element_tys.get(idx_usize).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "TupleFieldLoad field index {idx} is out of bounds for tuple with \
             {} elements",
            element_tys.len()
        ))
    })?;
    let (dest_ptr, dest_slot_ty) = place_pointer(fn_ctx, dest)?;
    if dest_slot_ty != field_ty {
        return Err(CodegenError::FailClosed(format!(
            "TupleFieldLoad dest slot type does not match element type: \
             dest={dest_slot_ty:?}, element={field_ty:?}"
        )));
    }
    let field_ptr = fn_ctx
        .builder
        .build_struct_gep(struct_ty, tuple_ptr, idx, &format!("tuple_{idx}_load_ptr"))
        .map_err(|e| {
            CodegenError::Llvm(format!("TupleFieldLoad struct_gep element {idx}: {e:?}"))
        })?;
    let field_val = fn_ctx
        .builder
        .build_load(field_ty, field_ptr, &format!("tuple_{idx}_load"))
        .map_err(|e| CodegenError::Llvm(format!("TupleFieldLoad element {idx} load: {e:?}")))?;
    fn_ctx
        .builder
        .build_store(dest_ptr, field_val)
        .map_err(|e| CodegenError::Llvm(format!("TupleFieldLoad element {idx} store: {e:?}")))?;
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
        // Actor link/monitor builtins.
        //
        // SHIM(B3→Cluster2): `link()` returns `Result<(), LinkError>` and
        // `monitor()` returns `MonitorRef { ref_id: i64 }`. Both require
        // composite-type construction (enum variant / struct literal) in the
        // LLVM IR, which the Cluster 1 spine does not yet support.
        //
        // WHY this shim exists: the producer (MIR lower.rs) calls
        // `lower_runtime_call("hew_actor_link", ...)` via
        // `user_name_to_c_symbol("link")`.  Codegen must not silently discard
        // the call, but also cannot construct the composite return today.
        // The FFI call is emitted; return wrapping is deferred.
        //
        // WHEN obsolete: when the Cluster 2 spine lands enum-variant and
        // struct-literal construction in `hew-codegen-rs`.  At that point,
        // `hew_actor_link` wraps the void return as `Ok(())`, and
        // `hew_actor_monitor` wraps the u64 ref_id as `MonitorRef { ref_id }`.
        //
        // WHAT the real solution looks like:
        //   hew_actor_link:   call void → alloca Result<(),LinkError> → store
        //                     discriminant 0 (Ok), zero-length payload.
        //   hew_actor_monitor: call i64 → alloca MonitorRef → store ref_id.
        "hew_actor_link" => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_actor_link): expected 2 args \
                     (parent, child), got {}",
                    args.len()
                )));
            }
            // arg0: parent actor handle (opaque ptr).
            let parent = load_duplex_handle(fn_ctx, args[0], "link_parent")?;
            // arg1: child actor handle (opaque ptr).
            let child = load_duplex_handle(fn_ctx, args[1], "link_child")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let llvm_args: [BasicMetadataValueEnum; 2] = [parent.into(), child.into()];
            fn_ctx
                .builder
                .build_call(fv, &llvm_args, "hew_actor_link_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_actor_link call: {e:?}")))?;
            // SHIM(B3→Cluster2): `link()` returns Result<(), LinkError>.
            // Composite-type construction is not yet available in the Cluster 1
            // spine. Until the Cluster 2 spine lands, producers must not wire a
            // dest slot for this call — fail-closed if they do.
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_actor_link Result<(),LinkError> return synthesis requires \
                     Cluster 2 composite-type spine; producer must not supply \
                     dest={d:?} until that lands"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        "hew_actor_monitor" => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_actor_monitor): expected 2 args \
                     (watcher, target), got {}",
                    args.len()
                )));
            }
            // arg0: watcher actor handle (opaque ptr).
            let watcher = load_duplex_handle(fn_ctx, args[0], "monitor_watcher")?;
            // arg1: target actor handle (opaque ptr).
            let target = load_duplex_handle(fn_ctx, args[1], "monitor_target")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let llvm_args: [BasicMetadataValueEnum; 2] = [watcher.into(), target.into()];
            fn_ctx
                .builder
                .build_call(fv, &llvm_args, "hew_actor_monitor_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_actor_monitor call: {e:?}")))?;
            // SHIM(B3→Cluster2): `monitor()` returns MonitorRef { ref_id: i64 }.
            // Struct-literal construction requires the Cluster 2 spine.
            // Producers must not wire a dest until that lands.
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_actor_monitor MonitorRef{{ref_id}} struct synthesis requires \
                     Cluster 2 composite-type spine; producer must not supply \
                     dest={d:?} until that lands"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_supervisor_stop(sup: *mut HewSupervisor) -> void
        // (`hew-runtime/src/supervisor.rs:1944`). Graceful shutdown: void return.
        // The `supervisor_stop(sup)` builtin passes a single supervisor handle;
        // the producer always supplies `dest: None`.
        "hew_supervisor_stop" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_supervisor_stop): expected 1 arg \
                     (sup), got {}",
                    args.len()
                )));
            }
            let sup_ptr = load_duplex_handle(fn_ctx, args[0], "supervisor_stop_arg0")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            fn_ctx
                .builder
                .build_call(fv, &[sup_ptr.into()], "hew_supervisor_stop_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_supervisor_stop call: {e:?}")))?;
            // Void return — producer supplies dest: None; fail-closed if not.
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_supervisor_stop is void; producer must not supply \
                     dest={d:?}"
                )));
            }
            let _ = (i32_ty, i64_ty);
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
        // ── Vec<T> indexing (C-2) ────────────────────────────────────────
        //
        // hew_vec_len(v: *mut HewVec) -> i64
        // args[0]: Place::Local(N) holding a `*mut HewVec` pointer — load the
        // ptr from the alloca. dest: Place::Local(M) of type i64 — store result.
        "hew_vec_len" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_vec_len): expected 1 arg (vec), got {}",
                    args.len()
                )));
            }
            let vec_ptr = load_duplex_handle(fn_ctx, args[0], "hew_vec_len arg0")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[vec_ptr.into()], "hew_vec_len_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_vec_len call: {e:?}")))?;
            let len_val = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_vec_len returned void".into()))?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_len: producer must supply a dest place for the length".into(),
                )
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, len_val)
                .map_err(|e| CodegenError::Llvm(format!("hew_vec_len store: {e:?}")))?;
            let _ = (i32_ty, ptr_ty);
        }
        // hew_vec_get_i32(v: *mut HewVec, index: i64) -> i32
        // hew_vec_get_i64(v: *mut HewVec, index: i64) -> i64
        // hew_vec_get_f64(v: *mut HewVec, index: i64) -> f64
        // hew_vec_get_ptr(v: *mut HewVec, index: i64) -> *mut c_void
        // hew_vec_get_str(v: *mut HewVec, index: i64) -> *const c_char
        //
        // All five share the same ABI shape: load vec ptr from arg0, load
        // index i64 from arg1, call, store result into dest. The result type
        // differs per variant and is encoded in the function return type from
        // `intern_runtime_decl`.
        "hew_vec_get_i32" | "hew_vec_get_i64" | "hew_vec_get_f64" | "hew_vec_get_ptr"
        | "hew_vec_get_str" => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 2 args (vec, index), got {}",
                    args.len()
                )));
            }
            // arg0: Vec pointer. `load_duplex_handle` loads a `ptr`-typed
            // value from any `ptr`-alloca — correct for Vec handles too.
            let vec_ptr = load_duplex_handle(fn_ctx, args[0], &format!("{symbol} arg0"))?;
            // arg1: index as i64.
            let index_val = load_int_arg(fn_ctx, args[1], i64_ty, &format!("{symbol} arg1"))?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), index_val.into()],
                    &format!("{symbol}_call"),
                )
                .map_err(|e| CodegenError::Llvm(format!("{symbol} call: {e:?}")))?;
            let result_val = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed(format!("{symbol} returned void")))?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(format!("{symbol}: producer must supply a dest place"))
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result_val)
                .map_err(|e| CodegenError::Llvm(format!("{symbol} store: {e:?}")))?;
            let _ = (i32_ty, ptr_ty);
        }
        // ── Vec<T> range-slice (C-3) ──────────────────────────────────────
        //
        // hew_vec_slice_range_{i32,i64,f64,ptr,str}(v: *mut HewVec,
        //   start: i64, end: i64) -> *mut HewVec
        //
        // All five share the same ABI shape: load vec ptr from arg0, load
        // start/end i64 from args 1..2, call, store the returned `*mut
        // HewVec` into dest (a ptr-typed Vec<T> alloca). The element type
        // is encoded in the suffix and selected by the MIR producer.
        "hew_vec_slice_range_i32"
        | "hew_vec_slice_range_i64"
        | "hew_vec_slice_range_f64"
        | "hew_vec_slice_range_ptr"
        | "hew_vec_slice_range_str" => {
            if args.len() != 3 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 3 args (vec, start, end), got {}",
                    args.len()
                )));
            }
            let vec_ptr = load_duplex_handle(fn_ctx, args[0], &format!("{symbol} arg0"))?;
            let start_val = load_int_arg(fn_ctx, args[1], i64_ty, &format!("{symbol} arg1"))?;
            let end_val = load_int_arg(fn_ctx, args[2], i64_ty, &format!("{symbol} arg2"))?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), start_val.into(), end_val.into()],
                    &format!("{symbol}_call"),
                )
                .map_err(|e| CodegenError::Llvm(format!("{symbol} call: {e:?}")))?;
            let result_val = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed(format!("{symbol} returned void")))?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(format!("{symbol}: producer must supply a dest place"))
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result_val)
                .map_err(|e| CodegenError::Llvm(format!("{symbol} store: {e:?}")))?;
            let _ = (i32_ty, ptr_ty);
        }
        // ── scope{}/spawn/await task ABI (Phase 2, inventory rows 2/3/4) ──────
        //
        // hew_scope_spawn(scope: *mut HewScope, actor: *mut c_void) -> i32
        // args[0]: scope ptr. args[1]: actor ptr (opaque c_void).
        // Destination: None — the i32 return is a runtime-internal signal.
        "hew_scope_spawn" => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_scope_spawn): expected 2 args \
                     (scope, actor), got {}",
                    args.len()
                )));
            }
            let scope_ptr = load_duplex_handle(fn_ctx, args[0], "hew_scope_spawn arg0")?;
            let actor_ptr = load_duplex_handle(fn_ctx, args[1], "hew_scope_spawn arg1")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[scope_ptr.into(), actor_ptr.into()],
                    "hew_scope_spawn_call",
                )
                .map_err(|e| CodegenError::Llvm(format!("hew_scope_spawn call: {e:?}")))?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_scope_spawn i32 return is runtime-internal; \
                     producer must not supply dest={d:?}"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_task_new() -> *mut HewTask
        // No args. dest: Place holding the task pointer.
        "hew_task_new" => {
            if !args.is_empty() {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_new): expected 0 args, got {}",
                    args.len()
                )));
            }
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[], "hew_task_new_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_task_new call: {e:?}")))?;
            let task_ptr = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_task_new returned void".into()))?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_task_new: producer must supply a dest place for the task ptr".into(),
                )
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, task_ptr)
                .map_err(|e| CodegenError::Llvm(format!("hew_task_new store: {e:?}")))?;
            let _ = (i32_ty, ptr_ty);
        }
        "hew_task_scope_new" => {
            if !args.is_empty() {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_scope_new): expected 0 args, got {}",
                    args.len()
                )));
            }
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[], "hew_task_scope_new_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_task_scope_new call: {e:?}")))?;
            let scope_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_task_scope_new returned void".into())
            })?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed("hew_task_scope_new requires a dest".into())
            })?;
            let (dest_ptr, _) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, scope_ptr)
                .map_err(|e| CodegenError::Llvm(format!("hew_task_scope_new store: {e:?}")))?;
            let _ = (i32_ty, ptr_ty);
        }
        "hew_task_scope_set_current" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_scope_set_current): expected 1 arg, got {}",
                    args.len()
                )));
            }
            let scope_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_scope_set_current arg0")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[scope_ptr.into()], "hew_task_scope_set_current_call")
                .map_err(|e| {
                    CodegenError::Llvm(format!("hew_task_scope_set_current call: {e:?}"))
                })?;
            if let Some(dest_place) = dest {
                let prev = call.try_as_basic_value().basic().ok_or_else(|| {
                    CodegenError::FailClosed("hew_task_scope_set_current returned void".into())
                })?;
                let (dest_ptr, _) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx.builder.build_store(dest_ptr, prev).map_err(|e| {
                    CodegenError::Llvm(format!("hew_task_scope_set_current store: {e:?}"))
                })?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        "hew_task_scope_destroy" | "hew_task_scope_join_all" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 1 arg, got {}",
                    args.len()
                )));
            }
            let scope_ptr = load_duplex_handle(fn_ctx, args[0], symbol)?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            fn_ctx
                .builder
                .build_call(fv, &[scope_ptr.into()], &format!("{symbol}_call"))
                .map_err(|e| CodegenError::Llvm(format!("{symbol} call: {e:?}")))?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "{symbol} returns void; producer must not supply dest={d:?}"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        "hew_task_scope_spawn" => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_scope_spawn): expected 2 args, got {}",
                    args.len()
                )));
            }
            let scope_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_scope_spawn arg0")?;
            let task_ptr = load_duplex_handle(fn_ctx, args[1], "hew_task_scope_spawn arg1")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[scope_ptr.into(), task_ptr.into()],
                    "hew_task_scope_spawn_call",
                )
                .map_err(|e| CodegenError::Llvm(format!("hew_task_scope_spawn call: {e:?}")))?;
            let _ = (i32_ty, ptr_ty);
        }
        "hew_task_scope_cancel_after_ns" => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_scope_cancel_after_ns): expected 2 args, got {}",
                    args.len()
                )));
            }
            let scope_ptr =
                load_duplex_handle(fn_ctx, args[0], "hew_task_scope_cancel_after_ns arg0")?;
            let nanos = load_int_arg(
                fn_ctx,
                args[1],
                i64_ty,
                "hew_task_scope_cancel_after_ns duration",
            )?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[scope_ptr.into(), nanos.into()],
                    "hew_task_scope_cancel_after_ns_call",
                )
                .map_err(|e| {
                    CodegenError::Llvm(format!("hew_task_scope_cancel_after_ns call: {e:?}"))
                })?;
            let _ = (i32_ty, ptr_ty);
        }
        // hew_task_spawn_thread(task: *mut HewTask, task_fn: TaskFn) -> void
        // args[0]: task ptr. args[1]: function pointer (ptr-typed in opaque-ptr mode).
        //
        // SHIM(row3→producer): the upcoming MIR producer for `spawn fn(...)` has
        // not landed yet (inventory row 3). The Place shape for a function-pointer
        // argument is an open design question — no existing arm loads a fn-ptr from
        // a Place. When the producer lands it must pick one of:
        //   (a) Place::Local(N) of a ptr-typed alloca storing the fn ptr, using
        //       `load_duplex_handle` (which loads any ptr from a ptr alloca).
        //   (b) A new Place variant specifically for function pointers.
        // Emitting a fail-closed arm now locks in the allowlist guard without
        // committing to the wrong ABI shape prematurely. WHEN-OBSOLETE: when the
        // `spawn fn(...)` MIR producer lands and picks a Place convention; replace
        // this shim with a real two-arg call matching that convention. WHAT: load
        // task ptr from args[0], load fn-ptr from args[1] using the chosen Place
        // shape, call void hew_task_spawn_thread(task, fn_ptr).
        "hew_task_spawn_thread" => {
            return Err(CodegenError::FailClosed(
                "Instr::CallRuntimeAbi(hew_task_spawn_thread): no MIR producer for \
                 spawn fn(...) has landed yet (inventory row 3). The fn-ptr Place \
                 convention is undecided. Wire this arm when the spawn producer lands \
                 and picks a Place shape for the function-pointer argument."
                    .to_string(),
            ));
        }
        // hew_task_await_blocking(task: *mut HewTask) -> *mut c_void
        // args[0]: task ptr. dest: Place for the result pointer (may be None for
        // void-result tasks; the blocking guarantee is unconditional).
        "hew_task_await_blocking" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_await_blocking): expected 1 arg \
                     (task), got {}",
                    args.len()
                )));
            }
            let task_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_await_blocking arg0")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[task_ptr.into()], "hew_task_await_blocking_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_task_await_blocking call: {e:?}")))?;
            // Result pointer — optional; void-result tasks may pass dest: None.
            if let Some(dest_place) = dest {
                let result_val = call.try_as_basic_value().basic().ok_or_else(|| {
                    CodegenError::FailClosed("hew_task_await_blocking returned void".into())
                })?;
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, result_val)
                    .map_err(|e| {
                        CodegenError::Llvm(format!("hew_task_await_blocking store: {e:?}"))
                    })?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_task_get_result(task: *mut HewTask) -> *mut c_void
        // args[0]: task ptr. dest: Place for the result pointer.
        // Must be called after hew_task_await_blocking (task is Done).
        "hew_task_get_result" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_get_result): expected 1 arg \
                     (task), got {}",
                    args.len()
                )));
            }
            let task_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_get_result arg0")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[task_ptr.into()], "hew_task_get_result_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_task_get_result call: {e:?}")))?;
            let result_val = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_task_get_result returned void".into())
            })?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_task_get_result: producer must supply a dest place for the result ptr"
                        .into(),
                )
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result_val)
                .map_err(|e| CodegenError::Llvm(format!("hew_task_get_result store: {e:?}")))?;
            let _ = (i32_ty, ptr_ty);
        }
        // hew_task_free(task: *mut HewTask) -> void
        // args[0]: task ptr. dest: None — frees the task allocation.
        "hew_task_free" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_free): expected 1 arg (task), got {}",
                    args.len()
                )));
            }
            let task_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_free arg0")?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            fn_ctx
                .builder
                .build_call(fv, &[task_ptr.into()], "hew_task_free_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_task_free call: {e:?}")))?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_task_free returns void; producer must not supply dest={d:?}"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        // ── supervisor child-slot lookup (S3) ────────────────────────────────
        //
        // hew_supervisor_child_get(sup: *mut HewSupervisor, key: u32)
        //     -> ChildLookupResult  (16-byte #[repr(C)] struct)
        //
        // MIR shape (from `lower_supervisor_child_get`):
        //   args[0]: Place::ActorHandle(N) — supervisor PID (ptr-typed alloca).
        //   args[1]: Place::Local(M)       — i64 slot index (ConstI64 from MIR).
        //   dest:    Place::Local(K)       — __HewChildLookupResult (struct alloca).
        //
        // ABI bridge:
        //   The runtime returns `{ i8, i8, [6 x i8], ptr }` by value (SysV
        //   amd64: two-register return, rdx:rax, 16 bytes total).  The MIR
        //   dest alloca is typed `{ i64, i64 }` (MIR's 2-field flattening of
        //   the C struct — see `CHILD_LOOKUP_RESULT_TY_NAME` in lower.rs:394).
        //   This arm bridges the two shapes:
        //     field 0 (i8 tag)    → zext to i64 → store into dest struct field 0
        //     field 3 (ptr handle)→ ptrtoint i64 → store into dest struct field 1
        //   Fields 1 (reason u8) and 2 ([6 x i8] padding) are discarded; the
        //   MIR branch-on-tag + Trap(SupervisorChildUnavailable) makes
        //   per-reason discrimination unnecessary at this layer.
        //
        //   `key` is `u32` in the runtime (i32 in LLVM); MIR emits i64 for the
        //   slot index (ConstI64). Truncate to i32 before the call.
        //
        // WASM: `uses_wasm_excluded_symbol` gates this symbol before WASM
        //   emission; supervisor tree requires the native scheduler runtime.
        "hew_supervisor_child_get" => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_supervisor_child_get): expected 2 args \
                     (sup, key), got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_supervisor_child_get: producer must supply a dest place \
                     (the __HewChildLookupResult alloca)"
                        .to_string(),
                )
            })?;

            // arg0: supervisor handle — ptr-typed (ActorHandle or actor-derived ptr).
            let sup_ptr = load_duplex_handle(fn_ctx, args[0], "supervisor_child_get sup")?;

            // arg1: slot index — i64 in MIR (ConstI64); truncate to i32 for the ABI.
            let key_i64 = load_int_arg(fn_ctx, args[1], i64_ty, "supervisor_child_get key_i64")?;
            let key_i32 = fn_ctx
                .builder
                .build_int_truncate(key_i64, i32_ty, "supervisor_child_get key_i32")
                .map_err(|e| {
                    CodegenError::Llvm(format!("supervisor_child_get key truncate: {e:?}"))
                })?;

            // Call hew_supervisor_child_get — returns { i8, i8, [6 x i8], ptr }.
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let llvm_args: [BasicMetadataValueEnum; 2] = [sup_ptr.into(), key_i32.into()];
            let call_site = fn_ctx
                .builder
                .build_call(fv, &llvm_args, "hew_supervisor_child_get_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_supervisor_child_get call: {e:?}")))?;
            let struct_val = call_site
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed(
                        "hew_supervisor_child_get returned void unexpectedly".into(),
                    )
                })?
                .into_struct_value();

            // Return type is `{ i64, i64 }` (aarch64 reg-return ABI).
            // Field 0: packed word — tag lives in the low byte; truncate to i8,
            //          then zext to i64 for the dest alloca's i64-typed slot.
            // Field 1: handle integer — already i64; use directly.
            let word0_i64 = fn_ctx
                .builder
                .build_extract_value(struct_val, 0, "child_word0_i64")
                .map_err(|e| CodegenError::Llvm(format!("extractvalue word0: {e:?}")))?
                .into_int_value();
            let tag_i8 = fn_ctx
                .builder
                .build_int_truncate(word0_i64, fn_ctx.ctx.i8_type(), "child_tag_i8")
                .map_err(|e| CodegenError::Llvm(format!("trunc tag: {e:?}")))?;
            let tag_i64 = fn_ctx
                .builder
                .build_int_z_extend(tag_i8, i64_ty, "child_tag_i64")
                .map_err(|e| CodegenError::Llvm(format!("zext tag: {e:?}")))?;

            // field 1: handle integer — already i64.
            let handle_i64 = fn_ctx
                .builder
                .build_extract_value(struct_val, 1, "child_handle_i64")
                .map_err(|e| CodegenError::Llvm(format!("extractvalue handle: {e:?}")))?
                .into_int_value();

            // The dest alloca has struct type { i64, i64 } (the MIR 2-field
            // flattening of ChildLookupResult registered in lower.rs:412).
            let (dest_ptr, dest_slot_ty) = place_pointer(fn_ctx, dest_place)?;
            let dest_struct_ty = match dest_slot_ty {
                BasicTypeEnum::StructType(st) => st,
                other => {
                    return Err(CodegenError::FailClosed(format!(
                        "hew_supervisor_child_get: dest Place must be a struct \
                         ({{i64, i64}} for __HewChildLookupResult), got {other:?}"
                    )));
                }
            };
            // Store tag into field 0 of the dest alloca.
            let tag_field_ptr = fn_ctx
                .builder
                .build_struct_gep(dest_struct_ty, dest_ptr, 0, "dest_tag_field_ptr")
                .map_err(|e| {
                    CodegenError::Llvm(format!("supervisor_child_get dest tag GEP: {e:?}"))
                })?;
            fn_ctx
                .builder
                .build_store(tag_field_ptr, tag_i64)
                .map_err(|e| {
                    CodegenError::Llvm(format!("supervisor_child_get tag store: {e:?}"))
                })?;
            // Store handle into field 1 of the dest alloca.
            let handle_field_ptr = fn_ctx
                .builder
                .build_struct_gep(dest_struct_ty, dest_ptr, 1, "dest_handle_field_ptr")
                .map_err(|e| {
                    CodegenError::Llvm(format!("supervisor_child_get dest handle GEP: {e:?}"))
                })?;
            fn_ctx
                .builder
                .build_store(handle_field_ptr, handle_i64)
                .map_err(|e| {
                    CodegenError::Llvm(format!("supervisor_child_get handle store: {e:?}"))
                })?;
            let _ = (i32_ty, ptr_ty);
        }

        // ── Regex literal match arm lowering ──────────────────────────────────
        //
        // Both arms load the compiled `*HewRegex` handle from the module-level
        // `@hew_regex_handles` global array using the `literal_id` (arg[1]) as
        // the GEP index. The array is populated by the `hew_module_init_regex`
        // constructor registered in `@llvm.global_ctors` (see
        // `emit_regex_module_init`). If the global is absent (no regex literals
        // in the pipeline), these arms fail closed — they should never be reached
        // without a corresponding `emit_regex_module_init` call.

        // hew_regex_match(re: *const HewRegex, text: *const c_char) -> i32
        //
        // args[0]: scrutinee (string local; stored as ptr in LLVM)
        // args[1]: literal_id (ConstI64 local; used as GEP index into global array)
        // dest:    i32 local (MIR emits IntCmp(NotEq, result, 0) immediately after)
        "hew_regex_match" => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_regex_match): expected 2 args \
                     (scrutinee, literal_id), got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_regex_match: producer must supply a dest place for the i32 result".into(),
                )
            })?;
            // arg0: scrutinee — ResolvedTy::String (stored as ptr in LLVM).
            let (scrutinee_alloca, scrutinee_llvm_ty) = place_pointer(fn_ctx, args[0])?;
            let text_ptr = fn_ctx
                .builder
                .build_load(scrutinee_llvm_ty, scrutinee_alloca, "regex_match_text")
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_match text load: {e:?}")))?
                .into_pointer_value();
            // arg1: literal_id — ConstI64 → used as GEP index into @hew_regex_handles.
            let lit_id = load_int_arg(fn_ctx, args[1], i64_ty, "hew_regex_match lit_id")?;
            let handle_arr_global =
                fn_ctx
                    .llvm_mod
                    .get_global("hew_regex_handles")
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "hew_regex_match: @hew_regex_handles global not found — \
                     regex_literals must be non-empty in the pipeline to emit the global"
                                .into(),
                        )
                    })?;
            // The global is [N x ptr]; GEP with [0, lit_id] to reach slot lit_id.
            // We don't know N at this point (it's a module-level choice), but
            // LLVM's typed GEP for arrays only validates the outer-index (0) at
            // IR level; the inner index is bounds-checked by the type-checker at
            // compile time (literal_id < N is an invariant). Using i64 for both
            // indices; LLVM normalises to the GEP element type internally.
            let handle_arr_ty = handle_arr_global.get_value_type().into_array_type();
            let slot_ptr = unsafe {
                fn_ctx
                    .builder
                    .build_gep(
                        handle_arr_ty,
                        handle_arr_global.as_pointer_value(),
                        &[i64_ty.const_zero(), lit_id],
                        "regex_handle_slot",
                    )
                    .map_err(|e| CodegenError::Llvm(format!("hew_regex_match GEP: {e:?}")))?
            };
            let handle = fn_ctx
                .builder
                .build_load(ptr_ty, slot_ptr, "regex_handle")
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_match handle load: {e:?}")))?
                .into_pointer_value();
            // Call hew_regex_match(handle, text) -> i32.
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let llvm_args: [BasicMetadataValueEnum; 2] = [handle.into(), text_ptr.into()];
            let call_site = fn_ctx
                .builder
                .build_call(fv, &llvm_args, "hew_regex_match_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_match call: {e:?}")))?;
            let result_i32 = call_site
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_regex_match returned void".into()))?;
            // Store i32 into dest.
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result_i32)
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_match store: {e:?}")))?;
        }

        // hew_regex_capture(re: *const HewRegex, text: *const c_char,
        //                   capture_idx: i64) -> *mut c_char
        //
        // args[0]: scrutinee (string local; stored as ptr)
        // args[1]: literal_id (ConstI64; GEP into @hew_regex_handles)
        // args[2]: capture_idx (ConstI64; 0-based into pattern's named capture list)
        // dest:    i64 local (captures stored as i64 for null-check via IntCmp)
        //
        // The returned *mut c_char is converted to i64 via ptrtoint so it fits
        // into the MIR i64 capture place. The MIR null-check (`IntCmp(Eq, cap, 0)`)
        // fires when the capture did not participate (null → 0 → branch to next arm).
        "hew_regex_capture" => {
            if args.len() != 3 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_regex_capture): expected 3 args \
                     (scrutinee, literal_id, capture_idx), got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_regex_capture: producer must supply a dest place for the capture ptr"
                        .into(),
                )
            })?;
            // arg0: scrutinee — ResolvedTy::String (ptr in LLVM).
            let (scrutinee_alloca, scrutinee_llvm_ty) = place_pointer(fn_ctx, args[0])?;
            let text_ptr = fn_ctx
                .builder
                .build_load(scrutinee_llvm_ty, scrutinee_alloca, "regex_cap_text")
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_capture text load: {e:?}")))?
                .into_pointer_value();
            // arg1: literal_id — GEP into @hew_regex_handles.
            let lit_id = load_int_arg(fn_ctx, args[1], i64_ty, "hew_regex_capture lit_id")?;
            let handle_arr_global =
                fn_ctx
                    .llvm_mod
                    .get_global("hew_regex_handles")
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "hew_regex_capture: @hew_regex_handles global not found".into(),
                        )
                    })?;
            let handle_arr_ty = handle_arr_global.get_value_type().into_array_type();
            let slot_ptr = unsafe {
                fn_ctx
                    .builder
                    .build_gep(
                        handle_arr_ty,
                        handle_arr_global.as_pointer_value(),
                        &[i64_ty.const_zero(), lit_id],
                        "regex_cap_handle_slot",
                    )
                    .map_err(|e| CodegenError::Llvm(format!("hew_regex_capture GEP: {e:?}")))?
            };
            let handle = fn_ctx
                .builder
                .build_load(ptr_ty, slot_ptr, "regex_cap_handle")
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_capture handle load: {e:?}")))?
                .into_pointer_value();
            // arg2: capture_idx — i64.
            let cap_idx = load_int_arg(fn_ctx, args[2], i64_ty, "hew_regex_capture cap_idx")?;
            // Call hew_regex_capture(handle, text, cap_idx) -> *mut c_char.
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let llvm_args: [BasicMetadataValueEnum; 3] =
                [handle.into(), text_ptr.into(), cap_idx.into()];
            let call_site = fn_ctx
                .builder
                .build_call(fv, &llvm_args, "hew_regex_capture_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_capture call: {e:?}")))?;
            let cap_ptr = call_site
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_regex_capture returned void".into()))?
                .into_pointer_value();
            // Convert *mut c_char → i64 (ptrtoint) so the capture place (ResolvedTy::I64)
            // receives the pointer bit-pattern. The MIR null-check (IntCmp Eq 0) fires on
            // null return (group did not participate → branch to next arm, fail-closed).
            let cap_as_i64 = fn_ctx
                .builder
                .build_ptr_to_int(cap_ptr, i64_ty, "regex_cap_as_i64")
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_capture ptrtoint: {e:?}")))?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, cap_as_i64)
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_capture store: {e:?}")))?;
        }

        // hew_regex_free_capture(ptr: *mut c_char) -> void
        //
        // args[0]: the i64 capture place (pointer bit-pattern stored as i64 via ptrtoint).
        //          Converted back to *mut c_char via inttoptr before the call.
        // dest:    must be None (void return).
        //
        // Emitted by MIR at arm-body exit (success path, after all captures are
        // extracted) and in partial-failure cleanup blocks (captures[0..j] already
        // allocated when capture[j] returned null).
        "hew_regex_free_capture" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_regex_free_capture): expected 1 arg \
                     (capture ptr as i64), got {}",
                    args.len()
                )));
            }
            // The capture place is ResolvedTy::I64 (pointer bit-pattern via ptrtoint).
            // Load the i64 value and convert back to *mut c_char via inttoptr.
            let cap_as_i64 = load_int_arg(fn_ctx, args[0], i64_ty, "hew_regex_free_cap_i64")?;
            let cap_ptr = fn_ctx
                .builder
                .build_int_to_ptr(cap_as_i64, ptr_ty, "hew_regex_free_cap_ptr")
                .map_err(|e| {
                    CodegenError::Llvm(format!("hew_regex_free_capture inttoptr: {e:?}"))
                })?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            fn_ctx
                .builder
                .build_call(fv, &[cap_ptr.into()], "hew_regex_free_capture_call")
                .map_err(|e| CodegenError::Llvm(format!("hew_regex_free_capture call: {e:?}")))?;
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
/// requested `expected` i64 type. Today every M2 substrate integer arg
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
                "{label}: Place {place:?} resolves to non-i64 type {other:?}; \
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

fn print_kind_for_resolved_ty(ty: &ResolvedTy) -> Option<PrintKind> {
    match ty {
        ResolvedTy::I32 => Some(PrintKind::I32),
        ResolvedTy::I64 => Some(PrintKind::I64),
        ResolvedTy::U32 => Some(PrintKind::U32),
        ResolvedTy::U64 => Some(PrintKind::U64),
        ResolvedTy::F64 => Some(PrintKind::F64),
        ResolvedTy::Bool => Some(PrintKind::Bool),
        ResolvedTy::String => Some(PrintKind::Str),
        _ => None,
    }
}

fn print_kind_tag(kind: PrintKind) -> u64 {
    match kind {
        PrintKind::I32 => 0,
        PrintKind::I64 => 1,
        PrintKind::F64 => 2,
        PrintKind::Bool => 3,
        PrintKind::Str => 4,
        PrintKind::U32 => 5,
        PrintKind::U64 => 6,
    }
}

fn expect_int_value<'ctx>(
    value: BasicValueEnum<'ctx>,
    label: &str,
) -> CodegenResult<IntValue<'ctx>> {
    match value {
        BasicValueEnum::IntValue(v) => Ok(v),
        other => Err(CodegenError::FailClosed(format!(
            "{label}: expected integer value, got {other:?}"
        ))),
    }
}

fn emit_print_value_call<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    runtime_symbol: &str,
    arg: Place,
    kind: PrintKind,
    newline: bool,
    callee: &str,
) -> CodegenResult<()> {
    let arg_resolved_ty = place_resolved_ty(fn_ctx, arg)?;
    let actual_kind = print_kind_for_resolved_ty(arg_resolved_ty).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "print intercept `{callee}` received unsupported argument type {arg_resolved_ty:?}"
        ))
    })?;
    if actual_kind != kind {
        return Err(CodegenError::FailClosed(format!(
            "print intercept `{callee}` expected {kind:?}, got {actual_kind:?} \
             from argument type {arg_resolved_ty:?}"
        )));
    }

    let (arg_ptr, arg_ty) = place_pointer(fn_ctx, arg)?;
    let loaded = fn_ctx
        .builder
        .build_load(arg_ty, arg_ptr, "print_arg")
        .map_err(|e| CodegenError::Llvm(format!("print arg load: {e:?}")))?;
    let i64_ty = fn_ctx.ctx.i64_type();
    let bits = match kind {
        PrintKind::I32 | PrintKind::U32 | PrintKind::Bool => {
            let int_value = expect_int_value(loaded, "print integer/bool payload")?;
            fn_ctx
                .builder
                .build_int_z_extend(int_value, i64_ty, "print_narrow_bits")
                .map_err(|e| CodegenError::Llvm(format!("print payload zext: {e:?}")))?
        }
        PrintKind::I64 | PrintKind::U64 => {
            let int_value = expect_int_value(loaded, "print i64/u64 payload")?;
            if int_value.get_type().get_bit_width() != 64 {
                return Err(CodegenError::FailClosed(format!(
                    "print intercept `{callee}` expected a 64-bit integer payload, \
                     got {} bits",
                    int_value.get_type().get_bit_width()
                )));
            }
            int_value
        }
        PrintKind::F64 => match loaded {
            BasicValueEnum::FloatValue(v) => fn_ctx
                .builder
                .build_bit_cast(v, i64_ty, "print_f64_bits")
                .map_err(|e| CodegenError::Llvm(format!("print f64 bitcast: {e:?}")))?
                .into_int_value(),
            other => {
                return Err(CodegenError::FailClosed(format!(
                    "print f64 payload must be a float value, got {other:?}"
                )));
            }
        },
        PrintKind::Str => match loaded {
            BasicValueEnum::PointerValue(v) => fn_ctx
                .builder
                .build_ptr_to_int(v, i64_ty, "print_str_bits")
                .map_err(|e| CodegenError::Llvm(format!("print str ptrtoint: {e:?}")))?,
            other => {
                return Err(CodegenError::FailClosed(format!(
                    "print string payload must be a pointer value, got {other:?}"
                )));
            }
        },
    };

    let print_fn = declare_print_runtime(fn_ctx.ctx, fn_ctx.llvm_mod, runtime_symbol);
    let kind_tag = fn_ctx.ctx.i8_type().const_int(print_kind_tag(kind), false);
    let newline_flag = fn_ctx
        .ctx
        .bool_type()
        .const_int(if newline { 1 } else { 0 }, false);
    fn_ctx
        .builder
        .build_call(
            print_fn,
            &[kind_tag.into(), bits.into(), newline_flag.into()],
            "hew_print_value_call",
        )
        .map_err(|e| CodegenError::Llvm(format!("hew_print_value call: {e:?}")))?;
    Ok(())
}

fn actor_payload_ptr_size<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    value: Place,
    label: &str,
) -> CodegenResult<(PointerValue<'ctx>, IntValue<'ctx>)> {
    if matches!(place_resolved_ty(fn_ctx, value)?, ResolvedTy::Unit) {
        return Ok((
            fn_ctx.ctx.ptr_type(AddressSpace::default()).const_null(),
            fn_ctx.ctx.i64_type().const_zero(),
        ));
    }
    let (ptr, ty) = place_pointer(fn_ctx, value)?;
    let size = ty.size_of().ok_or_else(|| {
        CodegenError::FailClosed(format!("{label}: payload type has no static size: {ty:?}"))
    })?;
    let i64_ty = fn_ctx.ctx.i64_type();
    let size = if size.get_type() == i64_ty {
        size
    } else {
        fn_ctx
            .builder
            .build_int_z_extend(size, i64_ty, &format!("{label}_size"))
            .map_err(|e| CodegenError::Llvm(format!("{label} size zext: {e:?}")))?
    };
    Ok((ptr, size))
}

fn load_actor_id<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    actor_ptr: PointerValue<'ctx>,
) -> CodegenResult<IntValue<'ctx>> {
    let id_slot = unsafe {
        fn_ctx
            .builder
            .build_gep(
                fn_ctx.ctx.i8_type(),
                actor_ptr,
                &[fn_ctx
                    .ctx
                    .i64_type()
                    .const_int(HEW_ACTOR_OFFSET_ID as u64, false)],
                "actor_id_slot",
            )
            .map_err(|e| CodegenError::Llvm(format!("actor id gep: {e:?}")))?
    };
    Ok(fn_ctx
        .builder
        .build_load(fn_ctx.ctx.i64_type(), id_slot, "actor_id")
        .map_err(|e| CodegenError::Llvm(format!("actor id load: {e:?}")))?
        .into_int_value())
}

fn get_or_declare_free<'ctx>(fn_ctx: &FnCtx<'_, 'ctx>) -> FunctionValue<'ctx> {
    if let Some(fv) = fn_ctx.llvm_mod.get_function("free") {
        return fv;
    }
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    fn_ctx.llvm_mod.add_function(
        "free",
        fn_ctx.ctx.void_type().fn_type(&[ptr_ty.into()], false),
        Some(Linkage::External),
    )
}

fn emit_trap_with_code(fn_ctx: &FnCtx<'_, '_>, code: u64, label: &str) -> CodegenResult<()> {
    let trap_with_code_fn = match fn_ctx.llvm_mod.get_function("hew_trap_with_code") {
        Some(fv) => fv,
        None => {
            let i32_ty = fn_ctx.ctx.i32_type();
            let fn_ty = fn_ctx.ctx.void_type().fn_type(&[i32_ty.into()], false);
            fn_ctx
                .llvm_mod
                .add_function("hew_trap_with_code", fn_ty, Some(Linkage::External))
        }
    };
    let code_val = fn_ctx.ctx.i32_type().const_int(code, false);
    fn_ctx
        .builder
        .build_call(trap_with_code_fn, &[code_val.into()], label)
        .map_err(|e| CodegenError::Llvm(format!("hew_trap_with_code call: {e:?}")))?;
    let trap_intrinsic = Intrinsic::find("llvm.trap")
        .ok_or_else(|| CodegenError::Llvm("llvm.trap intrinsic not found in LLVM build".into()))?;
    let trap_fn = trap_intrinsic
        .get_declaration(fn_ctx.llvm_mod, &[])
        .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
    fn_ctx
        .builder
        .build_call(trap_fn, &[], &format!("{label}_llvm_trap"))
        .map_err(|e| CodegenError::Llvm(format!("llvm.trap call: {e:?}")))?;
    fn_ctx
        .builder
        .build_unreachable()
        .map_err(|e| CodegenError::Llvm(format!("unreachable: {e:?}")))?;
    Ok(())
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
                    ));
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
            let callee_symbol_entry = *fn_symbols.get(callee).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "Call to `{callee}` with no declared symbol (coroutine targets are not callable yet)"
                ))
            })?;
            match callee_symbol_entry {
                FnSymbol::PrintIntercept {
                    runtime_symbol,
                    kind,
                    newline,
                } => {
                    if dest.is_some() {
                        return Err(CodegenError::FailClosed(format!(
                            "print intercept `{callee}` must not carry a Terminator::Call dest"
                        )));
                    }
                    let [arg] = args.as_slice() else {
                        return Err(CodegenError::FailClosed(format!(
                            "print intercept `{callee}` expects exactly one argument, got {}",
                            args.len()
                        )));
                    };
                    emit_print_value_call(fn_ctx, runtime_symbol, *arg, kind, newline, callee)?;
                }
                FnSymbol::Real {
                    value,
                    return_ty,
                    returns_unit,
                } => {
                    let mut arg_vals: Vec<inkwell::values::BasicMetadataValueEnum> =
                        Vec::with_capacity(args.len());
                    for arg in args {
                        let (arg_ptr, arg_ty) = place_pointer(fn_ctx, *arg)?;
                        let loaded = fn_ctx
                            .builder
                            .build_load(arg_ty, arg_ptr, "call_arg")
                            .map_err(|e| CodegenError::Llvm(format!("call arg load: {e:?}")))?;
                        arg_vals.push(metadata_value_from_basic(loaded));
                    }
                    let call_site = fn_ctx
                        .builder
                        .build_call(value, &arg_vals, "call_result")
                        .map_err(|e| CodegenError::Llvm(format!("build_call: {e:?}")))?;
                    if let Some(dest_place) = dest {
                        if returns_unit {
                            return Err(CodegenError::FailClosed(format!(
                                "Call to unit-returning fn `{callee}` must not carry a Terminator::Call dest"
                            )));
                        }
                        let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
                        if dest_ty != return_ty {
                            return Err(CodegenError::FailClosed(format!(
                                "Call dest type {dest_ty:?} does not match callee return {:?}",
                                return_ty
                            )));
                        }
                        let ret_val = call_site.try_as_basic_value().basic().ok_or_else(|| {
                            CodegenError::FailClosed(
                                "Call to void-returning fn is not usable with Terminator::Call dest"
                                    .into(),
                            )
                        })?;
                        fn_ctx
                            .builder
                            .build_store(dest_ptr, ret_val)
                            .map_err(|e| CodegenError::Llvm(format!("call store: {e:?}")))?;
                    } else if !returns_unit {
                        return Err(CodegenError::FailClosed(format!(
                            "Call to value-returning fn `{callee}` must carry a Terminator::Call dest"
                        )));
                    }
                }
            }
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
            // The exit-code constants here MUST stay in lock-step
            // with canonical `HEW_TRAP_*` in
            // `hew-runtime/src/internal/types.rs`; the native supervisor
            // module re-exports those constants for native callers.
            const HEW_TRAP_INTEGER_OVERFLOW: u64 = 201;
            const HEW_TRAP_DIVIDE_BY_ZERO: u64 = 202;
            const HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE: u64 = 203;
            const HEW_TRAP_SHIFT_OUT_OF_RANGE: u64 = 204;
            const HEW_TRAP_INDEX_OUT_OF_BOUNDS: u64 = 205;
            const HEW_TRAP_ACTOR_SEND_FAILED: u64 = 206;
            const HEW_TRAP_MACHINE_DISPATCH_UNREACHABLE: u64 = 207;
            const HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH: u64 = 208;
            let code: u64 = match *kind {
                TrapKind::IntegerOverflow => HEW_TRAP_INTEGER_OVERFLOW,
                TrapKind::DivideByZero => HEW_TRAP_DIVIDE_BY_ZERO,
                TrapKind::SignedMinDivNegOne => HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
                TrapKind::ShiftOutOfRange => HEW_TRAP_SHIFT_OUT_OF_RANGE,
                TrapKind::IndexOutOfBounds => HEW_TRAP_INDEX_OUT_OF_BOUNDS,
                TrapKind::SupervisorChildUnavailable => HEW_TRAP_ACTOR_SEND_FAILED,
                TrapKind::MachineDispatchUnreachable => HEW_TRAP_MACHINE_DISPATCH_UNREACHABLE,
                TrapKind::ExhaustivenessFallthrough => HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH,
            };
            emit_trap_with_code(fn_ctx, code, "trap")?;
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
        Terminator::Send {
            actor,
            msg_type,
            value,
            next,
        } => {
            let actor_ptr = load_duplex_handle(fn_ctx, *actor, "actor_send receiver")?;
            let actor_id = load_actor_id(fn_ctx, actor_ptr)?;
            let (payload_ptr, payload_size) =
                actor_payload_ptr_size(fn_ctx, *value, "actor_send_payload")?;
            let send = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                "hew_actor_send_by_id",
            )?;
            let msg_type = fn_ctx.ctx.i32_type().const_int(*msg_type as u64, false);
            // Bind the i32 return value. A nonzero status means the recipient
            // was gone, the mailbox was full, or the remote partition rejected
            // the message. We must not silently proceed — that could leave the
            // caller operating on stale state or attempting a follow-up ask
            // on a dead actor.
            let send_status = fn_ctx
                .builder
                .build_call(
                    send,
                    &[
                        actor_id.into(),
                        msg_type.into(),
                        payload_ptr.into(),
                        payload_size.into(),
                    ],
                    "hew_actor_send_by_id_call",
                )
                .map_err(|e| CodegenError::Llvm(format!("hew_actor_send_by_id call: {e:?}")))?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_actor_send_by_id returned void".into())
                })?
                .into_int_value();

            // Branch: status == 0 → next_bb; status != 0 → send_fail_bb.
            let send_fail_bb = fn_ctx
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_parent())
                .map(|f| fn_ctx.ctx.append_basic_block(f, "actor_send_fail"))
                .ok_or_else(|| CodegenError::Llvm("send block has no parent function".into()))?;
            let zero = fn_ctx.ctx.i32_type().const_zero();
            let send_failed = fn_ctx
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    send_status,
                    zero,
                    "actor_send_failed",
                )
                .map_err(|e| CodegenError::Llvm(format!("send status cmp: {e:?}")))?;

            let next_bb = *fn_ctx
                .blocks
                .get(next)
                .ok_or_else(|| CodegenError::FailClosed(format!("Send next bb{next} missing")))?;
            fn_ctx
                .builder
                .build_conditional_branch(send_failed, send_fail_bb, next_bb)
                .map_err(|e| CodegenError::Llvm(format!("send status branch: {e:?}")))?;

            fn_ctx.builder.position_at_end(send_fail_bb);
            emit_trap_with_code(fn_ctx, 206, "actor_send_fail")?;
        }
        Terminator::Ask {
            actor,
            msg_type,
            value,
            reply_dest,
            next,
        } => {
            let actor_ptr = load_duplex_handle(fn_ctx, *actor, "actor_ask receiver")?;
            let (payload_ptr, payload_size) =
                actor_payload_ptr_size(fn_ctx, *value, "actor_ask_payload")?;
            let ask = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                "hew_actor_ask",
            )?;
            let msg_type = fn_ctx.ctx.i32_type().const_int(*msg_type as u64, false);
            let reply_ptr = fn_ctx
                .builder
                .build_call(
                    ask,
                    &[
                        actor_ptr.into(),
                        msg_type.into(),
                        payload_ptr.into(),
                        payload_size.into(),
                    ],
                    "hew_actor_ask_call",
                )
                .map_err(|e| CodegenError::Llvm(format!("hew_actor_ask call: {e:?}")))?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_actor_ask returned void".into()))?
                .into_pointer_value();
            let ok_bb = fn_ctx.ctx.append_basic_block(
                fn_ctx
                    .builder
                    .get_insert_block()
                    .and_then(|bb| bb.get_parent())
                    .ok_or_else(|| CodegenError::Llvm("ask block has no parent function".into()))?,
                "actor_ask_reply_ok",
            );
            let null_bb = fn_ctx.ctx.append_basic_block(
                ok_bb
                    .get_parent()
                    .ok_or_else(|| CodegenError::Llvm("ask ok block has no parent".into()))?,
                "actor_ask_reply_null",
            );
            let is_null = fn_ctx
                .builder
                .build_is_null(reply_ptr, "actor_ask_reply_is_null")
                .map_err(|e| CodegenError::Llvm(format!("ask null compare: {e:?}")))?;
            fn_ctx
                .builder
                .build_conditional_branch(is_null, null_bb, ok_bb)
                .map_err(|e| CodegenError::Llvm(format!("ask null branch: {e:?}")))?;

            fn_ctx.builder.position_at_end(null_bb);
            let trap_intrinsic = Intrinsic::find("llvm.trap").ok_or_else(|| {
                CodegenError::Llvm("llvm.trap intrinsic not found in LLVM build".into())
            })?;
            let trap_fn = trap_intrinsic
                .get_declaration(fn_ctx.llvm_mod, &[])
                .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
            fn_ctx
                .builder
                .build_call(trap_fn, &[], "actor_ask_null_trap")
                .map_err(|e| CodegenError::Llvm(format!("ask null trap: {e:?}")))?;
            fn_ctx
                .builder
                .build_unreachable()
                .map_err(|e| CodegenError::Llvm(format!("ask null unreachable: {e:?}")))?;

            fn_ctx.builder.position_at_end(ok_bb);
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *reply_dest)?;
            let reply_val = fn_ctx
                .builder
                .build_load(dest_ty, reply_ptr, "actor_ask_reply_value")
                .map_err(|e| CodegenError::Llvm(format!("ask reply load: {e:?}")))?;
            fn_ctx
                .builder
                .build_store(dest_ptr, reply_val)
                .map_err(|e| CodegenError::Llvm(format!("ask reply store: {e:?}")))?;
            let free = get_or_declare_free(fn_ctx);
            fn_ctx
                .builder
                .build_call(free, &[reply_ptr.into()], "actor_ask_reply_free")
                .map_err(|e| CodegenError::Llvm(format!("free ask reply: {e:?}")))?;
            let next_bb = *fn_ctx
                .blocks
                .get(next)
                .ok_or_else(|| CodegenError::FailClosed(format!("Ask next bb{next} missing")))?;
            fn_ctx
                .builder
                .build_unconditional_branch(next_bb)
                .map_err(|e| CodegenError::Llvm(format!("ask br: {e:?}")))?;
        }
        Terminator::Select { arms, .. } => {
            emit_select_terminator(fn_ctx, arms)?;
        }
    }
    Ok(())
}

/// Emit the LLVM IR for `Terminator::Select` with `ActorAsk` +
/// optional `AfterTimer` arms.
///
/// Producer contract (`Terminator::Select { arms, next }`):
/// - Every arm carries `body_block` (the block reached on win) and
///   `binding` (the per-arm reply slot for `ActorAsk` arms; `None`
///   for `AfterTimer`).
/// - At most one `AfterTimer` arm is present (HIR enforces).
/// - `StreamNext` / `TaskAwait` arms are not in scope for this lane
///   and remain fail-closed below (defence-in-depth — the MIR
///   producer rejects them before this codegen runs).
///
/// Emitted shape (per slice 3 plan §6 + runtime ABI). For each
/// `ActorAsk` arm: allocate `ch = hew_reply_channel_new()`, store
/// into a stack array slot, then call `hew_actor_ask_with_channel`.
/// If that call returns non-zero, branch to a mid-setup error
/// recovery block that frees every channel allocated so far and
/// traps (Risk R3). Read the AfterTimer arm's duration if present,
/// convert ns → ms, saturate to `i32::MAX`; otherwise use -1
/// (wait indefinitely). Call `hew_select_first(arr, n_asks, timeout_ms)`
/// to get the winner index. Dispatch via switch: an ActorAsk arm
/// index routes to that arm's winner block; the default routes to
/// the AfterTimer winner block (when present) or a trap. In each
/// winner block, `hew_reply_wait(ch[winner])` reads the reply, the
/// value is stored into `arm.binding`'s slot, `libc::free(reply_ptr)`
/// releases the buffer, and `hew_reply_channel_free(ch[winner])`
/// releases the channel. For every loser j, `hew_reply_channel_cancel(ch[j])`
/// runs BEFORE `hew_reply_channel_free(ch[j])` — Risk R4: cancel
/// before free prevents UAF when a reply races our free.
/// Finally each winner block branches to the arm's MIR-allocated
/// `body_block`.
fn stream_callback_type_suffix(item_ty: BasicTypeEnum<'_>) -> String {
    let mut hash = 0xcbf29ce484222325_u64;
    for byte in item_ty.print_to_string().to_string().bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("{hash:016x}")
}

fn get_or_create_select_stream_callback<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    item_ty: BasicTypeEnum<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let callback_name = format!(
        "hew_select_stream_reply_ready_{}",
        stream_callback_type_suffix(item_ty)
    );
    if let Some(fv) = fn_ctx.llvm_mod.get_function(&callback_name) {
        return Ok(fv);
    }

    let ctx = fn_ctx.ctx;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    let item_size = item_ty.size_of().ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "stream callback item type has no static size: {item_ty:?}"
        ))
    })?;
    let fn_ty = ctx
        .void_type()
        .fn_type(&[ptr_ty.into(), ptr_ty.into()], false);
    let callback = fn_ctx
        .llvm_mod
        .add_function(&callback_name, fn_ty, Some(Linkage::Private));
    let entry = ctx.append_basic_block(callback, "entry");
    let null_item_bb = ctx.append_basic_block(callback, "null_item");
    let non_null_bb = ctx.append_basic_block(callback, "non_null");
    let delivered_bb = ctx.append_basic_block(callback, "delivered");
    let not_delivered_bb = ctx.append_basic_block(callback, "not_delivered");
    let free_item_bb = ctx.append_basic_block(callback, "free_item");
    let return_bb = ctx.append_basic_block(callback, "return");

    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    let ch = callback
        .get_nth_param(0)
        .ok_or_else(|| CodegenError::Llvm("stream callback missing channel param".into()))?
        .into_pointer_value();
    let item = callback
        .get_nth_param(1)
        .ok_or_else(|| CodegenError::Llvm("stream callback missing item param".into()))?
        .into_pointer_value();
    let item_is_null = builder
        .build_is_null(item, "stream_item_is_null")
        .map_err(|e| CodegenError::Llvm(format!("stream callback item null cmp: {e:?}")))?;
    builder
        .build_conditional_branch(item_is_null, null_item_bb, non_null_bb)
        .map_err(|e| CodegenError::Llvm(format!("stream callback item null branch: {e:?}")))?;

    let reply = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply",
    )?;

    builder.position_at_end(null_item_bb);
    builder
        .build_call(
            reply,
            &[
                ch.into(),
                ptr_ty.const_null().into(),
                i64_ty.const_zero().into(),
            ],
            "stream_reply_null_item",
        )
        .map_err(|e| CodegenError::Llvm(format!("stream callback null hew_reply call: {e:?}")))?;
    builder
        .build_unconditional_branch(return_bb)
        .map_err(|e| CodegenError::Llvm(format!("stream callback null return: {e:?}")))?;

    builder.position_at_end(non_null_bb);
    let item_value = builder
        .build_load(item_ty, item, "stream_item_value")
        .map_err(|e| CodegenError::Llvm(format!("stream callback item load: {e:?}")))?;
    let item_slot = builder
        .build_alloca(item_ty, "stream_item_slot")
        .map_err(|e| CodegenError::Llvm(format!("stream callback item slot alloca: {e:?}")))?;
    builder
        .build_store(item_slot, item_value)
        .map_err(|e| CodegenError::Llvm(format!("stream callback item store: {e:?}")))?;
    let item_size = if item_size.get_type() == i64_ty {
        item_size
    } else {
        builder
            .build_int_z_extend(item_size, i64_ty, "stream_item_size")
            .map_err(|e| CodegenError::Llvm(format!("stream callback item size zext: {e:?}")))?
    };
    let delivered = builder
        .build_call(
            reply,
            &[ch.into(), item_slot.into(), item_size.into()],
            "stream_reply_delivered",
        )
        .map_err(|e| CodegenError::Llvm(format!("stream callback hew_reply call: {e:?}")))?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_reply returned void".into()))?
        .into_int_value();
    builder
        .build_conditional_branch(delivered, delivered_bb, not_delivered_bb)
        .map_err(|e| CodegenError::Llvm(format!("stream callback delivered branch: {e:?}")))?;

    builder.position_at_end(delivered_bb);
    builder
        .build_unconditional_branch(free_item_bb)
        .map_err(|e| CodegenError::Llvm(format!("stream callback delivered return: {e:?}")))?;

    builder.position_at_end(not_delivered_bb);
    builder
        .build_unconditional_branch(free_item_bb)
        .map_err(|e| CodegenError::Llvm(format!("stream callback item free branch: {e:?}")))?;

    builder.position_at_end(free_item_bb);
    let free_fn = match fn_ctx.llvm_mod.get_function("free") {
        Some(fv) => fv,
        None => fn_ctx.llvm_mod.add_function(
            "free",
            ctx.void_type().fn_type(&[ptr_ty.into()], false),
            Some(Linkage::External),
        ),
    };
    builder
        .build_call(free_fn, &[item.into()], "stream_item_free")
        .map_err(|e| CodegenError::Llvm(format!("stream callback item free: {e:?}")))?;
    builder
        .build_unconditional_branch(return_bb)
        .map_err(|e| CodegenError::Llvm(format!("stream callback free return: {e:?}")))?;

    builder.position_at_end(return_bb);
    builder
        .build_return(None)
        .map_err(|e| CodegenError::Llvm(format!("stream callback return: {e:?}")))?;
    Ok(callback)
}

fn load_current_task_scope<'ctx>(fn_ctx: &FnCtx<'_, 'ctx>) -> CodegenResult<PointerValue<'ctx>> {
    let ctx_ptr = fn_ctx.execution_context.ok_or_else(|| {
        CodegenError::FailClosed("select{} TaskAwait arm requires an execution context".into())
    })?;
    let offset = fn_ctx
        .ctx
        .i64_type()
        .const_int(HEW_CTX_OFFSET_TASK_SCOPE as u64, false);
    let field_ptr = unsafe {
        fn_ctx
            .builder
            .build_gep(
                fn_ctx.ctx.i8_type(),
                ctx_ptr,
                &[offset],
                "select_task_scope_ptr",
            )
            .map_err(|e| CodegenError::Llvm(format!("select task scope gep: {e:?}")))?
    };
    Ok(fn_ctx
        .builder
        .build_load(
            fn_ctx.ctx.ptr_type(AddressSpace::default()),
            field_ptr,
            "select_task_scope",
        )
        .map_err(|e| CodegenError::Llvm(format!("select task scope load: {e:?}")))?
        .into_pointer_value())
}

fn emit_select_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    arms: &[hew_mir::SelectArm],
) -> CodegenResult<()> {
    use hew_mir::SelectArmKind;

    if arms.is_empty() {
        // HIR rejects empty selects with SelectNoArms; defence-in-depth.
        return Err(CodegenError::FailClosed(
            "select{} terminator carries zero arms (HIR should have rejected with SelectNoArms)"
                .to_string(),
        ));
    }

    // Partition arms by kind, preserving original arm indices for the
    // body-block dispatch. ActorAsk arms feed the `hew_select_first`
    // channel array; the AfterTimer arm (at most one — HIR enforces)
    // supplies the deadline.
    let mut wait_arm_indices: Vec<usize> = Vec::with_capacity(arms.len());
    let mut after_arm_index: Option<usize> = None;
    for (i, arm) in arms.iter().enumerate() {
        match &arm.kind {
            SelectArmKind::ActorAsk { .. }
            | SelectArmKind::StreamNext { .. }
            | SelectArmKind::TaskAwait { .. } => wait_arm_indices.push(i),
            SelectArmKind::AfterTimer { .. } => {
                if after_arm_index.is_some() {
                    return Err(CodegenError::FailClosed(
                        "select{} carries more than one AfterTimer arm \
                         (HIR should have rejected)"
                            .to_string(),
                    ));
                }
                after_arm_index = Some(i);
            }
        }
    }

    if wait_arm_indices.is_empty() {
        // A select with only an `after` arm is structurally valid but
        // has no race to dispatch: we'd reduce to "wait then run the
        // after-arm body". The HIR forbids select{} composed only of
        // an `after` arm (a sealed-shape invariant); fail closed.
        return Err(CodegenError::FailClosed(
            "select{} carries no value-producing arms (only AfterTimer): \
             HIR should have rejected as a sealed-shape violation"
                .to_string(),
        ));
    }

    let n_waits: usize = wait_arm_indices.len();
    let n_waits_i32 = i32::try_from(n_waits).map_err(|_| {
        CodegenError::FailClosed(format!(
            "select{{}} arm count {n_waits} exceeds i32::MAX — runtime ABI is i32-bound"
        ))
    })?;

    let ctx = fn_ctx.ctx;
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    // Allocate the channel array on the stack: [N x ptr]. Per-ActorAsk
    // arm index i stores the freshly-allocated `*mut HewReplyChannel`
    // at `channel_array[i]`. The array is referenced by the winner
    // dispatch (free/cancel) and by `hew_select_first`.
    let arr_ty = ptr_ty.array_type(n_waits_i32 as u32);
    let arr_ptr = fn_ctx
        .builder
        .build_alloca(arr_ty, "select_channels")
        .map_err(|e| CodegenError::Llvm(format!("select channel array alloca: {e:?}")))?;
    let pending_id_arr_ty = i64_ty.array_type(n_waits_i32 as u32);
    let pending_id_arr_ptr = fn_ctx
        .builder
        .build_alloca(pending_id_arr_ty, "select_pending_read_ids")
        .map_err(|e| CodegenError::Llvm(format!("select pending-read array alloca: {e:?}")))?;

    // Resolve the parent function once for block allocation.
    let parent_fn = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::Llvm("select block has no parent function".into()))?;

    let channel_new = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_new",
    )?;
    let ask_with_channel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_ask_with_channel",
    )?;
    let channel_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_cancel",
    )?;
    let channel_free = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_free",
    )?;
    let channel_retain = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_retain",
    )?;
    let signal_ready = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_signal_ready",
    )?;
    let reply_wait = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_wait",
    )?;
    let select_first = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_select_first",
    )?;
    let stream_poll = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_stream_poll",
    )?;
    let stream_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_stream_cancel_pending_read",
    )?;
    let task_completion_observe = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_completion_observe",
    )?;
    let task_completion_unobserve = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_completion_unobserve",
    )?;
    let task_get_result = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_get_result",
    )?;
    let libc_free = get_or_declare_free(fn_ctx);

    // Per-ActorAsk arm: allocate channel, store into array, issue ask.
    // On any ask-issue failure, branch to a recovery block that frees
    // every channel allocated up to that point and traps. (Risk R3.)
    //
    // Channel-array-slot GEP helper: `&channel_array[i]`.
    let slot_ptr = |i: usize| -> CodegenResult<PointerValue<'ctx>> {
        let i_u32 = u32::try_from(i).map_err(|_| {
            CodegenError::FailClosed(format!("select arm index {i} exceeds u32::MAX"))
        })?;
        let idx0 = i32_ty.const_zero();
        let idx1 = i32_ty.const_int(u64::from(i_u32), false);
        let gep = unsafe {
            fn_ctx
                .builder
                .build_gep(arr_ty, arr_ptr, &[idx0, idx1], &format!("ch_slot_{i}"))
                .map_err(|e| CodegenError::Llvm(format!("ch slot gep: {e:?}")))?
        };
        Ok(gep)
    };

    let pending_id_slot_ptr = |i: usize| -> CodegenResult<PointerValue<'ctx>> {
        let i_u32 = u32::try_from(i).map_err(|_| {
            CodegenError::FailClosed(format!("select arm index {i} exceeds u32::MAX"))
        })?;
        let idx0 = i32_ty.const_zero();
        let idx1 = i32_ty.const_int(u64::from(i_u32), false);
        let gep = unsafe {
            fn_ctx
                .builder
                .build_gep(
                    pending_id_arr_ty,
                    pending_id_arr_ptr,
                    &[idx0, idx1],
                    &format!("pending_read_slot_{i}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("pending-read slot gep: {e:?}")))?
        };
        Ok(gep)
    };

    for (slot_idx, &arm_idx) in wait_arm_indices.iter().enumerate() {
        // Allocate the channel.
        let ch_val = fn_ctx
            .builder
            .build_call(channel_new, &[], &format!("select_ch_new_{slot_idx}"))
            .map_err(|e| CodegenError::Llvm(format!("hew_reply_channel_new call: {e:?}")))?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_reply_channel_new returned void".into()))?
            .into_pointer_value();

        // Store into channel_array[slot_idx]. We perform the store
        // BEFORE issuing the ask so the recovery block sees a
        // consistent array view if the ask issue fails.
        let slot = slot_ptr(slot_idx)?;
        fn_ctx
            .builder
            .build_store(slot, ch_val)
            .map_err(|e| CodegenError::Llvm(format!("ch slot store: {e:?}")))?;

        let (status, current_has_retained_observer) = match &arms[arm_idx].kind {
            SelectArmKind::ActorAsk {
                actor,
                msg_type,
                value,
                ..
            } => {
                let actor_ptr = load_duplex_handle(fn_ctx, *actor, "select_actor_handle")?;
                let (payload_ptr, payload_size) =
                    actor_payload_ptr_size(fn_ctx, *value, "select_ask_payload")?;
                let msg_type_val = i32_ty.const_int(*msg_type as u64, false);
                let status = fn_ctx
                    .builder
                    .build_call(
                        ask_with_channel,
                        &[
                            actor_ptr.into(),
                            msg_type_val.into(),
                            payload_ptr.into(),
                            payload_size.into(),
                            ch_val.into(),
                        ],
                        &format!("select_ask_issue_{slot_idx}"),
                    )
                    .map_err(|e| {
                        CodegenError::Llvm(format!("hew_actor_ask_with_channel call: {e:?}"))
                    })?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("hew_actor_ask_with_channel returned void".into())
                    })?
                    .into_int_value();
                (status, false)
            }
            SelectArmKind::StreamNext { stream } => {
                let binding_place = arms[arm_idx].binding.ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "select{{}} StreamNext arm {arm_idx} carries no binding Place (the producer must \
                         populate `SelectArm.binding` with the stream item slot)"
                    ))
                })?;
                let (_, item_ty) = place_pointer(fn_ctx, binding_place)?;
                let stream_callback = get_or_create_select_stream_callback(fn_ctx, item_ty)?;
                fn_ctx
                    .builder
                    .build_call(
                        channel_retain,
                        &[ch_val.into()],
                        &format!("select_stream_ch_retain_{slot_idx}"),
                    )
                    .map_err(|e| CodegenError::Llvm(format!("stream channel retain: {e:?}")))?;
                let stream_ptr = load_duplex_handle(fn_ctx, *stream, "select_stream_handle")?;
                let pending_id = fn_ctx
                    .builder
                    .build_call(
                        stream_poll,
                        &[
                            stream_ptr.into(),
                            stream_callback.as_global_value().as_pointer_value().into(),
                            ch_val.into(),
                        ],
                        &format!("select_stream_poll_{slot_idx}"),
                    )
                    .map_err(|e| CodegenError::Llvm(format!("hew_stream_poll call: {e:?}")))?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("hew_stream_poll returned void".into())
                    })?
                    .into_int_value();
                let pending_slot = pending_id_slot_ptr(slot_idx)?;
                fn_ctx
                    .builder
                    .build_store(pending_slot, pending_id)
                    .map_err(|e| CodegenError::Llvm(format!("pending-read id store: {e:?}")))?;
                let failed = fn_ctx
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        pending_id,
                        i64_ty.const_zero(),
                        &format!("select_stream_poll_failed_{slot_idx}"),
                    )
                    .map_err(|e| CodegenError::Llvm(format!("stream poll cmp: {e:?}")))?;
                let status = fn_ctx
                    .builder
                    .build_int_z_extend(failed, i32_ty, &format!("select_stream_status_{slot_idx}"))
                    .map_err(|e| CodegenError::Llvm(format!("stream poll status zext: {e:?}")))?;
                (status, true)
            }
            SelectArmKind::TaskAwait { task } => {
                fn_ctx
                    .builder
                    .build_call(
                        channel_retain,
                        &[ch_val.into()],
                        &format!("select_task_ch_retain_{slot_idx}"),
                    )
                    .map_err(|e| CodegenError::Llvm(format!("task channel retain: {e:?}")))?;
                let scope_ptr = load_current_task_scope(fn_ctx)?;
                let task_ptr = load_duplex_handle(fn_ctx, *task, "select_task_handle")?;
                let status = fn_ctx
                    .builder
                    .build_call(
                        task_completion_observe,
                        &[
                            scope_ptr.into(),
                            task_ptr.into(),
                            signal_ready.as_global_value().as_pointer_value().into(),
                            ch_val.into(),
                        ],
                        &format!("select_task_observe_{slot_idx}"),
                    )
                    .map_err(|e| {
                        CodegenError::Llvm(format!("hew_task_completion_observe call: {e:?}"))
                    })?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("hew_task_completion_observe returned void".into())
                    })?
                    .into_int_value();
                (status, true)
            }
            SelectArmKind::AfterTimer { .. } => {
                unreachable!("wait_arm_indices excludes AfterTimer")
            }
        };

        // Branch on status: 0 → next ask setup or select_first; non-zero
        // → mid-setup recovery (free every channel allocated through
        // `slot_idx` inclusive, then trap).
        let zero = i32_ty.const_zero();
        let failed = fn_ctx
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                status,
                zero,
                &format!("select_ask_failed_{slot_idx}"),
            )
            .map_err(|e| CodegenError::Llvm(format!("select ask cmp: {e:?}")))?;
        let setup_ok_bb = ctx.append_basic_block(parent_fn, &format!("select_setup_ok_{slot_idx}"));
        let setup_fail_bb =
            ctx.append_basic_block(parent_fn, &format!("select_setup_fail_{slot_idx}"));
        fn_ctx
            .builder
            .build_conditional_branch(failed, setup_fail_bb, setup_ok_bb)
            .map_err(|e| CodegenError::Llvm(format!("select setup br: {e:?}")))?;

        // Recovery block: free channels [0..=slot_idx] (each was
        // allocated and stored; the ask-issue failure also released
        // the queued sender ref on the failing channel inside the
        // runtime, but the caller-side ref is still live — see
        // `submit_ask_with_reply_channel`'s KeepCreatorRef behaviour).
        // Cancel before free for any channel where an ask was
        // successfully submitted (i < slot_idx); the failing channel
        // (slot_idx) only needs `free`.
        fn_ctx.builder.position_at_end(setup_fail_bb);
        for (j, &prev_arm_idx) in wait_arm_indices.iter().enumerate().take(slot_idx) {
            let cleanup_slot = slot_ptr(j)?;
            let cleanup_ch = fn_ctx
                .builder
                .build_load(ptr_ty, cleanup_slot, &format!("select_cleanup_load_{j}"))
                .map_err(|e| CodegenError::Llvm(format!("setup-fail load: {e:?}")))?
                .into_pointer_value();
            match &arms[prev_arm_idx].kind {
                SelectArmKind::StreamNext { stream } => {
                    let stream_ptr = load_duplex_handle(fn_ctx, *stream, "select_cleanup_stream")?;
                    let pending_slot = pending_id_slot_ptr(j)?;
                    let pending_id = fn_ctx
                        .builder
                        .build_load(
                            i64_ty,
                            pending_slot,
                            &format!("select_cleanup_pending_id_{j}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("setup-fail pending id load: {e:?}"))
                        })?
                        .into_int_value();
                    fn_ctx
                        .builder
                        .build_call(
                            stream_cancel,
                            &[stream_ptr.into(), pending_id.into()],
                            &format!("select_cleanup_stream_cancel_{j}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("setup-fail stream cancel: {e:?}"))
                        })?;
                }
                SelectArmKind::TaskAwait { task } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[cleanup_ch.into()],
                            &format!("select_cleanup_cancel_{j}"),
                        )
                        .map_err(|e| CodegenError::Llvm(format!("setup-fail cancel: {e:?}")))?;
                    let scope_ptr = load_current_task_scope(fn_ctx)?;
                    let task_ptr = load_duplex_handle(fn_ctx, *task, "select_cleanup_task")?;
                    fn_ctx
                        .builder
                        .build_call(
                            task_completion_unobserve,
                            &[
                                scope_ptr.into(),
                                task_ptr.into(),
                                signal_ready.as_global_value().as_pointer_value().into(),
                                cleanup_ch.into(),
                            ],
                            &format!("select_cleanup_task_unobserve_{j}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("setup-fail task unobserve: {e:?}"))
                        })?;
                }
                SelectArmKind::ActorAsk { .. } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[cleanup_ch.into()],
                            &format!("select_cleanup_cancel_{j}"),
                        )
                        .map_err(|e| CodegenError::Llvm(format!("setup-fail cancel: {e:?}")))?;
                }
                SelectArmKind::AfterTimer { .. } => {
                    unreachable!("wait_arm_indices excludes AfterTimer")
                }
            }
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[cleanup_ch.into()],
                    &format!("select_cleanup_free_{j}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("setup-fail free: {e:?}")))?;
        }
        // Failing channel: free the caller-side ref (no cancel — no
        // ask was successfully submitted, so no late replier exists).
        fn_ctx
            .builder
            .build_call(
                channel_free,
                &[ch_val.into()],
                &format!("select_setup_fail_free_self_{slot_idx}"),
            )
            .map_err(|e| CodegenError::Llvm(format!("setup-fail self free: {e:?}")))?;
        if current_has_retained_observer {
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[ch_val.into()],
                    &format!("select_setup_fail_free_observer_{slot_idx}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("setup-fail observer free: {e:?}")))?;
        }

        // Trap with HEW_TRAP_ACTOR_SEND_FAILED (the same diagnostic
        // code `Terminator::Send` uses on send-status failure — the
        // ask submission failed because the receiver mailbox was
        // unreachable, which is the same supervisor-visible failure).
        emit_select_setup_failure_trap(fn_ctx)?;

        fn_ctx.builder.position_at_end(setup_ok_bb);
    }

    // Determine the deadline. If the select carries an AfterTimer arm,
    // load its duration (i64 ns), convert to i32 ms (saturating). With
    // no AfterTimer arm, use -1 (wait indefinitely).
    let timeout_ms_val = if let Some(idx) = after_arm_index {
        let duration_place = match &arms[idx].kind {
            SelectArmKind::AfterTimer { duration } => *duration,
            _ => unreachable!("after_arm_index only set for AfterTimer arms"),
        };
        let (dur_ptr, dur_ty) = place_pointer(fn_ctx, duration_place)?;
        let dur_ns = fn_ctx
            .builder
            .build_load(dur_ty, dur_ptr, "select_after_dur_ns")
            .map_err(|e| CodegenError::Llvm(format!("select after dur load: {e:?}")))?
            .into_int_value();
        // ns → ms = dur_ns / 1_000_000.
        let ms_per_ns = i64_ty.const_int(1_000_000, false);
        let dur_ms_i64 = fn_ctx
            .builder
            .build_int_signed_div(dur_ns, ms_per_ns, "select_after_dur_ms_i64")
            .map_err(|e| CodegenError::Llvm(format!("select after dur sdiv: {e:?}")))?;
        // Saturate to i32::MAX (the runtime ABI is i32; a duration
        // > ~24.8 days clamps to "effectively wait forever" but stays
        // non-negative so `hew_select_first` doesn't interpret it as
        // -1 = infinite).
        let i32_max_as_i64 = i64_ty.const_int(i64::from(i32::MAX) as u64, true);
        let too_big = fn_ctx
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SGT,
                dur_ms_i64,
                i32_max_as_i64,
                "select_after_dur_too_big",
            )
            .map_err(|e| CodegenError::Llvm(format!("select after dur cmp: {e:?}")))?;
        let clamped = fn_ctx
            .builder
            .build_select(
                too_big,
                i32_max_as_i64,
                dur_ms_i64,
                "select_after_dur_clamped",
            )
            .map_err(|e| CodegenError::Llvm(format!("select after dur select: {e:?}")))?
            .into_int_value();
        // Negative durations clamp to 0 (immediate timeout) so we
        // never accidentally collide with the -1 wait-forever
        // sentinel.
        let zero_i64 = i64_ty.const_zero();
        let neg = fn_ctx
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLT,
                clamped,
                zero_i64,
                "select_after_dur_neg",
            )
            .map_err(|e| CodegenError::Llvm(format!("select after dur neg cmp: {e:?}")))?;
        let clamped_nonneg = fn_ctx
            .builder
            .build_select(neg, zero_i64, clamped, "select_after_dur_nonneg")
            .map_err(|e| CodegenError::Llvm(format!("select after dur nonneg select: {e:?}")))?
            .into_int_value();
        fn_ctx
            .builder
            .build_int_truncate(clamped_nonneg, i32_ty, "select_after_dur_ms")
            .map_err(|e| CodegenError::Llvm(format!("select after dur trunc: {e:?}")))?
    } else {
        // No AfterTimer arm: wait indefinitely.
        i32_ty.const_int(u64::from(u32::MAX), true) // -1 in two's complement
    };

    // GEP the array down to a `*mut *mut HewReplyChannel` for the call.
    let idx0 = i32_ty.const_zero();
    let arr_first_ptr = unsafe {
        fn_ctx
            .builder
            .build_gep(arr_ty, arr_ptr, &[idx0, idx0], "select_channels_first")
            .map_err(|e| CodegenError::Llvm(format!("select arr first gep: {e:?}")))?
    };
    let count_val = i32_ty.const_int(n_waits_i32 as u64, true);
    let winner = fn_ctx
        .builder
        .build_call(
            select_first,
            &[
                arr_first_ptr.into(),
                count_val.into(),
                timeout_ms_val.into(),
            ],
            "select_winner_idx",
        )
        .map_err(|e| CodegenError::Llvm(format!("hew_select_first call: {e:?}")))?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_select_first returned void".into()))?
        .into_int_value();

    // Allocate per-arm winner blocks and an AfterTimer-winner block
    // (if applicable). Each winner block handles its own loser
    // cleanup, reply read, and branch to the MIR-allocated body block.
    let mut winner_bbs: Vec<inkwell::basic_block::BasicBlock<'_>> =
        Vec::with_capacity(wait_arm_indices.len());
    for slot_idx in 0..wait_arm_indices.len() {
        winner_bbs.push(ctx.append_basic_block(parent_fn, &format!("select_win_ask_{slot_idx}")));
    }
    let after_winner_bb =
        after_arm_index.map(|_| ctx.append_basic_block(parent_fn, "select_win_after"));
    // Fallback "unreachable" block: -1 with no AfterTimer arm should
    // never happen because we passed -1 timeout, but defence-in-depth.
    let no_winner_bb = ctx.append_basic_block(parent_fn, "select_no_winner_trap");

    // Build the switch on `winner`:
    //   case 0 → winner_bbs[0]
    //   case 1 → winner_bbs[1]
    //   ...
    //   default → after_winner_bb (if present) else no_winner_bb
    let default_bb = after_winner_bb.unwrap_or(no_winner_bb);
    let cases: Vec<(IntValue<'_>, inkwell::basic_block::BasicBlock<'_>)> = winner_bbs
        .iter()
        .enumerate()
        .map(|(slot_idx, bb)| (i32_ty.const_int(slot_idx as u64, true), *bb))
        .collect();
    fn_ctx
        .builder
        .build_switch(winner, default_bb, &cases)
        .map_err(|e| CodegenError::Llvm(format!("select winner switch: {e:?}")))?;

    // No-winner fallback: trap. (Reachable only if the runtime
    // contract is violated — `hew_select_first` returned a value
    // outside `0..n_asks` and there was no AfterTimer arm.)
    fn_ctx.builder.position_at_end(no_winner_bb);
    emit_select_no_winner_trap(fn_ctx)?;

    // For each ActorAsk winner block: read the reply, cancel+free
    // every loser, branch to the arm body block.
    for (winner_slot, &arm_idx) in wait_arm_indices.iter().enumerate() {
        let arm = &arms[arm_idx];
        let binding_place = arm.binding.ok_or_else(|| {
            CodegenError::FailClosed(format!(
                 "select{{}} value-producing arm {arm_idx} carries no binding Place (the producer must \
                 populate `SelectArm.binding` with the per-arm reply slot)"
            ))
        })?;
        let body_block_id = arm.body_block;

        fn_ctx.builder.position_at_end(winner_bbs[winner_slot]);

        // Load the winning channel from the array.
        let win_slot_ptr = slot_ptr(winner_slot)?;
        let win_ch = fn_ctx
            .builder
            .build_load(
                ptr_ty,
                win_slot_ptr,
                &format!("select_win_ch_load_{winner_slot}"),
            )
            .map_err(|e| CodegenError::Llvm(format!("select winner load: {e:?}")))?
            .into_pointer_value();

        let (dest_ptr, dest_ty) = place_pointer(fn_ctx, binding_place)?;
        let task_result_ptr = if matches!(arm.kind, SelectArmKind::TaskAwait { .. }) {
            let task_place = match &arm.kind {
                SelectArmKind::TaskAwait { task } => *task,
                _ => unreachable!("checked above"),
            };
            let task_ptr = load_duplex_handle(fn_ctx, task_place, "select_task_winner")?;
            Some(
                fn_ctx
                    .builder
                    .build_call(
                        task_get_result,
                        &[task_ptr.into()],
                        &format!("select_task_get_result_{winner_slot}"),
                    )
                    .map_err(|e| CodegenError::Llvm(format!("hew_task_get_result call: {e:?}")))?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("hew_task_get_result returned void".into())
                    })?
                    .into_pointer_value(),
            )
        } else {
            None
        };
        let reply_ptr = if let Some(result_ptr) = task_result_ptr {
            result_ptr
        } else {
            fn_ctx
                .builder
                .build_call(
                    reply_wait,
                    &[win_ch.into()],
                    &format!("select_reply_wait_{winner_slot}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("hew_reply_wait call: {e:?}")))?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_reply_wait returned void".into()))?
                .into_pointer_value()
        };

        // Defensive null check on the reply pointer. `hew_reply_wait`
        // returns null when the published value pointer was null —
        // which happens on the allocation-failure-during-publish path
        // (`take_ready_reply` at `hew-runtime/src/reply_channel.rs:164`
        // returns null after `hew_reply_channel_mark_allocation_failed`
        // toggled the flag). `hew_select_first` will route us here
        // because the channel reached `ready=true` with a null value;
        // dereferencing would SIGSEGV. Mirrors `Terminator::Ask`'s
        // null-trap shape (`llvm.rs:4831-4854`).
        let null_bb =
            ctx.append_basic_block(parent_fn, &format!("select_reply_null_trap_{winner_slot}"));
        let ok_bb = ctx.append_basic_block(parent_fn, &format!("select_reply_ok_{winner_slot}"));
        let is_null = fn_ctx
            .builder
            .build_is_null(reply_ptr, &format!("select_reply_is_null_{winner_slot}"))
            .map_err(|e| CodegenError::Llvm(format!("select reply null cmp: {e:?}")))?;
        fn_ctx
            .builder
            .build_conditional_branch(is_null, null_bb, ok_bb)
            .map_err(|e| CodegenError::Llvm(format!("select reply null branch: {e:?}")))?;
        // Null-reply trap branch.
        fn_ctx.builder.position_at_end(null_bb);
        let trap_intrinsic = Intrinsic::find("llvm.trap").ok_or_else(|| {
            CodegenError::Llvm("llvm.trap intrinsic not found in LLVM build".into())
        })?;
        let trap_fn = trap_intrinsic
            .get_declaration(fn_ctx.llvm_mod, &[])
            .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
        fn_ctx
            .builder
            .build_call(
                trap_fn,
                &[],
                &format!("select_reply_null_trap_call_{winner_slot}"),
            )
            .map_err(|e| CodegenError::Llvm(format!("select reply null trap: {e:?}")))?;
        fn_ctx
            .builder
            .build_unreachable()
            .map_err(|e| CodegenError::Llvm(format!("select reply null unreachable: {e:?}")))?;
        // Ok branch: load + store + frees.
        fn_ctx.builder.position_at_end(ok_bb);
        let reply_val = fn_ctx
            .builder
            .build_load(
                dest_ty,
                reply_ptr,
                &format!("select_reply_value_{winner_slot}"),
            )
            .map_err(|e| CodegenError::Llvm(format!("select reply load: {e:?}")))?;
        fn_ctx
            .builder
            .build_store(dest_ptr, reply_val)
            .map_err(|e| CodegenError::Llvm(format!("select reply store: {e:?}")))?;

        if task_result_ptr.is_none() {
            fn_ctx
                .builder
                .build_call(
                    libc_free,
                    &[reply_ptr.into()],
                    &format!("select_reply_free_{winner_slot}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("select reply free: {e:?}")))?;
        }

        // Free the winning channel's caller-side ref (no cancel — we
        // consumed its reply, the channel is not abandoned).
        fn_ctx
            .builder
            .build_call(
                channel_free,
                &[win_ch.into()],
                &format!("select_win_ch_free_{winner_slot}"),
            )
            .map_err(|e| CodegenError::Llvm(format!("select winner ch free: {e:?}")))?;

        // Loser cleanup: every OTHER ActorAsk arm gets cancel + free,
        // in that order (Risk R4 — cancel BEFORE free is the UAF
        // mitigation for late-reply races on cancelled channels).
        for (loser_slot, &loser_arm_idx) in wait_arm_indices.iter().enumerate() {
            if loser_slot == winner_slot {
                continue;
            }
            let loser_slot_ptr = slot_ptr(loser_slot)?;
            let loser_ch = fn_ctx
                .builder
                .build_load(
                    ptr_ty,
                    loser_slot_ptr,
                    &format!("select_loser_load_w{winner_slot}_l{loser_slot}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("select loser load: {e:?}")))?
                .into_pointer_value();
            match &arms[loser_arm_idx].kind {
                SelectArmKind::StreamNext { stream } => {
                    let stream_ptr = load_duplex_handle(fn_ctx, *stream, "select_loser_stream")?;
                    let pending_slot = pending_id_slot_ptr(loser_slot)?;
                    let pending_id = fn_ctx
                        .builder
                        .build_load(
                            i64_ty,
                            pending_slot,
                            &format!("select_loser_pending_id_w{winner_slot}_l{loser_slot}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("select loser pending id load: {e:?}"))
                        })?
                        .into_int_value();
                    fn_ctx
                        .builder
                        .build_call(
                            stream_cancel,
                            &[stream_ptr.into(), pending_id.into()],
                            &format!("select_loser_stream_cancel_w{winner_slot}_l{loser_slot}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("select loser stream cancel: {e:?}"))
                        })?;
                }
                SelectArmKind::TaskAwait { task } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[loser_ch.into()],
                            &format!("select_loser_cancel_w{winner_slot}_l{loser_slot}"),
                        )
                        .map_err(|e| CodegenError::Llvm(format!("select loser cancel: {e:?}")))?;
                    let scope_ptr = load_current_task_scope(fn_ctx)?;
                    let task_ptr = load_duplex_handle(fn_ctx, *task, "select_loser_task")?;
                    fn_ctx
                        .builder
                        .build_call(
                            task_completion_unobserve,
                            &[
                                scope_ptr.into(),
                                task_ptr.into(),
                                signal_ready.as_global_value().as_pointer_value().into(),
                                loser_ch.into(),
                            ],
                            &format!("select_loser_task_unobserve_w{winner_slot}_l{loser_slot}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("select loser task unobserve: {e:?}"))
                        })?;
                }
                SelectArmKind::ActorAsk { .. } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[loser_ch.into()],
                            &format!("select_loser_cancel_w{winner_slot}_l{loser_slot}"),
                        )
                        .map_err(|e| CodegenError::Llvm(format!("select loser cancel: {e:?}")))?;
                }
                SelectArmKind::AfterTimer { .. } => {
                    unreachable!("wait_arm_indices excludes AfterTimer")
                }
            }
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[loser_ch.into()],
                    &format!("select_loser_free_w{winner_slot}_l{loser_slot}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("select loser free: {e:?}")))?;
        }

        // Branch into the arm body block.
        let body_bb = *fn_ctx.blocks.get(&body_block_id).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "select{{}} ActorAsk arm {arm_idx} body block {body_block_id} \
                 missing from FnCtx.blocks"
            ))
        })?;
        fn_ctx
            .builder
            .build_unconditional_branch(body_bb)
            .map_err(|e| CodegenError::Llvm(format!("select win br: {e:?}")))?;
    }

    // AfterTimer winner: every ActorAsk arm is a loser; cancel + free
    // each, then branch to the AfterTimer arm's body block.
    if let (Some(after_idx), Some(after_bb)) = (after_arm_index, after_winner_bb) {
        let after_arm = &arms[after_idx];
        let body_block_id = after_arm.body_block;
        fn_ctx.builder.position_at_end(after_bb);
        for (loser_slot, &loser_arm_idx) in wait_arm_indices.iter().enumerate() {
            let loser_slot_ptr = slot_ptr(loser_slot)?;
            let loser_ch = fn_ctx
                .builder
                .build_load(
                    ptr_ty,
                    loser_slot_ptr,
                    &format!("select_after_loser_load_{loser_slot}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("select after loser load: {e:?}")))?
                .into_pointer_value();
            match &arms[loser_arm_idx].kind {
                SelectArmKind::StreamNext { stream } => {
                    let stream_ptr =
                        load_duplex_handle(fn_ctx, *stream, "select_after_loser_stream")?;
                    let pending_slot = pending_id_slot_ptr(loser_slot)?;
                    let pending_id = fn_ctx
                        .builder
                        .build_load(
                            i64_ty,
                            pending_slot,
                            &format!("select_after_loser_pending_id_{loser_slot}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("select after loser pending id load: {e:?}"))
                        })?
                        .into_int_value();
                    fn_ctx
                        .builder
                        .build_call(
                            stream_cancel,
                            &[stream_ptr.into(), pending_id.into()],
                            &format!("select_after_loser_stream_cancel_{loser_slot}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("select after loser stream cancel: {e:?}"))
                        })?;
                }
                SelectArmKind::TaskAwait { task } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[loser_ch.into()],
                            &format!("select_after_loser_cancel_{loser_slot}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("select after loser cancel: {e:?}"))
                        })?;
                    let scope_ptr = load_current_task_scope(fn_ctx)?;
                    let task_ptr = load_duplex_handle(fn_ctx, *task, "select_after_loser_task")?;
                    fn_ctx
                        .builder
                        .build_call(
                            task_completion_unobserve,
                            &[
                                scope_ptr.into(),
                                task_ptr.into(),
                                signal_ready.as_global_value().as_pointer_value().into(),
                                loser_ch.into(),
                            ],
                            &format!("select_after_loser_task_unobserve_{loser_slot}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("select after loser task unobserve: {e:?}"))
                        })?;
                }
                SelectArmKind::ActorAsk { .. } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[loser_ch.into()],
                            &format!("select_after_loser_cancel_{loser_slot}"),
                        )
                        .map_err(|e| {
                            CodegenError::Llvm(format!("select after loser cancel: {e:?}"))
                        })?;
                }
                SelectArmKind::AfterTimer { .. } => {
                    unreachable!("wait_arm_indices excludes AfterTimer")
                }
            }
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[loser_ch.into()],
                    &format!("select_after_loser_free_{loser_slot}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("select after loser free: {e:?}")))?;
        }
        let body_bb = *fn_ctx.blocks.get(&body_block_id).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "select{{}} AfterTimer arm {after_idx} body block {body_block_id} \
                 missing from FnCtx.blocks"
            ))
        })?;
        fn_ctx
            .builder
            .build_unconditional_branch(body_bb)
            .map_err(|e| CodegenError::Llvm(format!("select after br: {e:?}")))?;
    }

    Ok(())
}

/// Emit a fail-closed trap for a `Terminator::Select` mid-setup
/// failure (an ask-issue returned non-zero, leaving partially-allocated
/// channels which the recovery path frees before reaching here). Uses
/// `hew_trap_with_code(HEW_TRAP_ACTOR_SEND_FAILED=206)` so the
/// supervisor sees the same diagnostic as `Terminator::Send` failures;
/// follows with `llvm.trap` + `unreachable` as the non-actor fallback
/// (mirrors the `Terminator::Send` send-fail trap shape).
fn emit_select_setup_failure_trap<'ctx>(fn_ctx: &FnCtx<'_, 'ctx>) -> CodegenResult<()> {
    const HEW_TRAP_ACTOR_SEND_FAILED: u64 = 206;
    let trap_with_code_fn = match fn_ctx.llvm_mod.get_function("hew_trap_with_code") {
        Some(fv) => fv,
        None => {
            let i32_ty = fn_ctx.ctx.i32_type();
            let fn_ty = fn_ctx.ctx.void_type().fn_type(&[i32_ty.into()], false);
            fn_ctx
                .llvm_mod
                .add_function("hew_trap_with_code", fn_ty, Some(Linkage::External))
        }
    };
    let code = fn_ctx
        .ctx
        .i32_type()
        .const_int(HEW_TRAP_ACTOR_SEND_FAILED, false);
    fn_ctx
        .builder
        .build_call(trap_with_code_fn, &[code.into()], "select_setup_fail_trap")
        .map_err(|e| CodegenError::Llvm(format!("select setup fail trap: {e:?}")))?;
    let trap_intrinsic = Intrinsic::find("llvm.trap")
        .ok_or_else(|| CodegenError::Llvm("llvm.trap intrinsic not found in LLVM build".into()))?;
    let trap_fn = trap_intrinsic
        .get_declaration(fn_ctx.llvm_mod, &[])
        .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
    fn_ctx
        .builder
        .build_call(trap_fn, &[], "select_setup_fail_llvm_trap")
        .map_err(|e| CodegenError::Llvm(format!("select setup fail llvm.trap: {e:?}")))?;
    fn_ctx
        .builder
        .build_unreachable()
        .map_err(|e| CodegenError::Llvm(format!("select setup fail unreachable: {e:?}")))?;
    Ok(())
}

/// Emit a fail-closed trap for the unreachable "no winner" arm of the
/// select winner-dispatch switch. Reachable only if the runtime
/// contract is violated (e.g. `hew_select_first` returned -1 even
/// though we passed `-1` as the timeout and provided non-empty
/// channels).
fn emit_select_no_winner_trap<'ctx>(fn_ctx: &FnCtx<'_, 'ctx>) -> CodegenResult<()> {
    let trap_intrinsic = Intrinsic::find("llvm.trap")
        .ok_or_else(|| CodegenError::Llvm("llvm.trap intrinsic not found in LLVM build".into()))?;
    let trap_fn = trap_intrinsic
        .get_declaration(fn_ctx.llvm_mod, &[])
        .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
    fn_ctx
        .builder
        .build_call(trap_fn, &[], "select_no_winner_trap")
        .map_err(|e| CodegenError::Llvm(format!("select no winner trap: {e:?}")))?;
    fn_ctx
        .builder
        .build_unreachable()
        .map_err(|e| CodegenError::Llvm(format!("select no winner unreachable: {e:?}")))?;
    Ok(())
}

fn emit_cancel_trap_or_return(fn_ctx: &FnCtx<'_, '_>) -> CodegenResult<()> {
    match fn_ctx.return_ty {
        BasicTypeEnum::IntType(i) => {
            let ret = i.const_zero();
            fn_ctx
                .builder
                .build_return(Some(&ret))
                .map_err(|e| CodegenError::Llvm(format!("cancel return: {e:?}")))?;
        }
        BasicTypeEnum::PointerType(p) => {
            let ret = p.const_null();
            fn_ctx
                .builder
                .build_return(Some(&ret))
                .map_err(|e| CodegenError::Llvm(format!("cancel return: {e:?}")))?;
        }
        other => {
            return Err(CodegenError::FailClosed(format!(
                "cancel exit cannot synthesize return for {other:?}"
            )));
        }
    }
    Ok(())
}

fn emit_cancel_drops(
    fn_ctx: &FnCtx<'_, '_>,
    block_id: u32,
    drop_plans: &[(ExitPath, hew_mir::DropPlan)],
) -> CodegenResult<()> {
    if let Some((_, plan)) = drop_plans
        .iter()
        .find(|(exit, _)| matches!(exit, ExitPath::Cancel { block } if *block == block_id))
    {
        for drop in &plan.drops {
            emit_one_elab_drop(fn_ctx, drop)?;
        }
    }
    Ok(())
}

fn emit_cooperate_check<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    block_id: u32,
    drop_plans: &[(ExitPath, hew_mir::DropPlan)],
) -> CodegenResult<()> {
    let cooperate_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_cooperate",
    )?;
    let call = fn_ctx
        .builder
        .build_call(cooperate_fn, &[], "hew_actor_cooperate")
        .map_err(|e| CodegenError::Llvm(format!("hew_actor_cooperate call: {e:?}")))?;
    let signal = call
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_cooperate returned void".into()))?
        .into_int_value();
    let cancel_code = fn_ctx.ctx.i32_type().const_int(2, false);
    let is_cancel = fn_ctx
        .builder
        .build_int_compare(
            inkwell::IntPredicate::EQ,
            signal,
            cancel_code,
            "hew_cooperate_is_cancel",
        )
        .map_err(|e| CodegenError::Llvm(format!("cooperate cancel compare: {e:?}")))?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::Llvm("cooperate check outside function".into()))?;
    let cancel_bb = fn_ctx.ctx.append_basic_block(parent, "cancel_exit");
    let continue_bb = fn_ctx.ctx.append_basic_block(parent, "after_cooperate");
    fn_ctx
        .builder
        .build_conditional_branch(is_cancel, cancel_bb, continue_bb)
        .map_err(|e| CodegenError::Llvm(format!("cooperate cancel branch: {e:?}")))?;

    fn_ctx.builder.position_at_end(cancel_bb);
    emit_cancel_drops(fn_ctx, block_id, drop_plans)?;
    emit_cancel_trap_or_return(fn_ctx)?;

    fn_ctx.builder.position_at_end(continue_bb);
    Ok(())
}

fn cooperate_site_matches(site: &CooperateSite, block_id: u32, kind: CooperateKind) -> bool {
    site.bb_id == block_id && site.kind == kind
}

// ---------------------------------------------------------------------------
// Per-function declaration + body lowering
// ---------------------------------------------------------------------------

fn declare_function<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    func: &RawMirFunction,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<FnSymbol<'ctx>> {
    let linkage = if func.name == "main" {
        Some(Linkage::External)
    } else {
        Some(Linkage::Internal)
    };
    let return_ty_llvm = resolve_ty(ctx, &func.return_ty, record_layouts)?;
    // Accept integer, pointer, and struct return types. Integer covers
    // the original Cluster 1 spine; pointer covers `String` (a
    // `*mut c_char` / opaque `ptr` in LLVM IR) which is lowerable via
    // `Instr::StringLit`. Struct returns are needed by synthesised
    // machine `__step(self, event) -> <Name>` functions, which return
    // the tagged-union machine struct by value (Slice 5). LLVM emits
    // these as natural-ABI struct returns; the caller-side store-back
    // is wired by the `m.step(ev)` slice. Bool/Unit/F32/F64 reach
    // `primitive_to_llvm` with valid shapes but the MIR `Instr` stream
    // cannot populate the return slot for them — the function body
    // would emit `ret` against an uninitialised alloca. Reject those at
    // declaration time so the failure is loud and well-located.
    // LESSONS: `boundary-fail-closed`.
    if !matches!(
        return_ty_llvm,
        BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_) | BasicTypeEnum::StructType(_)
    ) {
        return Err(CodegenError::Unsupported(
            "only integer-, pointer-, and struct-returning functions are lowerable; \
             non-integer/non-pointer/non-struct return types are out of the current spine subset",
        ));
    }
    // Resolve parameter types from `func.params`. Each parameter type must
    // be representable as a `BasicMetadataTypeEnum` (integers and pointers
    // are both legal LLVM value-param types for the current spine subset).
    // The parameter-prologue in `lower_function` stores each LLVM function
    // argument into the corresponding `locals[i]` alloca slot.
    let ctx_ptr_ty = ctx.ptr_type(AddressSpace::default());
    let mut param_tys: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(
        func.params.len() + usize::from(func.call_conv.carries_execution_context()),
    );
    if func.call_conv.carries_execution_context() {
        param_tys.push(ctx_ptr_ty.into());
    }
    for param_ty in &func.params {
        let llvm_ty = resolve_ty(ctx, param_ty, record_layouts)?;
        param_tys.push(metadata_type_from_basic(llvm_ty));
    }
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
    Ok(FnSymbol::Real {
        value: llvm_fn,
        return_ty: return_ty_llvm,
        returns_unit: matches!(func.return_ty, ResolvedTy::Unit),
    })
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
///
/// `checked` carries the cooperate-site side table computed by the MIR
/// dataflow pass. Empty/missing checked MIR means no cooperate injection
/// for legacy hand-built codegen tests; full lowered pipelines always carry
/// a matching checked function.
#[expect(
    clippy::too_many_arguments,
    reason = "module-lowering context is deliberately passed as explicit borrows"
)]
fn lower_function<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    func: &RawMirFunction,
    fn_symbols: &FnSymbolMap<'ctx>,
    elab: Option<&ElaboratedMirFunction>,
    checked: Option<&CheckedMirFunction>,
    record_layouts: &RecordLayoutMap<'ctx>,
    actor_layouts: &[ActorLayout],
    machine_layouts: &MachineLayoutMap<'ctx>,
) -> CodegenResult<()> {
    let symbol = *fn_symbols.get(&func.name).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "function `{}` was not declared before body lowering",
            func.name
        ))
    })?;
    let (llvm_fn, return_ty_llvm, _returns_unit) = symbol.real(&func.name, "lower_function")?;
    let context_marker_findings = validate_context_markers(func);
    if !context_marker_findings.is_empty() {
        return Err(CodegenError::FailClosed(format!(
            "function `{}` has invalid execution-context markers: {context_marker_findings:?}",
            func.name
        )));
    }
    let builder = ctx.create_builder();

    let entry_block = func.blocks.first().ok_or_else(|| {
        CodegenError::FailClosed(format!("function `{}` has zero blocks", func.name))
    })?;

    // Keep the LLVM entry block predecessor-free even when MIR loops target
    // bb0. The prologue holds allocas, parameter stores, and the function-entry
    // cooperate call, then branches into the first MIR block.
    let prologue_bb = ctx.append_basic_block(llvm_fn, "entry");

    // Build every MIR block up front so terminators can name forward targets.
    let mut blocks = HashMap::new();
    for block in &func.blocks {
        let bb = ctx.append_basic_block(llvm_fn, &format!("bb{}", block.id));
        blocks.insert(block.id, bb);
    }

    let entry_bb = *blocks.get(&entry_block.id).expect("entry block in map");
    builder.position_at_end(prologue_bb);

    let return_slot = builder
        .build_alloca(return_ty_llvm, "return_slot")
        .map_err(|e| CodegenError::Llvm(format!("alloca return_slot: {e:?}")))?;

    let execution_context = if func.call_conv.carries_execution_context() {
        let param = llvm_fn.get_nth_param(0).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "context-bearing function `{}` has no leading execution-context LLVM parameter",
                func.name
            ))
        })?;
        Some(param.into_pointer_value())
    } else {
        None
    };

    let mut locals: HashMap<u32, (PointerValue, BasicTypeEnum)> = HashMap::new();
    let mut local_tys: HashMap<u32, ResolvedTy> = HashMap::new();
    for (idx, ty) in func.locals.iter().enumerate() {
        // Use `resolve_ty` (not bare `primitive_to_llvm`) so record-typed
        // locals materialise as struct allocas rather than tripping the
        // D10-violation arm. Record-typed locals get an alloca sized to
        // the full struct; codegen for `RecordInit` writes into this slot
        // in place via per-field GEP+store.
        let llvm_ty = resolve_ty(ctx, ty, record_layouts)?;
        let idx_u32 = u32::try_from(idx).map_err(|_| {
            CodegenError::FailClosed("function exceeds u32::MAX locals — impossible".into())
        })?;
        let slot = builder
            .build_alloca(llvm_ty, &format!("local_{idx}"))
            .map_err(|e| CodegenError::Llvm(format!("alloca local {idx}: {e:?}")))?;
        locals.insert(idx_u32, (slot, llvm_ty));
        local_tys.insert(idx_u32, ty.clone());
    }

    // Parameter prologue: store each LLVM function argument into the
    // corresponding `locals[i]` alloca slot. Parameters occupy
    // `locals[0..func.params.len()]` by the invariant established in
    // `RawMirFunction` (see `lower_params` in hew-mir/src/lower.rs).
    // This runs after all allocas so the store is always in the entry
    // block and dominates every use in successor blocks.
    for (param_idx, _param_ty) in func.params.iter().enumerate() {
        let param_idx_u32 = u32::try_from(param_idx).map_err(|_| {
            CodegenError::FailClosed("function exceeds u32::MAX params — impossible".into())
        })?;
        let llvm_param_idx = param_idx + usize::from(func.call_conv.carries_execution_context());
        let llvm_param = llvm_fn
            .get_nth_param(llvm_param_idx as u32)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "function `{}` has no LLVM param at index {llvm_param_idx}; \
                     declare_function and lower_function param counts disagree",
                    func.name
                ))
            })?;
        let (slot, _slot_ty) = *locals.get(&param_idx_u32).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "function `{}` has no local slot for param index {param_idx}",
                func.name
            ))
        })?;
        builder
            .build_store(slot, llvm_param)
            .map_err(|e| CodegenError::Llvm(format!("param store {param_idx}: {e:?}")))?;
    }

    let actor_state_ty = if func.call_conv == FunctionCallConv::ActorHandler {
        actor_name_from_handler_symbol(&func.name)
            .and_then(|actor_name| record_layouts.get(actor_name).copied())
    } else {
        None
    };

    let fn_ctx = FnCtx {
        ctx,
        llvm_mod,
        builder,
        return_slot,
        return_ty: return_ty_llvm,
        return_resolved_ty: func.return_ty.clone(),
        execution_context,
        execution_context_is_actor_handler: func.call_conv == FunctionCallConv::ActorHandler,
        actor_state_ty,
        locals,
        local_tys,
        blocks: blocks.clone(),
        runtime_decls: RefCell::new(HashMap::new()),
        record_layouts,
        fn_symbols,
        actor_layouts,
        machine_layouts,
    };

    // Extract drop_plans from the matched elaborated function, or use an
    // empty slice when no elaborated function is available (hand-built test
    // pipelines that inline Instr::Drop instead of using drop_plans).
    let empty_plans: Vec<(ExitPath, hew_mir::DropPlan)> = Vec::new();
    let drop_plans: &[(ExitPath, hew_mir::DropPlan)] = elab
        .map(|e| e.drop_plans.as_slice())
        .unwrap_or(empty_plans.as_slice());
    let cooperate_sites: &[CooperateSite] =
        checked.map(|c| c.cooperate_sites.as_slice()).unwrap_or(&[]);
    if let Some(site) = cooperate_sites
        .iter()
        .find(|site| site.kind == CooperateKind::FunctionEntry && site.bb_id != entry_block.id)
    {
        return Err(CodegenError::FailClosed(format!(
            "function `{}` has FunctionEntry cooperate site on bb{}, expected bb{}",
            func.name, site.bb_id, entry_block.id
        )));
    }
    if let Some(site) = cooperate_sites
        .iter()
        .find(|site| site.kind == CooperateKind::LoopBackEdge && !blocks.contains_key(&site.bb_id))
    {
        return Err(CodegenError::FailClosed(format!(
            "function `{}` has LoopBackEdge cooperate site on missing bb{}",
            func.name, site.bb_id
        )));
    }

    if cooperate_sites
        .iter()
        .any(|site| cooperate_site_matches(site, entry_block.id, CooperateKind::FunctionEntry))
    {
        emit_cooperate_check(&fn_ctx, entry_block.id, drop_plans)?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(entry_bb)
        .map_err(|e| CodegenError::Llvm(format!("prologue br: {e:?}")))?;

    for block in &func.blocks {
        let bb = *blocks.get(&block.id).expect("block in map");
        fn_ctx.builder.position_at_end(bb);
        for instr in &block.instructions {
            lower_instruction(&fn_ctx, instr, block.id, drop_plans)?;
        }
        if cooperate_sites
            .iter()
            .any(|site| cooperate_site_matches(site, block.id, CooperateKind::LoopBackEdge))
        {
            emit_cooperate_check(&fn_ctx, block.id, drop_plans)?;
        }
        // Emit LIFO drops from the elaborated drop plan BEFORE the
        // terminator so the alloca null-stores precede the ret/br.
        // LESSONS: cleanup-all-exits (P0), lifecycle-symmetry (P0).
        emit_elab_drops(&fn_ctx, block.id, drop_plans)?;
        lower_terminator(&fn_ctx, fn_symbols, &block.terminator)?;
    }

    Ok(())
}

/// Emit a C-ABI terminate trampoline for an actor's `#[on(stop)]` hooks.
///
/// The runtime's `terminate_fn` slot has ABI `fn(*mut c_void state) -> void`.
/// Each ActorHandler-lowered `__on_stop__<i>` function has ABI
/// `fn(*mut HewExecutionContext) -> void`. This trampoline bridges the two
/// and fans out to all stop hooks in lexical declaration order:
///
/// 1. Call `hew_require_execution_context()` to get the context already
///    installed by `call_terminate_fn` before this trampoline is entered.
/// 2. Load the actor pointer from context offset 0 (HEW_CTX_OFFSET_ACTOR).
/// 3. Acquire the actor-state lock via `hew_actor_state_lock_acquire`.
/// 4. For each `<Actor>__on_stop__<i>` in declaration order, call it with
///    the ActorHandler ABI (ctx as first arg).
/// 5. Release the lock via `hew_actor_state_lock_release`.
///
/// The `state` parameter is unused — the execution context carries the actor
/// pointer (offset 0) and all other dispatch-substrate state. `state` is
/// present only to satisfy the `terminate_fn: fn(*mut c_void) -> void` ABI.
///
/// Panic safety: if any on(stop) body panics, `call_terminate_fn`'s
/// `catch_unwind` catches it and releases the lock via
/// `hew_actor_state_lock_release_after_panic` (LESSONS: cleanup-all-exits P0).
fn emit_actor_terminate_trampoline<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    layout: &ActorLayout,
    fn_symbols: &FnSymbolMap<'ctx>,
) -> CodegenResult<()> {
    // Resolve all per-hook LLVM functions up front so we fail-closed before
    // emitting any IR.
    let mut on_stop_fns = Vec::with_capacity(layout.on_stop_symbols.len());
    for sym in &layout.on_stop_symbols {
        let entry = fn_symbols.get(sym).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "terminate trampoline for `{}` references undeclared on_stop handler `{sym}`",
                layout.name
            ))
        })?;
        let (fn_val, _, _) = entry.real(sym, "terminate trampoline")?;
        on_stop_fns.push(fn_val);
    }

    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let trampoline_name = format!("__terminate_{}", layout.name);
    let fn_ty = ctx.void_type().fn_type(&[ptr_ty.into()], false);
    let trampoline_fn = llvm_mod.add_function(&trampoline_name, fn_ty, Some(Linkage::Internal));
    let builder = ctx.create_builder();
    let entry = ctx.append_basic_block(trampoline_fn, "entry");
    builder.position_at_end(entry);

    // Declare required runtime functions inline (the trampoline has no FnCtx).
    let require_ctx_ty = ptr_ty.fn_type(&[], false);
    let require_ctx_fn = llvm_mod
        .get_function("hew_require_execution_context")
        .unwrap_or_else(|| {
            llvm_mod.add_function("hew_require_execution_context", require_ctx_ty, None)
        });
    let lock_acquire_ty = ctx.i32_type().fn_type(&[ptr_ty.into()], false);
    let lock_acquire_fn = llvm_mod
        .get_function("hew_actor_state_lock_acquire")
        .unwrap_or_else(|| {
            llvm_mod.add_function("hew_actor_state_lock_acquire", lock_acquire_ty, None)
        });
    let lock_release_ty = ctx.i32_type().fn_type(&[ptr_ty.into()], false);
    let lock_release_fn = llvm_mod
        .get_function("hew_actor_state_lock_release")
        .unwrap_or_else(|| {
            llvm_mod.add_function("hew_actor_state_lock_release", lock_release_ty, None)
        });

    // Get the current execution context (installed by call_terminate_fn).
    let ctx_ptr = builder
        .build_call(require_ctx_fn, &[], "terminate_ctx")
        .map_err(|e| CodegenError::Llvm(format!("terminate trampoline: require_ctx call: {e:?}")))?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_require_execution_context returned void".into())
        })?
        .into_pointer_value();

    // Load the actor pointer from context offset 0 (HEW_CTX_OFFSET_ACTOR).
    let actor_ptr = unsafe {
        builder
            .build_gep(
                ctx.i8_type(),
                ctx_ptr,
                &[ctx.i64_type().const_int(HEW_CTX_OFFSET_ACTOR as u64, false)],
                "terminate_actor_slot",
            )
            .map_err(|e| CodegenError::Llvm(format!("terminate trampoline: actor gep: {e:?}")))?
    };
    let actor_val = builder
        .build_load(ptr_ty, actor_ptr, "terminate_actor")
        .map_err(|e| CodegenError::Llvm(format!("terminate trampoline: actor load: {e:?}")))?
        .into_pointer_value();

    // Acquire the actor-state lock (same protocol as dispatch trampolines).
    builder
        .build_call(
            lock_acquire_fn,
            &[actor_val.into()],
            "terminate_lock_acquire",
        )
        .map_err(|e| CodegenError::Llvm(format!("terminate trampoline: lock acquire: {e:?}")))?;

    // Call each on(stop) handler in lexical declaration order with the
    // ActorHandler ABI (ctx as first arg). All hooks share the single
    // acquire/release pair above.
    for (i, on_stop_fn) in on_stop_fns.iter().enumerate() {
        builder
            .build_call(
                *on_stop_fn,
                &[ctx_ptr.into()],
                &format!("terminate_on_stop_call_{i}"),
            )
            .map_err(|e| {
                CodegenError::Llvm(format!("terminate trampoline: on_stop[{i}] call: {e:?}"))
            })?;
    }

    // Release the actor-state lock.
    builder
        .build_call(
            lock_release_fn,
            &[actor_val.into()],
            "terminate_lock_release",
        )
        .map_err(|e| CodegenError::Llvm(format!("terminate trampoline: lock release: {e:?}")))?;

    builder
        .build_return(None)
        .map_err(|e| CodegenError::Llvm(format!("terminate trampoline: return: {e:?}")))?;

    Ok(())
}

fn emit_actor_dispatch_trampoline<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    layout: &ActorLayout,
    fn_symbols: &FnSymbolMap<'ctx>,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<()> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let dispatch_name = format!("__hew_actor_dispatch_{}", layout.name);
    let fn_ty = ctx.void_type().fn_type(
        &[
            ptr_ty.into(),
            ptr_ty.into(),
            i32_ty.into(),
            ptr_ty.into(),
            i64_ty.into(),
        ],
        false,
    );
    let dispatch_fn = llvm_mod.add_function(&dispatch_name, fn_ty, Some(Linkage::Internal));
    let builder = ctx.create_builder();
    let entry = ctx.append_basic_block(dispatch_fn, "entry");
    let default_bb = ctx.append_basic_block(dispatch_fn, "unknown_msg_type");
    let after_bb = ctx.append_basic_block(dispatch_fn, "dispatch_done");
    builder.position_at_end(entry);
    let msg_type = dispatch_fn
        .get_nth_param(2)
        .ok_or_else(|| {
            CodegenError::FailClosed("dispatch trampoline missing msg_type param".into())
        })?
        .into_int_value();
    let mut cases = Vec::with_capacity(layout.handlers.len());
    for handler in &layout.handlers {
        let bb = ctx.append_basic_block(dispatch_fn, &format!("msg_{}", handler.msg_type));
        cases.push((i32_ty.const_int(handler.msg_type as u64, false), bb));
    }
    builder
        .build_switch(msg_type, default_bb, &cases)
        .map_err(|e| CodegenError::Llvm(format!("actor dispatch switch: {e:?}")))?;

    for (handler, (_, bb)) in layout.handlers.iter().zip(cases.iter()) {
        builder.position_at_end(*bb);
        let symbol = fn_symbols.get(&handler.symbol).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "actor dispatch `{dispatch_name}` references undeclared handler `{}`",
                handler.symbol
            ))
        })?;
        let (handler_fn, return_ty, returns_unit) =
            symbol.real(&handler.symbol, "actor dispatch handler")?;
        let ctx_arg = dispatch_fn
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("dispatch missing ctx param".into()))?;
        let data_ptr = dispatch_fn
            .get_nth_param(3)
            .ok_or_else(|| CodegenError::FailClosed("dispatch missing data param".into()))?
            .into_pointer_value();
        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(1 + handler.param_tys.len());
        args.push(ctx_arg.into());
        for (idx, param_ty) in handler.param_tys.iter().enumerate() {
            let llvm_ty = resolve_ty(ctx, param_ty, record_layouts)?;
            let loaded = builder
                .build_load(llvm_ty, data_ptr, &format!("msg_arg_{idx}"))
                .map_err(|e| CodegenError::Llvm(format!("actor dispatch arg load: {e:?}")))?;
            args.push(metadata_value_from_basic(loaded));
        }
        let call = builder
            .build_call(handler_fn, &args, &format!("call_{}", handler.name))
            .map_err(|e| CodegenError::Llvm(format!("actor dispatch handler call: {e:?}")))?;
        if !returns_unit {
            let ret_val = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "actor handler `{}` has non-unit MIR return but LLVM call returned void",
                    handler.symbol
                ))
            })?;
            if ret_val.get_type() != return_ty {
                return Err(CodegenError::FailClosed(format!(
                    "actor handler `{}` return type mismatch: call={:?}, declared={return_ty:?}",
                    handler.symbol,
                    ret_val.get_type()
                )));
            }
            let mut runtime_decls = RuntimeDeclMap::new();
            let reply_channel =
                intern_runtime_decl(ctx, llvm_mod, &mut runtime_decls, "hew_get_reply_channel")?;
            let reply = intern_runtime_decl(ctx, llvm_mod, &mut runtime_decls, "hew_reply")?;
            let ch = builder
                .build_call(reply_channel, &[], "hew_get_reply_channel_call")
                .map_err(|e| CodegenError::Llvm(format!("get reply channel call: {e:?}")))?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_get_reply_channel returned void".into())
                })?
                .into_pointer_value();
            let ret_slot = builder
                .build_alloca(return_ty, "actor_reply_slot")
                .map_err(|e| CodegenError::Llvm(format!("actor reply alloca: {e:?}")))?;
            builder
                .build_store(ret_slot, ret_val)
                .map_err(|e| CodegenError::Llvm(format!("actor reply store: {e:?}")))?;
            let size = return_ty.size_of().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "actor handler `{}` reply type has no static size: {return_ty:?}",
                    handler.symbol
                ))
            })?;
            let size = if size.get_type() == i64_ty {
                size
            } else {
                builder
                    .build_int_z_extend(size, i64_ty, "actor_reply_size")
                    .map_err(|e| CodegenError::Llvm(format!("reply size zext: {e:?}")))?
            };
            builder
                .build_call(
                    reply,
                    &[ch.into(), ret_slot.into(), size.into()],
                    "hew_reply_call",
                )
                .map_err(|e| CodegenError::Llvm(format!("hew_reply call: {e:?}")))?;
        }
        builder
            .build_unconditional_branch(after_bb)
            .map_err(|e| CodegenError::Llvm(format!("actor dispatch branch: {e:?}")))?;
    }

    builder.position_at_end(default_bb);
    let trap = Intrinsic::find("llvm.trap")
        .and_then(|intrinsic| intrinsic.get_declaration(llvm_mod, &[]))
        .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
    builder
        .build_call(trap, &[], "actor_dispatch_unknown_msg_trap")
        .map_err(|e| CodegenError::Llvm(format!("actor dispatch trap: {e:?}")))?;
    builder
        .build_unreachable()
        .map_err(|e| CodegenError::Llvm(format!("actor dispatch unreachable: {e:?}")))?;

    builder.position_at_end(after_bb);
    builder
        .build_return(None)
        .map_err(|e| CodegenError::Llvm(format!("actor dispatch return: {e:?}")))?;
    Ok(())
}

fn actor_name_from_handler_symbol(symbol: &str) -> Option<&str> {
    symbol
        .split_once("__recv__")
        .map(|(actor_name, _)| actor_name)
        .or_else(|| symbol.strip_suffix("__init"))
        .or_else(|| symbol.strip_suffix("__on_start"))
        // Indexed stop-hook symbols: `<Actor>__on_stop__<N>`.
        // Strip the numeric suffix first, then the `__on_stop` infix.
        .or_else(|| {
            let after_on_stop = symbol.split_once("__on_stop__")?;
            // Verify the suffix is a decimal index (no empty or non-numeric).
            after_on_stop.1.parse::<usize>().ok()?;
            Some(after_on_stop.0)
        })
}

// ---------------------------------------------------------------------------
// Module-level orchestration
// ---------------------------------------------------------------------------

/// Build the LLVM module for a lowered pipeline.
///
/// `checked_mir` is all-or-nothing — if non-empty, every `raw_mir` function
/// MUST have a matching entry. Fails closed at codegen time if violated.
fn build_module<'ctx>(
    ctx: &'ctx Context,
    pipeline: &IrPipeline,
    name: &str,
) -> CodegenResult<LlvmModule<'ctx>> {
    let llvm_mod = ctx.create_module(name);
    // Register every named-form record from `pipeline.record_layouts` as an
    // LLVM named struct on `ctx` BEFORE any function declaration or body
    // lowering touches `resolve_ty`. Records can appear in function return
    // types (declare_function) and in local slot types (lower_function);
    // both paths consult `record_layouts` via `resolve_ty`, so the map must
    // be populated up front. Empty `record_layouts` (most existing pipelines)
    // produces an empty map and changes no behaviour for record-free code.
    let mut record_layouts = register_record_layouts(ctx, &pipeline.record_layouts)?;
    // Slice 5: register machine tagged-union types alongside records.
    // `register_machine_layouts` inserts the outer struct of each machine
    // and its `<Name>Event` companion into `record_layouts` so `resolve_ty`
    // resolves `ResolvedTy::Named { name: "Foo" }` to the machine's outer
    // struct the same way it resolves a record. The richer per-variant
    // metadata (inner structs, tag int type) lives in `machine_layouts`.
    let mut machine_layouts = register_machine_layouts(
        ctx,
        &llvm_mod,
        &pipeline.machine_layouts,
        &mut record_layouts,
    )?;
    // Register user-defined enum layouts into the same `machine_layouts` map.
    // Enum-typed locals use the same `Place::MachineTag` / `Place::MachineVariant`
    // addressing as machine-typed locals; the map lookup in
    // `machine_layout_for_local` is keyed by type name and does not
    // distinguish surface form.
    register_enum_layouts(
        ctx,
        &pipeline.enum_layouts,
        &mut record_layouts,
        &mut machine_layouts,
    )?;
    let mut fn_symbols: FnSymbolMap<'ctx> = HashMap::new();
    predeclare_stdlib_catalog(ctx, &llvm_mod, &mut fn_symbols, &record_layouts)?;
    for func in &pipeline.raw_mir {
        let sym = declare_function(ctx, &llvm_mod, func, &record_layouts)?;
        fn_symbols.insert(func.name.clone(), sym);
    }
    for actor in &pipeline.actor_layouts {
        emit_actor_dispatch_trampoline(ctx, &llvm_mod, actor, &fn_symbols, &record_layouts)?;
        if !actor.on_stop_symbols.is_empty() {
            emit_actor_terminate_trampoline(ctx, &llvm_mod, actor, &fn_symbols)?;
        }
    }
    // Supervisor bootstraps replace the MIR-side synthesised body wholesale
    // with the canonical `hew_supervisor_new` → `add_child_spec` × N →
    // `start` call sequence (S-D.3). The set of bootstrap symbols is
    // collected so the body-lowering loop below skips them — the MIR-side
    // body exists only to carry the declaration signature.
    let supervisor_bootstrap_symbols: std::collections::HashSet<String> = pipeline
        .supervisor_layouts
        .iter()
        .map(|s| s.bootstrap_symbol.clone())
        .collect();
    for sup in &pipeline.supervisor_layouts {
        emit_supervisor_bootstrap_body(ctx, &llvm_mod, sup, &fn_symbols)?;
    }
    for func in &pipeline.raw_mir {
        if supervisor_bootstrap_symbols.contains(&func.name) {
            // Body already emitted by `emit_supervisor_bootstrap_body`. Skip
            // the normal MIR lowering — the MIR-side synthesised body is a
            // stub kept only to carry the bootstrap function's signature
            // through `declare_function`.
            continue;
        }
        // Match by name: elaborated_mir is parallel to raw_mir when the
        // full pipeline runs. Hand-built test pipelines may leave
        // elaborated_mir empty; `find` returns `None` in that case and
        // `lower_function` falls back to the inline Instr::Drop path.
        let elab = pipeline.elaborated_mir.iter().find(|e| e.name == func.name);
        let checked = if pipeline.checked_mir.is_empty() {
            None
        } else {
            Some(
                pipeline
                    .checked_mir
                    .iter()
                    .find(|c| c.name == func.name)
                    .ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "function `{}` has no matching checked MIR for cooperate-site lowering",
                            func.name
                        ))
                    })?,
            )
        };
        lower_function(
            ctx,
            &llvm_mod,
            func,
            &fn_symbols,
            elab,
            checked,
            &record_layouts,
            &pipeline.actor_layouts,
            &machine_layouts,
        )?;
    }
    // Emit regex module-init infrastructure if the module uses any regex literals.
    // This must come AFTER all function bodies are lowered (the init function
    // references globals declared here) and BEFORE llvm_mod.verify() so the
    // verifier can check the emitted IR.
    if !pipeline.regex_literals.is_empty() {
        emit_regex_module_init(ctx, &llvm_mod, &pipeline.regex_literals)?;
    }
    llvm_mod
        .verify()
        .map_err(|e| CodegenError::LlvmVerify(e.to_string()))?;
    Ok(llvm_mod)
}

/// Emit the module-level regex infrastructure when `pipeline.regex_literals` is non-empty.
///
/// Emits:
/// 1. `@hew_regex_handles : [N x ptr]` — global mutable array of `*mut HewRegex` handles,
///    initialised to null. The module-init constructor populates each slot.
/// 2. Per literal: `@hew_regex_pattern_<i> : [M x i8]` — private NUL-terminated i8 constant
///    holding the pattern bytes.
/// 3. `@hew_module_init_regex` — void() constructor that calls `hew_regex_compile` for each
///    pattern and stores the result into `@hew_regex_handles[i]`. Traps fail-closed if any
///    compile returns null (the type-checker guaranteed the pattern is valid; null means OOM
///    or an internal invariant was broken).
/// 4. `@llvm.global_ctors = appending [{ i32, ptr, ptr } { 65535, @hew_module_init_regex,
///    null }]` — ensures the constructor runs before `main`.
///
/// WHY `@llvm.global_ctors` not a call from `main`: every user-defined entrypoint (not just
/// `main`) runs after module init; a ctors entry is the substrate-correct mechanism.
/// WHEN-OBSOLETE: if Hew gains a module-level DSL for eager resource initialisation, this
/// synthesised constructor is the expected substrate for that feature.
/// WHAT: the `lower_call_runtime_abi` arms for `hew_regex_match` and `hew_regex_capture` load
/// from this global array by GEP (literal_id → slot index).
fn emit_regex_module_init<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    literals: &[RegexLiteral],
) -> CodegenResult<()> {
    if literals.is_empty() {
        return Ok(());
    }
    let n = u32::try_from(literals.len()).map_err(|_| {
        CodegenError::FailClosed("regex literal count exceeds u32::MAX — impossible".into())
    })?;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i8_ty = ctx.i8_type();
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();

    // 1. Global handle array: [N x ptr] initialised to all-null.
    let handle_arr_ty = ptr_ty.array_type(n);
    let handles_global = llvm_mod.add_global(handle_arr_ty, None, "hew_regex_handles");
    let null_ptrs: Vec<inkwell::values::PointerValue<'ctx>> =
        (0..n).map(|_| ptr_ty.const_null()).collect();
    let null_arr_init = ptr_ty.const_array(&null_ptrs);
    handles_global.set_initializer(&null_arr_init);
    handles_global.set_linkage(Linkage::Private);

    // 2. Per-literal pattern constant: private unnamed_addr [M x i8].
    let mut pattern_ptrs: Vec<inkwell::values::PointerValue<'ctx>> =
        Vec::with_capacity(literals.len());
    for lit in literals {
        let bytes_with_nul: Vec<u8> = lit.pattern.bytes().chain(std::iter::once(0u8)).collect();
        let len = u32::try_from(bytes_with_nul.len()).map_err(|_| {
            CodegenError::FailClosed(format!(
                "regex pattern {lit_id} is too long for LLVM constant",
                lit_id = lit.literal_id
            ))
        })?;
        let arr_ty = i8_ty.array_type(len);
        let global_name = format!("hew_regex_pattern_{}", lit.literal_id);
        let pat_global = llvm_mod.add_global(arr_ty, None, &global_name);
        let bytes_as_i8: Vec<inkwell::values::IntValue<'ctx>> = bytes_with_nul
            .iter()
            .map(|b| i8_ty.const_int(u64::from(*b), false))
            .collect();
        let arr_init = i8_ty.const_array(&bytes_as_i8);
        pat_global.set_initializer(&arr_init);
        pat_global.set_linkage(Linkage::Private);
        pat_global.set_constant(true);
        pat_global.set_unnamed_addr(true);
        pattern_ptrs.push(pat_global.as_pointer_value());
    }

    // 3. Constructor function: `void hew_module_init_regex()`.
    let void_ty = ctx.void_type();
    let init_fn_ty = void_ty.fn_type(&[], false);
    let init_fn =
        llvm_mod.add_function("hew_module_init_regex", init_fn_ty, Some(Linkage::Private));
    let entry_bb = ctx.append_basic_block(init_fn, "entry");
    let builder = ctx.create_builder();
    builder.position_at_end(entry_bb);

    // Intern hew_regex_compile declaration.
    let compile_fn = {
        let compile_ty = ptr_ty.fn_type(&[ptr_ty.into()], false);
        llvm_mod
            .get_function("hew_regex_compile")
            .unwrap_or_else(|| {
                llvm_mod.add_function("hew_regex_compile", compile_ty, Some(Linkage::External))
            })
    };

    // hew_trap_with_code for fail-closed on null return.
    let trap_fn = {
        let trap_ty = void_ty.fn_type(&[i32_ty.into(), i32_ty.into()], false);
        llvm_mod
            .get_function("hew_trap_with_code")
            .unwrap_or_else(|| {
                llvm_mod.add_function("hew_trap_with_code", trap_ty, Some(Linkage::External))
            })
    };

    for (i, pat_ptr) in pattern_ptrs.iter().enumerate() {
        // Call hew_regex_compile(pattern).
        let call_result = builder
            .build_call(
                compile_fn,
                &[(*pat_ptr).into()],
                &format!("compile_regex_{i}"),
            )
            .map_err(|e| {
                CodegenError::Llvm(format!("hew_regex_compile call in module-init: {e:?}"))
            })?;
        let handle = call_result
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_regex_compile returned void — signature mismatch".into(),
                )
            })?
            .into_pointer_value();

        // Fail-closed trap if null: pattern was validated by the checker; null is OOM or
        // a code invariant violation. Trap code 206 (InvalidRegex) per hew-runtime TrapKind.
        // WHY trap not unreachable: LLVM may speculate past `unreachable` at -O2; a call to
        // `hew_trap_with_code` is opaque to the optimiser and always terminates. WHEN-OBSOLETE:
        // when LLVM's opaque-pointer + assume attributes land in hew-codegen-rs, the assume
        // could suppress the branch; for now the explicit call is the correct substrate.
        let null_check_bb = ctx.append_basic_block(init_fn, &format!("null_check_{i}"));
        let ok_bb = ctx.append_basic_block(init_fn, &format!("ok_{i}"));
        let is_null = builder
            .build_is_null(handle, &format!("is_null_{i}"))
            .map_err(|e| CodegenError::Llvm(format!("is_null check in module-init: {e:?}")))?;
        builder
            .build_conditional_branch(is_null, null_check_bb, ok_bb)
            .map_err(|e| CodegenError::Llvm(format!("null-check branch in module-init: {e:?}")))?;

        builder.position_at_end(null_check_bb);
        // Trap code 209 — module-init regex compile failure (internal invariant
        // violation; patterns are validated by the type-checker so this branch
        // is dead code in correctly-compiled programs). 209 is the next unused
        // code after 208 (ExhaustivenessFallthrough). WHEN-OBSOLETE: when
        // TrapKind gains a dedicated InvalidRegex variant, add it at code 209
        // and update the constant table in lower_call_runtime_abi.
        // WHY not 206: 206 is HEW_TRAP_ACTOR_SEND_FAILED; reusing it would
        // make diagnostics misleading.
        const HEW_TRAP_MODULE_INIT_REGEX_FAILED: u64 = 209;
        let trap_code = i32_ty.const_int(HEW_TRAP_MODULE_INIT_REGEX_FAILED, false);
        let lit_id_code = i32_ty.const_int(u64::from(literals[i].literal_id), false);
        builder
            .build_call(
                trap_fn,
                &[trap_code.into(), lit_id_code.into()],
                "trap_null_regex",
            )
            .map_err(|e| CodegenError::Llvm(format!("trap call in module-init: {e:?}")))?;
        builder
            .build_unreachable()
            .map_err(|e| CodegenError::Llvm(format!("unreachable in module-init: {e:?}")))?;

        builder.position_at_end(ok_bb);
        // Store handle into @hew_regex_handles[i].
        let idx = i64_ty.const_int(u64::try_from(i).unwrap_or(u64::MAX), false);
        let slot_ptr = unsafe {
            builder
                .build_gep(
                    handle_arr_ty,
                    handles_global.as_pointer_value(),
                    &[i64_ty.const_zero(), idx],
                    &format!("regex_slot_{i}"),
                )
                .map_err(|e| CodegenError::Llvm(format!("GEP for regex handle slot {i}: {e:?}")))?
        };
        builder
            .build_store(slot_ptr, handle)
            .map_err(|e| CodegenError::Llvm(format!("store regex handle {i}: {e:?}")))?;
    }
    builder
        .build_return(None)
        .map_err(|e| CodegenError::Llvm(format!("return in hew_module_init_regex: {e:?}")))?;

    // 4. Register in @llvm.global_ctors (appending linkage, priority 65535).
    // The struct type is { i32, ptr, ptr } — priority, fn ptr, associated data (null).
    let ctor_entry_ty = ctx.struct_type(&[i32_ty.into(), ptr_ty.into(), ptr_ty.into()], false);
    let ctor_arr_ty = ctor_entry_ty.array_type(1);
    let priority = i32_ty.const_int(65535, false);
    let init_fn_ptr = init_fn.as_global_value().as_pointer_value();
    let assoc_data = ptr_ty.const_null();
    let ctor_entry =
        ctor_entry_ty.const_named_struct(&[priority.into(), init_fn_ptr.into(), assoc_data.into()]);
    let ctor_arr_init = ctor_entry_ty.const_array(&[ctor_entry]);
    // LLVM requires appending linkage for @llvm.global_ctors; the name is magic.
    let ctors_global = llvm_mod.add_global(ctor_arr_ty, None, "llvm.global_ctors");
    ctors_global.set_initializer(&ctor_arr_init);
    ctors_global.set_linkage(Linkage::Appending);

    Ok(())
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
            // `--import-undefined`: undefined runtime symbols (e.g.
            // `hew_trap_with_code` from `Terminator::Trap` lowering) become
            // wasm imports the host module provides. The native link path
            // resolves these against the hew-runtime static archive in
            // `hew-cli/src/link.rs`; the codegen-internal wasm link in this
            // helper does not pull in the runtime, so the symbols must be
            // host-imports instead of hard link errors. WASM-TODO(#1451):
            // a follow-up wires per-kind trap reporting into a real wasm
            // runtime stub.
            .arg("--import-undefined")
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
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
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
            record_layouts: Vec::new(),
            actor_layouts: Vec::new(),
            supervisor_layouts: Vec::new(),
            machine_layouts: Vec::new(),
            enum_layouts: Vec::new(),
            regex_literals: Vec::new(),
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
    fn actor_handler_signature_leads_with_execution_context_pointer() {
        let handler = RawMirFunction {
            name: "handler".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: FunctionCallConv::ActorHandler,
            params: vec![ResolvedTy::I64],
            locals: vec![ResolvedTy::I64],
            blocks: vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![
                    Instr::EnterContext,
                    Instr::Move {
                        dest: Place::ReturnSlot,
                        src: Place::Local(0),
                    },
                    Instr::ExitContext,
                ],
                terminator: Terminator::Return,
            }],
            decisions: Vec::new(),
        };
        let pipeline = IrPipeline {
            thir: Vec::new(),
            raw_mir: vec![handler],
            checked_mir: Vec::new(),
            elaborated_mir: Vec::new(),
            diagnostics: Vec::new(),
            record_layouts: Vec::new(),
            actor_layouts: Vec::new(),
            supervisor_layouts: Vec::new(),
            machine_layouts: Vec::new(),
            enum_layouts: Vec::new(),
            regex_literals: Vec::new(),
        };
        let ctx = Context::create();
        let m = build_module(&ctx, &pipeline, "handler_ctx_test")
            .expect("actor-handler module must build");
        let ir = m.print_to_string().to_string();
        assert!(
            ir.contains("define internal i64 @handler(ptr %0, i64 %1)"),
            "handler ABI must lead with opaque HewExecutionContext* before user params:\n{ir}"
        );
        assert!(
            !ir.contains("hew_actor_state_lock_acquire"),
            "actor-state locking belongs in the scheduler wrapper, not emitted handler IR:\n{ir}"
        );
    }

    #[test]
    fn context_field_actor_offset_emits_gep_and_load() {
        let handler = RawMirFunction {
            name: "handler_ctx_field".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: FunctionCallConv::ActorHandler,
            params: vec![],
            locals: vec![
                ResolvedTy::Pointer {
                    is_mutable: true,
                    pointee: Box::new(ResolvedTy::Never),
                },
                ResolvedTy::I64,
            ],
            blocks: vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![
                    Instr::EnterContext,
                    Instr::ContextField {
                        dest: Place::Local(0),
                        offset: HEW_CTX_OFFSET_ACTOR,
                    },
                    Instr::ConstI64 {
                        dest: Place::Local(1),
                        value: 0,
                    },
                    Instr::Move {
                        dest: Place::ReturnSlot,
                        src: Place::Local(1),
                    },
                    Instr::ExitContext,
                ],
                terminator: Terminator::Return,
            }],
            decisions: Vec::new(),
        };
        let pipeline = IrPipeline {
            thir: Vec::new(),
            raw_mir: vec![handler],
            checked_mir: Vec::new(),
            elaborated_mir: Vec::new(),
            diagnostics: Vec::new(),
            record_layouts: Vec::new(),
            actor_layouts: Vec::new(),
            supervisor_layouts: Vec::new(),
            machine_layouts: Vec::new(),
            enum_layouts: Vec::new(),
            regex_literals: Vec::new(),
        };
        let ctx = Context::create();
        let m = build_module(&ctx, &pipeline, "ctx_field_test")
            .expect("ContextField actor load must build");
        let ir = m.print_to_string().to_string();
        assert!(
            ir.contains("getelementptr i8, ptr %0, i64 0"),
            "ContextField must lower to byte-offset GEP from ctx arg:\n{ir}"
        );
        assert!(
            ir.contains("load ptr, ptr %ctx_field_0_ptr"),
            "ContextField actor offset must load a pointer from the GEP:\n{ir}"
        );
    }

    // `String` is now a lowerable return type (maps to opaque `ptr`).
    // This test exercises a String-returning function with a `StringLit`
    // instruction that populates the return slot. It must build and verify.
    #[test]
    fn string_literal_return_builds_and_verifies() {
        // fn main() -> String { "hello" }
        // Locals: [0: String (the lit dest), ReturnSlot: String]
        let return_ty = ResolvedTy::String;
        let main = RawMirFunction {
            name: "main".to_string(),
            return_ty: return_ty.clone(),
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![return_ty.clone()],
            blocks: vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![
                    Instr::StringLit {
                        bytes: b"hello".to_vec(),
                        dest: Place::Local(0),
                    },
                    Instr::Move {
                        dest: Place::ReturnSlot,
                        src: Place::Local(0),
                    },
                ],
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
            record_layouts: Vec::new(),
            actor_layouts: Vec::new(),
            supervisor_layouts: Vec::new(),
            machine_layouts: Vec::new(),
            enum_layouts: Vec::new(),
            regex_literals: Vec::new(),
        };
        let ctx = Context::create();
        let m =
            build_module(&ctx, &pipeline, "string_lit_test").expect("StringLit module must build");
        assert!(m.verify().is_ok(), "StringLit module must pass LLVM verify");
        // Confirm the textual IR contains the global string constant.
        let ir = m.print_to_string().to_string();
        assert!(
            ir.contains("hello"),
            "emitted IR must contain the literal bytes: {ir}"
        );
    }

    // Float return is still unsupported — verify the fail-closed boundary.
    #[test]
    fn unsupported_float_return_fails_closed() {
        let return_ty = ResolvedTy::F64;
        let main = RawMirFunction {
            name: "main".to_string(),
            return_ty: return_ty.clone(),
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
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
            record_layouts: Vec::new(),
            actor_layouts: Vec::new(),
            supervisor_layouts: Vec::new(),
            machine_layouts: Vec::new(),
            enum_layouts: Vec::new(),
            regex_literals: Vec::new(),
        };
        let ctx = Context::create();
        let err =
            build_module(&ctx, &pipeline, "float_return").expect_err("F64 return must be rejected");
        assert!(
            matches!(err, CodegenError::Unsupported(_)),
            "F64 return must surface as Unsupported, got: {err:?}"
        );
    }

    // ── Terminator::Select codegen ──────────────────────────────────
    //
    // ActorAsk and AfterTimer arms emit real IR (slice 3); StreamNext
    // and TaskAwait remain fail-closed as defence-in-depth (the MIR
    // producer rejects them with SelectArmNotImplemented before reaching
    // codegen — but the codegen branch still guards against any future
    // shape change that lets one slip through).

    /// Construct an `IrPipeline` carrying a single function whose only
    /// block is a `Terminator::Select` with the given single arm. Used
    /// for the defence-in-depth fail-closed tests (StreamNext / TaskAwait
    /// kinds; an `ActorAsk` arm without surrounding setup would fail at
    /// `actor_payload_ptr_size` / `load_duplex_handle` for missing
    /// locals — those positive paths are covered by the
    /// `pipeline_with_select_*_arms` builders below).
    fn pipeline_with_select_terminator(arm_kind: hew_mir::SelectArmKind) -> IrPipeline {
        let main = RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
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
            record_layouts: Vec::new(),
            actor_layouts: Vec::new(),
            supervisor_layouts: Vec::new(),
            machine_layouts: Vec::new(),
            enum_layouts: Vec::new(),
            regex_literals: Vec::new(),
        }
    }

    /// Stream-next arms allocate their own readiness channel, register
    /// a pending read, include that channel in `hew_select_first`, and
    /// cancel the pending read when they lose.
    #[test]
    fn select_stream_next_arm_emits_readiness_proxy() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::StreamNext {
                    stream: Place::DuplexHandle(0),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::AfterTimer {
                    duration: Place::Local(2),
                },
                body_block: 11,
                binding: None,
            },
        ];
        let locals = vec![duplex_ty(), ResolvedTy::I64, ResolvedTy::Duration];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
        let ir = emit_select_ir("select_stream_next", &pipeline);
        assert_eq!(ir.matches("call ptr @hew_reply_channel_new(").count(), 1);
        assert!(ir.contains("call void @hew_reply_channel_retain("));
        assert!(ir.contains("call i64 @hew_stream_poll("));
        assert!(
            ir.contains("%stream_item_value = load i64, ptr %1"),
            "stream callback must load the item bytes as the arm binding type; ir:\n{ir}"
        );
        assert!(
            ir.contains("store i64 %stream_item_value, ptr %stream_item_slot"),
            "stream callback must reply with the loaded item value, not the item pointer; ir:\n{ir}"
        );
        assert_eq!(
            ir.matches("call void @free(ptr %1)").count(),
            1,
            "stream callback must free the malloc'd stream buffer once via the shared free block; ir:\n{ir}"
        );
        assert!(ir.contains("call void @hew_stream_cancel_pending_read("));
        assert!(ir.contains("call i32 @hew_select_first("));
    }

    /// Two stream arms observing the same source still allocate and
    /// register distinct per-arm readiness channels.
    #[test]
    fn select_stream_next_duplicate_source_uses_per_arm_channels() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::StreamNext {
                    stream: Place::DuplexHandle(0),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::StreamNext {
                    stream: Place::DuplexHandle(0),
                },
                body_block: 11,
                binding: Some(Place::Local(2)),
            },
        ];
        let locals = vec![duplex_ty(), ResolvedTy::String, ResolvedTy::String];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
        let ir = emit_select_ir("select_stream_duplicate", &pipeline);
        assert_eq!(ir.matches("call ptr @hew_reply_channel_new(").count(), 2);
        assert_eq!(ir.matches("call i64 @hew_stream_poll(").count(), 2);
    }

    /// Task-await arms observe completion through a retained reply-channel
    /// readiness proxy and read the completed result only after winning.
    #[test]
    fn select_task_await_arm_emits_completion_proxy() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::TaskAwait {
                    task: Place::DuplexHandle(0),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::AfterTimer {
                    duration: Place::Local(2),
                },
                body_block: 11,
                binding: None,
            },
        ];
        let locals = vec![task_ty(), ResolvedTy::I64, ResolvedTy::Duration];
        let pipeline = pipeline_with_select_actor_handler_arms(arms, locals, &[10, 11], 99);
        let ir = emit_select_ir("select_task_await", &pipeline);
        assert_eq!(ir.matches("call ptr @hew_reply_channel_new(").count(), 1);
        assert!(ir.contains("call void @hew_reply_channel_retain("));
        assert!(ir.contains("call i32 @hew_task_completion_observe("));
        assert!(ir.contains("call i32 @hew_task_completion_unobserve("));
        assert!(ir.contains("call ptr @hew_task_get_result("));
    }

    /// A select carrying only an `AfterTimer` arm has no race; the
    /// HIR should have rejected the shape, but codegen must refuse
    /// defensively (no ActorAsk arms means nothing to race).
    #[test]
    fn select_no_actor_ask_arms_fails_closed() {
        let pipeline = pipeline_with_select_terminator(hew_mir::SelectArmKind::AfterTimer {
            duration: Place::Local(0),
        });
        let ctx = Context::create();
        let err = build_module(&ctx, &pipeline, "select_after_only")
            .expect_err("Terminator::Select with only AfterTimer must fail closed");
        let msg = match err {
            CodegenError::FailClosed(s) => s,
            other => panic!("expected FailClosed, got {other:?}"),
        };
        assert!(
            msg.contains("no value-producing arms"),
            "after-only FailClosed must explain the sealed-shape violation: {msg}"
        );
    }

    // ── Terminator::Select positive-path emit (ActorAsk + AfterTimer) ─

    /// Build an `IrPipeline` whose `main` issues a `Terminator::Select`
    /// with the given arm vector. Local 0 is a `Duplex` (the actor
    /// handle source for each ActorAsk arm); locals 1.. are per-arm
    /// reply slots (one per ActorAsk arm) plus a single i64 reply
    /// payload slot (unit-arg variant uses `ResolvedTy::Unit` to skip
    /// payload pointer load). Local for after-arm duration is
    /// allocated as `ResolvedTy::Duration` (i64 ns).
    ///
    /// The resulting function has one originating block (id 0) sealed
    /// by `Terminator::Select` and per-arm body blocks (each just
    /// `Terminator::Return`) plus a join block (also `Return`).
    fn pipeline_with_select_arms(
        arms: Vec<hew_mir::SelectArm>,
        locals: Vec<ResolvedTy>,
        body_block_ids: &[u32],
        join_block_id: u32,
    ) -> IrPipeline {
        // Build the originating block + per-arm body blocks + join block.
        let mut blocks: Vec<BasicBlock> = Vec::new();
        blocks.push(BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Select {
                arms,
                next: join_block_id,
            },
        });
        for &bb_id in body_block_ids {
            blocks.push(BasicBlock {
                id: bb_id,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Goto {
                    target: join_block_id,
                },
            });
        }
        blocks.push(BasicBlock {
            id: join_block_id,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Return,
        });

        let main = RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals,
            blocks,
            decisions: Vec::new(),
        };
        IrPipeline {
            thir: Vec::new(),
            raw_mir: vec![main],
            checked_mir: Vec::new(),
            elaborated_mir: Vec::new(),
            diagnostics: Vec::new(),
            record_layouts: Vec::new(),
            actor_layouts: Vec::new(),
            supervisor_layouts: Vec::new(),
            machine_layouts: Vec::new(),
            enum_layouts: Vec::new(),
            regex_literals: Vec::new(),
        }
    }

    fn pipeline_with_select_actor_handler_arms(
        arms: Vec<hew_mir::SelectArm>,
        locals: Vec<ResolvedTy>,
        body_block_ids: &[u32],
        join_block_id: u32,
    ) -> IrPipeline {
        let mut pipeline = pipeline_with_select_arms(arms, locals, body_block_ids, join_block_id);
        pipeline.raw_mir[0].call_conv = hew_mir::FunctionCallConv::ActorHandler;
        pipeline.raw_mir[0].blocks[0]
            .instructions
            .insert(0, Instr::EnterContext);
        let join_block = pipeline
            .raw_mir
            .get_mut(0)
            .and_then(|main| {
                main.blocks
                    .iter_mut()
                    .find(|block| block.id == join_block_id)
            })
            .expect("select actor-handler helper must include join block");
        join_block.instructions.push(Instr::ExitContext);
        pipeline
    }

    /// Helper: a `ResolvedTy::Named { name: "Duplex", .. }` so the
    /// codegen treats local 0 as an actor handle (the same shape
    /// `Place::DuplexHandle` references via `load_duplex_handle`).
    fn duplex_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: Vec::new(),
        }
    }

    fn task_ty() -> ResolvedTy {
        ResolvedTy::Task(Box::new(ResolvedTy::I64))
    }

    /// Render the LLVM IR of the emitted module so test bodies can
    /// inspect the call-site ordering, channel-array shape, and per-arm
    /// dispatch (the "ground-truth" the runtime ABI demands).
    fn emit_select_ir(name: &str, pipeline: &IrPipeline) -> String {
        let ctx = Context::create();
        let llvm_mod = build_module(&ctx, pipeline, name).expect("select pipeline must compile");
        llvm_mod.print_to_string().to_string()
    }

    /// Two ActorAsk arms (no AfterTimer): emit shows exactly 2 channel
    /// allocations, 2 ask-issues, 1 `hew_select_first` call with
    /// timeout=-1, a switch on the winner index, per-winner reply
    /// wait, and 1 cancel + 1 free for the losing arm in each winner
    /// branch.
    #[test]
    fn select_two_actor_ask_arms_emit_full_dispatch() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "ping".to_string(),
                    args: Vec::new(),
                    msg_type: 7,
                    value: Place::Local(3), // unit payload slot
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "pong".to_string(),
                    args: Vec::new(),
                    msg_type: 8,
                    value: Place::Local(3),
                },
                body_block: 11,
                binding: Some(Place::Local(2)),
            },
        ];
        let locals = vec![
            duplex_ty(),      // 0: actor handle
            ResolvedTy::I64,  // 1: arm 0 reply slot
            ResolvedTy::I64,  // 2: arm 1 reply slot
            ResolvedTy::Unit, // 3: payload (unit)
        ];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
        let ir = emit_select_ir("select_two_asks", &pipeline);

        assert_eq!(
            ir.matches("call ptr @hew_reply_channel_new(").count(),
            2,
            "expected 2 channel allocations; ir:\n{ir}"
        );
        assert_eq!(
            ir.matches("call i32 @hew_actor_ask_with_channel(").count(),
            2,
            "expected 2 ask-issues; ir:\n{ir}"
        );
        assert_eq!(
            ir.matches("call i32 @hew_select_first(").count(),
            1,
            "expected 1 hew_select_first call; ir:\n{ir}"
        );
        // No after arm → timeout_ms = -1.
        assert!(
            ir.contains("@hew_select_first(ptr %select_channels_first, i32 2, i32 -1)"),
            "hew_select_first must be called with count=2, timeout=-1; ir:\n{ir}"
        );
        // One reply-wait per winner branch (two branches, one wait each).
        assert_eq!(
            ir.matches("call ptr @hew_reply_wait(").count(),
            2,
            "expected 2 reply-wait calls (one per winner branch); ir:\n{ir}"
        );
        // Cancel/free counts across the whole module:
        //   - winner branches: 2 (each cancels 1 loser, frees winner-self + loser).
        //   - setup_fail_0: 0 cancels (arm 0 failed first), 1 self-free.
        //   - setup_fail_1: 1 cancel (arm 0 was successfully submitted),
        //     2 frees (arm 0 + self).
        // Totals: 2 (win) + 1 (setup_fail_1) = 3 cancels;
        //         (1+1)*2 (winners) + 1 (setup_fail_0) + 2 (setup_fail_1) = 7 frees.
        assert_eq!(
            ir.matches("call void @hew_reply_channel_cancel(").count(),
            3,
            "expected 3 cancels (1 loser per winner branch + 1 recovery on \
             arm-1 ask-issue failure); ir:\n{ir}"
        );
        assert_eq!(
            ir.matches("call void @hew_reply_channel_free(").count(),
            7,
            "expected 7 channel frees across winner + setup-fail paths; ir:\n{ir}"
        );
    }

    /// One ActorAsk + one AfterTimer arm: the AfterTimer's duration
    /// drives the `hew_select_first` deadline (ns→ms / 1_000_000).
    #[test]
    fn select_actor_ask_plus_after_timer_emits_deadline() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "ping".to_string(),
                    args: Vec::new(),
                    msg_type: 7,
                    value: Place::Local(3),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::AfterTimer {
                    duration: Place::Local(2),
                },
                body_block: 11,
                binding: None,
            },
        ];
        let locals = vec![
            duplex_ty(),          // 0: actor handle
            ResolvedTy::I64,      // 1: reply slot
            ResolvedTy::Duration, // 2: after-arm duration (ns, i64)
            ResolvedTy::Unit,     // 3: payload
        ];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
        let ir = emit_select_ir("select_ask_and_after", &pipeline);

        // 1 channel allocation, 1 ask-issue, 1 hew_select_first call.
        assert_eq!(ir.matches("call ptr @hew_reply_channel_new(").count(), 1);
        assert_eq!(
            ir.matches("call i32 @hew_actor_ask_with_channel(").count(),
            1
        );
        assert_eq!(ir.matches("call i32 @hew_select_first(").count(), 1);
        // Deadline derived from i64 ns: should show sdiv by 1_000_000.
        assert!(
            ir.contains("sdiv i64") && ir.contains("1000000"),
            "expected ns→ms sdiv by 1_000_000; ir:\n{ir}"
        );
        // Two winner branches: ActorAsk winner + AfterTimer winner.
        // Each branch cancels + frees the loser(s); the AfterTimer
        // branch cancels + frees the single ActorAsk channel.
        assert!(
            ir.contains("select_win_ask_0:") && ir.contains("select_win_after:"),
            "expected both winner branch labels; ir:\n{ir}"
        );
    }

    /// Loser-cleanup order invariant (Risk R4): cancel BEFORE free on
    /// every loser channel — the cancel flag + ref count are what
    /// prevent UAF on a late-reply race.
    #[test]
    fn select_loser_cleanup_cancels_before_freeing() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "a".to_string(),
                    args: Vec::new(),
                    msg_type: 1,
                    value: Place::Local(3),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "b".to_string(),
                    args: Vec::new(),
                    msg_type: 2,
                    value: Place::Local(3),
                },
                body_block: 11,
                binding: Some(Place::Local(2)),
            },
        ];
        let locals = vec![
            duplex_ty(),
            ResolvedTy::I64,
            ResolvedTy::I64,
            ResolvedTy::Unit,
        ];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
        let ir = emit_select_ir("select_loser_order", &pipeline);

        // The loser cleanup lives in the per-winner "ok" block
        // (`select_reply_ok_N`) following the defensive null-reply
        // check on `hew_reply_wait`. We anchor on the named loser-load
        // SSA value emitted by the codegen (`select_loser_load_wN_lM`)
        // so we test the SAME channel's cleanup ordering, not arbitrary
        // `hew_reply_channel_*` calls that may interleave the
        // winner-self free.
        //
        // The two winners produce distinct SSA names
        // (`select_loser_load_w0_l1` vs `select_loser_load_w1_l0`),
        // so anchoring on the SSA name uniquely identifies each
        // winner's loser cleanup regardless of block-label
        // intervening text. We do not need to bound the search
        // region — the SSA name is globally unique in this module.
        let cancel0_idx = ir
            .find("call void @hew_reply_channel_cancel(ptr %select_loser_load_w0_l1)")
            .expect("loser cancel for arm-1 in win-0 region");
        let free0_idx = ir
            .find("call void @hew_reply_channel_free(ptr %select_loser_load_w0_l1)")
            .expect("loser free for arm-1 in win-0 region");
        assert!(
            cancel0_idx < free0_idx,
            "Risk R4: cancel must precede free in win-0 loser cleanup"
        );

        let cancel1_idx = ir
            .find("call void @hew_reply_channel_cancel(ptr %select_loser_load_w1_l0)")
            .expect("loser cancel for arm-0 in win-1 region");
        let free1_idx = ir
            .find("call void @hew_reply_channel_free(ptr %select_loser_load_w1_l0)")
            .expect("loser free for arm-0 in win-1 region");
        assert!(
            cancel1_idx < free1_idx,
            "Risk R4: cancel must precede free in win-1 loser cleanup"
        );
    }

    /// Mid-setup error recovery (Risk R3): if any `hew_actor_ask_with_channel`
    /// returns non-zero, every channel allocated through that point is
    /// freed and the path traps. We assert the IR contains a
    /// `select_setup_fail_N` block per arm with the recovery sequence.
    #[test]
    fn select_setup_failure_branches_free_allocated_channels_and_trap() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "a".to_string(),
                    args: Vec::new(),
                    msg_type: 1,
                    value: Place::Local(3),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "b".to_string(),
                    args: Vec::new(),
                    msg_type: 2,
                    value: Place::Local(3),
                },
                body_block: 11,
                binding: Some(Place::Local(2)),
            },
        ];
        let locals = vec![
            duplex_ty(),
            ResolvedTy::I64,
            ResolvedTy::I64,
            ResolvedTy::Unit,
        ];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
        let ir = emit_select_ir("select_setup_recovery", &pipeline);

        // Per-arm setup-failure blocks must exist.
        assert!(
            ir.contains("select_setup_fail_0:") && ir.contains("select_setup_fail_1:"),
            "expected per-arm setup-failure blocks; ir:\n{ir}"
        );
        // The recovery path must call hew_trap_with_code(206) and
        // llvm.trap then unreachable (matching the Send-fail shape).
        assert!(
            ir.contains("@hew_trap_with_code") && ir.contains("call void @llvm.trap"),
            "expected hew_trap_with_code + llvm.trap on setup-fail path; ir:\n{ir}"
        );
    }

    /// Setup-fail-1 (arm 1's ask fails) must cancel-then-free arm 0's
    /// channel (an ask was successfully submitted on arm 0, so a late
    /// reply is possible) before freeing arm 1's own channel.
    #[test]
    fn select_setup_failure_in_second_arm_cleans_up_first_arm_with_cancel_first() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "a".to_string(),
                    args: Vec::new(),
                    msg_type: 1,
                    value: Place::Local(3),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "b".to_string(),
                    args: Vec::new(),
                    msg_type: 2,
                    value: Place::Local(3),
                },
                body_block: 11,
                binding: Some(Place::Local(2)),
            },
        ];
        let locals = vec![
            duplex_ty(),
            ResolvedTy::I64,
            ResolvedTy::I64,
            ResolvedTy::Unit,
        ];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
        let ir = emit_select_ir("select_setup_recovery_late", &pipeline);

        let fail1_idx = ir
            .find("select_setup_fail_1:")
            .expect("setup-fail-1 block must exist");
        // Bound the region by the next block label after fail-1; for
        // simplicity, search the rest of the IR for the cancel before
        // any free.
        let fail1_region = &ir[fail1_idx..];
        let cancel_idx = fail1_region
            .find("call void @hew_reply_channel_cancel(")
            .expect("setup-fail-1 must cancel arm-0's channel");
        let free_idx = fail1_region
            .find("call void @hew_reply_channel_free(")
            .expect("setup-fail-1 must free at least one channel");
        assert!(
            cancel_idx < free_idx,
            "setup-fail-1 must cancel arm-0 BEFORE any free (UAF mitigation); region:\n{fail1_region}"
        );
    }

    /// The winner-switch dispatches on the i32 returned by
    /// `hew_select_first`. Each ActorAsk arm slot index is a case;
    /// the default lands on the AfterTimer winner block when present.
    #[test]
    fn select_winner_switch_uses_arm_slot_index() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "a".to_string(),
                    args: Vec::new(),
                    msg_type: 1,
                    value: Place::Local(3),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::AfterTimer {
                    duration: Place::Local(2),
                },
                body_block: 11,
                binding: None,
            },
        ];
        let locals = vec![
            duplex_ty(),
            ResolvedTy::I64,
            ResolvedTy::Duration,
            ResolvedTy::Unit,
        ];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
        let ir = emit_select_ir("select_switch_shape", &pipeline);

        // An LLVM `switch i32 %winner_idx, label %<default> [ ...
        // i32 0, label %select_win_ask_0 ... ]` pattern must appear.
        assert!(
            ir.contains("switch i32 %select_winner_idx") && ir.contains("select_win_ask_0"),
            "expected switch on winner index with per-arm case; ir:\n{ir}"
        );
        assert!(
            ir.contains("select_win_after"),
            "default arm of the switch must route to the AfterTimer winner block; ir:\n{ir}"
        );
    }

    /// Channel-array allocation: a fixed-size `[N x ptr]` alloca with
    /// N = number of ActorAsk arms; the `hew_select_first` call site
    /// receives the array-first GEP, not the alloca handle directly.
    #[test]
    fn select_channel_array_layout_matches_runtime_abi() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "a".to_string(),
                    args: Vec::new(),
                    msg_type: 1,
                    value: Place::Local(3),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "b".to_string(),
                    args: Vec::new(),
                    msg_type: 2,
                    value: Place::Local(3),
                },
                body_block: 11,
                binding: Some(Place::Local(2)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "c".to_string(),
                    args: Vec::new(),
                    msg_type: 3,
                    value: Place::Local(5),
                },
                body_block: 12,
                binding: Some(Place::Local(4)),
            },
        ];
        let locals = vec![
            duplex_ty(),
            ResolvedTy::I64,
            ResolvedTy::I64,
            ResolvedTy::Unit,
            ResolvedTy::I64,
            ResolvedTy::Unit,
        ];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11, 12], 99);
        let ir = emit_select_ir("select_three_asks", &pipeline);

        // [3 x ptr] alloca for the channel array.
        assert!(
            ir.contains("alloca [3 x ptr]"),
            "expected [3 x ptr] channel-array alloca; ir:\n{ir}"
        );
        // Three ask-issues + one select_first call.
        assert_eq!(
            ir.matches("call i32 @hew_actor_ask_with_channel(").count(),
            3
        );
        assert!(ir.contains("@hew_select_first(ptr %select_channels_first, i32 3, i32 -1)"));
    }

    /// Producer-bridge: the SelectArm.binding Place is the slot codegen
    /// writes the reply into on win. We verify the winner block emits
    /// a load from the reply pointer + a store into the binding
    /// alloca, then frees the reply buffer.
    #[test]
    fn select_winner_writes_reply_into_binding_place_and_frees_buffer() {
        let arms = vec![hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "ping".to_string(),
                args: Vec::new(),
                msg_type: 1,
                value: Place::Local(2),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        }];
        let locals = vec![duplex_ty(), ResolvedTy::I64, ResolvedTy::Unit];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10], 99);
        let ir = emit_select_ir("select_one_ask_binding", &pipeline);

        // The reply-wait result is loaded, stored into the binding,
        // and then `free`d (libc free, not channel_free).
        assert!(
            ir.contains("@hew_reply_wait("),
            "winner must wait on its channel; ir:\n{ir}"
        );
        assert!(
            ir.contains("call void @free("),
            "winner must free the reply buffer via libc free; ir:\n{ir}"
        );
        // The binding's alloca (local_1) must receive a store of the
        // loaded reply value.
        assert!(
            ir.contains("%local_1") && ir.contains("store i64"),
            "binding slot (local_1) must receive the reply value; ir:\n{ir}"
        );
    }

    /// Each winner branch defensively traps if `hew_reply_wait` returns
    /// null (the allocation-failure-during-publish path documented at
    /// `hew-runtime/src/reply_channel.rs:164`). Mirrors the null-trap
    /// shape of `Terminator::Ask`'s lowering.
    #[test]
    fn select_winner_traps_on_null_reply_pointer() {
        let arms = vec![hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "ping".to_string(),
                args: Vec::new(),
                msg_type: 1,
                value: Place::Local(2),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        }];
        let locals = vec![duplex_ty(), ResolvedTy::I64, ResolvedTy::Unit];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10], 99);
        let ir = emit_select_ir("select_null_reply_trap", &pipeline);

        assert!(
            ir.contains("select_reply_null_trap_0:") && ir.contains("select_reply_ok_0:"),
            "expected null-trap + ok blocks per winner; ir:\n{ir}"
        );
        // The conditional branch must route null to the trap block.
        assert!(
            ir.contains("br i1 %select_reply_is_null_0, label %select_reply_null_trap_0"),
            "expected conditional branch on null reply; ir:\n{ir}"
        );
    }

    /// Module-verification: the emitted module passes `Module::verify`.
    /// This is the floor invariant — any structural error in the
    /// channel-array, switch, or per-arm CFG composition is caught
    /// here even if the per-feature tests above pass.
    #[test]
    fn select_emitted_module_verifies() {
        let arms = vec![
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::ActorAsk {
                    actor: Place::DuplexHandle(0),
                    method: "ping".to_string(),
                    args: Vec::new(),
                    msg_type: 7,
                    value: Place::Local(3),
                },
                body_block: 10,
                binding: Some(Place::Local(1)),
            },
            hew_mir::SelectArm {
                kind: hew_mir::SelectArmKind::AfterTimer {
                    duration: Place::Local(2),
                },
                body_block: 11,
                binding: None,
            },
        ];
        let locals = vec![
            duplex_ty(),
            ResolvedTy::I64,
            ResolvedTy::Duration,
            ResolvedTy::Unit,
        ];
        let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
        let ctx = Context::create();
        let llvm_mod =
            build_module(&ctx, &pipeline, "select_verify").expect("pipeline must compile");
        if let Err(e) = llvm_mod.verify() {
            panic!(
                "emitted Select module failed verification: {}\nIR:\n{}",
                e.to_string(),
                llvm_mod.print_to_string().to_string()
            );
        }
    }

    /// Verify that `resolve_ty` looks up a generic-enum local by its mangled
    /// key (`"Option$$i64"`) rather than the bare name (`"Option"`), and that
    /// `build_module` produces a valid LLVM module when `Option<i64>` is the
    /// type of a local variable.
    ///
    /// Prior to this fix, `resolve_ty` used the bare name, found nothing in
    /// the record-layout map, fell through to `primitive_to_llvm`, and
    /// emitted the D10-violation diagnostic. The pipeline now succeeds: the
    /// alloca for `local_0` acquires the tagged-union struct type registered
    /// under `"Option$$i64"`.
    #[test]
    fn generic_enum_local_resolves_by_mangled_key() {
        use hew_hir::mangle as hir_mangle;

        // Build the mangled key the same way `register_enum_layouts` does.
        let mangled = hir_mangle("Option", &[ResolvedTy::I64]);
        assert_eq!(
            mangled, "Option$$i64",
            "mangle scheme must match registration"
        );

        // EnumLayout with that mangled name: `enum Option<i64> { None, Some(i64) }`
        let enum_layout = EnumLayout {
            name: mangled,
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "None".to_string(),
                    field_tys: vec![],
                },
                MachineVariantLayout {
                    name: "Some".to_string(),
                    field_tys: vec![ResolvedTy::I64],
                },
            ],
        };

        // Function that has `Option<i64>` as a local (local_0) but only
        // reads an i64 constant into the return slot — the alloca for
        // local_0 is what exercises `resolve_ty` with a non-empty args vec.
        let func = RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![
                // local_0: Option<i64> — must resolve via mangled key
                ResolvedTy::Named {
                    name: "Option".to_string(),
                    args: vec![ResolvedTy::I64],
                },
                // local_1: i64 — return value
                ResolvedTy::I64,
            ],
            blocks: vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![
                    Instr::ConstI64 {
                        dest: Place::Local(1),
                        value: 42,
                    },
                    Instr::Move {
                        dest: Place::ReturnSlot,
                        src: Place::Local(1),
                    },
                ],
                terminator: Terminator::Return,
            }],
            decisions: Vec::<DecisionFact>::new(),
        };

        let pipeline = IrPipeline {
            thir: Vec::new(),
            raw_mir: vec![func],
            checked_mir: Vec::new(),
            elaborated_mir: Vec::new(),
            diagnostics: Vec::new(),
            record_layouts: Vec::new(),
            actor_layouts: Vec::new(),
            supervisor_layouts: Vec::new(),
            machine_layouts: Vec::new(),
            enum_layouts: vec![enum_layout],
            regex_literals: Vec::new(),
        };

        let ctx = Context::create();
        let m = build_module(&ctx, &pipeline, "generic_enum_mangle_test")
            .expect("Option<i64> local must resolve via mangled key; bare-name lookup is wrong");
        assert!(
            m.verify().is_ok(),
            "emitted module must pass LLVM verify:\n{}",
            m.print_to_string().to_string()
        );
    }
}
