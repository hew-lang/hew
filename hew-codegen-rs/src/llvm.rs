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

use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

use hew_mir::{CmpPred, Instr, IrPipeline, Place, RawMirFunction, Terminator};
use hew_types::ResolvedTy;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::TargetMachine;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
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

struct FnCtx<'ctx> {
    builder: Builder<'ctx>,
    return_slot: PointerValue<'ctx>,
    return_ty: BasicTypeEnum<'ctx>,
    /// Local-register id → (stack slot, slot's LLVM type). Keyed by the
    /// `Place::Local(N)` index — an MIR identity, not a checker derivative.
    locals: HashMap<u32, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
    /// Block id → LLVM `BasicBlock`. Populated up front so terminators can
    /// name forward targets.
    blocks: HashMap<u32, inkwell::basic_block::BasicBlock<'ctx>>,
}

/// Module-level symbol table populated by the declaration pass. Keyed by
/// function name (a normal module-level identity); the value carries the
/// LLVM function value plus its return type so `Terminator::Call` can
/// resolve forward without re-deriving anything from the MIR shape.
type FnSymbolMap<'ctx> = HashMap<String, (FunctionValue<'ctx>, BasicTypeEnum<'ctx>)>;

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
    fn_ctx: &FnCtx<'ctx>,
    place: Place,
) -> CodegenResult<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
    match place {
        Place::Local(id) => fn_ctx.locals.get(&id).copied().ok_or_else(|| {
            CodegenError::FailClosed(format!("local {id} not allocated before use"))
        }),
        Place::ReturnSlot => Ok((fn_ctx.return_slot, fn_ctx.return_ty)),
        // M2 unified-concurrency substrate Place variants (declared
        // scaffold; no HIR construction surface yet). Codegen for the
        // `hew_duplex_*` C-ABI lowering lands in M2 slice 5. Fail
        // closed at this seam so any reach-through during the M2
        // ramp-up surfaces immediately rather than emitting a binary
        // with a misrouted load/store. LESSONS: boundary-fail-closed.
        Place::DuplexHandle(_)
        | Place::LambdaActorHandle(_)
        | Place::SendHalf(_)
        | Place::RecvHalf(_) => Err(CodegenError::FailClosed(
            "M2 Duplex / lambda-actor Place lowering is not yet wired; \
             slice 5 wires the hew_duplex_* C-ABI lowering"
                .to_string(),
        )),
    }
}

fn lower_instruction(fn_ctx: &FnCtx<'_>, instr: &Instr) -> CodegenResult<()> {
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
        Instr::Drop {
            place: _,
            ty: _,
            drop_fn,
        } => {
            // Fail-closed substrate guard. `drop_fn: None` is a
            // legitimate no-op for `@linear` (no implicit drop) and
            // for non-Named drops. `drop_fn: Some(_)` means the
            // elaborator decided a destructor must run — but the
            // backend dispatch for that destructor has not been
            // wired yet. Returning `FailClosed` here ensures any
            // future construction surface that emits a real
            // `Instr::Drop { drop_fn: Some(_) }` fails the build
            // loudly rather than silently leaking the resource. The
            // C4 follow-on PR replaces this with a real call to the
            // registered close method. LESSONS:
            // boundary-fail-closed (codegen-side complement to the
            // checker-output-boundary row).
            if let Some(name) = drop_fn {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::Drop carries drop_fn={name:?} but codegen \
                     dispatch is not wired yet; refusing to silently \
                     emit a no-op for a resource that must be closed",
                )));
            }
            let _ = ctx;
        }
    }
    Ok(())
}

fn lower_terminator<'ctx>(
    fn_ctx: &FnCtx<'ctx>,
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
        Terminator::Panic => {
            // Cluster 1 never constructs Panic. If it ever surfaces here,
            // fail closed rather than emit a no-op terminator that LLVM
            // would reject anyway.
            return Err(CodegenError::Unsupported(
                "Terminator::Panic — panic lowering is Cluster 3 work",
            ));
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
                    // TODO: emit actor-ask lowering for the ask arm.
                    // The runtime must (a) issue an ask on a reply
                    // channel, (b) park the current coroutine, (c)
                    // resume with the reply on win, (d) on loss
                    // withdraw the envelope from the target mailbox
                    // by correlation id if not yet dispatched, else
                    // tombstone the reply sink so a late reply is
                    // classified as OrphanedAsk and dropped
                    // silently. Note: `Terminator::Send` is also
                    // declared-only today; the actor-call surface
                    // needs full lowering before this arm can ship.
                    "select{} actor-ask arm awaits runtime substrate: \
                     actor-call lowering (Terminator::Send) plus \
                     correlation-id withdraw / reply-sink tombstone \
                     for losing arms"
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

fn lower_function<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    func: &RawMirFunction,
    fn_symbols: &FnSymbolMap<'ctx>,
) -> CodegenResult<()> {
    let (llvm_fn, return_ty_llvm) = *fn_symbols.get(&func.name).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "function `{}` was not declared before body lowering",
            func.name
        ))
    })?;
    let _ = llvm_mod;

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
        builder,
        return_slot,
        return_ty: return_ty_llvm,
        locals,
        blocks: blocks.clone(),
    };

    for block in &func.blocks {
        let bb = *blocks.get(&block.id).expect("block in map");
        fn_ctx.builder.position_at_end(bb);
        for instr in &block.instructions {
            lower_instruction(&fn_ctx, instr)?;
        }
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
        lower_function(ctx, &llvm_mod, func, &fn_symbols)?;
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
            msg.contains("actor-ask") && msg.contains("Terminator::Send"),
            "actor-ask FailClosed must name the missing substrate \
             (Terminator::Send actor-call lowering): {msg}"
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
