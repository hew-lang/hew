//! LLVM switched-resume coroutine emission — the codegen side of Hew's
//! stackless continuation substrate (R326/R327, W6.007).
//!
//! A suspending Hew function (actor `await`, `scope` join, blocking `.recv()`,
//! generator `yield`) lowers to an LLVM coroutine: a `presplitcoroutine`
//! function carrying `llvm.coro.*` intrinsics. LLVM's CoroSplit pass then turns
//! that single function into a ramp + `.resume` / `.destroy` / `.cleanup`
//! outlines, spilling all cross-suspend state into one heap frame. The runtime
//! side (`hew-runtime/src/cont.rs`, the `HewCont` ABI) owns the frame allocator
//! and the resume/done/poll/destroy verbs that drive it.
//!
//! This module owns three responsibilities:
//!
//! 1. [`emit_coro_intrinsic_token`] — a thin raw-`LLVMBuildCall2` helper for the
//!    token-returning intrinsics (`llvm.coro.id`, `llvm.coro.save`). inkwell
//!    0.9's safe value layer panics on a `token` value (`Unsupported basic type:
//!    Token`), so those two calls thread the token as an opaque `LLVMValueRef`
//!    via the active `inkwell::llvm_sys` re-export — no new dependency. Every
//!    non-token coro intrinsic stays on the normal inkwell builder.
//!
//! 2. [`emit_coro_prologue`] + [`CoroContext::emit_suspend`] +
//!    [`emit_coro_frame_free`] — emit the canonical switched-resume skeleton
//!    (entry / `coro.alloc`-gated frame allocation / `coro.begin` / suspend
//!    points / `cleanup` / `coro.end`) around a caller-supplied body, routing
//!    frame allocation to the runtime's `hew_cont_frame_alloc` /
//!    `hew_cont_frame_free` (NOT libc `malloc` — the wasip1 requirement). The
//!    pointer `coro.begin` returns is the `HewCont` handle.
//!
//! 3. [`run_coro_passes`] — runs LLVM's coro pass pipeline
//!    (`coro-early,coro-split,coro-cleanup`) when any function in the module
//!    carries the `presplitcoroutine` marker. CoroSplit MUST run before the
//!    backend sees the function, otherwise the `presplitcoroutine` body reaches
//!    instruction selection un-split and the intrinsics fail to lower.
//!
//! # Hard constraints (from the W6.006 feasibility spike — non-negotiable)
//!
//! - **`coro.id` promise pointer MUST be `ptr null`.** A non-null promise (the
//!   C++ `std::coroutine` promise slot) segfaults LLVM 22's
//!   `normalizeCoroutine`. Hew never uses the promise: yielded/awaited values
//!   thread through an explicit out-pointer (the `HewCont` value slot), which
//!   the substrate wants anyway.
//! - **inkwell panics on token-returning intrinsics.** Hence
//!   [`emit_coro_intrinsic_token`]; see responsibility 1.

use inkwell::attributes::Attribute;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::TargetMachine;
use inkwell::types::AsTypeRef;
use inkwell::values::{AsValueRef, FunctionValue, IntValue, PointerValue};
use inkwell::AddressSpace;

use inkwell::llvm_sys::core::LLVMBuildCall2;
use inkwell::llvm_sys::prelude::{LLVMBuilderRef, LLVMValueRef};

use crate::llvm::{CodegenError, CodegenResult, LlvmResultExt};

/// The runtime symbol the coro frame allocator routes to. Size-only shape that
/// `llvm.coro.alloc` → dynamic-allocation gating expects; see
/// `hew-runtime/src/cont.rs`.
pub const CONT_FRAME_ALLOC: &str = "hew_cont_frame_alloc";
/// The runtime symbol the coro frame free routes to. Pointer-only shape that
/// `llvm.coro.free` produces; see `hew-runtime/src/cont.rs`.
pub const CONT_FRAME_FREE: &str = "hew_cont_frame_free";

/// Resolve a non-overloaded coro intrinsic declaration via inkwell's typed
/// `Intrinsic::find`, the same path codegen uses for `llvm.trap`. Fails closed
/// if the LLVM build does not expose the intrinsic.
fn coro_intrinsic<'ctx>(
    llvm_mod: &LlvmModule<'ctx>,
    name: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    coro_intrinsic_typed(llvm_mod, name, &[])
}

/// Resolve a coro intrinsic declaration, passing `type_args` for an overloaded
/// intrinsic. `llvm.coro.size` is overloaded on its result type, so it must be
/// resolved as the base name `llvm.coro.size` with `[i64]` — the `.i64` suffix
/// in the mangled name is what `get_declaration` PRODUCES, not what
/// `Intrinsic::find` consumes.
fn coro_intrinsic_typed<'ctx>(
    llvm_mod: &LlvmModule<'ctx>,
    name: &str,
    type_args: &[inkwell::types::BasicTypeEnum<'ctx>],
) -> CodegenResult<FunctionValue<'ctx>> {
    Intrinsic::find(name)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!("{name} intrinsic not in this LLVM build"))
        })?
        .get_declaration(llvm_mod, type_args)
        .ok_or_else(|| CodegenError::FailClosed(format!("{name} declaration failed")))
}

/// Emit a call to a token-returning coro intrinsic via raw `LLVMBuildCall2`,
/// returning the result as an opaque `LLVMValueRef` (a `token`).
///
/// inkwell 0.9's safe value layer panics on a `token` value, so `llvm.coro.id`
/// and `llvm.coro.save` — both of which return a `token` the verifier requires
/// downstream coro intrinsics consume — cannot be threaded through the safe
/// builder. This helper drops to the active `inkwell::llvm_sys` (no new
/// dependency: inkwell re-exports the exact llvm-sys it links) to issue the
/// call and keep the token opaque. The non-token intrinsics
/// (`begin`→ptr, `suspend`→i8, `size`→i64, `free`→ptr, `alloc`→i1) thread fine
/// through the normal inkwell builder.
///
/// # Safety
///
/// `builder` must be a live LLVM builder positioned at a valid insertion point,
/// and `args` must match `intrinsic`'s declared parameter types. The returned
/// `LLVMValueRef` is owned by the LLVM context (no manual free).
pub unsafe fn emit_coro_intrinsic_token(
    builder: LLVMBuilderRef,
    intrinsic: FunctionValue<'_>,
    args: &mut [LLVMValueRef],
    name: &std::ffi::CStr,
) -> LLVMValueRef {
    // SAFETY: caller guarantees a live builder at a valid insertion point and
    // args matching the intrinsic signature. `get_type`/`as_value_ref` produce
    // the callee's function type + value the C API consumes.
    unsafe {
        LLVMBuildCall2(
            builder,
            intrinsic.get_type().as_type_ref(),
            intrinsic.as_value_ref(),
            args.as_mut_ptr(),
            args.len() as u32,
            name.as_ptr(),
        )
    }
}

/// Declare (or fetch) the `hew_cont_frame_alloc(size: u64) -> ptr` runtime symbol.
///
/// `hew_cont_frame_alloc` takes `u64` (not `size_t`) on every target — including
/// wasm32 — matching `hew_alloc`. The LLVM declaration therefore always uses
/// `i64`, never `i32`, regardless of the target pointer or size width. Using
/// `runtime_size_ty` (i32 on wasm32) would produce an `(i32)->ptr` import that
/// the wasm-ld/instantiation validator rejects against the runtime's `(i64)->ptr`
/// export (exact-signature matching). See `hew-runtime/src/cont.rs:134`.
fn declare_frame_alloc<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
) -> FunctionValue<'ctx> {
    if let Some(f) = llvm_mod.get_function(CONT_FRAME_ALLOC) {
        return f;
    }
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    llvm_mod.add_function(
        CONT_FRAME_ALLOC,
        ptr_ty.fn_type(&[ctx.i64_type().into()], false),
        Some(Linkage::External),
    )
}

/// Declare (or fetch) the `hew_cont_frame_free(ptr)` runtime symbol.
fn declare_frame_free<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
) -> FunctionValue<'ctx> {
    if let Some(f) = llvm_mod.get_function(CONT_FRAME_FREE) {
        return f;
    }
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    llvm_mod.add_function(
        CONT_FRAME_FREE,
        ctx.void_type().fn_type(&[ptr_ty.into()], false),
        Some(Linkage::External),
    )
}

/// Handles to the live state of a coroutine being emitted, passed to a body
/// callback so it can place suspend points and read the frame handle.
pub struct CoroContext<'a, 'ctx> {
    pub ctx: &'ctx Context,
    pub llvm_mod: &'a LlvmModule<'ctx>,
    pub builder: &'a inkwell::builder::Builder<'ctx>,
    /// The coroutine function being filled (carries `presplitcoroutine`).
    pub function: FunctionValue<'ctx>,
    /// The `coro.begin` handle — the `HewCont` frame pointer.
    pub handle: PointerValue<'ctx>,
    /// The opaque `coro.id` token, needed by `coro.free` / `coro.end`.
    pub id_token: LLVMValueRef,
}

impl<'a, 'ctx> CoroContext<'a, 'ctx> {
    /// Emit a suspend point + its 3-way switch. After `coro.suspend`, control
    /// goes to `suspend_return_block` (default — return to the caller/executor),
    /// `resume_block` (case 0 — a `hew_cont_resume` advanced the coroutine), or
    /// `cleanup_block` (case 1 — a `hew_cont_destroy` is tearing it down). The
    /// caller wires the body continuation into `resume_block` and the teardown
    /// into `cleanup_block`. `is_final` marks the terminal suspend (`i1 true`),
    /// after which `coro.done` becomes true.
    ///
    /// Models the spike's `loop.body` arm: `%s = coro.suspend(token none,
    /// i1 is_final); switch i8 %s [default -> caller-return, 0 -> resume,
    /// 1 -> cleanup]`.
    pub fn emit_suspend(
        &self,
        resume_block: inkwell::basic_block::BasicBlock<'ctx>,
        cleanup_block: inkwell::basic_block::BasicBlock<'ctx>,
        suspend_return_block: inkwell::basic_block::BasicBlock<'ctx>,
        is_final: bool,
        label: &str,
    ) -> CodegenResult<()> {
        let suspend = coro_intrinsic(self.llvm_mod, "llvm.coro.suspend")?;
        let i1_ty = self.ctx.bool_type();
        // Emit `%save = coro.save(handle)` immediately before `coro.suspend` and
        // thread its token in. This is the canonical switched-resume shape LLVM
        // documents (`%save = coro.save(%hdl); coro.suspend(token %save, …)`):
        // CoroSplit uses the `coro.save` token to mark where the suspend's
        // resume index is stored in the frame. A `coro.suspend(token none, …)`
        // (no save) is NOT recognised by LLVM 22's CoroSplit as a real suspend
        // point — the function is left un-split and the intrinsics reach the
        // backend un-lowered. `coro.save` returns a `token`, so it routes
        // through the raw path like `coro.id`.
        let coro_save = coro_intrinsic(self.llvm_mod, "llvm.coro.save")?;
        let save_token = unsafe {
            let mut args: [LLVMValueRef; 1] = [self.handle.as_value_ref()];
            let name = std::ffi::CString::new(format!("{label}.save")).unwrap();
            emit_coro_intrinsic_token(self.builder.as_mut_ptr(), coro_save, &mut args, &name)
        };
        // `coro.suspend(token %save, i1 is_final)`.
        let s = unsafe {
            let mut args: [LLVMValueRef; 2] = [
                save_token,
                i1_ty.const_int(u64::from(is_final), false).as_value_ref(),
            ];
            let name = std::ffi::CString::new(format!("{label}.s")).unwrap();
            emit_coro_intrinsic_token(self.builder.as_mut_ptr(), suspend, &mut args, &name)
        };
        // switch i8 %s, default=suspend_return [0 -> resume, 1 -> cleanup]
        let i8_ty = self.ctx.i8_type();
        unsafe {
            let sw = inkwell::llvm_sys::core::LLVMBuildSwitch(
                self.builder.as_mut_ptr(),
                s,
                suspend_return_block.as_mut_ptr(),
                2,
            );
            inkwell::llvm_sys::core::LLVMAddCase(
                sw,
                i8_ty.const_int(0, false).as_value_ref(),
                resume_block.as_mut_ptr(),
            );
            inkwell::llvm_sys::core::LLVMAddCase(
                sw,
                i8_ty.const_int(1, false).as_value_ref(),
                cleanup_block.as_mut_ptr(),
            );
        }
        Ok(())
    }
}

/// Emit the switched-resume coroutine prologue into `function`'s entry block and
/// return a [`CoroContext`] the caller uses to place suspend points + the body.
///
/// Emits, exactly as the W6.006 spike proved:
/// ```text
/// entry:
///   %id   = coro.id(0, ptr null /*promise — MUST be null*/, ptr null, ptr null)
///   %need = coro.alloc(%id)
///   br %need, dyn.alloc, coro.begin
/// dyn.alloc:
///   %size = coro.size.i64()
///   %mem  = hew_cont_frame_alloc(%size)
///   br coro.begin
/// coro.begin:
///   %hdl  = coro.begin(%id, phi[null, %mem])   ; %hdl is the HewCont handle
/// ```
/// The builder is left positioned at the end of `coro.begin`, ready for the
/// body. The caller is responsible for the suspend points, the `cleanup` block
/// (frame-owned value drops + `coro.free` → `hew_cont_frame_free`), and the
/// `coro.end` in the final return block.
pub fn emit_coro_prologue<'a, 'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &'a LlvmModule<'ctx>,
    builder: &'a inkwell::builder::Builder<'ctx>,
    function: FunctionValue<'ctx>,
) -> CodegenResult<CoroContext<'a, 'ctx>> {
    // Mark the function as a presplit coroutine so CoroSplit processes it.
    // MUST be an enum attribute (unquoted in text IR): CoroSplit checks
    // `F.hasFnAttr(Attribute::PresplitCoroutine)`, which resolves enum attrs
    // only. `create_string_attribute` produces a *quoted* string attribute that
    // is invisible to that check — CoroSplit silently skips the function and
    // the coro intrinsics reach instruction selection un-lowered (LLVM ERROR:
    // Cannot select: intrinsic %llvm.coro.size). The enum kind ID 0 returned by
    // `get_named_enum_kind_id` for an unknown name — verify nonzero at runtime.
    let kind_id = Attribute::get_named_enum_kind_id("presplitcoroutine");
    if kind_id == 0 {
        // JUSTIFIED: LLVM must expose this enum attribute for CoroSplit to see
        // Hew coroutine ramps; release builds must fail closed if that changes.
        return Err(CodegenError::FailClosed(
            "presplitcoroutine is not a known LLVM enum attribute in this build".into(),
        ));
    }
    let presplit = ctx.create_enum_attribute(kind_id, 0);
    function.add_attribute(inkwell::attributes::AttributeLoc::Function, presplit);

    let entry = ctx.append_basic_block(function, "entry");
    let dyn_alloc = ctx.append_basic_block(function, "dyn.alloc");
    let begin = ctx.append_basic_block(function, "coro.begin");

    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();

    // entry: id = coro.id(0, ptr null, ptr null, ptr null) — promise MUST be null.
    builder.position_at_end(entry);
    let coro_id = coro_intrinsic(llvm_mod, "llvm.coro.id")?;
    let id_token = unsafe {
        let null_ptr = ptr_ty.const_null();
        let mut args: [LLVMValueRef; 4] = [
            i32_ty.const_int(0, false).as_value_ref(),
            null_ptr.as_value_ref(), // promise — null (spike constraint 1)
            null_ptr.as_value_ref(),
            null_ptr.as_value_ref(),
        ];
        let name = std::ffi::CString::new("coro.id").unwrap();
        emit_coro_intrinsic_token(builder.as_mut_ptr(), coro_id, &mut args, &name)
    };

    // need = coro.alloc(id) — i1 gating dynamic allocation. `coro.alloc` takes
    // the token, so it routes through the raw path; its i1 result re-imports to
    // an inkwell IntValue.
    let coro_alloc = coro_intrinsic(llvm_mod, "llvm.coro.alloc")?;
    let need = unsafe {
        let mut args: [LLVMValueRef; 1] = [id_token];
        let name = std::ffi::CString::new("coro.need.alloc").unwrap();
        let raw = emit_coro_intrinsic_token(builder.as_mut_ptr(), coro_alloc, &mut args, &name);
        // SAFETY: coro.alloc returns an i1; raw is that LLVMValueRef.
        IntValue::new(raw)
    };
    builder
        .build_conditional_branch(need, dyn_alloc, begin)
        .llvm_ctx("coro.alloc conditional branch")?;

    // dyn.alloc: size = coro.size.i64(); mem = hew_cont_frame_alloc(size).
    // `coro.size` is overloaded on its result type — resolve the base name with
    // the i64 type arg so get_declaration mangles it to `llvm.coro.size.i64`.
    // `hew_cont_frame_alloc` takes `u64` on all targets (NOT `size_t`), so the
    // i64 result flows straight through — no truncation for wasm32.
    builder.position_at_end(dyn_alloc);
    let coro_size = coro_intrinsic_typed(llvm_mod, "llvm.coro.size", &[ctx.i64_type().into()])?;
    let size_i64 = builder
        .build_call(coro_size, &[], "coro.size")
        .llvm_ctx("coro.size call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("coro.size returned no value".into()))?
        .into_int_value();
    let frame_alloc = declare_frame_alloc(ctx, llvm_mod);
    let mem = builder
        .build_call(frame_alloc, &[size_i64.into()], "coro.frame")
        .llvm_ctx("hew_cont_frame_alloc call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("frame alloc returned no value".into()))?
        .into_pointer_value();
    builder
        .build_unconditional_branch(begin)
        .llvm_ctx("dyn.alloc -> coro.begin branch")?;

    // coro.begin: hdl = coro.begin(id, phi[null, mem]). `coro.begin` takes the
    // token first arg, so it routes through the raw path; its ptr result
    // re-imports to an inkwell PointerValue — the HewCont handle.
    builder.position_at_end(begin);
    let mem_phi = builder
        .build_phi(ptr_ty, "coro.mem")
        .llvm_ctx("coro.mem phi")?;
    mem_phi.add_incoming(&[(&ptr_ty.const_null(), entry), (&mem, dyn_alloc)]);
    let coro_begin = coro_intrinsic(llvm_mod, "llvm.coro.begin")?;
    let handle = unsafe {
        let mut args: [LLVMValueRef; 2] = [
            id_token,
            mem_phi.as_basic_value().into_pointer_value().as_value_ref(),
        ];
        let name = std::ffi::CString::new("coro.handle").unwrap();
        let raw = emit_coro_intrinsic_token(builder.as_mut_ptr(), coro_begin, &mut args, &name);
        // SAFETY: coro.begin returns a ptr (the frame handle); raw is it.
        PointerValue::new(raw)
    };

    Ok(CoroContext {
        ctx,
        llvm_mod,
        builder,
        function,
        handle,
        id_token,
    })
}

/// Emit the coroutine cleanup arm: free the frame via `coro.free` →
/// `hew_cont_frame_free`, then BRANCH into the shared `join_block` (the
/// suspend-return block that carries the single fallthrough `coro.end` + `ret`).
///
/// This is the `coro.destroy` teardown half of the epilogue. It deliberately
/// does NOT emit `coro.end` or `ret`: LLVM permits exactly one fallthrough
/// `coro.end` per coroutine, and that single one lives in `join_block`, reached
/// by BOTH this cleanup edge and the suspend-return (yield-to-executor) edge.
/// Owning a second `coro.end` here is what made CoroSplit emit `unreachable` for
/// the suspend-return block inside the `.resume` outline, crashing the second
/// `await` (a multi-suspend body's later yield routes through that outline).
///
/// The caller positions the builder at the cleanup block first and drops any
/// frame-owned heap values BEFORE calling this (single teardown owner discipline
/// — see `hew-runtime/src/cont.rs`). After this returns the builder is left at
/// the end of `free_block`'s branch; the caller emits the single `coro.end` +
/// `ret` into `join_block`.
pub fn emit_coro_frame_free<'ctx>(
    cc: &CoroContext<'_, 'ctx>,
    free_block: inkwell::basic_block::BasicBlock<'ctx>,
    join_block: inkwell::basic_block::BasicBlock<'ctx>,
) -> CodegenResult<()> {
    let ptr_ty = cc.ctx.ptr_type(AddressSpace::default());
    // %freemem = coro.free(id, hdl); br (freemem != null) -> dyn.free, join
    let coro_free = coro_intrinsic(cc.llvm_mod, "llvm.coro.free")?;
    let freemem = unsafe {
        let mut args: [LLVMValueRef; 2] = [cc.id_token, cc.handle.as_value_ref()];
        let name = std::ffi::CString::new("coro.freemem").unwrap();
        let raw = emit_coro_intrinsic_token(cc.builder.as_mut_ptr(), coro_free, &mut args, &name);
        // SAFETY: coro.free returns a ptr (the frame mem or null); raw is it.
        PointerValue::new(raw)
    };
    let is_null = cc
        .builder
        .build_is_null(freemem, "coro.freemem.isnull")
        .llvm_ctx("coro.free null check")?;
    cc.builder
        .build_conditional_branch(is_null, join_block, free_block)
        .llvm_ctx("coro.free branch")?;

    // dyn.free: hew_cont_frame_free(freemem); br join
    cc.builder.position_at_end(free_block);
    let frame_free = declare_frame_free(cc.ctx, cc.llvm_mod);
    cc.builder
        .build_call(frame_free, &[freemem.into()], "")
        .llvm_ctx("hew_cont_frame_free call")?;
    cc.builder
        .build_unconditional_branch(join_block)
        .llvm_ctx("dyn.free -> join branch")?;

    let _ = ptr_ty;
    Ok(())
}

/// Emit the single fallthrough `coro.end(hdl, false, none)` followed by
/// `ret ptr hdl` into the block the builder is currently positioned at.
///
/// This is the coroutine ramp's one return point, shared by the suspend-return
/// (yield-to-executor) edge and the cleanup (frame-free) edge. CoroSplit reads
/// this `coro.end` to mark where the `.resume`/`.destroy` outlines return to the
/// executor — there must be EXACTLY ONE fallthrough `coro.end` per coroutine, so
/// every yield and every teardown path joins this block rather than carrying its
/// own `coro.end`. The caller positions the builder at the shared
/// suspend-return block before calling this.
pub fn emit_coro_end_ret<'ctx>(cc: &CoroContext<'_, 'ctx>) -> CodegenResult<()> {
    let coro_end = coro_intrinsic(cc.llvm_mod, "llvm.coro.end")?;
    unsafe {
        let c = cc.ctx.raw();
        let token_ty = inkwell::llvm_sys::core::LLVMTokenTypeInContext(c);
        let none = inkwell::llvm_sys::core::LLVMConstNull(token_ty);
        let mut args: [LLVMValueRef; 3] = [
            cc.handle.as_value_ref(),
            cc.ctx.bool_type().const_int(0, false).as_value_ref(),
            none,
        ];
        let name = std::ffi::CString::new("").unwrap();
        emit_coro_intrinsic_token(cc.builder.as_mut_ptr(), coro_end, &mut args, &name);
    }
    cc.builder
        .build_return(Some(&cc.handle))
        .llvm_ctx("coro suspend-return coro.end + ret handle")?;
    Ok(())
}

/// Whether any function in the module carries the `presplitcoroutine` attribute
/// (i.e. is a coroutine CoroSplit must process before the backend).
///
/// Checks the *enum* attribute (the kind `emit_coro_prologue` adds via
/// `create_enum_attribute`). `get_string_attribute` would always return `None`
/// for an enum attribute, so we use `get_enum_attribute` with the kind ID.
pub fn module_has_coroutines(llvm_mod: &LlvmModule<'_>) -> bool {
    let kind_id = Attribute::get_named_enum_kind_id("presplitcoroutine");
    if kind_id == 0 {
        return false;
    }
    let mut f = llvm_mod.get_first_function();
    while let Some(func) = f {
        if func
            .get_enum_attribute(inkwell::attributes::AttributeLoc::Function, kind_id)
            .is_some()
        {
            return true;
        }
        f = func.get_next_function();
    }
    false
}

/// Run LLVM's coroutine lowering passes over the module.
///
/// `coro-early` (function scope) → `coro-split` (cgscc; the outline-producing
/// pass) → `coro-cleanup` (function scope). After this every
/// `presplitcoroutine` function is replaced by its ramp + `.resume` /
/// `.destroy` / `.cleanup` outlines with the intrinsics lowered, ready for the
/// backend. Idempotent + cheap when no coroutine is present (the caller gates
/// on [`module_has_coroutines`]).
///
/// MUST run before `TargetMachine::write_to_file`: a `presplitcoroutine` body
/// that reaches instruction selection un-split fails to lower the coro
/// intrinsics. The standard `-O0` module pipeline includes these, but the
/// codegen object-emission path uses `write_to_file` directly (no module
/// pipeline), so they are run explicitly here.
pub fn run_coro_passes(llvm_mod: &LlvmModule<'_>, machine: &TargetMachine) -> CodegenResult<()> {
    let options = inkwell::passes::PassBuilderOptions::create();
    // `coro-split` is a CGSCC pass: under the new pass manager it MUST be
    // wrapped in the `cgscc(...)` adaptor or it runs at module scope as a
    // no-op — the `presplitcoroutine` body then reaches the backend un-split
    // and the coro intrinsics fail to select. `coro-early` (function scope) and
    // `coro-cleanup` (function scope) run as plain pipeline entries; only the
    // split pass needs the explicit CGSCC nesting.
    llvm_mod
        .run_passes(
            "coro-early,cgscc(coro-split),coro-cleanup",
            machine,
            options,
        )
        .map_err(|e| CodegenError::Llvm(format!("coro pass pipeline failed: {e}")))
}
