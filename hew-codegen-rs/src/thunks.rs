//! Synthesized thunk / wrapper / trampoline emission for the LLVM backend.
//!
//! Pure relocation (R4 god-module carve) of the standalone thunk-and-wrapper
//! emitters out of llvm.rs: the derived `eq`/`hash` thunk bodies, the
//! closure-env free thunk and call-context resolver, the tuple/collection
//! in-place thunk bodies, the owned-element / lambda-env drop thunks, the
//! spawn-task direct/closure wrappers and their name builders, and the actor
//! terminate / dispatch trampolines.
//!
//! Mirrors the `crate::coro` carve: shared codegen context and the clone/drop
//! in-place machinery are imported from `crate::llvm`, which calls back via
//! `crate::thunks::*`. No behaviour change — every emitted `.ll` is
//! byte-identical before and after.

use inkwell::context::Context;
use std::collections::HashSet;

use inkwell::intrinsics::Intrinsic;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, FloatType, IntType, StructType};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

use hew_hir::mangle_dotted_name;
use hew_mir::{
    model::CoalesceKeyKind, ActorLayout, IoHandleKind, LambdaEnvFieldDrop, Place,
    SpawnEnvFieldOwnership, StateFieldCloneKind,
};
use hew_runtime::internal::types::HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH;
use hew_types::{BuiltinType, ResolvedTy};

use crate::layout::mix_into_hash_acc;
#[allow(unused_imports)]
use crate::llvm::*;
use crate::runtime_abi::intern_runtime_decl;

fn task_wrapper_name(callee_symbol: &str) -> String {
    format!("__hew_task_wrapper_{}", sanitize_symbol(callee_symbol))
}

fn task_closure_wrapper_name(fn_symbol: &str) -> String {
    format!("__hew_task_closure_wrapper_{}", sanitize_symbol(fn_symbol))
}

pub(crate) fn get_or_create_task_wrapper<'ctx>(
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
    let body_call = builder
        .build_call(callee_value, &[ctx_param.into()], "task_body_call")
        .llvm_ctx("task wrapper body call")?;

    // Value-returning task: publish the body's `T` result through the task's
    // own result buffer BEFORE marking the task complete, so the awaiter's
    // resume edge reads it via `hew_task_get_result`. The adapter
    // (`__hew_task_entry_<fn>`, TaskEntry call-conv) returns its body's `T`; a
    // unit task returns the i8 unit stand-in and publishes nothing.
    //
    // Drop-exactly-once: `hew_task_set_result` deep-copies `sizeof(T)` bytes of
    // the value REPRESENTATION into a malloc buffer the task owns and frees
    // (as raw bytes) at scope join. For a managed `T` (string/record) those
    // bytes are the owning handle, MOVED out of the body's return slot here and
    // MOVED out of the buffer into the awaiter's binding on resume — the buffer
    // free never drops the handle, so it lives exactly once.
    if !callee_returns_unit {
        let result_val = body_call.try_as_basic_value().basic().ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "SpawnTaskDirect value-task callee `{callee_symbol}` returned void but \
                 its MIR return type is non-unit"
            ))
        })?;
        // Stack slot holding the value representation; `hew_task_set_result`
        // copies `sizeof(T)` bytes from it into the task-owned result buffer.
        let result_slot = builder
            .build_alloca(callee_return_ty, "task_result_slot")
            .llvm_ctx("task wrapper result slot alloca")?;
        builder
            .build_store(result_slot, result_val)
            .llvm_ctx("task wrapper result store")?;
        let (size, _align) = abi_size_align(callee_return_ty, Some(fn_ctx.target_data))?;
        let size_ty = runtime_size_ty(fn_ctx.ctx, fn_ctx.llvm_mod);
        let size_val = size_ty.const_int(size, false);
        let set_result = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_task_set_result",
        )?;
        builder
            .build_call(
                set_result,
                &[task_param.into(), result_slot.into(), size_val.into()],
                "hew_task_set_result_call",
            )
            .llvm_ctx("hew_task_set_result call")?;
    }

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
        .llvm_ctx("hew_task_complete_threaded call")?;
    builder.build_return(None).llvm_ctx("task wrapper return")?;
    Ok(wrapper)
}

pub(crate) fn emit_spawn_task_direct(
    fn_ctx: &FnCtx<'_, '_>,
    task: Place,
    callee_symbol: &str,
) -> CodegenResult<()> {
    let spilled_ctx = fn_ctx.execution_context.ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SpawnTaskDirect spawn site for `{callee_symbol}` requires a caller-side \
             ctx-bearing execution context; MIR lowering should reject Default-callconv \
             enclosing functions before codegen"
        ))
    })?;
    // W6.010: in a coroutine the spilled `ctx` param is the unwound dispatch
    // frame after a suspend; `hew_task_spawn_thread_with_inherited_context`
    // immediately derefs `parent_ctx` to snapshot supervisor/trace/cancel, so a
    // scope/fork placed after an await must pass the resume-installed live
    // context, not the dangling param. The live context restores
    // actor/arena/lock_seat/supervisor; task_scope/cancel_token/trace are
    // fail-safe null — the spawned child inherits no trace/cancel, which the
    // runtime tolerates (null parent_cancel_token → no child token; trace is a
    // by-value copy). A non-coroutine handler keeps the cheaper spilled param.
    let parent_ctx = live_execution_context_ptr(fn_ctx, spilled_ctx)?;
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
        .llvm_ctx("hew_task_spawn_thread_with_inherited_context call")?;
    Ok(())
}

/// Synthesize the per-lambda capture-env dropper:
/// `extern fn(env: *mut c_void)` — drops each owned env field by its
/// declared class, then frees the heap env allocation. The runtime calls
/// it exactly once after the dispatch loop stops; it is the SOLE teardown
/// owner of the boxed env (`lifecycle-symmetry`). Field classes:
///
/// * `None` — BitCopy, nothing to release.
/// * `String` — the env owns an independent `hew_string_clone`; release
///   via `hew_string_drop`.
/// * `WeakSelfHandle` — the downgraded self handle from the
///   MakeLambdaActor back-fill; release via `hew_lambda_actor_weak_drop`
///   (a weak drop never releases the actor itself). Null-safe at the
///   runtime, so an env dropped before back-fill (construction failure)
///   stays sound.
pub(crate) fn emit_lambda_env_dropper<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    name: &str,
    env_struct: StructType<'ctx>,
    field_drops: &[LambdaEnvFieldDrop],
) -> CodegenResult<FunctionValue<'ctx>> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let fn_ty = ctx.void_type().fn_type(&[ptr_ty.into()], false);
    let func = llvm_mod.add_function(name, fn_ty, Some(Linkage::Internal));
    let entry = ctx.append_basic_block(func, "entry");
    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    let env_ptr = func
        .get_nth_param(0)
        .ok_or_else(|| CodegenError::FailClosed(format!("env dropper `{name}` missing env param")))?
        .into_pointer_value();
    for (idx, class) in field_drops.iter().enumerate() {
        // Symbol AND declared signature both derive from the typed field
        // class in one match — no string self-compare to re-derive what
        // the class already knows.
        // `hew_string_drop(s: *mut c_char)` returns void;
        // `hew_lambda_actor_weak_drop(weak)` returns i32 (ignored).
        let (drop_symbol, sig) = match class {
            LambdaEnvFieldDrop::None => continue,
            LambdaEnvFieldDrop::String => (
                "hew_string_drop",
                ctx.void_type().fn_type(&[ptr_ty.into()], false),
            ),
            LambdaEnvFieldDrop::WeakSelfHandle => (
                "hew_lambda_actor_weak_drop",
                ctx.i32_type().fn_type(&[ptr_ty.into()], false),
            ),
        };
        let field_idx = u32::try_from(idx).map_err(|_| {
            CodegenError::FailClosed("lambda env field count exceeds u32::MAX".into())
        })?;
        let field_ptr = builder
            .build_struct_gep(
                env_struct,
                env_ptr,
                field_idx,
                &format!("env_drop_field_{idx}_ptr"),
            )
            .llvm_ctx("lambda env dropper gep")?;
        let field_val = builder
            .build_load(ptr_ty, field_ptr, &format!("env_drop_field_{idx}"))
            .llvm_ctx("lambda env dropper field load")?;
        let drop_fn = llvm_mod
            .get_function(drop_symbol)
            .unwrap_or_else(|| llvm_mod.add_function(drop_symbol, sig, Some(Linkage::External)));
        builder
            .build_call(drop_fn, &[field_val.into()], &format!("env_drop_{idx}"))
            .llvm_ctx("lambda env dropper field drop call")?;
    }
    let free_fn = get_or_declare_libc_free(ctx, llvm_mod);
    builder
        .build_call(free_fn, &[env_ptr.into()], "env_drop_free")
        .llvm_ctx("lambda env dropper free call")?;
    builder
        .build_return(None)
        .llvm_ctx("lambda env dropper ret")?;
    Ok(func)
}

/// Resolve the `void(*)(void*)` destructor for a reply type `R` as a function
/// pointer (or a null `ptr` for a bit-copy `R`), to register on the reply
/// channel via `hew_reply_channel_set_reply_drop_fn`. The channel's free leg
/// runs it on a delivered-but-never-consumed reply (await timeout / cancel),
/// releasing any heap embedded in `R` instead of leaking it (#1739, #1735).
///
/// Strategy mirrors the actor `state_drop_fn` thunk
/// (`emit_actor_state_drop_body`) — a runtime-registered typed drop thunk —
/// and reuses the canonical per-field drop machinery
/// (`classify_state_field_with_enum_layouts` → `emit_field_drop_step`) so the
/// reply drop and every other owned-value drop stay byte-for-byte consistent
/// (`lifecycle-symmetry`):
///   * **bit-copy / user-owned-handle `R`** → null: no embedded heap to
///     release; the free leg's null check skips the destructor and frees the
///     buffer alone (the prior behaviour).
///   * **record / enum `R`** → the already-synthesised
///     `__hew_{record,enum}_drop_inplace_<R>` thunk. Its body is seeded for
///     every actor-handler return type by `collect_xnode_codec_drop_seeds`, so
///     it is guaranteed defined; it is already the `void(void*)` in-place-drop
///     shape the channel slot needs.
///   * **leaf-owned `R`** (String/Bytes/Vec/HashMap/HashSet/Closure) → a thin
///     synthesised `__hew_reply_drop_<R>(*mut buf)` that drops `R` in place by
///     treating the byte-copied buffer as a single-field `{ R }` aggregate and
///     running the canonical field-drop step over it. Idempotent: the same `R`
///     reuses one thunk across every ask site.
///
/// Fail-closed: an owned `R` the classifier cannot map to a drop strategy is a
/// `CodegenError::FailClosed`, never a silent null (which would reinstate the
/// leak).
pub(crate) fn ask_reply_drop_thunk_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    reply_ty: &ResolvedTy,
) -> CodegenResult<PointerValue<'ctx>> {
    let ctx = fn_ctx.ctx;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    // Tuple reply: there is no `StateFieldCloneKind` arm for a tuple (the
    // classifier below fails closed on it), and no registry seed pass emits a
    // tuple drop body. Synthesise it via the SAME owned-tuple drop spine the
    // waiter-side `ElabDrop::TupleInPlace` consume uses
    // (`emit_tuple_drop_inplace_body_only` → `__hew_tuple_drop_inplace_<key>`),
    // so a never-consumed tuple reply and a consumed one drop byte-for-byte
    // identically (`lifecycle-symmetry`). A tuple carrying a nested owned tuple
    // fails closed inside the spine — exactly as the consume side does — so this
    // never silently under-drops.
    if let ResolvedTy::Tuple(elems) = reply_ty {
        let tuple_key = tuple_thunk_key(elems);
        let tuple_llvm_ty = resolve_ty(ctx, fn_ctx.target_data, reply_ty, fn_ctx.record_layouts)?;
        emit_tuple_drop_inplace_body_only(fn_ctx, &tuple_key, elems, tuple_llvm_ty)?;
        let helper = get_or_declare_tuple_drop_inplace(ctx, fn_ctx.llvm_mod, &tuple_key);
        if helper.count_basic_blocks() == 0 {
            return Err(CodegenError::FailClosed(format!(
                "owned ask reply tuple `{reply_ty:?}` resolved \
                 `__hew_tuple_drop_inplace_{tuple_key}` but it has no body after \
                 synthesis; refusing to register a dangling reply destructor (#1739)"
            )));
        }
        return Ok(helper.as_global_value().as_pointer_value());
    }

    // Classify R into the per-field drop kind the canonical drop emitter
    // consumes. The classifier needs a record-layout slice; reconstruct it from
    // the codegen resolved-field table exactly as `tuple_inplace_field_kinds`
    // does, so the reply path and the tuple/record paths share one authority.
    let record_layouts: Vec<hew_mir::RecordLayout> = fn_ctx
        .record_field_resolved_tys
        .iter()
        .map(|(name, tys)| hew_mir::RecordLayout {
            name: name.clone(),
            field_tys: tys.clone(),
            // Drop-classifier reconstruction (no `-g` consumer); names absent.
            field_names: Vec::new(),
        })
        .collect();
    let mut visited = HashSet::new();
    let kind = hew_mir::classify_state_field_with_enum_layouts(
        reply_ty,
        &record_layouts,
        fn_ctx.enum_layouts,
        &mut visited,
    )
    .map_err(|e| {
        CodegenError::FailClosed(format!(
            "owned ask reply type {reply_ty:?} is not drop-classifiable: {e}; a reply that \
             times out or is cancelled before consumption would leak its embedded heap \
             (#1739 reply-channel ownership)"
        ))
    })?;

    match &kind {
        // No embedded heap: the free leg frees the buffer alone (prior
        // behaviour). Opaque handles are user-owned (`.free()` is explicit) and
        // a borrowed `Connection` is not released by the reply path, so both are
        // also null here — matching `emit_field_drop_step`'s no-op arms.
        StateFieldCloneKind::BitCopy { .. }
        | StateFieldCloneKind::OpaqueHandle { .. }
        | StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Connection,
        } => Ok(ptr_ty.const_null()),
        // Record / enum reply: register the already-seeded in-place drop thunk.
        StateFieldCloneKind::UserRecord { name } => {
            Ok(
                get_or_declare_record_drop_inplace(ctx, fn_ctx.llvm_mod, name)
                    .as_global_value()
                    .as_pointer_value(),
            )
        }
        StateFieldCloneKind::Enum { name } => {
            Ok(get_or_declare_enum_drop_inplace(ctx, fn_ctx.llvm_mod, name)
                .as_global_value()
                .as_pointer_value())
        }
        // Leaf-owned reply: synthesise `__hew_reply_drop_<R>` over a `{ R }`
        // wrapper whose field 0 is R's ABI representation in the buffer.
        _ => {
            let sym = format!("__hew_reply_drop_{}", hew_hir::mangle_resolved_ty(reply_ty));
            if let Some(existing) = fn_ctx.llvm_mod.get_function(&sym) {
                return Ok(existing.as_global_value().as_pointer_value());
            }
            let field_abi = primitive_to_llvm(ctx, fn_ctx.target_data, reply_ty)?;
            let wrapper = ctx.struct_type(&[field_abi], false);
            let f = fn_ctx.llvm_mod.add_function(
                &sym,
                ctx.void_type().fn_type(&[ptr_ty.into()], false),
                Some(Linkage::Internal),
            );
            emit_aggregate_drop_inplace_body(
                ctx,
                fn_ctx.llvm_mod,
                f,
                wrapper,
                std::slice::from_ref(&kind),
            )?;
            Ok(f.as_global_value().as_pointer_value())
        }
    }
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
        .llvm_ctx("hew_task_get_env call")?
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
        .llvm_ctx("closure task body call")?;

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
        .llvm_ctx("hew_task_complete_threaded call")?;
    builder
        .build_return(None)
        .llvm_ctx("closure task wrapper return")?;
    Ok(wrapper)
}

pub(crate) fn emit_spawn_task_closure(
    fn_ctx: &FnCtx<'_, '_>,
    task: Place,
    fn_symbol: &str,
    env: Place,
    env_ty: &ResolvedTy,
    env_ownership: &[SpawnEnvFieldOwnership],
) -> CodegenResult<()> {
    let spilled_ctx = fn_ctx.execution_context.ok_or_else(|| {
        CodegenError::FailClosed("SpawnTaskClosure spawn site requires an execution context".into())
    })?;
    // W6.010: same coro-aware routing as `emit_spawn_task_direct` — a
    // `scope`/`fork` closure spawn after an await must pass the resume-installed
    // live context, not the unwound spilled param that
    // `hew_task_spawn_thread_with_inherited_context` would deref.
    let parent_ctx = live_execution_context_ptr(fn_ctx, spilled_ctx)?;
    let task_ptr = load_duplex_handle(fn_ctx, task, "SpawnTaskClosure task")?;
    let env_struct = record_struct_for(fn_ctx, env_ty)?;
    let (env_ptr, env_slot_ty) = place_pointer(fn_ctx, env)?;
    if env_slot_ty != BasicTypeEnum::StructType(env_struct) {
        return Err(CodegenError::FailClosed(format!(
            "SpawnTaskClosure env place type {env_slot_ty:?} does not match registered env \
             struct {env_struct:?}"
        )));
    }
    let field_count = env_struct.count_fields() as usize;
    if env_ownership.len() != field_count {
        return Err(CodegenError::FailClosed(format!(
            "SpawnTaskClosure environment for `{fn_symbol}` has {field_count} fields but \
             ownership manifest has {} entries",
            env_ownership.len()
        )));
    }
    let Some(env_size) = env_struct.size_of() else {
        return Err(CodegenError::FailClosed(
            "SpawnTaskClosure could not compute closure environment byte size".into(),
        ));
    };
    // Thread the environment struct's authoritative ABI alignment into the Rc
    // allocation. The runtime over-aligns the payload to this value instead of
    // guessing from the source pointer's trailing zeros, which would
    // under-align an over-aligned closure capture.
    let env_align = fn_ctx.ctx.i64_type().const_int(
        u64::from(fn_ctx.target_data.get_abi_alignment(&env_struct)),
        false,
    );
    let rc_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_rc_new",
    )?;
    let null_drop = fn_ctx.ctx.ptr_type(AddressSpace::default()).const_null();
    let owned_field_kinds: Vec<(u32, StateFieldCloneKind)> =
        if env_ownership.contains(&SpawnEnvFieldOwnership::OwnsMoved) {
            crate::llvm::env_field_drop_kinds(fn_ctx, fn_symbol, env, "spawn task")?
                .into_iter()
                .enumerate()
                .filter_map(|(idx, kind)| {
                    (env_ownership[idx] == SpawnEnvFieldOwnership::OwnsMoved).then_some((
                        u32::try_from(idx).expect("spawn env field count exceeds u32::MAX"),
                        kind,
                    ))
                })
                .collect()
        } else {
            Vec::new()
        };
    let drop_fn = if owned_field_kinds.is_empty() {
        null_drop
    } else {
        get_or_emit_spawn_env_rc_drop_thunk(fn_ctx, fn_symbol, env_struct, &owned_field_kinds)?
            .as_global_value()
            .as_pointer_value()
    };
    let rc_env = fn_ctx
        .builder
        .build_call(
            rc_new,
            &[
                env_ptr.into(),
                env_size.into(),
                env_align.into(),
                drop_fn.into(),
            ],
            "hew_closure_env_rc_new",
        )
        .llvm_ctx("hew_rc_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_rc_new returned void".into()))?
        .into_pointer_value();
    fn_ctx.call_runtime_void(
        "hew_task_set_env",
        &[task_ptr.into(), rc_env.into()],
        "hew_task_set_env_call",
        "hew_task_set_env call",
    )?;

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
        .llvm_ctx("hew_task_spawn_thread_with_inherited_context call")?;
    Ok(())
}

/// Synthesise a payload-only callback for an Rc task environment.
///
/// `hew_rc_new` owns and reclaims the allocation itself, so this callback drops
/// only the manifest-owned fields and must never call `hew_dyn_box_free`.
fn get_or_emit_spawn_env_rc_drop_thunk<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    fn_symbol: &str,
    env_struct: StructType<'ctx>,
    owned_field_kinds: &[(u32, StateFieldCloneKind)],
) -> CodegenResult<FunctionValue<'ctx>> {
    let symbol = format!("__hew_spawn_env_rc_drop_{fn_symbol}");
    if let Some(existing) = fn_ctx.llvm_mod.get_function(&symbol) {
        return Ok(existing);
    }
    let ctx = fn_ctx.ctx;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let thunk = fn_ctx.llvm_mod.add_function(
        &symbol,
        ctx.void_type().fn_type(&[ptr_ty.into()], false),
        Some(Linkage::Private),
    );
    let entry = ctx.append_basic_block(thunk, "entry");
    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    let env = thunk
        .get_nth_param(0)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "spawn env Rc drop thunk `{symbol}`: synthesised function missing env parameter"
            ))
        })?
        .into_pointer_value();
    for (field_idx, kind) in owned_field_kinds.iter().rev() {
        crate::llvm::emit_field_drop_step(
            ctx,
            fn_ctx.llvm_mod,
            &builder,
            Some(env_struct),
            env,
            *field_idx,
            kind,
        )?;
    }
    builder
        .build_return(None)
        .llvm_ctx_with(|| format!("spawn env Rc drop thunk `{symbol}`: return"))?;
    Ok(thunk)
}

/// Synthesise (once per closure shim) the `void(ptr env)` free thunk the
/// env box's header slot carries. The thunk is the SOLE teardown owner of an
/// escaping (heap-boxed) closure env — it runs in two acts, both keyed to the
/// captures region pointer the pair stores:
///
///   1. **Drop each owned captured field** in reverse (LIFO) declaration
///      order through the canonical per-field drop authority
///      ([`crate::llvm::emit_field_drop_step`] over the captures region's
///      [`StructType`]). A `string` capture is released via `hew_string_drop`,
///      a `Vec`/`HashMap`/`HashSet` via its managed free, a captured
///      closure-pair via its OWN planted free thunk (the recursive
///      env-of-env case), a record/enum via its in-place drop helper — the
///      identical classifier+step the actor-`state_drop_fn` and ask-reply
///      paths use, so a captured value drops byte-for-byte as it would in any
///      other owning aggregate (`lifecycle-symmetry`). BitCopy fields are
///      no-ops. A field whose type the classifier cannot map to a drop
///      strategy fails closed at synthesis (caller side), never silently
///      leaks-by-omission.
///   2. **Free the box**: `hew_dyn_box_free(env - HEADER, total, align)` with
///      the exact (size, align) the matching `hew_dyn_box_alloc` used.
///
/// Before this fix the thunk freed only the box, leaking every owned capture
/// (a runtime-built `string`/`Vec` captured into a returned closure → 2 leaks
/// under `leaks --atExit`). The drop manifest is derived once at the
/// `emit_closure_env_heap_box` call site (classifying the env record's field
/// `ResolvedTy`s) and passed in as `field_kinds`, paired with the captures-
/// region `env_struct` so the per-field GEPs land on the validated layout.
///
/// Private linkage — internal to the module, referenced only through the
/// header slot planted by `emit_closure_env_heap_box`.
pub(crate) fn get_or_emit_closure_env_free_thunk<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    fn_symbol: &str,
    total_size: u64,
    env_struct: StructType<'ctx>,
    field_kinds: &[StateFieldCloneKind],
) -> CodegenResult<FunctionValue<'ctx>> {
    let symbol = format!("__hew_closure_env_free_{fn_symbol}");
    if let Some(existing) = fn_ctx.llvm_mod.get_function(&symbol) {
        return Ok(existing);
    }
    let ctx = fn_ctx.ctx;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    let void_ty = ctx.void_type();
    let box_free_fn = fn_ctx
        .llvm_mod
        .get_function("hew_dyn_box_free")
        .unwrap_or_else(|| {
            let ty = void_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false);
            fn_ctx
                .llvm_mod
                .add_function("hew_dyn_box_free", ty, Some(Linkage::External))
        });
    let thunk = fn_ctx.llvm_mod.add_function(
        &symbol,
        void_ty.fn_type(&[ptr_ty.into()], false),
        Some(Linkage::Private),
    );
    let entry = ctx.append_basic_block(thunk, "entry");
    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    let env = thunk
        .get_nth_param(0)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "closure env free thunk `{symbol}`: synthesised function missing \
                 the env parameter"
            ))
        })?
        .into_pointer_value();

    // Act 1: drop each owned captured field in reverse declaration order
    // (LIFO — the same order the elaboration pass drops scope-exit locals and
    // the record/actor-state drop bodies use). `env` IS the captures region
    // pointer (the header sits BEFORE it at `env - HEADER`), so the env struct
    // GEPs land directly on the capture fields. BitCopy / Connection / opaque
    // arms are no-ops inside `emit_field_drop_step`; every owning class routes
    // to its single canonical release.
    for (idx, kind) in field_kinds.iter().enumerate().rev() {
        let field_idx = u32::try_from(idx).map_err(|_| {
            CodegenError::FailClosed(format!(
                "closure env free thunk `{symbol}`: capture field count exceeds u32::MAX"
            ))
        })?;
        crate::llvm::emit_field_drop_step(
            ctx,
            fn_ctx.llvm_mod,
            &builder,
            Some(env_struct),
            env,
            field_idx,
            kind,
        )?;
    }

    // Act 2: free the box. box = env - HEADER (in-bounds: the header is part
    // of the allocation).
    let neg_header = i64_ty.const_int(CLOSURE_ENV_BOX_HEADER.wrapping_neg(), true);
    let box_ptr =
        unsafe { builder.build_in_bounds_gep(ctx.i8_type(), env, &[neg_header], "env_box_base") }
            .llvm_ctx_with(|| format!("closure env free thunk `{symbol}`: box base gep"))?;
    builder
        .build_call(
            box_free_fn,
            &[
                box_ptr.into(),
                i64_ty.const_int(total_size, false).into(),
                i64_ty.const_int(CLOSURE_ENV_BOX_ALIGN, false).into(),
            ],
            "env_box_free",
        )
        .llvm_ctx_with(|| format!("closure env free thunk `{symbol}`: free call"))?;
    builder
        .build_return(None)
        .llvm_ctx_with(|| format!("closure env free thunk `{symbol}`: return"))?;
    Ok(thunk)
}

pub(crate) fn closure_call_context<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    // W6.010: the closure callee receives this pointer as its leading
    // `HewExecutionContext*` and may read context fields through it. In a
    // coroutine the spilled `execution_context` param is the unwound dispatch
    // frame after a suspend, so a context-dependent closure invoked after an
    // await must receive the resume-installed live context (via
    // `live_execution_context_ptr`), not the dangling param. The zeroed
    // `closure_call_fallback_context` is a Default-callconv function with no real
    // context (never a coroutine), so it is passed through unchanged.
    if let Some(spilled_ctx) = fn_ctx.execution_context {
        return live_execution_context_ptr(fn_ctx, spilled_ctx);
    }
    fn_ctx
        .closure_call_fallback_context
        .ok_or_else(|| CodegenError::FailClosed("CallClosure requires an execution context".into()))
}

/// Structural, injective per-tuple thunk key (`tuple_<elem>_<elem>...`), used as
/// the `__hew_tuple_*_inplace` symbol suffix. Two structurally identical tuples
/// share one thunk; two distinct shapes get distinct symbols.
///
/// Derived from `hew_hir::mangle_resolved_ty`, the canonical structural
/// `ResolvedTy` encoder (distinct types render to distinct fragments), rather
/// than a lossy `{:?}`-debug-then-sanitize transform. Two structurally distinct
/// tuple shapes therefore never collapse onto one key — the lazy idempotent
/// thunk-body emission (`count_basic_blocks() == 0`) would otherwise silently
/// reuse the first shape's clone/drop helper, dropping a field through the wrong
/// thunk (e.g. an `i64` field freed through a `string`-drop). `mangle_resolved_
/// ty` already replaces `::` with `_` and emits only `[A-Za-z0-9_]`, so the key
/// is LLVM-symbol-clean without a sanitize pass. (F-SEC-1 hardening.)
pub(crate) fn tuple_thunk_key(elems: &[ResolvedTy]) -> String {
    let mut out = String::from("tuple");
    for e in elems {
        out.push('_');
        out.push_str(&hew_hir::mangle_resolved_ty(e));
    }
    out
}

/// Emit the bodies of the collection-handle clone/drop wrapper thunks (#1722),
/// idempotently (no-op if a body already exists — same lazy contract as the
/// tuple thunk emitter). `clone_sym`/`drop_sym` are the element's canonical
/// collection clone/free runtime symbols (see `collection_elem_clone_drop_syms`).
///
/// The element slot holds a single owned collection handle (a pointer). The
/// clone thunk is COPY-IN: it loads the source handle, deep-clones it, and
/// OVERWRITES the destination slot with the fresh handle — `hew_vec_push_owned`
/// first memcpy's the shallow handle into the slot, which this store harmlessly
/// replaces, so the source retains sole ownership of the original (no alias).
/// The drop thunk loads the slot's handle and frees it (the free primitives
/// no-op on null, so a moved-out/cleared slot is safe).
pub(crate) fn emit_collection_handle_thunk_bodies<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    key: &str,
    clone_sym: &'static str,
    drop_sym: &'static str,
) -> CodegenResult<()> {
    let ctx = fn_ctx.ctx;
    let llvm_mod = fn_ctx.llvm_mod;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();

    let clone_fn = get_or_declare_collection_clone_inplace(ctx, llvm_mod, key);
    if clone_fn.count_basic_blocks() == 0 {
        let runtime_clone = llvm_mod.get_function(clone_sym).unwrap_or_else(|| {
            llvm_mod.add_function(
                clone_sym,
                ptr_ty.fn_type(&[ptr_ty.into()], false),
                Some(Linkage::External),
            )
        });
        let builder = ctx.create_builder();
        let entry = ctx.append_basic_block(clone_fn, "entry");
        builder.position_at_end(entry);
        let src = clone_fn
            .get_nth_param(0)
            .ok_or_else(|| {
                CodegenError::FailClosed("collection clone thunk missing src param".to_string())
            })?
            .into_pointer_value();
        let dst = clone_fn
            .get_nth_param(1)
            .ok_or_else(|| {
                CodegenError::FailClosed("collection clone thunk missing dst param".to_string())
            })?
            .into_pointer_value();
        let src_handle = builder
            .build_load(ptr_ty, src, "coll_src_handle")
            .llvm_ctx("collection clone load src handle")?;
        let cloned = builder
            .build_call(runtime_clone, &[src_handle.into()], "coll_clone_call")
            .llvm_ctx("collection clone call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "collection clone primitive `{clone_sym}` returned void"
                ))
            })?;
        builder
            .build_store(dst, cloned)
            .llvm_ctx("collection clone store dst handle")?;
        builder
            .build_return(Some(&i32_ty.const_zero()))
            .llvm_ctx("collection clone ret")?;
    }

    let drop_fn = get_or_declare_collection_drop_inplace(ctx, llvm_mod, key);
    if drop_fn.count_basic_blocks() == 0 {
        let runtime_drop = llvm_mod.get_function(drop_sym).unwrap_or_else(|| {
            llvm_mod.add_function(
                drop_sym,
                ctx.void_type().fn_type(&[ptr_ty.into()], false),
                Some(Linkage::External),
            )
        });
        let builder = ctx.create_builder();
        let entry = ctx.append_basic_block(drop_fn, "entry");
        builder.position_at_end(entry);
        let slot = drop_fn
            .get_nth_param(0)
            .ok_or_else(|| {
                CodegenError::FailClosed("collection drop thunk missing slot param".to_string())
            })?
            .into_pointer_value();
        let handle = builder
            .build_load(ptr_ty, slot, "coll_slot_handle")
            .llvm_ctx("collection drop load slot handle")?;
        builder
            .build_call(runtime_drop, &[handle.into()], "coll_drop_call")
            .llvm_ctx("collection drop call")?;
        builder.build_return(None).llvm_ctx("collection drop ret")?;
    }
    Ok(())
}

/// Synthesize the `__hew_tuple_{clone,drop}_inplace_<key>` bodies for an owned
/// tuple shape. Reuses the per-field clone/drop step machinery
/// (`emit_field_clone_step` / `emit_field_drop_step`) over the tuple's struct
/// fields — the same primitives the record bodies use — so the rollback and
/// drop discipline are identical (`lifecycle-symmetry`). Idempotent: no-ops if
/// the bodies already exist (a tuple shape may be referenced by multiple owned
/// Vecs).
pub(crate) fn emit_tuple_inplace_thunk_bodies<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    tuple_key: &str,
    elems: &[ResolvedTy],
    tuple_llvm_ty: BasicTypeEnum<'ctx>,
) -> CodegenResult<()> {
    let ctx = fn_ctx.ctx;
    let llvm_mod = fn_ctx.llvm_mod;
    let (tuple_struct, kinds) = tuple_inplace_field_kinds(fn_ctx, tuple_key, elems, tuple_llvm_ty)?;

    let clone_fn = get_or_declare_tuple_clone_inplace(ctx, llvm_mod, tuple_key);
    if clone_fn.count_basic_blocks() == 0 {
        emit_aggregate_clone_inplace_body(ctx, llvm_mod, clone_fn, tuple_struct, &kinds)?;
    }
    let drop_fn = get_or_declare_tuple_drop_inplace(ctx, llvm_mod, tuple_key);
    if drop_fn.count_basic_blocks() == 0 {
        emit_aggregate_drop_inplace_body(ctx, llvm_mod, drop_fn, tuple_struct, &kinds)?;
    }
    Ok(())
}

/// Resolve an owned Vec element `ResolvedTy` to its `(thunk-kind, layout-key)`.
///
/// The layout key is the registry name the per-type inplace helper bodies are
/// emitted under (`__hew_record_clone_inplace_<key>` etc.). Records resolve via
/// the `record_field_resolved_tys` map (the codegen record registry); enums
/// resolve via `enum_layouts` using the same name/args mangling scheme as
/// `collect_enum_inplace_drop_seeds`; tuples resolve to a structural key (the
/// thunk body is synthesized on demand). Returns `None` for any shape with no
/// in-place thunk path (primitives, builtins) — the caller must fail closed.
pub(crate) fn owned_elem_thunk_key(
    fn_ctx: &FnCtx<'_, '_>,
    elem_ty: &ResolvedTy,
) -> Option<(OwnedElemThunkKind, String)> {
    // Tuple element (W5.016 F2): no registry entry — synthesize a structural
    // thunk key. Only owned tuples (at least one heap-owning field) reach the
    // owned ABI; an all-BitCopy tuple stays on the existing `_layout` path
    // (the constructor's `resolved_ty_element_owns_heap_for_owned_vec` guard
    // excludes it).
    if let ResolvedTy::Tuple(elems) = elem_ty {
        return Some((OwnedElemThunkKind::Tuple, tuple_thunk_key(elems)));
    }
    // Nested collection element (#1722): `Vec<E>` / `HashMap` / `HashSet`. Like
    // the tuple shape there is no registry entry — synthesize a structural
    // thunk key (`mangle_resolved_ty` distinguishes `Vec<string>` from
    // `Vec<i64>`). Gated on a resolvable clone/free primitive so a closure-pair
    // `Vec<fn>` element resolves to `None` here (fail closed at descriptor
    // build) rather than naming a thunk that cannot be synthesized.
    if matches!(
        elem_ty,
        ResolvedTy::Named {
            builtin: Some(
                hew_types::BuiltinType::Vec
                    | hew_types::BuiltinType::HashMap
                    | hew_types::BuiltinType::HashSet
            ),
            ..
        }
    ) && collection_elem_clone_drop_syms(fn_ctx, elem_ty).is_some()
    {
        return Some((
            OwnedElemThunkKind::Collection,
            hew_hir::mangle_resolved_ty(elem_ty),
        ));
    }
    let ResolvedTy::Named { name, args, .. } = elem_ty else {
        return None;
    };
    // Enum-first: an owned-payload / recursive enum is the F3/F4 shape.
    let short = short_name(name);
    let enum_key = if args.is_empty() {
        fn_ctx
            .enum_layouts
            .iter()
            .find(|el| el.name == *name || short_name(&el.name) == short)
            .map(|el| el.name.clone())
    } else {
        let mangled = mangle_with_shortened_args(short, args);
        fn_ctx
            .enum_layouts
            .iter()
            .find(|el| el.name == mangled || el.name == *name)
            .map(|el| el.name.clone())
    };
    if let Some(key) = enum_key {
        return Some((OwnedElemThunkKind::Enum, key));
    }
    // Machine: an enum at the value-classification layer (same tagged-union
    // substrate; the clone/drop synthesis emits its
    // `__hew_enum_{clone,drop}_inplace_<Machine>` bodies over the machine
    // layout). Generic instantiations canonicalize to the bare decl name —
    // the same short-name registration `resolve_ty`'s fallback resolves the
    // element struct through. State-side machine entries are distinguished
    // from event companions / plain enums by their state-name table.
    if fn_ctx
        .machine_layouts
        .get(short)
        .is_some_and(|m| m.state_name_table.is_some())
    {
        return Some((OwnedElemThunkKind::Enum, short.to_string()));
    }
    // Record: look up the codegen record registry key (mangled if generic).
    // A generic instantiation registers on the bare-normalised spine, so
    // shorten the type-arg spine before mangling.
    let lookup_key = if args.is_empty() {
        name.clone()
    } else {
        mangle_with_shortened_args(short_name(name), args)
    };
    if fn_ctx
        .record_field_resolved_tys
        .contains_key(lookup_key.as_str())
    {
        return Some((OwnedElemThunkKind::Record, lookup_key));
    }
    if fn_ctx.record_field_resolved_tys.contains_key(short) {
        return Some((OwnedElemThunkKind::Record, short.to_string()));
    }
    None
}

// W3.032 Slice 3c: per-type equality thunk for layout-backed `Vec::contains`.
//
// `get_or_emit_eq_thunk` returns a `FunctionValue` matching the equality thunk
// ABI from the W3.032 plan: `unsafe extern "C" fn(*const c_void, *const c_void)
// -> i32` with `1` = equal, `0` = not equal.  The function is internally linked
// (`define internal i32 @__hew_eq_thunk_*`) and is intended to be invoked by
// `hew_vec_contains_thunk` from `hew-runtime/src/vec.rs`.
//
// **Authority boundary** — Codegen does NOT call `ty_is_eq_eligible` here nor
// inspect any Hew-level ownership/marker information.  The sole gate is the
// checker rewrite that lands the `"hew_vec_contains_thunk"` callee symbol;
// reaching this function implies that gate has fired.  The defensive
// `CodegenError::FailClosed` for float-shaped LLVM fields below is fault
// isolation — it detects a checker-gate bypass and refuses to emit `fcmp` —
// not an independent eligibility decision.
//
// **Dedup key** — Structured: derived from the element type's LLVM textual
// form (sanitised) plus its target-data ABI size/align.  Two structurally
// different LLVM types with the same size/align (e.g. `{i64, i64}` vs
// `{i32, i64}` after padding) get different keys and therefore different
// thunks, satisfying the W3.032 hard requirement that dedup not collapse on
// `{size, align}` alone.  The `__hew_eq_thunk_` prefix reserves the symbol in
// the Hew ABI namespace.
pub(crate) fn eq_thunk_struct_key(ty: BasicTypeEnum<'_>) -> String {
    // `print_to_string()` includes named-struct identity (e.g. `%Point`) for
    // named struct types and the literal field shape (e.g. `{ i64, i64 }`)
    // for anonymous/tuple structs.  That is exactly the structural fingerprint
    // we want.  Replace whitespace and non-alphanumeric runs so the resulting
    // string is a legal LLVM symbol suffix.
    let raw = ty.print_to_string().to_string();
    let mut out = String::with_capacity(raw.len());
    let mut last_under = false;
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch);
            last_under = false;
        } else if !last_under {
            out.push('_');
            last_under = true;
        }
    }
    while out.ends_with('_') {
        out.pop();
    }
    out
}

fn eq_thunk_resolved_key(resolved_ty: Option<&ResolvedTy>) -> String {
    let Some(resolved_ty) = resolved_ty else {
        return "untyped".to_string();
    };
    let raw = format!("{resolved_ty:?}");
    let mut out = String::with_capacity(raw.len());
    let mut last_under = false;
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch);
            last_under = false;
        } else if !last_under {
            out.push('_');
            last_under = true;
        }
    }
    while out.ends_with('_') {
        out.pop();
    }
    if out.is_empty() {
        "resolved".to_string()
    } else {
        out
    }
}

fn eq_struct_field_resolved_tys<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    st: StructType<'ctx>,
    resolved_ty: Option<&ResolvedTy>,
) -> Option<Vec<ResolvedTy>> {
    if let Some(ResolvedTy::Tuple(fields)) = resolved_ty {
        return Some(fields.clone());
    }
    if let Some(ResolvedTy::Named { name, args, .. }) = resolved_ty {
        let short = short_name(name);
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_with_shortened_args(short, args)
        };
        if let Some(fields) = fn_ctx.record_field_resolved_tys.get(key.as_str()) {
            return Some(fields.clone());
        }
        if let Some(fields) = fn_ctx.record_field_resolved_tys.get(name.as_str()) {
            return Some(fields.clone());
        }
        if let Some(fields) = fn_ctx.record_field_resolved_tys.get(short) {
            return Some(fields.clone());
        }
    }
    st.get_name()
        .and_then(|cstr| cstr.to_str().ok())
        .and_then(|name| fn_ctx.record_field_resolved_tys.get(name))
        .cloned()
}

fn resolved_vec_elem_ty(resolved_ty: Option<&ResolvedTy>) -> CodegenResult<&ResolvedTy> {
    match resolved_ty {
        Some(ResolvedTy::Named {
            builtin: Some(BuiltinType::Vec),
            args,
            ..
        }) if args.len() == 1 => Ok(&args[0]),
        Some(other) => Err(CodegenError::FailClosed(format!(
            "eq_thunk: pointer field `{other:?}` has no structural equality path"
        ))),
        None => Err(CodegenError::FailClosed(
            "eq_thunk: pointer field reached equality thunk without resolved Hew type".into(),
        )),
    }
}

fn emit_eq_i32_result_branch<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    raw: IntValue<'ctx>,
    success_bb: inkwell::basic_block::BasicBlock<'ctx>,
    fail_bb: inkwell::basic_block::BasicBlock<'ctx>,
    name: &str,
) -> CodegenResult<()> {
    let nz = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::NE,
            raw,
            raw.get_type().const_zero(),
            &format!("{name}_nz"),
        )
        .llvm_ctx("eq_thunk runtime ne")?;
    fn_ctx
        .builder
        .build_conditional_branch(nz, success_bb, fail_bb)
        .llvm_ctx("eq_thunk runtime br")?;
    Ok(())
}

pub(crate) fn get_or_emit_eq_thunk<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    elem_ty: BasicTypeEnum<'ctx>,
    resolved_ty: Option<&ResolvedTy>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let (size, align) = abi_size_align(elem_ty, Some(fn_ctx.target_data))?;
    let key = eq_thunk_struct_key(elem_ty);
    let resolved_key = eq_thunk_resolved_key(resolved_ty);
    let name = format!("__hew_eq_thunk_{size}_{align}_{key}_{resolved_key}");
    if let Some(existing) = fn_ctx.llvm_mod.get_function(&name) {
        return Ok(existing);
    }

    // Eagerly insert the function declaration before any recursive call so
    // that thunks for nested aggregates can find this one via name lookup.
    // (Hew has no self-referential records — `Ty` is not recursive — so the
    // recursion would terminate anyway, but eager insertion is the
    // canonical inkwell pattern.)
    let ctx = fn_ctx.ctx;
    let i32_ty = ctx.i32_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let fn_ty = i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false);
    let func = fn_ctx
        .llvm_mod
        .add_function(&name, fn_ty, Some(Linkage::Internal));

    // Save the caller's insert block so we can restore it after building the
    // thunk body — `FnCtx::builder` is shared with the outer function.
    let saved_block = fn_ctx.builder.get_insert_block();

    let entry_bb = ctx.append_basic_block(func, "entry");
    let ret_true_bb = ctx.append_basic_block(func, "eq_thunk_ret_true");
    let ret_false_bb = ctx.append_basic_block(func, "eq_thunk_ret_false");

    fn_ctx.builder.position_at_end(entry_bb);
    let lhs = func
        .get_nth_param(0)
        .ok_or_else(|| CodegenError::FailClosed("eq_thunk: missing lhs param".into()))?
        .into_pointer_value();
    let rhs = func
        .get_nth_param(1)
        .ok_or_else(|| CodegenError::FailClosed("eq_thunk: missing rhs param".into()))?
        .into_pointer_value();

    emit_eq_thunk_body(
        fn_ctx,
        elem_ty,
        resolved_ty,
        lhs,
        rhs,
        ret_true_bb,
        ret_false_bb,
    )?;

    // Return slots.
    fn_ctx.builder.position_at_end(ret_true_bb);
    fn_ctx
        .builder
        .build_return(Some(&i32_ty.const_int(1, false)))
        .llvm_ctx("eq_thunk ret true")?;
    fn_ctx.builder.position_at_end(ret_false_bb);
    fn_ctx
        .builder
        .build_return(Some(&i32_ty.const_zero()))
        .llvm_ctx("eq_thunk ret false")?;

    // Restore the outer function's builder position.
    if let Some(bb) = saved_block {
        fn_ctx.builder.position_at_end(bb);
    }

    Ok(func)
}

/// Integer type with the same bit width as `float_ty`, for bit-casting a float
/// to its raw bit pattern (f32 -> i32, f64 -> i64). Used by the structural eq
/// and hash thunks so floats compare and hash bitwise/total. The width is
/// always 32 or 64 here, so `custom_width_int_type` cannot fail in practice;
/// the `Result` is surfaced fail-closed rather than unwrapped.
fn float_bits_int_type<'ctx>(
    ctx: &'ctx Context,
    float_ty: FloatType<'ctx>,
) -> CodegenResult<IntType<'ctx>> {
    let width = float_ty.get_bit_width();
    let width_nz = std::num::NonZeroU32::new(width).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "float_bits_int_type: float type reported a zero bit width ({width})"
        ))
    })?;
    ctx.custom_width_int_type(width_nz).map_err(|e| {
        CodegenError::Llvm(format!(
            "float_bits_int_type: custom_width_int_type({width}): {e}"
        ))
    })
}

/// Emit a chain of field-by-field equality checks for `elem_ty`.  After all
/// checks succeed control flows to `ret_true_bb`; any failed check branches
/// directly to `ret_false_bb`.  The builder must be positioned at a block
/// that has no terminator on entry.
fn emit_eq_thunk_body<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    elem_ty: BasicTypeEnum<'ctx>,
    resolved_ty: Option<&ResolvedTy>,
    lhs: PointerValue<'ctx>,
    rhs: PointerValue<'ctx>,
    ret_true_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ret_false_bb: inkwell::basic_block::BasicBlock<'ctx>,
) -> CodegenResult<()> {
    match elem_ty {
        BasicTypeEnum::IntType(int_ty) => {
            let lv = fn_ctx
                .builder
                .build_load(int_ty, lhs, "eq_lhs")
                .llvm_ctx("eq_thunk lhs load")?
                .into_int_value();
            let rv = fn_ctx
                .builder
                .build_load(int_ty, rhs, "eq_rhs")
                .llvm_ctx("eq_thunk rhs load")?
                .into_int_value();
            let eq = fn_ctx
                .builder
                .build_int_compare(IntPredicate::EQ, lv, rv, "eq_field")
                .llvm_ctx("eq_thunk icmp")?;
            fn_ctx
                .builder
                .build_conditional_branch(eq, ret_true_bb, ret_false_bb)
                .llvm_ctx("eq_thunk br")?;
            Ok(())
        }
        BasicTypeEnum::StructType(st) => {
            // Tagged-union (user enum / machine) detection. The outer LLVM
            // struct is a NAMED struct registered under the enum/machine name
            // in `machine_layouts` (see `register_enum_layouts` /
            // `register_machine_layouts`). Such a value must NOT be compared
            // field-by-field as a plain record: outer field 1 is the
            // `[N x i8]` payload union sized to the widest variant, so its
            // inactive-variant and trailing padding bytes are indeterminate,
            // and a heap/string payload field would compare raw pointer bytes.
            // Route to tag-dispatched equality, which compares the
            // discriminant first and then only the active variant's declared
            // fields. LESSONS: match-fail-closed (P0), codegen-abi-authority.
            if let Some(layout) = st
                .get_name()
                .and_then(|c| c.to_str().ok())
                .and_then(|name| fn_ctx.machine_layouts.get(name))
            {
                return emit_eq_thunk_enum(fn_ctx, layout, lhs, rhs, ret_true_bb, ret_false_bb);
            }
            let field_resolved_tys = eq_struct_field_resolved_tys(fn_ctx, st, resolved_ty);
            let field_count = st.count_fields();
            if field_count == 0 {
                // Zero-field aggregates (unit-like records / `()` tuple) are
                // trivially equal.
                fn_ctx
                    .builder
                    .build_unconditional_branch(ret_true_bb)
                    .llvm_ctx("eq_thunk zero-field br")?;
                return Ok(());
            }
            let func = fn_ctx
                .builder
                .get_insert_block()
                .and_then(|b| b.get_parent())
                .ok_or_else(|| {
                    CodegenError::FailClosed("eq_thunk body: missing enclosing function".into())
                })?;
            for idx in 0..field_count {
                let field_ty = st.get_field_type_at_index(idx).ok_or_else(|| {
                    CodegenError::FailClosed(format!("eq_thunk: struct {st:?} missing field {idx}"))
                })?;
                let lhs_field = fn_ctx
                    .builder
                    .build_struct_gep(st, lhs, idx, &format!("eq_lhs_f{idx}"))
                    .llvm_ctx("eq_thunk lhs gep")?;
                let rhs_field = fn_ctx
                    .builder
                    .build_struct_gep(st, rhs, idx, &format!("eq_rhs_f{idx}"))
                    .llvm_ctx("eq_thunk rhs gep")?;
                let field_resolved_ty = field_resolved_tys
                    .as_ref()
                    .and_then(|tys| tys.get(idx as usize));
                // Continuation block: where control resumes after a successful
                // field comparison, unless this is the final field, in which
                // case we branch directly to `ret_true_bb`.
                let next_bb = if idx + 1 < field_count {
                    fn_ctx
                        .ctx
                        .append_basic_block(func, &format!("eq_thunk_cont_{}", idx + 1))
                } else {
                    ret_true_bb
                };
                emit_eq_thunk_field_check(
                    fn_ctx,
                    field_ty,
                    field_resolved_ty,
                    lhs_field,
                    rhs_field,
                    next_bb,
                    ret_false_bb,
                )?;
                if idx + 1 < field_count {
                    fn_ctx.builder.position_at_end(next_bb);
                }
            }
            Ok(())
        }
        BasicTypeEnum::ArrayType(arr_ty) => {
            // Arrays of equality-eligible elements: compare element-by-element.
            // Elements may be integers, floats (compared bitwise via the
            // FloatType arm of `emit_eq_thunk_field_check`), or nested
            // aggregates; the checker rejects managed/owned leaves upstream.
            let len = arr_ty.len();
            if len == 0 {
                fn_ctx
                    .builder
                    .build_unconditional_branch(ret_true_bb)
                    .llvm_ctx("eq_thunk empty array br")?;
                return Ok(());
            }
            let func = fn_ctx
                .builder
                .get_insert_block()
                .and_then(|b| b.get_parent())
                .ok_or_else(|| {
                    CodegenError::FailClosed("eq_thunk body: missing enclosing function".into())
                })?;
            let elem_ty = arr_ty.get_element_type();
            let elem_resolved_ty = match resolved_ty {
                Some(ResolvedTy::Array(elem, _)) => Some(elem.as_ref()),
                _ => None,
            };
            for idx in 0..len {
                let zero = fn_ctx.ctx.i32_type().const_zero();
                let idx_v = fn_ctx.ctx.i32_type().const_int(u64::from(idx), false);
                let lhs_elem = unsafe {
                    fn_ctx
                        .builder
                        .build_in_bounds_gep(arr_ty, lhs, &[zero, idx_v], &format!("eq_lhs_a{idx}"))
                        .llvm_ctx("eq_thunk lhs array gep")?
                };
                let rhs_elem = unsafe {
                    fn_ctx
                        .builder
                        .build_in_bounds_gep(arr_ty, rhs, &[zero, idx_v], &format!("eq_rhs_a{idx}"))
                        .llvm_ctx("eq_thunk rhs array gep")?
                };
                let next_bb = if idx + 1 < len {
                    fn_ctx
                        .ctx
                        .append_basic_block(func, &format!("eq_thunk_arr_cont_{}", idx + 1))
                } else {
                    ret_true_bb
                };
                emit_eq_thunk_field_check(
                    fn_ctx,
                    elem_ty,
                    elem_resolved_ty,
                    lhs_elem,
                    rhs_elem,
                    next_bb,
                    ret_false_bb,
                )?;
                if idx + 1 < len {
                    fn_ctx.builder.position_at_end(next_bb);
                }
            }
            Ok(())
        }
        BasicTypeEnum::FloatType(float_ty) => {
            // Structural float equality is BITWISE/total, not IEEE numeric:
            // bit-cast each float to a same-width integer and compare the bit
            // patterns. This is reflexive (NaN==NaN when bits are identical)
            // and treats +0.0 / -0.0 as distinct — the property structural
            // equality needs so the equal-implies-equal-hash contract holds
            // (the hash thunk hashes the same bit pattern). A bare scalar
            // `f64 == f64` keeps IEEE `fcmp` semantics via MIR `FloatCmp`; only
            // the structural path (record/enum/Vec element) reaches this thunk.
            let lv = fn_ctx
                .builder
                .build_load(float_ty, lhs, "eq_lhs")
                .llvm_ctx("eq_thunk float lhs load")?
                .into_float_value();
            let rv = fn_ctx
                .builder
                .build_load(float_ty, rhs, "eq_rhs")
                .llvm_ctx("eq_thunk float rhs load")?
                .into_float_value();
            let int_ty = float_bits_int_type(fn_ctx.ctx, float_ty)?;
            let lv_bits = fn_ctx
                .builder
                .build_bit_cast(lv, int_ty, "eq_lhs_bits")
                .llvm_ctx("eq_thunk float lhs bitcast")?
                .into_int_value();
            let rv_bits = fn_ctx
                .builder
                .build_bit_cast(rv, int_ty, "eq_rhs_bits")
                .llvm_ctx("eq_thunk float rhs bitcast")?
                .into_int_value();
            let eq = fn_ctx
                .builder
                .build_int_compare(IntPredicate::EQ, lv_bits, rv_bits, "eq_float_bits")
                .llvm_ctx("eq_thunk float icmp")?;
            fn_ctx
                .builder
                .build_conditional_branch(eq, ret_true_bb, ret_false_bb)
                .llvm_ctx("eq_thunk float br")?;
            Ok(())
        }
        BasicTypeEnum::PointerType(_) => emit_eq_thunk_field_check(
            fn_ctx,
            elem_ty,
            resolved_ty,
            lhs,
            rhs,
            ret_true_bb,
            ret_false_bb,
        ),
        BasicTypeEnum::VectorType(_) | BasicTypeEnum::ScalableVectorType(_) => {
            Err(CodegenError::FailClosed(format!(
                "eq_thunk: unsupported LLVM field shape {elem_ty:?} — \
             checker eligibility gate should have rejected this element type"
            )))
        }
    }
}

/// Compare a single field/element at `lhs_field`/`rhs_field` of type
/// `field_ty`.  Integer fields compare in-place and conditionally branch
/// to `success_bb`/`fail_bb`.  Aggregate fields delegate to a nested
/// `__hew_eq_thunk_*` function and check its `i32` return: non-zero ⇒
/// success, zero ⇒ fail.
fn emit_eq_thunk_field_check<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    field_ty: BasicTypeEnum<'ctx>,
    resolved_ty: Option<&ResolvedTy>,
    lhs_field: PointerValue<'ctx>,
    rhs_field: PointerValue<'ctx>,
    success_bb: inkwell::basic_block::BasicBlock<'ctx>,
    fail_bb: inkwell::basic_block::BasicBlock<'ctx>,
) -> CodegenResult<()> {
    match field_ty {
        BasicTypeEnum::IntType(int_ty) => {
            let lv = fn_ctx
                .builder
                .build_load(int_ty, lhs_field, "eq_field_lhs")
                .llvm_ctx("eq_thunk field lhs load")?
                .into_int_value();
            let rv = fn_ctx
                .builder
                .build_load(int_ty, rhs_field, "eq_field_rhs")
                .llvm_ctx("eq_thunk field rhs load")?
                .into_int_value();
            let eq = fn_ctx
                .builder
                .build_int_compare(IntPredicate::EQ, lv, rv, "eq_field_cmp")
                .llvm_ctx("eq_thunk field icmp")?;
            fn_ctx
                .builder
                .build_conditional_branch(eq, success_bb, fail_bb)
                .llvm_ctx("eq_thunk field br")?;
            Ok(())
        }
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_) => {
            // Nested aggregate: recurse via a separate thunk function.
            let nested = get_or_emit_eq_thunk(fn_ctx, field_ty, resolved_ty)?;
            let call = fn_ctx
                .builder
                .build_call(
                    nested,
                    &[lhs_field.into(), rhs_field.into()],
                    "eq_field_nested",
                )
                .llvm_ctx("eq_thunk nested call")?;
            let raw = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("eq_thunk nested call returned void".into())
                })?
                .into_int_value();
            emit_eq_i32_result_branch(fn_ctx, raw, success_bb, fail_bb, "eq_field_nested")
        }
        BasicTypeEnum::FloatType(float_ty) => {
            // Bitwise/total float field equality — see `emit_eq_thunk_body`'s
            // FloatType arm for the semantics. Compare the bit patterns so the
            // relation is reflexive and matches the bit-pattern hash.
            let lv = fn_ctx
                .builder
                .build_load(float_ty, lhs_field, "eq_field_lhs")
                .llvm_ctx("eq_thunk field float lhs load")?
                .into_float_value();
            let rv = fn_ctx
                .builder
                .build_load(float_ty, rhs_field, "eq_field_rhs")
                .llvm_ctx("eq_thunk field float rhs load")?
                .into_float_value();
            let int_ty = float_bits_int_type(fn_ctx.ctx, float_ty)?;
            let lv_bits = fn_ctx
                .builder
                .build_bit_cast(lv, int_ty, "eq_field_lhs_bits")
                .llvm_ctx("eq_thunk field float lhs bitcast")?
                .into_int_value();
            let rv_bits = fn_ctx
                .builder
                .build_bit_cast(rv, int_ty, "eq_field_rhs_bits")
                .llvm_ctx("eq_thunk field float rhs bitcast")?
                .into_int_value();
            let eq = fn_ctx
                .builder
                .build_int_compare(IntPredicate::EQ, lv_bits, rv_bits, "eq_field_float_cmp")
                .llvm_ctx("eq_thunk field float icmp")?;
            fn_ctx
                .builder
                .build_conditional_branch(eq, success_bb, fail_bb)
                .llvm_ctx("eq_thunk field float br")?;
            Ok(())
        }
        BasicTypeEnum::PointerType(_) => match resolved_ty {
            Some(ResolvedTy::String) => {
                let lhs_v = fn_ctx
                    .builder
                    .build_load(field_ty, lhs_field, "eq_string_lhs")
                    .llvm_ctx("eq_thunk string lhs load")?
                    .into_pointer_value();
                let rhs_v = fn_ctx
                    .builder
                    .build_load(field_ty, rhs_field, "eq_string_rhs")
                    .llvm_ctx("eq_thunk string rhs load")?
                    .into_pointer_value();
                let eq_i32 = fn_ctx.call_runtime_int(
                    "hew_string_equals",
                    &[lhs_v.into(), rhs_v.into()],
                    "eq_string_equals",
                    "eq_thunk string equals",
                )?;
                emit_eq_i32_result_branch(fn_ctx, eq_i32, success_bb, fail_bb, "eq_string")
            }
            Some(ResolvedTy::Named {
                builtin: Some(BuiltinType::Vec),
                args,
                ..
            }) if args.len() == 1 => {
                let elem_resolved_ty = resolved_vec_elem_ty(resolved_ty)?;
                let elem_ty = resolve_ty(
                    fn_ctx.ctx,
                    fn_ctx.target_data,
                    elem_resolved_ty,
                    fn_ctx.record_layouts,
                )?;
                let elem_eq = get_or_emit_eq_thunk(fn_ctx, elem_ty, Some(elem_resolved_ty))?;
                let elem_eq_ptr = elem_eq.as_global_value().as_pointer_value();
                let lhs_v = fn_ctx
                    .builder
                    .build_load(field_ty, lhs_field, "eq_vec_lhs")
                    .llvm_ctx("eq_thunk vec lhs load")?
                    .into_pointer_value();
                let rhs_v = fn_ctx
                    .builder
                    .build_load(field_ty, rhs_field, "eq_vec_rhs")
                    .llvm_ctx("eq_thunk vec rhs load")?
                    .into_pointer_value();
                let eq_i32 = fn_ctx.call_runtime_int(
                    "hew_vec_equals_thunk",
                    &[lhs_v.into(), rhs_v.into(), elem_eq_ptr.into()],
                    "eq_vec_equals",
                    "eq_thunk vec equals",
                )?;
                emit_eq_i32_result_branch(fn_ctx, eq_i32, success_bb, fail_bb, "eq_vec")
            }
            Some(other) => Err(CodegenError::FailClosed(format!(
                "eq_thunk: type `{other:?}` has no structural equality path"
            ))),
            None => Err(CodegenError::FailClosed(
                "eq_thunk: pointer field reached equality thunk without resolved Hew type".into(),
            )),
        },
        BasicTypeEnum::VectorType(_) | BasicTypeEnum::ScalableVectorType(_) => {
            Err(CodegenError::FailClosed(format!(
                "eq_thunk: unsupported nested LLVM field shape {field_ty:?}"
            )))
        }
    }
}

/// Emit tag-dispatched equality for a tagged-union (user enum / machine)
/// value addressed by `lhs`/`rhs` (pointers to the outer `{ tag, payload }`
/// struct). The discriminant tags are compared first: unequal tags branch to
/// `ret_false_bb`. Equal tags switch on the (now shared) tag value to a
/// per-variant block that compares only the active variant's declared
/// payload fields, recursing through `emit_eq_thunk_field_check`. A tag
/// outside the declared variant set traps `HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH`
/// (208) rather than comparing indeterminate bytes.
///
/// WHY tag-dispatch instead of a whole-struct field-by-field compare: the
/// outer struct's field 1 is a `[N x i8]` union sized to the widest variant.
/// For any narrower active variant the trailing bytes are indeterminate, and
/// a managed/heap payload field (string, owned aggregate) would compare raw
/// pointer bytes — both semantically wrong. Comparing the discriminant plus
/// only the active variant's fields is the sole correct equality. The variant
/// layout is read from the single `MachineCodegenLayout` authority so this
/// dispatch cannot drift from construction. LESSONS: match-fail-closed (P0),
/// codegen-abi-authority (P0).
///
/// Note on payload eligibility: `emit_eq_thunk_field_check` fails closed on
/// float/pointer payload fields. The checker's `ty_is_eq_eligible` already
/// rejects float and managed (string/heap) enum payloads before any
/// `Vec::contains` lowering can route an enum element here, so those arms are
/// fault isolation against a checker-gate bypass, not an independent
/// eligibility decision.
fn emit_eq_thunk_enum<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    layout: &MachineCodegenLayout<'ctx>,
    lhs: PointerValue<'ctx>,
    rhs: PointerValue<'ctx>,
    ret_true_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ret_false_bb: inkwell::basic_block::BasicBlock<'ctx>,
) -> CodegenResult<()> {
    let ctx = fn_ctx.ctx;
    let func = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|b| b.get_parent())
        .ok_or_else(|| {
            CodegenError::FailClosed("eq_thunk enum: missing enclosing function".into())
        })?;
    let outer = layout.outer_struct;
    let tag_ty = layout.tag_int_ty;

    // Load both discriminant tags (outer field 0) and compare.
    let lhs_tag_ptr = fn_ctx
        .builder
        .build_struct_gep(outer, lhs, 0, "eq_enum_lhs_tag_ptr")
        .llvm_ctx("eq_thunk enum lhs tag gep")?;
    let rhs_tag_ptr = fn_ctx
        .builder
        .build_struct_gep(outer, rhs, 0, "eq_enum_rhs_tag_ptr")
        .llvm_ctx("eq_thunk enum rhs tag gep")?;
    let lhs_tag = fn_ctx
        .builder
        .build_load(tag_ty, lhs_tag_ptr, "eq_enum_lhs_tag")
        .llvm_ctx("eq_thunk enum lhs tag load")?
        .into_int_value();
    let rhs_tag = fn_ctx
        .builder
        .build_load(tag_ty, rhs_tag_ptr, "eq_enum_rhs_tag")
        .llvm_ctx("eq_thunk enum rhs tag load")?
        .into_int_value();
    let tags_eq = fn_ctx
        .builder
        .build_int_compare(IntPredicate::EQ, lhs_tag, rhs_tag, "eq_enum_tag_eq")
        .llvm_ctx("eq_thunk enum tag icmp")?;
    let tags_equal_bb = ctx.append_basic_block(func, "eq_enum_tags_equal");
    fn_ctx
        .builder
        .build_conditional_branch(tags_eq, tags_equal_bb, ret_false_bb)
        .llvm_ctx("eq_thunk enum tag br")?;

    // Tags equal: switch on the shared tag to per-variant payload compare.
    fn_ctx.builder.position_at_end(tags_equal_bb);
    let trap_bb = ctx.append_basic_block(func, "eq_enum_tag_oob_trap");
    let mut variant_blocks: Vec<inkwell::basic_block::BasicBlock<'ctx>> =
        Vec::with_capacity(layout.variant_struct_tys.len());
    let mut cases: Vec<(IntValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> =
        Vec::with_capacity(layout.variant_struct_tys.len());
    for idx in 0..layout.variant_struct_tys.len() {
        let bb = ctx.append_basic_block(func, &format!("eq_enum_variant_{idx}"));
        cases.push((tag_ty.const_int(idx as u64, false), bb));
        variant_blocks.push(bb);
    }
    fn_ctx
        .builder
        .build_switch(lhs_tag, trap_bb, &cases)
        .llvm_ctx("eq_thunk enum tag switch")?;

    // Out-of-range tag: under the layout invariant neither operand can carry
    // a tag outside the declared variant set, so reaching here means memory
    // corruption or a producer-gate bypass. Trap fail-closed rather than
    // compare indeterminate payload bytes. Code mirrors
    // `TrapKind::ExhaustivenessFallthrough` lowering (see the `Terminator::Trap`
    // arm) — single-sourced from the runtime constant.
    fn_ctx.builder.position_at_end(trap_bb);
    emit_trap_with_code(
        fn_ctx,
        HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH as u64,
        "eq_enum_tag_oob",
    )?;

    // Per-variant payload comparison.
    for (idx, variant_struct) in layout.variant_struct_tys.iter().enumerate() {
        fn_ctx.builder.position_at_end(variant_blocks[idx]);
        let field_count = variant_struct.count_fields();
        if field_count == 0 {
            // Payload-less variant: equal tags ⇒ equal values.
            fn_ctx
                .builder
                .build_unconditional_branch(ret_true_bb)
                .llvm_ctx("eq_thunk enum empty-variant br")?;
            continue;
        }
        // GEP each operand to outer field 1 (the payload union), then index
        // it as the active variant's struct. Opaque pointers make the
        // reinterpret a no-op: a `ptr` is a `ptr`; the following GEP is typed
        // against `variant_struct`. Mirrors `Place::EnumVariant` lowering.
        let lhs_payload = fn_ctx
            .builder
            .build_struct_gep(outer, lhs, 1, &format!("eq_enum_lhs_payload_{idx}"))
            .llvm_ctx("eq_thunk enum lhs payload gep")?;
        let rhs_payload = fn_ctx
            .builder
            .build_struct_gep(outer, rhs, 1, &format!("eq_enum_rhs_payload_{idx}"))
            .llvm_ctx("eq_thunk enum rhs payload gep")?;
        let variant_resolved_tys = layout.variant_field_tys.get(idx);
        for field_idx in 0..field_count {
            let field_ty = variant_struct
                .get_field_type_at_index(field_idx)
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "eq_thunk enum: variant {idx} missing field {field_idx}"
                    ))
                })?;
            let field_resolved_ty =
                variant_resolved_tys.and_then(|tys| tys.get(field_idx as usize));
            let lhs_field = fn_ctx
                .builder
                .build_struct_gep(
                    *variant_struct,
                    lhs_payload,
                    field_idx,
                    &format!("eq_enum_lhs_v{idx}_f{field_idx}"),
                )
                .llvm_ctx("eq_thunk enum lhs field gep")?;
            let rhs_field = fn_ctx
                .builder
                .build_struct_gep(
                    *variant_struct,
                    rhs_payload,
                    field_idx,
                    &format!("eq_enum_rhs_v{idx}_f{field_idx}"),
                )
                .llvm_ctx("eq_thunk enum rhs field gep")?;
            let next_bb = if field_idx + 1 < field_count {
                ctx.append_basic_block(func, &format!("eq_enum_v{idx}_cont_{}", field_idx + 1))
            } else {
                ret_true_bb
            };
            emit_eq_thunk_field_check(
                fn_ctx,
                field_ty,
                field_resolved_ty,
                lhs_field,
                rhs_field,
                next_bb,
                ret_false_bb,
            )?;
            if field_idx + 1 < field_count {
                fn_ctx.builder.position_at_end(next_bb);
            }
        }
    }
    Ok(())
}

/// Emit field-by-field FNV-1a mixing into `acc_slot` for the value of type
/// `ty` rooted at `base_ptr`.  Walks structs in declaration order (the
/// authoritative order is the LLVM struct's field index, which matches
/// `RecordLayout.field_tys` ordering installed by `fill_record_layout_bodies`).
/// Nested aggregates fold via a separate, deduplicated nested thunk.
fn emit_hash_thunk_body<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    ty: BasicTypeEnum<'ctx>,
    resolved_ty: Option<&ResolvedTy>,
    base_ptr: PointerValue<'ctx>,
    acc_slot: PointerValue<'ctx>,
) -> CodegenResult<()> {
    let ctx = fn_ctx.ctx;
    let i64_ty = ctx.i64_type();
    match ty {
        BasicTypeEnum::IntType(int_ty) => {
            // Typed-width load: bool/i8 loads `i8`, char loads `i32`, i64
            // loads `i64`.  Zero-extend to i64 before mixing so all widths
            // funnel through the same FNV-1a step.
            //
            // Defence-in-depth for `bool`: Hew stores `bool` as an i8 where
            // only the low bit is semantically defined (`0 = false`, `1 =
            // true`); the checker guarantees the upper 7 bits are zero, but
            // a stray non-canonical byte (an upstream invariant violation)
            // must not silently desynchronise hashing from equality.  When
            // the caller has told us the load is a `bool`, mask the loaded
            // byte to its low bit before the FNV-1a mix.  General i8/u8/i16
            // /i32/i64 fields are hashed full-width.
            let loaded = fn_ctx
                .builder
                .build_load(int_ty, base_ptr, "hash_field_load")
                .llvm_ctx("hash_thunk int load")?
                .into_int_value();
            let masked = if matches!(resolved_ty, Some(ResolvedTy::Bool)) {
                let one = int_ty.const_int(1, false);
                fn_ctx
                    .builder
                    .build_and(loaded, one, "hash_bool_mask")
                    .llvm_ctx("hash_thunk bool mask")?
            } else {
                loaded
            };
            let zext = if int_ty.get_bit_width() < 64 {
                fn_ctx
                    .builder
                    .build_int_z_extend(masked, i64_ty, "hash_field_zext")
                    .llvm_ctx("hash_thunk zext")?
            } else {
                masked
            };
            mix_into_hash_acc(fn_ctx, acc_slot, zext)?;
            Ok(())
        }
        BasicTypeEnum::StructType(st) => {
            // Walk the optional ResolvedTy in lockstep with the LLVM struct
            // fields so per-field bool detection survives recursion into
            // nested records.  `field_resolved_tys` is sourced from the
            // pipeline's record-layout table keyed by the struct's LLVM
            // name; if the lookup fails we walk without ResolvedTy info
            // (hand-built test fixtures may not register a layout for
            // every named local).
            let field_resolved_tys: Option<&Vec<ResolvedTy>> = st
                .get_name()
                .and_then(|cstr| cstr.to_str().ok())
                .and_then(|name| fn_ctx.record_field_resolved_tys.get(name));
            let field_count = st.count_fields();
            for idx in 0..field_count {
                let field_ty = st.get_field_type_at_index(idx).ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "hash_thunk: struct {st:?} missing field index {idx}"
                    ))
                })?;
                let field_resolved = field_resolved_tys.and_then(|tys| tys.get(idx as usize));
                let field_ptr = fn_ctx
                    .builder
                    .build_struct_gep(st, base_ptr, idx, &format!("hash_f{idx}"))
                    .llvm_ctx("hash_thunk struct gep")?;
                match field_ty {
                    BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_) => {
                        // Recurse via a separate dedup'd nested thunk so the
                        // emitted IR is compact and any nested record sharing
                        // produces one hash thunk per type per module.
                        let nested = get_or_emit_hash_thunk(fn_ctx, field_ty, field_resolved)?;
                        let call = fn_ctx
                            .builder
                            .build_call(nested, &[field_ptr.into()], "hash_nested_call")
                            .llvm_ctx("hash_thunk nested call")?;
                        let nested_hash = call
                            .try_as_basic_value()
                            .basic()
                            .ok_or_else(|| {
                                CodegenError::FailClosed(
                                    "hash_thunk nested call returned void".into(),
                                )
                            })?
                            .into_int_value();
                        mix_into_hash_acc(fn_ctx, acc_slot, nested_hash)?;
                    }
                    _ => {
                        emit_hash_thunk_body(fn_ctx, field_ty, field_resolved, field_ptr, acc_slot)?
                    }
                }
            }
            Ok(())
        }
        BasicTypeEnum::ArrayType(arr_ty) => {
            let len = arr_ty.len();
            let elem_ty = arr_ty.get_element_type();
            for idx in 0..len {
                let zero = ctx.i32_type().const_zero();
                let idx_v = ctx.i32_type().const_int(u64::from(idx), false);
                let elem_ptr = unsafe {
                    fn_ctx
                        .builder
                        .build_in_bounds_gep(
                            arr_ty,
                            base_ptr,
                            &[zero, idx_v],
                            &format!("hash_a{idx}"),
                        )
                        .llvm_ctx("hash_thunk arr gep")?
                };
                // Arrays of bool are not currently expressible in the Hew
                // surface (bool aggregates are not first-class), so we walk
                // array elements without a ResolvedTy hint.  When that
                // changes, thread an `Option<&ResolvedTy>` through here.
                emit_hash_thunk_body(fn_ctx, elem_ty, None, elem_ptr, acc_slot)?;
            }
            Ok(())
        }
        BasicTypeEnum::FloatType(float_ty) => {
            // Hash the float BIT PATTERN so `==` (bitwise/total — see the eq
            // thunk) implies an equal hash: bit-cast to a same-width integer,
            // zero-extend to i64, and run the same FNV-1a mix every scalar
            // field uses. NaN hashes by its bits like any other value; +0.0 and
            // -0.0 hash differently (matching their bitwise inequality).
            let loaded = fn_ctx
                .builder
                .build_load(float_ty, base_ptr, "hash_float_load")
                .llvm_ctx("hash_thunk float load")?
                .into_float_value();
            let int_ty = float_bits_int_type(ctx, float_ty)?;
            let bits = fn_ctx
                .builder
                .build_bit_cast(loaded, int_ty, "hash_float_bits")
                .llvm_ctx("hash_thunk float bitcast")?
                .into_int_value();
            let zext = if int_ty.get_bit_width() < 64 {
                fn_ctx
                    .builder
                    .build_int_z_extend(bits, i64_ty, "hash_float_zext")
                    .llvm_ctx("hash_thunk float zext")?
            } else {
                bits
            };
            mix_into_hash_acc(fn_ctx, acc_slot, zext)?;
            Ok(())
        }
        BasicTypeEnum::PointerType(_) => match resolved_ty {
            // A `string` field inside a hash key: load the owned `*const c_char`
            // and hash its NUL-terminated payload via `hew_string_hash_fnv1a`
            // (the hash twin of the eq thunk's `hew_string_equals`). Hashing the
            // payload — not the pointer word — keeps distinct-but-equal record
            // keys in the same bucket and equal-by-value keys colliding, exactly
            // as the equality witness compares them.
            Some(ResolvedTy::String) => {
                let str_ptr = fn_ctx
                    .builder
                    .build_load(ty, base_ptr, "hash_string_load")
                    .llvm_ctx("hash_thunk string load")?
                    .into_pointer_value();
                let hash = fn_ctx.call_runtime_int(
                    "hew_string_hash_fnv1a",
                    &[str_ptr.into()],
                    "hash_string_fnv1a",
                    "hash_thunk string hash",
                )?;
                mix_into_hash_acc(fn_ctx, acc_slot, hash)?;
                Ok(())
            }
            // No fail-open pointer-hash fallback: any other pointer-shaped field
            // means the checker eligibility gate was bypassed. Refuse loudly
            // rather than mix the raw pointer word (which would silently break
            // hash/eq agreement). LESSONS: no-fail-open-fallback-after-authority.
            Some(other) => Err(CodegenError::FailClosed(format!(
                "hash_thunk: pointer field of type `{other:?}` has no structural hash path"
            ))),
            None => Err(CodegenError::FailClosed(
                "hash_thunk: pointer field reached hash thunk without resolved Hew type".into(),
            )),
        },
        BasicTypeEnum::VectorType(_) | BasicTypeEnum::ScalableVectorType(_) => {
            Err(CodegenError::FailClosed(format!(
                "hash_thunk: unsupported field shape {ty:?} — checker eligibility \
                 gate should have rejected this field type"
            )))
        }
    }
}

/// Return (creating on demand) the per-type FNV-1a hash thunk for `elem_ty`.
///
/// Signature: `i64 __hew_hash_thunk_<size>_<align>_<struct_key>(ptr key)`.
/// Dedup key matches `eq_thunk_struct_key` so the (size, align, struct shape)
/// triple is the identity surface for both thunk families.  Linkage is
/// `Internal` so the thunk does not leak into the public ABI surface.
///
/// `resolved_ty`, when `Some`, is the Hew-level type of the value at
/// `base_ptr`; it lets the body emitter distinguish a `bool` field (which
/// is masked to its low bit) from a full-width integer.  Cross-shape dedup
/// is preserved: the dedup key embeds the LLVM struct identity (which
/// includes the Hew type name), so two records with identical (size,
/// align) but different shapes emit two distinct thunks.
pub(crate) fn get_or_emit_hash_thunk<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    elem_ty: BasicTypeEnum<'ctx>,
    resolved_ty: Option<&ResolvedTy>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let (size, align) = abi_size_align(elem_ty, Some(fn_ctx.target_data))?;
    let key = eq_thunk_struct_key(elem_ty);
    let name = format!("__hew_hash_thunk_{size}_{align}_{key}");
    if let Some(existing) = fn_ctx.llvm_mod.get_function(&name) {
        return Ok(existing);
    }

    let ctx = fn_ctx.ctx;
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let fn_ty = i64_ty.fn_type(&[ptr_ty.into()], false);
    let func = fn_ctx
        .llvm_mod
        .add_function(&name, fn_ty, Some(Linkage::Internal));

    let saved_block = fn_ctx.builder.get_insert_block();

    let entry_bb = ctx.append_basic_block(func, "entry");
    let exit_bb = ctx.append_basic_block(func, "hash_thunk_exit");
    fn_ctx.builder.position_at_end(entry_bb);

    // FNV-1a 64-bit offset basis.
    let acc_slot = fn_ctx
        .builder
        .build_alloca(i64_ty, "hash_acc")
        .llvm_ctx("hash_thunk alloca")?;
    fn_ctx
        .builder
        .build_store(acc_slot, i64_ty.const_int(0xcbf29ce484222325_u64, false))
        .llvm_ctx("hash_thunk init store")?;

    let key_ptr = func
        .get_nth_param(0)
        .ok_or_else(|| CodegenError::FailClosed("hash_thunk: missing key param".into()))?
        .into_pointer_value();

    emit_hash_thunk_body(fn_ctx, elem_ty, resolved_ty, key_ptr, acc_slot)?;

    fn_ctx
        .builder
        .build_unconditional_branch(exit_bb)
        .llvm_ctx("hash_thunk br exit")?;
    fn_ctx.builder.position_at_end(exit_bb);
    let final_acc = fn_ctx
        .builder
        .build_load(i64_ty, acc_slot, "hash_acc_final")
        .llvm_ctx("hash_thunk acc final load")?
        .into_int_value();
    fn_ctx
        .builder
        .build_return(Some(&final_acc))
        .llvm_ctx("hash_thunk return")?;

    if let Some(bb) = saved_block {
        fn_ctx.builder.position_at_end(bb);
    }

    Ok(func)
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
pub(crate) fn emit_actor_terminate_trampoline<'ctx>(
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
        // Fail-closed: a suspending `#[on(stop)]` hook (one containing a `sleep`/
        // `await`, lowered as a coroutine ramp) cannot be driven to completion at
        // teardown. Terminate runs when the actor is already in its terminal state
        // (`call_terminate_fn`), so there is no scheduler activation to park and
        // resume the continuation into — the hook would suspend and never resume,
        // silently truncating teardown work. Refuse at compile time rather than
        // emit a discard-the-handle trampoline that drops everything after the
        // suspension (#2269 doctrine: fail closed, never silent wrong-answer).
        if crate::llvm::lifecycle_hook_is_coroutine(fn_val) {
            return Err(CodegenError::FailClosed(format!(
                "actor `{}`: an `#[on(stop)]` hook (`{sym}`) that suspends (it contains a \
                 `sleep`/`await`) is not supported — teardown runs after the actor has \
                 reached its terminal state, so a suspended stop hook cannot be resumed and \
                 the work after the suspension point would be silently lost. Keep `#[on(stop)]` \
                 non-suspending.",
                layout.name
            )));
        }
        on_stop_fns.push(fn_val);
    }

    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let trampoline_name = format!("__terminate_{}", mangle_dotted_name(&layout.name));
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
        .llvm_ctx("terminate trampoline: require_ctx call")?
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
            .llvm_ctx("terminate trampoline: actor gep")?
    };
    let actor_val = builder
        .build_load(ptr_ty, actor_ptr, "terminate_actor")
        .llvm_ctx("terminate trampoline: actor load")?
        .into_pointer_value();

    // Acquire the actor-state lock (same protocol as dispatch trampolines).
    builder
        .build_call(
            lock_acquire_fn,
            &[actor_val.into()],
            "terminate_lock_acquire",
        )
        .llvm_ctx("terminate trampoline: lock acquire")?;

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
            .llvm_ctx_with(|| format!("terminate trampoline: on_stop[{i}] call"))?;
    }

    // Release the actor-state lock.
    builder
        .build_call(
            lock_release_fn,
            &[actor_val.into()],
            "terminate_lock_release",
        )
        .llvm_ctx("terminate trampoline: lock release")?;

    builder
        .build_return(None)
        .llvm_ctx("terminate trampoline: return")?;

    Ok(())
}

pub(crate) fn coalesce_key_fn_name(actor_name: &str) -> String {
    format!("__hew_coalesce_key_{}", mangle_dotted_name(actor_name))
}

pub(crate) fn message_drop_fn_name(actor_name: &str) -> String {
    format!("__hew_message_drop_{}", mangle_dotted_name(actor_name))
}

/// Emit the runtime-registered key extractor for one coalescing actor.
///
/// The callback rebuilds the same anonymous payload struct used by
/// `emit_actor_dispatch_trampoline`, so sender packing, dispatch unpacking, and
/// key extraction share one layout authority.
pub(crate) fn emit_coalesce_key_fn<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    target_data: &TargetData,
    layout: &ActorLayout,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let plan = layout.coalesce_key_plan.as_ref().ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "actor `{}` requested coalesce key emission without a MIR key plan",
            layout.name
        ))
    })?;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let size_ty = runtime_size_ty(ctx, llvm_mod);
    let fn_name = coalesce_key_fn_name(&layout.name);
    let fn_ty = i64_ty.fn_type(&[i32_ty.into(), ptr_ty.into(), size_ty.into()], false);
    let key_fn = llvm_mod.add_function(&fn_name, fn_ty, Some(Linkage::Internal));
    let builder = ctx.create_builder();
    let entry = ctx.append_basic_block(key_fn, "entry");
    let default_bb = ctx.append_basic_block(key_fn, "unkeyed_msg_type");
    builder.position_at_end(entry);

    let msg_type = key_fn
        .get_nth_param(0)
        .ok_or_else(|| CodegenError::FailClosed("coalesce key fn missing msg_type".into()))?
        .into_int_value();
    let payload = key_fn
        .get_nth_param(1)
        .ok_or_else(|| CodegenError::FailClosed("coalesce key fn missing payload".into()))?
        .into_pointer_value();

    let mut cases = Vec::with_capacity(plan.entries.len());
    for entry in &plan.entries {
        let bb = ctx.append_basic_block(key_fn, &format!("msg_{}", entry.msg_type));
        cases.push((i32_ty.const_int(entry.msg_type as u64, false), bb));
    }
    builder
        .build_switch(msg_type, default_bb, &cases)
        .llvm_ctx("coalesce key msg_type switch")?;

    for (entry, (_, bb)) in plan.entries.iter().zip(cases.iter()) {
        builder.position_at_end(*bb);
        let handler = layout
            .handlers
            .iter()
            .find(|handler| handler.msg_type == entry.msg_type)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "coalesce key plan for actor `{}` references unknown msg_type {}",
                    layout.name, entry.msg_type
                ))
            })?;
        let param_index = usize::try_from(entry.param_index).map_err(|_| {
            CodegenError::FailClosed(format!(
                "coalesce key param index {} does not fit usize",
                entry.param_index
            ))
        })?;
        let param_ty = handler.param_tys.get(param_index).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "coalesce key plan for actor `{}` handler `{}` references param {} of {}",
                layout.name,
                handler.name,
                param_index,
                handler.param_tys.len()
            ))
        })?;
        let field_ty = resolve_ty(ctx, target_data, param_ty, record_layouts)?;
        let field_ptr = if handler.param_tys.len() > 1 {
            let mut packed_field_tys = Vec::with_capacity(handler.param_tys.len());
            for ty in &handler.param_tys {
                packed_field_tys.push(resolve_ty(ctx, target_data, ty, record_layouts)?);
            }
            let packed_st = ctx.struct_type(&packed_field_tys, false);
            builder
                .build_struct_gep(
                    packed_st,
                    payload,
                    entry.param_index,
                    "coalesce_key_field_ptr",
                )
                .llvm_ctx("coalesce key packed field gep")?
        } else {
            if entry.param_index != 0 {
                return Err(CodegenError::FailClosed(format!(
                    "single-parameter handler `{}` has coalesce key index {}",
                    handler.name, entry.param_index
                )));
            }
            payload
        };
        let loaded = builder
            .build_load(field_ty, field_ptr, "coalesce_key_field")
            .llvm_ctx("coalesce key field load")?;
        let key = match entry.kind {
            CoalesceKeyKind::IntZext { .. } | CoalesceKeyKind::BoolZext => {
                let value = loaded.into_int_value();
                match value.get_type().get_bit_width() {
                    0..=63 => builder
                        .build_int_z_extend(value, i64_ty, "coalesce_key_zext")
                        .llvm_ctx("coalesce key integer zext")?,
                    64 => value,
                    width => {
                        return Err(CodegenError::FailClosed(format!(
                            "coalesce key integer field for actor `{}` has unsupported LLVM width {width}",
                            layout.name
                        )));
                    }
                }
            }
            CoalesceKeyKind::StringHash => {
                let string_ptr = loaded.into_pointer_value();
                let hash_fn = llvm_mod
                    .get_function("hew_string_hash_fnv1a")
                    .unwrap_or_else(|| {
                        llvm_mod.add_function(
                            "hew_string_hash_fnv1a",
                            i64_ty.fn_type(&[ptr_ty.into()], false),
                            Some(Linkage::External),
                        )
                    });
                builder
                    .build_call(hash_fn, &[string_ptr.into()], "coalesce_string_hash")
                    .llvm_ctx("coalesce string hash call")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "hew_string_hash_fnv1a returned void in coalesce key fn".into(),
                        )
                    })?
                    .into_int_value()
            }
        };
        builder
            .build_return(Some(&key))
            .llvm_ctx("coalesce key return")?;
    }

    builder.position_at_end(default_bb);
    // Only coalescing message types require `key_field`; mixed handlers are valid.
    // This least-surprise rule intentionally avoids a blanket annotation burden on
    // unrelated handlers. Pointer identity prevents unkeyed handlers from matching.
    let default_key = builder
        .build_ptr_to_int(payload, i64_ty, "unkeyed_payload_identity")
        .llvm_ctx("coalesce unkeyed payload identity")?;
    builder
        .build_return(Some(&default_key))
        .llvm_ctx("coalesce unkeyed return")?;
    Ok(key_fn)
}

/// Emit a per-actor typed payload destructor for mailbox eviction legs.
///
/// Dispatch moves owned fields out before freeing a consumed node, so this
/// callback is registered only on eviction/replacement paths where dispatch
/// will never run.
pub(crate) fn emit_actor_message_drop_fn<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    target_data: &TargetData,
    layout: &ActorLayout,
    record_layouts: &RecordLayoutMap<'ctx>,
    mir_record_layouts: &[hew_mir::RecordLayout],
    enum_layouts: &[hew_mir::EnumLayout],
) -> CodegenResult<FunctionValue<'ctx>> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let size_ty = runtime_size_ty(ctx, llvm_mod);
    let fn_name = message_drop_fn_name(&layout.name);
    let drop_fn = llvm_mod.add_function(
        &fn_name,
        ctx.void_type()
            .fn_type(&[i32_ty.into(), ptr_ty.into(), size_ty.into()], false),
        Some(Linkage::Internal),
    );
    let builder = ctx.create_builder();
    let entry = ctx.append_basic_block(drop_fn, "entry");
    let default_bb = ctx.append_basic_block(drop_fn, "unknown_msg_type");
    builder.position_at_end(entry);
    let msg_type = drop_fn
        .get_nth_param(0)
        .ok_or_else(|| CodegenError::FailClosed("message drop fn missing msg_type".into()))?
        .into_int_value();
    let payload = drop_fn
        .get_nth_param(1)
        .ok_or_else(|| CodegenError::FailClosed("message drop fn missing payload".into()))?
        .into_pointer_value();

    let mut cases = Vec::with_capacity(layout.handlers.len());
    for handler in &layout.handlers {
        let bb = ctx.append_basic_block(drop_fn, &format!("msg_{}", handler.msg_type));
        cases.push((i32_ty.const_int(handler.msg_type as u64, false), bb));
    }
    builder
        .build_switch(msg_type, default_bb, &cases)
        .llvm_ctx("message drop msg_type switch")?;

    for (handler, (_, bb)) in layout.handlers.iter().zip(cases.iter()) {
        builder.position_at_end(*bb);
        if handler.param_tys.is_empty() {
            builder
                .build_return(None)
                .llvm_ctx("unit message drop return")?;
            continue;
        }

        let mut field_tys = Vec::with_capacity(handler.param_tys.len());
        let mut field_kinds = Vec::with_capacity(handler.param_tys.len());
        for param_ty in &handler.param_tys {
            field_tys.push(resolve_ty(ctx, target_data, param_ty, record_layouts)?);
            let mut visited = HashSet::new();
            field_kinds.push(
                hew_mir::classify_state_field_with_enum_layouts(
                    param_ty,
                    mir_record_layouts,
                    enum_layouts,
                    &mut visited,
                )
                .map_err(|error| {
                    CodegenError::FailClosed(format!(
                        "actor `{}` handler `{}` payload type `{param_ty:?}` is not drop-classifiable: {error}",
                        layout.name, handler.name
                    ))
                })?,
            );
        }
        let payload_struct = ctx.struct_type(&field_tys, false);
        let helper_name = format!(
            "{}_{}",
            message_drop_fn_name(&layout.name),
            handler.msg_type
        );
        let helper = llvm_mod.add_function(
            &helper_name,
            ctx.void_type().fn_type(&[ptr_ty.into()], false),
            Some(Linkage::Internal),
        );
        emit_aggregate_drop_inplace_body(ctx, llvm_mod, helper, payload_struct, &field_kinds)?;
        builder
            .build_call(helper, &[payload.into()], "drop_message_payload")
            .llvm_ctx("message payload drop helper call")?;
        builder.build_return(None).llvm_ctx("message drop return")?;
    }

    builder.position_at_end(default_bb);
    builder
        .build_return(None)
        .llvm_ctx("unknown message drop return")?;
    Ok(drop_fn)
}

pub(crate) fn emit_actor_dispatch_trampoline<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    target_data: &TargetData,
    layout: &ActorLayout,
    // NEW-3a: per-handler suspendable predicate, parallel to `layout.handlers`.
    // `Some(true)`  — the handler's MIR carries `Terminator::Suspend`; it is
    //                 emitted as a coroutine ramp and the trampoline DRIVES it.
    // `Some(false)` — a run-to-completion handler; the trampoline direct-calls
    //                 it (the byte-identical-to-baseline fast path).
    // `None`        — no raw MIR function matched the handler symbol; the
    //                 trampoline fails closed (the discriminator was not carried
    //                 — the R2 silent-no-op class, refused not defaulted).
    handler_suspendable: &[Option<bool>],
    fn_symbols: &FnSymbolMap<'ctx>,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<()> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    // `data_size` is `usize` in `HewDispatchFn` (i32 on wasm32, i64 native).
    // The scheduler registers this trampoline as the actor's dispatch fn
    // pointer and invokes it via the wasm function table; a width mismatch
    // here surfaces as a wasm `indirect call type mismatch` trap. Match the
    // runtime ABI width. See `runtime_size_ty`.
    let size_ty = runtime_size_ty(ctx, llvm_mod);
    let dispatch_name = format!("__hew_actor_dispatch_{}", mangle_dotted_name(&layout.name));
    // D-A.2 (R326/R327): the trampoline returns the dispatch suspend outcome as
    // a nullable continuation handle (`ptr`). Run-to-completion handlers — every
    // handler today, since no source construct produces a `coro.suspend` yet —
    // return `null`. A suspending handler (NEW-3) returns its `coro.begin`
    // handle, which the scheduler parks. Surfacing the handle through the return
    // value (vs. a `HewActor` field) keeps the offset-mirror untouched; codegen
    // registers this trampoline as an opaque `ptr`, so widening the return type
    // does not change the spawn-registration ABI. Matches `HewDispatchFn`
    // (hew-runtime/src/internal/types.rs).
    let fn_ty = ptr_ty.fn_type(
        &[
            ptr_ty.into(),
            ptr_ty.into(),
            i32_ty.into(),
            ptr_ty.into(),
            size_ty.into(),
            // P5-RX sub-stage 1: borrow_mode receipt discriminant. Must match
            // `HewDispatchFn` (hew-runtime/src/internal/types.rs) — the runtime
            // scheduler is the sole caller and registers this trampoline as the
            // actor's `dispatch` fn pointer, so the arities must stay in lockstep.
            i32_ty.into(),
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

    // P5-RX — borrow-load receipt primitive.
    //
    // Resolve the payload source pointer ONCE, before the msg_type switch,
    // from the `borrow_mode` discriminant (param 5):
    //   - copy mode (0): source = `data` (param 3), the node-owned buffer.
    //   - borrow mode (1): source = `hew_msg_envelope_payload_ptr(data)`,
    //     where `data` is the `HewMsgEnvelope*`; the borrowed payload is read
    //     read-only and the single drop stays owned by
    //     `hew_msg_envelope_release`. A null borrowed payload fails closed via
    //     `hew_panic` (Gate 2) rather than being loaded — see the guard below.
    //
    // The branch is real control flow (not a `select`): calling
    // `hew_msg_envelope_payload_ptr` on a copy-mode `data` buffer would be a
    // wild read, so the accessor is invoked only on the borrow arm.
    let borrow_mode = dispatch_fn
        .get_nth_param(5)
        .ok_or_else(|| CodegenError::FailClosed("dispatch missing borrow_mode param".into()))?
        .into_int_value();
    let data_ptr = dispatch_fn
        .get_nth_param(3)
        .ok_or_else(|| CodegenError::FailClosed("dispatch missing data param".into()))?
        .into_pointer_value();
    let is_borrow = builder
        .build_int_compare(
            inkwell::IntPredicate::NE,
            borrow_mode,
            i32_ty.const_zero(),
            "dispatch_is_borrow",
        )
        .llvm_ctx("dispatch borrow_mode compare")?;
    let borrow_src_bb = ctx.append_basic_block(dispatch_fn, "borrow_src");
    let copy_src_bb = ctx.append_basic_block(dispatch_fn, "copy_src");
    let src_merge_bb = ctx.append_basic_block(dispatch_fn, "payload_src");
    builder
        .build_conditional_branch(is_borrow, borrow_src_bb, copy_src_bb)
        .llvm_ctx("dispatch borrow_mode branch")?;

    builder.position_at_end(borrow_src_bb);
    let payload_ptr_fn = llvm_mod
        .get_function("hew_msg_envelope_payload_ptr")
        .unwrap_or_else(|| {
            // `*mut c_void hew_msg_envelope_payload_ptr(HewMsgEnvelope *env)`
            // (hew-runtime/src/mailbox.rs). Borrow accessor: returns the
            // payload pointer the envelope owns; the caller must not free it.
            let payload_fn_ty = ptr_ty.fn_type(&[ptr_ty.into()], false);
            llvm_mod.add_function(
                "hew_msg_envelope_payload_ptr",
                payload_fn_ty,
                Some(Linkage::External),
            )
        });
    let borrowed_payload = builder
        .build_call(payload_ptr_fn, &[data_ptr.into()], "envelope_payload_ptr")
        .llvm_ctx("hew_msg_envelope_payload_ptr call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_msg_envelope_payload_ptr returned void".into())
        })?
        .into_pointer_value();

    // GATE 2 (fail-closed null guard, P5-RX). The borrow arm reads the
    // payload through `borrowed_payload`; a null envelope payload pointer
    // (corrupt / mis-built envelope node, or a `clone_alias`/`fork` race
    // that left no buffer) must NEVER be by-value loaded below — that would
    // be a silent null deref / wild read at the `build_load`. Refuse hard:
    // null payload ⇒ `hew_panic` (longjmps to the scheduler crash frame, or
    // exits the process), never a quiet dereference. Real control flow, not
    // a `select`: the load only ever sees a proven-non-null pointer.
    let borrow_payload_is_null = builder
        .build_int_compare(
            inkwell::IntPredicate::EQ,
            borrowed_payload,
            ptr_ty.const_null(),
            "borrow_payload_is_null",
        )
        .llvm_ctx("dispatch borrow payload null compare")?;
    let borrow_null_bb = ctx.append_basic_block(dispatch_fn, "borrow_payload_null");
    let borrow_ok_bb = ctx.append_basic_block(dispatch_fn, "borrow_payload_ok");
    builder
        .build_conditional_branch(borrow_payload_is_null, borrow_null_bb, borrow_ok_bb)
        .llvm_ctx("dispatch borrow payload null branch")?;

    // Fail-closed arm: hard panic, never return to the load.
    builder.position_at_end(borrow_null_bb);
    let panic_fn = llvm_mod.get_function("hew_panic").unwrap_or_else(|| {
        // `void hew_panic(void)` (hew-runtime/src/actor.rs) — diverges via
        // the actor recovery longjmp or a clean process exit.
        let panic_ty = ctx.void_type().fn_type(&[], false);
        llvm_mod.add_function("hew_panic", panic_ty, Some(Linkage::External))
    });
    builder
        .build_call(panic_fn, &[], "borrow_payload_null_panic")
        .llvm_ctx("dispatch borrow null guard hew_panic call")?;
    builder
        .build_unreachable()
        .llvm_ctx("dispatch borrow null guard unreachable")?;

    // Proven-non-null arm: feed the merge phi.
    builder.position_at_end(borrow_ok_bb);
    builder
        .build_unconditional_branch(src_merge_bb)
        .llvm_ctx("dispatch borrow src branch")?;

    builder.position_at_end(copy_src_bb);
    builder
        .build_unconditional_branch(src_merge_bb)
        .llvm_ctx("dispatch copy src branch")?;

    builder.position_at_end(src_merge_bb);
    let payload_src = builder
        .build_phi(ptr_ty, "payload_src_ptr")
        .llvm_ctx("dispatch payload src phi")?;
    payload_src.add_incoming(&[(&borrowed_payload, borrow_ok_bb), (&data_ptr, copy_src_bb)]);
    let payload_src = payload_src.as_basic_value().into_pointer_value();

    let mut cases = Vec::with_capacity(layout.handlers.len() + 1);
    for handler in &layout.handlers {
        let bb = ctx.append_basic_block(dispatch_fn, &format!("msg_{}", handler.msg_type));
        cases.push((i32_ty.const_int(handler.msg_type as u64, false), bb));
    }

    // M-7-R: route `SYS_MSG_EXIT` (103) to the actor's `#[on(exit)]` hook when
    // one is declared. The case block (emitted after the handler arms) unpacks
    // the `ExitMessage { crashed_actor_id: u64, reason: i32, crash_kind: i32 }`
    // payload and calls `__on_exit(ctx, actor_id, crash_kind)`.
    const SYS_MSG_EXIT: u64 = 103;
    let on_exit_bb = if layout.on_exit_symbol.is_some() {
        let bb = ctx.append_basic_block(dispatch_fn, "msg_sys_exit");
        cases.push((i32_ty.const_int(SYS_MSG_EXIT, false), bb));
        Some(bb)
    } else {
        None
    };
    // A non-trapping actor (no `#[on(exit)]`) receiving a `SYS_MSG_EXIT`
    // must CRASH via the controlled crash path — the OTP "linked processes die
    // together" semantic — NOT fall through to the `default_bb` exhaustiveness
    // `llvm.trap` (which is UB: it SIGILLs on Linux and only accidentally
    // produced a terminal state on macOS). Route this case to a block that reads
    // the carried reason and calls `hew_actor_exit_unhandled`, which longjmps to
    // the scheduler crash frame (terminal `Crashed`, the carried reason stamped).
    let unhandled_exit_bb = if layout.on_exit_symbol.is_none() {
        let bb = ctx.append_basic_block(dispatch_fn, "msg_sys_exit_unhandled");
        cases.push((i32_ty.const_int(SYS_MSG_EXIT, false), bb));
        Some(bb)
    } else {
        None
    };
    builder
        .build_switch(msg_type, default_bb, &cases)
        .llvm_ctx("actor dispatch switch")?;

    // The trampoline returns a nullable suspend handle (D-A.2). Each handler arm
    // joins at `after_bb`, contributing its return value to a phi:
    //   - run-to-completion handler        → null (no suspend)
    //   - suspendable handler, poll Ready  → null (it completed this dispatch;
    //                                         the reply was already deposited)
    //   - suspendable handler, poll Pending→ the `coro.begin` handle (parked)
    // The phi over these incomings is the trampoline's return value. Every
    // incoming is a `ptr` (the trampoline return type), so a uniform
    // `PointerValue` incoming list matches the `add_incoming` shape the existing
    // `payload_src` phi above uses.
    let mut return_incomings: Vec<(PointerValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> =
        Vec::with_capacity(layout.handlers.len());

    for (idx, (handler, (_, bb))) in layout.handlers.iter().zip(cases.iter()).enumerate() {
        builder.position_at_end(*bb);
        let symbol = fn_symbols.get(&handler.symbol).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "actor dispatch `{dispatch_name}` references undeclared handler `{}`",
                handler.symbol
            ))
        })?;
        let (handler_fn, return_ty, returns_unit) =
            symbol.real(&handler.symbol, "actor dispatch handler")?;

        // NEW-3a (R2): the ramp-vs-direct decision MUST trace to the
        // block-terminator carrier the coroutine emission read, never to a name
        // or a default. A handler symbol with no matching raw MIR function never
        // carried the discriminator — fail closed (a missing discriminator is
        // the Lane-B silent-no-op, refused not defaulted-to-direct-call).
        let is_suspendable = match handler_suspendable.get(idx).copied().flatten() {
            Some(flag) => flag,
            None => {
                return Err(CodegenError::FailClosed(format!(
                    "actor dispatch `{dispatch_name}`: handler `{}` has no matching MIR \
                     function to derive its suspendable predicate from; the ramp-vs-direct \
                     discriminator was not carried (R2 — refuse rather than silently \
                     direct-call a possibly-suspendable handler)",
                    handler.symbol
                )));
            }
        };

        // NEW-3a boundary-fail-closed: a suspendable handler is emitted as a
        // coroutine ramp whose LLVM return type is `ptr` (the `coro.begin`
        // handle) — enforced by the fail-closed guard in `lower_function`. If
        // the trampoline marks a handler suspendable but its declared LLVM
        // return is NOT `ptr`, the predicate and the emission disagree; refuse
        // rather than drive a non-coroutine through the resume/poll verbs.
        if is_suspendable && !matches!(return_ty, BasicTypeEnum::PointerType(_)) {
            return Err(CodegenError::FailClosed(format!(
                "actor dispatch `{dispatch_name}`: handler `{}` is marked suspendable \
                 (its MIR carries Terminator::Suspend) but its declared LLVM return type \
                 is {return_ty:?}, not the coro-handle `ptr` a ramp must return; the \
                 trampoline predicate and the ramp emission disagree (R2/boundary-fail-closed)",
                handler.symbol
            )));
        }

        let ctx_arg = dispatch_fn
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("dispatch missing ctx param".into()))?;
        // One trailing borrow_mode arg matches the receive-handler ABI growth
        // in `declare_function` (gated on the `__recv__` symbol). Dormant this
        // sub-stage: the handler body ignores it. A suspendable handler needs NO
        // extra out-pointer argument: the coroutine body deposits its reply
        // directly via `hew_get_reply_channel` + `hew_reply` (W6.010 value
        // routing lives in the body, not a trampoline out-slot — the body
        // completes wherever the resume lands, including the scheduler's
        // `resume_park` after the trampoline frame has unwound).
        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(2 + handler.param_tys.len());
        args.push(ctx_arg.into());
        if handler.param_tys.len() > 1 {
            // Multi-arg packed-record wire: the sender (`lower_packed_args_
            // payload`) packed the args into an anonymous non-packed record
            // whose field types are exactly this handler's `param_tys` in
            // declaration order. Rebuild the identical literal struct type
            // here and read each param from its natural field offset — a
            // single base-pointer load per param (the pre-wire shape) would
            // read every param from offset 0. LLVM literal and named structs
            // with the same body share one layout, so both wire ends agree
            // by construction. Each field load reads exactly
            // `sizeof(field_ty)` bytes at its offset — the bounded-copy
            // invariant the single-arg wire carries.
            let mut packed_field_tys: Vec<BasicTypeEnum> =
                Vec::with_capacity(handler.param_tys.len());
            for param_ty in &handler.param_tys {
                packed_field_tys.push(resolve_ty(ctx, target_data, param_ty, record_layouts)?);
            }
            let packed_st = ctx.struct_type(&packed_field_tys, false);
            for (idx, field_ty) in packed_field_tys.iter().enumerate() {
                let field_idx = u32::try_from(idx).map_err(|_| {
                    CodegenError::FailClosed(format!(
                        "actor dispatch `{dispatch_name}`: handler `{}` param count \
                         exceeds u32::MAX — impossible in Hew",
                        handler.symbol
                    ))
                })?;
                let field_ptr = builder
                    .build_struct_gep(
                        packed_st,
                        payload_src,
                        field_idx,
                        &format!("msg_arg_{idx}_ptr"),
                    )
                    .llvm_ctx("actor dispatch packed arg gep")?;
                let loaded = builder
                    .build_load(*field_ty, field_ptr, &format!("msg_arg_{idx}"))
                    .llvm_ctx("actor dispatch packed arg load")?;
                args.push(metadata_value_from_basic(loaded));
            }
        } else {
            for (idx, param_ty) in handler.param_tys.iter().enumerate() {
                let llvm_ty = resolve_ty(ctx, target_data, param_ty, record_layouts)?;
                let loaded = builder
                    .build_load(llvm_ty, payload_src, &format!("msg_arg_{idx}"))
                    .llvm_ctx("actor dispatch arg load")?;
                args.push(metadata_value_from_basic(loaded));
            }
        }
        args.push(borrow_mode.into());
        let call = builder
            .build_call(handler_fn, &args, &format!("call_{}", handler.name))
            .llvm_ctx("actor dispatch handler call")?;

        if is_suspendable {
            // NEW-3a — DRIVE the handler's coroutine.
            //
            // The call result IS the `coro.begin` handle (a `ptr`), NOT a reply
            // value (E4): a suspendable handler's ramp returns the frame handle.
            // Drive ONE resume/poll step:
            //   handle = ramp(...)            ; the coro frame
            //   hew_cont_resume(handle)       ; run to the next suspend/completion
            //   poll = hew_cont_poll(handle, out)
            //   Pending → return handle       ; the scheduler parks it
            //   Ready   → extract `out` → hew_reply; return null (completed)
            let handle = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "suspendable handler `{}` coroutine ramp returned void; \
                     a ramp must return its coro.begin handle (ptr)",
                    handler.symbol
                ))
            })?;
            if !matches!(handle.get_type(), BasicTypeEnum::PointerType(_)) {
                return Err(CodegenError::FailClosed(format!(
                    "suspendable handler `{}` coroutine ramp returned {:?}, not the \
                     coro.begin handle (ptr)",
                    handler.symbol,
                    handle.get_type()
                )));
            }
            let handle = handle.into_pointer_value();

            // Drive the cont C-ABI verbs. These are runtime symbols
            // (`hew-runtime/src/cont.rs`); declared trampoline-locally via the
            // get-or-declare pattern (the same way this fn declares `hew_panic`
            // and `hew_msg_envelope_payload_ptr`) so they are not coupled to the
            // MIR-emitter runtime-symbol allowlist.
            let cont_resume_fn = llvm_mod.get_function("hew_cont_resume").unwrap_or_else(|| {
                // `void hew_cont_resume(void *handle)` (cont.rs:214).
                let ty = ctx.void_type().fn_type(&[ptr_ty.into()], false);
                llvm_mod.add_function("hew_cont_resume", ty, Some(Linkage::External))
            });
            let cont_poll_fn = llvm_mod.get_function("hew_cont_poll").unwrap_or_else(|| {
                // `ResumePoll hew_cont_poll(void *handle, void *out_value)`
                // (cont.rs:269). `ResumePoll` is `#[repr(i32)]`
                // (Pending=0, Ready=1, cont.rs:100-109), so the C return is i32.
                let ty = i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false);
                llvm_mod.add_function("hew_cont_poll", ty, Some(Linkage::External))
            });
            let cont_destroy_fn = llvm_mod
                .get_function("hew_cont_destroy")
                .unwrap_or_else(|| {
                    // `void hew_cont_destroy(void *handle)` (cont.rs:294). The SOLE
                    // teardown owner: runs the frame's `cleanup` outline (drops of
                    // frame-owned locals held across the await) then frees the frame.
                    // Null-safe at the runtime (cont.rs:295). Declared trampoline-local
                    // via the same get-or-declare pattern as `hew_cont_resume`/`poll`.
                    let ty = ctx.void_type().fn_type(&[ptr_ty.into()], false);
                    llvm_mod.add_function("hew_cont_destroy", ty, Some(Linkage::External))
                });

            // Do NOT resume here. Calling the ramp already ran the coroutine
            // body to its FIRST `coro.suspend` (the SuspendingAsk send + park);
            // `handle` is suspended at that point. An immediate `hew_cont_resume`
            // would drive the body PAST the suspend into its reply-bind block
            // before the reply has arrived, re-blocking the worker on
            // `hew_reply_wait` — the exact OS-thread block this change removes
            // (R1). Instead POLL the just-parked handle: a Pending poll means the
            // body is waiting on a readiness source (the ask reply), so the
            // trampoline returns the handle and the scheduler parks it; the real
            // resume is driven later by `enqueue_resume` → `resume_park` (which
            // does its own resume+poll) when the reply fires. A Ready poll means
            // the body completed without ever suspending on a real source (it ran
            // to its final suspend immediately), handled by the Ready arm below.
            // `cont_resume_fn` stays declared (the scheduler's resume_park uses
            // it); the trampoline simply does not drive the first resume.
            let _ = cont_resume_fn;

            // Poll the just-parked handle for its done-state ONLY (W6.010 value
            // routing lives in the coroutine body, which deposits its reply via
            // `hew_reply` at its final return — see the `Terminator::Return`
            // coroutine arm). The poll out-pointer is therefore unused here: pass
            // null. `hew_cont_poll` reads `coro.done`: Pending → the body is
            // waiting on a readiness source (park it); Ready → the body completed
            // (and already deposited its reply), just reclaim the frame.
            let poll = builder
                .build_call(
                    cont_poll_fn,
                    &[handle.into(), ptr_ty.const_null().into()],
                    "hew_cont_poll_call",
                )
                .llvm_ctx("hew_cont_poll call")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_cont_poll returned void".into()))?
                .into_int_value();

            // `poll == ResumePoll::Ready (1)` → completed this dispatch.
            // `poll == ResumePoll::Pending (0)` → suspended; park the handle.
            let ready_const = i32_ty.const_int(1, false);
            let is_ready = builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    poll,
                    ready_const,
                    "suspend_poll_is_ready",
                )
                .llvm_ctx("suspend poll ready compare")?;
            let ready_bb = ctx.append_basic_block(dispatch_fn, &format!("suspend_ready_{idx}"));
            let pending_bb = ctx.append_basic_block(dispatch_fn, &format!("suspend_pending_{idx}"));
            builder
                .build_conditional_branch(is_ready, ready_bb, pending_bb)
                .llvm_ctx("suspend poll branch")?;

            // Ready arm: the coroutine reached its final suspend on the FIRST
            // poll (it completed without ever parking on a real readiness source
            // — e.g. an await whose reply was already available). The coroutine
            // BODY already deposited its reply (`Terminator::Return` coroutine
            // arm: `hew_get_reply_channel` + `hew_reply`), so the trampoline does
            // NOT deposit here — doing so would double-reply. The trampoline only
            // reclaims the frame and returns null (no park).
            builder.position_at_end(ready_bb);
            // Reclaim the frame exactly once (F-A). The continuation reached its
            // final suspend (`coro.done`); `hew_cont_destroy` is its SOLE teardown
            // owner (cont.rs:283-285), running the `cleanup` outline — drops of
            // frame-owned locals held across the await — then freeing the frame.
            // Emitted for BOTH the unit and value cases (outside the reply block),
            // after the reply is deposited and before the merge into `after_bb`.
            //
            // DISJOINTNESS (no double-destroy with the scheduler's resume-reentry
            // teardown): this Ready arm fires only when the FIRST poll returns
            // Ready, in which case the trampoline returns `null` (the incoming
            // pushed below) so the scheduler's park edge — which parks only on a
            // NON-null handle (scheduler.rs ~1395) — never stores this handle into
            // `actor.suspended_cont`. The scheduler's `destroy_parked`
            // (coro_exec.rs:332) swaps that slot and so destroys ONLY handles that
            // were parked, i.e. handles whose first poll was Pending. A given
            // handle is therefore destroyed here (first-poll-Ready, never parked)
            // XOR by `destroy_parked` (first-poll-Pending, parked then resumed) —
            // mutually exclusive. cont.rs:99: a Ready continuation is reclaimed via
            // `hew_cont_destroy` exactly once.
            builder
                .build_call(cont_destroy_fn, &[handle.into()], "hew_cont_destroy_call")
                .llvm_ctx("hew_cont_destroy call")?;
            builder
                .build_unconditional_branch(after_bb)
                .llvm_ctx("suspend ready branch")?;
            return_incomings.push((ptr_ty.const_null(), ready_bb));

            // Pending arm: the coroutine suspended at a non-final point. Surface
            // the handle as the trampoline return so the scheduler park edge
            // fires (scheduler.rs park-on-non-null-handle).
            builder.position_at_end(pending_bb);
            builder
                .build_unconditional_branch(after_bb)
                .llvm_ctx("suspend pending branch")?;
            return_incomings.push((handle, pending_bb));
        } else {
            // Run-to-completion handler (byte-identical to baseline): treat the
            // call result as the reply value and deposit it, then join with a
            // null suspend handle.
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
                let reply_channel = intern_runtime_decl(
                    ctx,
                    llvm_mod,
                    &mut runtime_decls,
                    "hew_get_reply_channel",
                )?;
                let reply = intern_runtime_decl(ctx, llvm_mod, &mut runtime_decls, "hew_reply")?;
                let ch = builder
                    .build_call(reply_channel, &[], "hew_get_reply_channel_call")
                    .llvm_ctx("get reply channel call")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("hew_get_reply_channel returned void".into())
                    })?
                    .into_pointer_value();
                let ret_slot = builder
                    .build_alloca(return_ty, "actor_reply_slot")
                    .llvm_ctx("actor reply alloca")?;
                builder
                    .build_store(ret_slot, ret_val)
                    .llvm_ctx("actor reply store")?;
                let size = return_ty.size_of().ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "actor handler `{}` reply type has no static size: {return_ty:?}",
                        handler.symbol
                    ))
                })?;
                // Reconcile LLVM size_of (always i64) to the target-correct
                // `usize`/`size_t` (i32 on wasm32) for `hew_reply`'s `size` param.
                let size = if size.get_type() == size_ty {
                    size
                } else if size_ty.get_bit_width() < i64_ty.get_bit_width() {
                    builder
                        .build_int_truncate(size, size_ty, "actor_reply_size_trunc")
                        .llvm_ctx("reply size trunc")?
                } else {
                    builder
                        .build_int_z_extend(size, size_ty, "actor_reply_size_zext")
                        .llvm_ctx("reply size zext")?
                };
                builder
                    .build_call(
                        reply,
                        &[ch.into(), ret_slot.into(), size.into()],
                        "hew_reply_call",
                    )
                    .llvm_ctx("hew_reply call")?;
            }
            // The current block is still `*bb` (no new blocks on the direct
            // path) — record it as the phi predecessor with a null handle.
            let pred = builder.get_insert_block().ok_or_else(|| {
                CodegenError::FailClosed("dispatch direct arm has no insert block".into())
            })?;
            builder
                .build_unconditional_branch(after_bb)
                .llvm_ctx("actor dispatch branch")?;
            return_incomings.push((ptr_ty.const_null(), pred));
        }
    }

    // M-7-R: emit the `SYS_MSG_EXIT` → `#[on(exit)]` case body. The runtime
    // delivers `ExitMessage { crashed_actor_id: u64, reason: i32, crash_kind:
    // i32 }` as the message payload; unpack `crashed_actor_id` and `crash_kind`
    // (the M-6 projection) and call `__on_exit(ctx, actor_id, crash_kind,
    // borrow_mode)`. The hook is a run-to-completion ActorHandler returning unit,
    // so the dispatch contributes `null` to the suspend-handle phi.
    if let (Some(on_exit_bb), Some(on_exit_symbol)) = (on_exit_bb, &layout.on_exit_symbol) {
        builder.position_at_end(on_exit_bb);
        let on_exit_sym = fn_symbols.get(on_exit_symbol).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "actor dispatch `{dispatch_name}` references undeclared on_exit handler \
                 `{on_exit_symbol}`"
            ))
        })?;
        let (on_exit_fn, _on_exit_ret, _on_exit_unit) =
            on_exit_sym.real(on_exit_symbol, "actor dispatch on_exit handler")?;

        // The ExitMessage layout (hew-runtime/src/link.rs) is
        // `{ u64 crashed_actor_id, i32 reason, i32 crash_kind }`. Read
        // `crashed_actor_id` (field 0) and `crash_kind` (field 2) from the
        // payload by their natural #[repr(C)] offsets via a matching LLVM struct.
        let u64_ty = ctx.i64_type();
        let exit_msg_st = ctx.struct_type(&[u64_ty.into(), i32_ty.into(), i32_ty.into()], false);
        let actor_id_ptr = builder
            .build_struct_gep(exit_msg_st, payload_src, 0, "exit_actor_id_ptr")
            .llvm_ctx("on_exit actor_id gep")?;
        let actor_id = builder
            .build_load(u64_ty, actor_id_ptr, "exit_actor_id")
            .llvm_ctx("on_exit actor_id load")?;
        let crash_kind_ptr = builder
            .build_struct_gep(exit_msg_st, payload_src, 2, "exit_crash_kind_ptr")
            .llvm_ctx("on_exit crash_kind gep")?;
        let crash_kind = builder
            .build_load(i32_ty, crash_kind_ptr, "exit_crash_kind")
            .llvm_ctx("on_exit crash_kind load")?;

        let ctx_arg = dispatch_fn
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("dispatch missing ctx param".into()))?;
        // ABI: `__on_exit(ctx, __exit_actor_id: u64, __exit_kind_tag: i32)` — the
        // ActorHandler convention (ctx-leading) plus the two unpacked
        // CrashNotification fields. `__on_exit` is NOT a `__recv__` symbol, so it
        // does NOT carry the trailing `borrow_mode` arg (that ABI growth is gated
        // on the `__recv__` symbol in `declare_function`).
        let on_exit_args: Vec<BasicMetadataValueEnum> = vec![
            ctx_arg.into(),
            metadata_value_from_basic(actor_id),
            metadata_value_from_basic(crash_kind),
        ];
        builder
            .build_call(on_exit_fn, &on_exit_args, "call_on_exit")
            .llvm_ctx("actor dispatch on_exit call")?;
        builder
            .build_unconditional_branch(after_bb)
            .llvm_ctx("actor dispatch on_exit branch")?;
        return_incomings.push((ptr_ty.const_null(), on_exit_bb));
    }

    // Emit the unhandled-`SYS_MSG_EXIT` case body — a non-trapping actor
    // crashes via the controlled crash path. Read `reason` (field 1 of the
    // `ExitMessage { crashed_actor_id: u64, reason: i32, crash_kind: i32 }`
    // payload) and call `hew_actor_exit_unhandled(reason)`, which longjmps to the
    // scheduler crash frame (terminal Crashed, the carried reason stamped) — never
    // the exhaustiveness `llvm.trap` (UB / SIGILL on Linux).
    if let Some(unhandled_exit_bb) = unhandled_exit_bb {
        builder.position_at_end(unhandled_exit_bb);
        let u64_ty = ctx.i64_type();
        let exit_msg_st = ctx.struct_type(&[u64_ty.into(), i32_ty.into(), i32_ty.into()], false);
        let reason_ptr = builder
            .build_struct_gep(exit_msg_st, payload_src, 1, "exit_reason_ptr")
            .llvm_ctx("unhandled exit reason gep")?;
        let reason = builder
            .build_load(i32_ty, reason_ptr, "exit_reason")
            .llvm_ctx("unhandled exit reason load")?;
        let exit_unhandled_fn = llvm_mod
            .get_function("hew_actor_exit_unhandled")
            .unwrap_or_else(|| {
                let sig = ctx.void_type().fn_type(&[i32_ty.into()], false);
                llvm_mod.add_function("hew_actor_exit_unhandled", sig, Some(Linkage::External))
            });
        builder
            .build_call(
                exit_unhandled_fn,
                &[metadata_value_from_basic(reason)],
                "call_exit_unhandled",
            )
            .llvm_ctx("actor dispatch unhandled exit call")?;
        // hew_actor_exit_unhandled longjmps to the scheduler crash frame inside a
        // dispatch, so control never returns here; branch to after_bb to satisfy
        // the CFG (the branch is unreachable in practice).
        builder
            .build_unconditional_branch(after_bb)
            .llvm_ctx("actor dispatch unhandled exit branch")?;
        return_incomings.push((ptr_ty.const_null(), unhandled_exit_bb));
    }

    builder.position_at_end(default_bb);
    let trap = Intrinsic::find("llvm.trap")
        .and_then(|intrinsic| intrinsic.get_declaration(llvm_mod, &[]))
        .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
    builder
        .build_call(trap, &[], "actor_dispatch_unknown_msg_trap")
        .llvm_ctx("actor dispatch trap")?;
    builder
        .build_unreachable()
        .llvm_ctx("actor dispatch unreachable")?;

    builder.position_at_end(after_bb);
    // D-A.2 / NEW-3a: the trampoline returns the dispatch suspend outcome — a
    // nullable `coro.begin` handle. Run-to-completion handlers and Ready-poll
    // suspendable handlers contribute `null` (no park); a Pending-poll
    // suspendable handler contributes its handle (the scheduler parks it). An
    // actor with zero handlers has no incoming edge to `after_bb` from the
    // switch arms (only the trapping default reaches the switch), so fall back
    // to a plain null return rather than an empty phi.
    if return_incomings.is_empty() {
        builder
            .build_return(Some(&ptr_ty.const_null()))
            .llvm_ctx("actor dispatch return null suspend handle")?;
    } else {
        let phi = builder
            .build_phi(ptr_ty, "dispatch_suspend_handle")
            .llvm_ctx("dispatch suspend handle phi")?;
        let incoming_refs: Vec<(
            &dyn inkwell::values::BasicValue<'ctx>,
            inkwell::basic_block::BasicBlock<'ctx>,
        )> = return_incomings
            .iter()
            .map(|(v, bb)| (v as &dyn inkwell::values::BasicValue<'ctx>, *bb))
            .collect();
        phi.add_incoming(&incoming_refs);
        builder
            .build_return(Some(&phi.as_basic_value()))
            .llvm_ctx("actor dispatch return suspend handle phi")?;
    }
    Ok(())
}
