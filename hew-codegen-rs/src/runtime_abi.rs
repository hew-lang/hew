//! Runtime-ABI call lowering for the LLVM backend.
//!
//! Pure relocation (R4 god-module carve) of the runtime-call lowering edge out
//! of llvm.rs: [`lower_call_runtime_abi`] — the single dispatch that lowers
//! every `Instr::CallRuntimeAbi` to its C-ABI extern declaration and call,
//! covering the entire `RuntimeCallFamily` surface. Every `bytes` parameter
//! crosses the C-ABI boundary as a plain `ptr` (the caller's BytesTriple alloca
//! address); the declaration edges and the call edge apply that one rule with
//! no per-symbol allowlist.
//!
//! Mirrors the `crate::coro` carve: shared codegen context and the per-family
//! emit helpers are imported from `crate::llvm`; `crate::llvm` calls back via
//! `crate::runtime_abi::lower_call_runtime_abi`. The per-family arm bodies live
//! verbatim here — no behaviour change; every emitted `.ll` is byte-identical.

use inkwell::context::Context;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::TargetData;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, IntPredicate};

use hew_hir::stdlib_catalog::{self, BuiltinEntry, BuiltinLinkage, BuiltinTy};
use hew_mir::Place;
use hew_types::ResolvedTy;

use crate::layout::{
    get_or_declare_bool_vec_runtime, get_or_declare_owned_vec_runtime,
    layout_vec_element_needs_descriptor, zext_bool_i1_to_dest,
};
#[allow(unused_imports)]
use crate::llvm::*;

/// Decode a setup ABI whose positive return is an internal ref id and whose
/// negative return encodes an error enum as `-(variant + 1)`.
///
/// When `ok_ref_payload` is true, the Ok payload is `MonitorRef { ref_id }`;
/// otherwise the Ok payload is unit. A defensive zero return maps to
/// `zero_error_tag` rather than producing an uninitialised success value.
fn emit_signed_setup_result(
    fn_ctx: &FnCtx<'_, '_>,
    raw_result: IntValue<'_>,
    dest: Place,
    ok_ref_payload: bool,
    zero_error_tag: u64,
    helper: &str,
) -> CodegenResult<()> {
    let dest_local = composite_dest_local(dest, helper)?;
    let i64_ty = fn_ctx.ctx.i64_type();
    let zero = i64_ty.const_zero();
    let is_ok = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::SGT,
            raw_result,
            zero,
            &format!("{helper}_is_ok"),
        )
        .llvm_ctx_with(|| format!("{helper}: compare setup result"))?;
    let current_fn = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|block| block.get_parent())
        .ok_or_else(|| CodegenError::FailClosed(format!("{helper}: no parent function")))?;
    let ok_bb = fn_ctx
        .ctx
        .append_basic_block(current_fn, &format!("{helper}_ok"));
    let err_bb = fn_ctx
        .ctx
        .append_basic_block(current_fn, &format!("{helper}_err"));
    let merge_bb = fn_ctx
        .ctx
        .append_basic_block(current_fn, &format!("{helper}_merge"));
    fn_ctx
        .builder
        .build_conditional_branch(is_ok, ok_bb, err_bb)
        .llvm_ctx_with(|| format!("{helper}: branch on setup result"))?;

    fn_ctx.builder.position_at_end(ok_bb);
    store_composite_tag(fn_ctx, dest_local, 0, helper)?;
    if ok_ref_payload {
        let (monitor_ptr, monitor_ty) = place_pointer(
            fn_ctx,
            Place::MachineVariant {
                local: dest_local,
                variant_idx: 0,
                field_idx: 0,
            },
        )?;
        let BasicTypeEnum::StructType(monitor_struct) = monitor_ty else {
            return Err(CodegenError::FailClosed(format!(
                "{helper}: Ok payload must be MonitorRef struct, got {monitor_ty:?}"
            )));
        };
        let Some(BasicTypeEnum::IntType(ref_id_ty)) = monitor_struct.get_field_type_at_index(0)
        else {
            return Err(CodegenError::FailClosed(format!(
                "{helper}: MonitorRef.ref_id must be an integer field"
            )));
        };
        if ref_id_ty.get_bit_width() != raw_result.get_type().get_bit_width() {
            return Err(CodegenError::FailClosed(format!(
                "{helper}: MonitorRef.ref_id width {} does not match setup ABI width {}",
                ref_id_ty.get_bit_width(),
                raw_result.get_type().get_bit_width()
            )));
        }
        let ref_id_ptr = fn_ctx
            .builder
            .build_struct_gep(
                monitor_struct,
                monitor_ptr,
                0,
                &format!("{helper}_ref_id_ptr"),
            )
            .llvm_ctx_with(|| format!("{helper}: GEP MonitorRef.ref_id"))?;
        fn_ctx
            .builder
            .build_store(ref_id_ptr, raw_result)
            .llvm_ctx_with(|| format!("{helper}: store MonitorRef.ref_id"))?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(merge_bb)
        .llvm_ctx_with(|| format!("{helper}: Ok branch to merge"))?;

    fn_ctx.builder.position_at_end(err_bb);
    store_composite_tag(fn_ctx, dest_local, 1, helper)?;
    let (error_ptr, error_ty) = place_pointer(
        fn_ctx,
        Place::MachineVariant {
            local: dest_local,
            variant_idx: 1,
            field_idx: 0,
        },
    )?;
    let BasicTypeEnum::StructType(error_struct) = error_ty else {
        return Err(CodegenError::FailClosed(format!(
            "{helper}: Err payload must be a unit-variant enum struct, got {error_ty:?}"
        )));
    };
    let Some(BasicTypeEnum::IntType(error_tag_ty)) = error_struct.get_field_type_at_index(0) else {
        return Err(CodegenError::FailClosed(format!(
            "{helper}: error enum tag must be an integer field"
        )));
    };
    let negated = fn_ctx
        .builder
        .build_int_sub(zero, raw_result, &format!("{helper}_negated"))
        .llvm_ctx_with(|| format!("{helper}: negate setup error"))?;
    let ordinal = fn_ctx
        .builder
        .build_int_sub(
            negated,
            i64_ty.const_int(1, false),
            &format!("{helper}_error_ordinal"),
        )
        .llvm_ctx_with(|| format!("{helper}: decode setup error ordinal"))?;
    let is_zero = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            raw_result,
            zero,
            &format!("{helper}_is_zero"),
        )
        .llvm_ctx_with(|| format!("{helper}: detect invalid zero setup result"))?;
    let selected_tag = fn_ctx
        .builder
        .build_select(
            is_zero,
            i64_ty.const_int(zero_error_tag, false),
            ordinal,
            &format!("{helper}_selected_error_tag"),
        )
        .llvm_ctx_with(|| format!("{helper}: select setup error tag"))?
        .into_int_value();
    let stored_tag = match selected_tag
        .get_type()
        .get_bit_width()
        .cmp(&error_tag_ty.get_bit_width())
    {
        std::cmp::Ordering::Equal => selected_tag,
        std::cmp::Ordering::Less => fn_ctx
            .builder
            .build_int_z_extend(
                selected_tag,
                error_tag_ty,
                &format!("{helper}_error_tag_zext"),
            )
            .llvm_ctx_with(|| format!("{helper}: widen error tag"))?,
        std::cmp::Ordering::Greater => fn_ctx
            .builder
            .build_int_truncate(
                selected_tag,
                error_tag_ty,
                &format!("{helper}_error_tag_trunc"),
            )
            .llvm_ctx_with(|| format!("{helper}: truncate error tag"))?,
    };
    let error_tag_ptr = fn_ctx
        .builder
        .build_struct_gep(
            error_struct,
            error_ptr,
            0,
            &format!("{helper}_error_tag_ptr"),
        )
        .llvm_ctx_with(|| format!("{helper}: GEP error enum tag"))?;
    fn_ctx
        .builder
        .build_store(error_tag_ptr, stored_tag)
        .llvm_ctx_with(|| format!("{helper}: store setup error tag"))?;
    fn_ctx
        .builder
        .build_unconditional_branch(merge_bb)
        .llvm_ctx_with(|| format!("{helper}: Err branch to merge"))?;

    fn_ctx.builder.position_at_end(merge_bb);
    Ok(())
}

/// The LLVM struct type matching the runtime `#[repr(C)] BytesTriple`
/// (`{ ptr, i32, i32 }`, `hew-runtime/src/bytes.rs`). The canonical aggregate
/// the R5 classifier keys bytes-return producers on.
pub(crate) fn bytes_triple_llvm_ty<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
) -> inkwell::types::StructType<'ctx> {
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let i32_ty = fn_ctx.ctx.i32_type();
    fn_ctx
        .ctx
        .struct_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false)
}

/// Issue a call to a classified bytes-return producer `fv` and store the
/// returned `BytesTriple` into `dest_ptr` (a `{ptr,i32,i32}` slot), per the
/// `return_abi` chosen by [`crate::abi_class::declare_aggregate_return`].
///
/// The unified flow for every bytes-return runtime producer (replacing the
/// per-symbol `_raw` out-pointer twins):
///   - `RegisterPair`: the call returns the `[2 x i64]` register-pair carrier;
///     store the 16-byte aggregate directly into `dest_ptr` (the `[2 x i64]` and
///     `{ptr,i32,i32}` byte layouts are identical — the documented reconstruction).
///   - `Sret`: pass `dest_ptr` as the hidden sret first argument; the runtime
///     writes the triple in place, so no post-call store is needed.
pub(crate) fn store_classified_bytes_return<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    fv: inkwell::values::FunctionValue<'ctx>,
    return_abi: crate::abi_class::AggregateReturnAbi<'ctx>,
    non_sret_args: &[BasicMetadataValueEnum<'ctx>],
    dest_ptr: inkwell::values::PointerValue<'ctx>,
    symbol: &str,
) -> CodegenResult<()> {
    match return_abi {
        crate::abi_class::AggregateReturnAbi::RegisterPair { carrier } => {
            let call_site = fn_ctx
                .builder
                .build_call(fv, non_sret_args, &format!("{symbol}_call"))
                .llvm_ctx("classified bytes-return call")?;
            let pair = call_site.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "{symbol} returned void unexpectedly (register-pair class)"
                ))
            })?;
            // The returned `[2 x i64]` is 16 bytes, identical to the dest
            // `{ptr,i32,i32}` slot's byte layout. Store the aggregate directly.
            let _ = carrier;
            fn_ctx
                .builder
                .build_store(dest_ptr, pair)
                .llvm_ctx("classified bytes-return store")?;
            Ok(())
        }
        crate::abi_class::AggregateReturnAbi::Sret => {
            let mut sret_args: Vec<BasicMetadataValueEnum<'ctx>> =
                Vec::with_capacity(non_sret_args.len() + 1);
            sret_args.push(dest_ptr.into());
            sret_args.extend_from_slice(non_sret_args);
            fn_ctx
                .builder
                .build_call(fv, &sret_args, &format!("{symbol}_sret_call"))
                .llvm_ctx("classified bytes-return (sret) call")?;
            // The runtime wrote the triple through the sret pointer; no store.
            Ok(())
        }
    }
}

/// Declare `symbol` as a classified `-> bytes` producer and issue the call,
/// landing the returned `BytesTriple` in `dest_ptr` per the target's aggregate
/// return ABI.
///
/// The single flow for every bytes-return runtime/stdlib producer: classify the
/// `{ptr,i32,i32}` return via [`crate::abi_class::declare_aggregate_return`]
/// (RegisterPair on SysV/AAPCS, sret-Indirect on MSVC/wasm32), then land the
/// result with [`store_classified_bytes_return`]. Replaces the per-symbol
/// `{symbol}_raw` void+out-pointer twin that hand-faked the MSVC/wasm32 sret ABI.
/// `non_sret_param_tys` are the producer's declared parameter types EXCLUDING any
/// sret pointer; `non_sret_args` are the matching argument values.
pub(crate) fn emit_classified_bytes_return_call<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    symbol: &str,
    non_sret_param_tys: &[inkwell::types::BasicMetadataTypeEnum<'ctx>],
    non_sret_args: &[BasicMetadataValueEnum<'ctx>],
    dest_ptr: inkwell::values::PointerValue<'ctx>,
) -> CodegenResult<()> {
    emit_classified_bytes_return_call_raw(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        fn_ctx.target_data,
        &fn_ctx.builder,
        symbol,
        non_sret_param_tys,
        non_sret_args,
        dest_ptr,
    )
}

/// [`emit_classified_bytes_return_call`] over raw codegen primitives, for the
/// codec-thunk sites (`emit_de_value_cbor` etc.) that thread `ctx`/`builder`/
/// `target_data` directly rather than a [`FnCtx`].
#[allow(clippy::too_many_arguments)]
pub(crate) fn emit_classified_bytes_return_call_raw<'ctx>(
    ctx: &'ctx inkwell::context::Context,
    llvm_mod: &inkwell::module::Module<'ctx>,
    target_data: &inkwell::targets::TargetData,
    builder: &inkwell::builder::Builder<'ctx>,
    symbol: &str,
    non_sret_param_tys: &[inkwell::types::BasicMetadataTypeEnum<'ctx>],
    non_sret_args: &[BasicMetadataValueEnum<'ctx>],
    dest_ptr: inkwell::values::PointerValue<'ctx>,
) -> CodegenResult<()> {
    let triple = llvm_mod.get_triple();
    let triple_str = triple.as_str().to_string_lossy();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let bytes_triple_ty = ctx.struct_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false);
    let (fv, return_abi) = crate::abi_class::declare_aggregate_return(
        ctx,
        llvm_mod,
        target_data,
        &triple_str,
        symbol,
        bytes_triple_ty,
        non_sret_param_tys,
    )?;
    match return_abi {
        crate::abi_class::AggregateReturnAbi::RegisterPair { .. } => {
            let call_site = builder
                .build_call(fv, non_sret_args, &format!("{symbol}_call"))
                .llvm_ctx("classified bytes-return call")?;
            let pair = call_site.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "{symbol} returned void unexpectedly (register-pair class)"
                ))
            })?;
            builder
                .build_store(dest_ptr, pair)
                .llvm_ctx("classified bytes-return store")?;
            Ok(())
        }
        crate::abi_class::AggregateReturnAbi::Sret => {
            let mut sret_args: Vec<BasicMetadataValueEnum<'ctx>> =
                Vec::with_capacity(non_sret_args.len() + 1);
            sret_args.push(dest_ptr.into());
            sret_args.extend_from_slice(non_sret_args);
            builder
                .build_call(fv, &sret_args, &format!("{symbol}_sret_call"))
                .llvm_ctx("classified bytes-return (sret) call")?;
            Ok(())
        }
    }
}

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
///   The i32 status is discarded when the producer supplies no `dest`
///   (statement context) and materialized into `Result<(), SendError>`
///   via `emit_send_result_from_rc` when it does (value context).
///
/// Symbols on the M2 allowlist but not yet wired (e.g.
/// `hew_lambda_actor_release`) fall through to a fail-closed arm so
/// the producer surface and the codegen surface stay in lock-step.
pub(crate) fn lower_call_runtime_abi(
    fn_ctx: &FnCtx<'_, '_>,
    call: &hew_mir::RuntimeCall,
) -> CodegenResult<()> {
    use hew_types::runtime_call::{RuntimeCallFamily as F, VecGetElem};
    // `symbol` is DERIVED from the family at the codegen edge — kept as a
    // binding because the arm bodies use it for diagnostics, sub-dispatch
    // within a multi-family arm, and `intern_runtime_decl` (the LLVM
    // declare edge, where the C name is a name by nature).
    let symbol = call.symbol();
    let args = call.args();
    let dest = call.dest();
    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();
    let i8_ty = fn_ctx.ctx.i8_type();
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    match call.family() {
        F::DuplexPair => {
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
            let llvm_args: [BasicMetadataValueEnum; 4] =
                [cap0.into(), cap1.into(), out_a.into(), out_b.into()];
            fn_ctx.call_runtime_void(
                symbol,
                &llvm_args,
                "hew_duplex_pair_call",
                "hew_duplex_pair call",
            )?;
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
        F::DuplexSend => {
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
            let llvm_args: [BasicMetadataValueEnum; 3] =
                [handle.into(), msg_ptr.into(), len.into()];
            let call_site = fn_ctx.call_runtime(
                symbol,
                &llvm_args,
                "hew_duplex_send_call",
                "hew_duplex_send call",
            )?;
            // Statement context (dest=None) discards the rc — fire-and-forget
            // delivery. Value context materializes `Result<(), SendError>` from
            // the i32 status through the single D1 mapping authority.
            if let Some(d) = dest {
                let status = call_site
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "hew_duplex_send returned void — expected i32 status".into(),
                        )
                    })?
                    .into_int_value();
                emit_send_result_from_rc(fn_ctx, status, d, "hew_duplex_send")?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // ── Channel-Duplex split: half extraction ─────────────────────────
        //
        // hew_duplex_send_half(d) -> *mut HewSendHalfHandle
        // hew_duplex_recv_half(d) -> *mut HewRecvHalfHandle
        // Consume the unified handle and RETURN the direction-only half pointer.
        // arg0: the unified `DuplexHandle` Place — load the raw ptr from its
        // alloca. dest: the half Place (`SendHalf`/`RecvHalf`) — store the
        // returned half pointer into its alloca. The MIR producer consumed the
        // source binding, so the unified alloca is no longer drop-scheduled.
        F::DuplexSendHalf | F::DuplexRecvHalf => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 1 arg (duplex), got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "{symbol}: producer must supply a half dest place"
                ))
            })?;
            let duplex = load_duplex_handle(fn_ctx, args[0], "duplex_half_extract arg0")?;
            let half_ptr = fn_ctx.call_runtime_ptr(
                symbol,
                &[duplex.into()],
                "duplex_half_extract_call",
                "duplex half extract call",
            )?;
            let (dest_slot, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_slot, half_ptr)
                .llvm_ctx("duplex half-extract store")?;
            let _ = (i32_ty, ptr_ty);
        }
        // ── SendHalf send / try_send ──────────────────────────────────────
        //
        // hew_send_half_send(half, msg: *const u8, len: usize) -> i32
        // hew_send_half_try_send(half, msg, len) -> i32
        // Same ABI shape as hew_duplex_send: load the half ptr (borrow), spill
        // the integer message into a fresh i64 alloca, pass `(ptr, len)`, and
        // materialise `Result<(), SendError>` from the i32 status in value
        // context (discard in statement context).
        F::SendHalfSend | F::SendHalfTrySend => {
            if args.len() != 3 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 3 args (half, msg, len), got {}",
                    args.len()
                )));
            }
            let half = load_duplex_handle(fn_ctx, args[0], "half_send arg0")?;
            let msg_ptr = spill_int_arg_as_ptr(fn_ctx, args[1], i64_ty, "half_send_msg")?;
            let len = load_int_arg(fn_ctx, args[2], i64_ty, "half_send_len")?;
            let llvm_args: [BasicMetadataValueEnum; 3] =
                [half.into(), msg_ptr.into(), len.into()];
            let call_site = fn_ctx.call_runtime(
                symbol,
                &llvm_args,
                "half_send_call",
                "send-half send call",
            )?;
            if let Some(d) = dest {
                let status = call_site
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "{symbol} returned void — expected i32 status"
                        ))
                    })?
                    .into_int_value();
                emit_send_result_from_rc(fn_ctx, status, d, "send_half_send")?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // ── Recv family: RecvHalf recv / try_recv + unified Duplex recv ───
        //
        // hew_recv_half_recv(half, out_ptr, out_len) -> i32
        // hew_recv_half_try_recv(half, out_ptr, out_len) -> i32
        // hew_duplex_recv(d, out_ptr, out_len) -> i32
        // hew_duplex_try_recv(d, out_ptr, out_len) -> i32
        // The runtime writes the received payload to caller out-params and
        // returns a RecvError status. Materialise `Result<R, RecvError>`: on
        // status Ok copy the exact payload bytes into Result::Ok then free the
        // payload buffer; on any non-Ok status build Result::Err(<mapped
        // RecvError>). The producer supplies a dest (required — the status
        // carries the closed/empty signal even when the user discards it).
        F::RecvHalfRecv | F::RecvHalfTryRecv | F::DuplexRecv | F::DuplexTryRecv => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 1 arg (handle), got {}",
                    args.len()
                )));
            }
            let d = dest.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "{symbol}: producer must supply a Result dest place"
                ))
            })?;
            let handle = load_duplex_handle(fn_ctx, args[0], "duplex_recv arg0")?;
            // Fresh out-param slots: `*mut u8` payload ptr + `usize` length.
            let out_ptr_slot = fn_ctx
                .builder
                .build_alloca(ptr_ty, "duplex_recv_out_ptr")
                .llvm_ctx("duplex recv out_ptr alloca")?;
            let out_len_slot = fn_ctx
                .builder
                .build_alloca(i64_ty, "duplex_recv_out_len")
                .llvm_ctx("duplex recv out_len alloca")?;
            let status = fn_ctx
                .call_runtime(
                    symbol,
                    &[handle.into(), out_ptr_slot.into(), out_len_slot.into()],
                    "duplex_recv_call",
                    "duplex recv call",
                )?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "{symbol} returned void — expected i32 status"
                    ))
                })?
                .into_int_value();
            emit_recv_result_from_out_params(
                fn_ctx,
                status,
                out_ptr_slot,
                out_len_slot,
                d,
                symbol,
            )?;
            let _ = (i32_ty, ptr_ty);
        }
        // ── Half close (explicit) ─────────────────────────────────────────
        //
        // hew_duplex_close_half(half, direction: i32) -> i32
        // The direction (SendHalf=0 / RecvHalf=1) is selected from the
        // receiver `Place` variant — the same authority drop elaboration uses.
        // The i32 status materialises into `Result<(), CloseError>` in value
        // context; statement context discards it. The runtime's AtomicBool
        // double-close guard keeps a later scope-exit drop safe even though the
        // MIR producer consumed the half (so no scope-exit close is scheduled).
        F::DuplexCloseHalf => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_duplex_close_half): expected 1 arg (half), got {}",
                    args.len()
                )));
            }
            let direction = match args[0] {
                Place::SendHalf(_) => 0u64,
                Place::RecvHalf(_) => 1u64,
                other => {
                    return Err(CodegenError::FailClosed(format!(
                        "hew_duplex_close_half: receiver must be a SendHalf/RecvHalf place \
                         (the direction authority), got {other:?}"
                    )));
                }
            };
            let half = load_duplex_handle(fn_ctx, args[0], "half_close arg0")?;
            let direction_val = i32_ty.const_int(direction, false);
            let call_site = fn_ctx.call_runtime(
                symbol,
                &[half.into(), direction_val.into()],
                "half_close_call",
                "half close call",
            )?;
            if let Some(d) = dest {
                let status = call_site
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "hew_duplex_close_half returned void — expected i32 status".into(),
                        )
                    })?
                    .into_int_value();
                emit_close_result_from_rc(fn_ctx, status, d, "hew_duplex_close_half")?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // ── M2 lambda-actor send/ask/release/new ──────────────────────────
        //
        // hew_lambda_actor_send(actor: *mut HewLambdaActorHandle,
        //                       msg: *const u8, len: usize) -> i32
        // (`hew-runtime/src/lambda_actor.rs:918`). Same ABI shape as
        // hew_duplex_send: load the actor handle pointer from
        // `Place::LambdaActorHandle(N)`'s alloca and materialise the
        // message as `(ptr, len)` via `lambda_msg_ptr_len` — scalar
        // messages spill into the 8-byte zero-padded single-vertebra
        // slot; struct-typed messages (packed-args multi-arg record,
        // tuple, record) pass `(alloca, sizeof(struct))` directly. The
        // i32 status is discarded in statement context and materialized
        // into `Result<(), SendError>` in value context (see the `dest`
        // branch below).
        F::LambdaActorSend | F::LambdaActorWeakSend => {
            if args.len() != 3 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 3 args \
                     (actor_handle, msg, len), got {}",
                    args.len()
                )));
            }
            let handle = load_duplex_handle(fn_ctx, args[0], "hew_lambda_actor_send arg0")?;
            let (msg_ptr, len) = lambda_msg_ptr_len(fn_ctx, args[1], args[2], "lambda_send_msg")?;
            let llvm_args: [BasicMetadataValueEnum; 3] =
                [handle.into(), msg_ptr.into(), len.into()];
            let call_site = fn_ctx.call_runtime(
                symbol,
                &llvm_args,
                "hew_lambda_actor_send_call",
                "hew_lambda_actor_send call",
            )?;
            // Statement context (dest=None) discards the rc — fire-and-forget
            // delivery. Value context materializes `Result<(), SendError>` from
            // the i32 status through the single D1 mapping authority (shared
            // with `hew_duplex_send`: identical runtime status space).
            if let Some(d) = dest {
                let status = call_site
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "hew_lambda_actor_send returned void — expected i32 status".into(),
                        )
                    })?
                    .into_int_value();
                emit_send_result_from_rc(fn_ctx, status, d, "hew_lambda_actor_send")?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_lambda_actor_ask(actor: *mut HewLambdaActorHandle,
        //                      msg: *const u8, len: usize,
        //                      reply_out: *mut *mut u8,
        //                      reply_len_out: *mut usize) -> i32
        // (`hew-runtime/src/lambda_actor.rs:1000`). args[3] / args[4]
        // are MIR-side out-param slots (a Pointer local and an I64
        // local respectively); pass their alloca addresses through to
        // the runtime. Return is a SendError discriminant; discarded
        // in this MVP (reply-decode and reply-buffer-free are a
        // follow-on slice — `hew_reply_payload_free` must run on
        // *reply_out when non-null).
        F::LambdaActorAsk => {
            if args.len() != 6 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_lambda_actor_ask): expected 6 args \
                     (actor_handle, msg, len, reply_out, reply_len_out, askerror_local), got {}",
                    args.len()
                )));
            }
            let handle = load_duplex_handle(fn_ctx, args[0], "hew_lambda_actor_ask arg0")?;
            let (msg_ptr, len) = lambda_msg_ptr_len(fn_ctx, args[1], args[2], "lambda_ask_msg")?;
            let (reply_out_ptr, _) = place_pointer(fn_ctx, args[3])?;
            let (reply_len_out_ptr, _) = place_pointer(fn_ctx, args[4])?;
            // args[5]: codegen-only `AskError` Place::Local — the Err-path
            // materialises `Result::Err(AskError::<variant>)` through this
            // local via `emit_result_err`. NOT passed to the runtime call.
            let error_dest = args[5];
            let llvm_args: [BasicMetadataValueEnum; 5] = [
                handle.into(),
                msg_ptr.into(),
                len.into(),
                reply_out_ptr.into(),
                reply_len_out_ptr.into(),
            ];
            // Capture the call's i32 SendError status — Ok=0, anything
            // else is a failure (Closed=1, Full=2, ActorStopped=3,
            // DoubleClose=4, OrphanedAsk=5). Pre-fix codegen ignored
            // this and unconditionally loaded `*reply_out` (which the
            // runtime zeros on every failure path) → null-deref UB on
            // stopped / orphaned / send-failed paths. Branch on the
            // status: Ok → reply-decode; Err → materialise
            // `Result::Err(AskError::<variant>)` without touching the
            // null reply pointer. (Security review fix 2026-06-08.)
            let call_site = fn_ctx.call_runtime(
                symbol,
                &llvm_args,
                "hew_lambda_actor_ask_call",
                "hew_lambda_actor_ask call",
            )?;
            if let Some(d) = dest {
                let status_val = call_site
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "hew_lambda_actor_ask returned void — expected i32 status".into(),
                        )
                    })?
                    .into_int_value();
                let zero_i32 = i32_ty.const_zero();
                let is_ok = fn_ctx
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        status_val,
                        zero_i32,
                        "lambda_ask_status_is_ok",
                    )
                    .llvm_ctx("hew_lambda_actor_ask status icmp")?;
                let current_fn = fn_ctx
                    .builder
                    .get_insert_block()
                    .ok_or_else(|| CodegenError::FailClosed("no insert block".into()))?
                    .get_parent()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("no parent fn at lambda ask split".into())
                    })?;
                let ok_bb = fn_ctx.ctx.append_basic_block(current_fn, "lambda_ask_ok");
                let payload_ok_bb = fn_ctx
                    .ctx
                    .append_basic_block(current_fn, "lambda_ask_payload_ok");
                let payload_invalid_bb = fn_ctx
                    .ctx
                    .append_basic_block(current_fn, "lambda_ask_payload_invalid");
                let err_bb = fn_ctx.ctx.append_basic_block(current_fn, "lambda_ask_err");
                let merge_bb = fn_ctx
                    .ctx
                    .append_basic_block(current_fn, "lambda_ask_merge");
                fn_ctx
                    .builder
                    .build_conditional_branch(is_ok, ok_bb, err_bb)
                    .llvm_ctx("hew_lambda_actor_ask status cond br")?;

                // ── Ok branch: load reply out-params, then GUARD on
                //              the exact ABI byte width of Result::Ok<R>
                //              before decoding. Pre-guard, the codegen blindly
                //              loaded an i64 from `*reply_out` whenever
                //              status was Ok — but the runtime can return
                //              status=Ok with reply_ptr=null on reply-buffer
                //              allocation failure (`hew_reply` OOM path,
                //              `reply_channel.rs:426-440`), and a future
                //              short-reply body would over-read past the
                //              published buffer. Branch on the validity
                //              predicate: valid → decode; invalid →
                //              materialise `AskError::PayloadSizeMismatch`
                //              and bail without dereferencing reply_ptr.
                //              (Security review fix 2026-06-08, final.) ──
                fn_ctx.builder.position_at_end(ok_bb);
                let reply_ptr_val = fn_ctx
                    .builder
                    .build_load(ptr_ty, reply_out_ptr, "lambda_ask_reply_ptr_load")
                    .llvm_ctx("hew_lambda_actor_ask reply ptr load")?
                    .into_pointer_value();
                let reply_len_val = fn_ctx
                    .builder
                    .build_load(i64_ty, reply_len_out_ptr, "lambda_ask_reply_len_load")
                    .llvm_ctx("hew_lambda_actor_ask reply len load")?
                    .into_int_value();
                let dest_local = composite_dest_local(d, "hew_lambda_actor_ask")?;
                let result_layout = crate::layout::machine_layout_for_local(fn_ctx, dest_local)?;
                let ok_fields = result_layout.variant_field_tys.first().ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "hew_lambda_actor_ask dest local {dest_local} has no Ok variant in \
                         its Result layout"
                    ))
                })?;
                if ok_fields.len() > 1 {
                    return Err(CodegenError::FailClosed(format!(
                        "hew_lambda_actor_ask Result::Ok variant for dest local {dest_local} \
                         has {} fields; expected unit or single reply payload",
                        ok_fields.len()
                    )));
                }
                let ok_payload = if ok_fields.is_empty() {
                    None
                } else {
                    let (ok_field_ptr, ok_field_ty) = place_pointer(
                        fn_ctx,
                        Place::MachineVariant {
                            local: dest_local,
                            variant_idx: 0,
                            field_idx: 0,
                        },
                    )?;
                    let (reply_width, _reply_align) =
                        abi_size_align(ok_field_ty, Some(fn_ctx.target_data))?;
                    Some((ok_field_ptr, ok_field_ty, reply_width))
                };
                let null_ptr = ptr_ty.const_null();
                let expected_reply_width = ok_payload.map_or(0, |(_, _, width)| width);
                let expected_len = i64_ty.const_int(expected_reply_width, false);
                let is_len_ok = fn_ctx
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        reply_len_val,
                        expected_len,
                        "lambda_ask_reply_len_ok",
                    )
                    .llvm_ctx("hew_lambda_actor_ask reply_len EQ check")?;
                let is_payload_valid = if expected_reply_width == 0 {
                    is_len_ok
                } else {
                    let is_ptr_nonnull = fn_ctx
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::NE,
                            reply_ptr_val,
                            null_ptr,
                            "lambda_ask_reply_ptr_nonnull",
                        )
                        .llvm_ctx("hew_lambda_actor_ask reply_ptr null check")?;
                    fn_ctx
                        .builder
                        .build_and(is_ptr_nonnull, is_len_ok, "lambda_ask_payload_valid")
                        .llvm_ctx("hew_lambda_actor_ask payload validity AND")?
                };
                fn_ctx
                    .builder
                    .build_conditional_branch(is_payload_valid, payload_ok_bb, payload_invalid_bb)
                    .llvm_ctx("hew_lambda_actor_ask payload-valid cond br")?;

                // ── payload_ok branch: copy the exact reply bytes into
                //              Result::Ok, free the libc-allocated reply
                //              buffer. ──
                fn_ctx.builder.position_at_end(payload_ok_bb);
                // Materialise the reply into the user's `Result<T, AskError>`
                // dest via the same tagged-union helpers ordinary `Result`
                // producers use (`emit_result_ok` / `emit_node_lookup_call`
                // precedent): store tag=0 (Ok) into the dest's
                // MachineTag slot, then store the loaded reply value into
                // MachineVariant { variant_idx: 0, field_idx: 0 }. Storing
                // the raw i64 into the outer struct's start would clobber
                // the discriminant byte and yield a Result whose tag is
                // the low byte of the reply value — silent miscompile.
                store_composite_tag(fn_ctx, dest_local, 0, "hew_lambda_actor_ask")?;
                if let Some((ok_field_ptr, _ok_field_ty, reply_width)) = ok_payload {
                    fn_ctx
                        .builder
                        .build_memcpy(
                            ok_field_ptr,
                            1,
                            reply_ptr_val,
                            1,
                            i64_ty.const_int(reply_width, false),
                        )
                        .llvm_ctx("hew_lambda_actor_ask Result::Ok payload memcpy")?;
                }
                // Free the libc-allocated reply buffer the runtime
                // published (`hew_reply` copies the body's Box-allocated
                // payload into a libc-allocated buffer before publish so
                // the waiter — us — can free with libc::free). For
                // zero-length replies the runtime returns a null pointer
                // and `hew_reply_payload_free` no-ops (`libc::free(null)`).
                let free_fn = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_reply_payload_free",
                )?;
                fn_ctx
                    .builder
                    .build_call(
                        free_fn,
                        &[reply_ptr_val.into(), reply_len_val.into()],
                        "hew_reply_payload_free_call",
                    )
                    .llvm_ctx("hew_reply_payload_free call")?;
                fn_ctx
                    .builder
                    .build_unconditional_branch(merge_bb)
                    .llvm_ctx("hew_lambda_actor_ask payload-ok branch to merge")?;

                // ── payload_invalid branch: runtime returned status=Ok
                //              but the reply payload is null, short, or wide.
                //              Free the libc payload unconditionally
                //              (no-op on null, valid for any length, so
                //              even a short non-null buffer is reclaimed
                //              not leaked), then materialise
                //              `Result::Err(AskError::PayloadSizeMismatch)`.
                //              Distinct from the Err branch below (which
                //              handles non-Ok status); this branch handles
                //              status=Ok / payload-malformed (today this
                //              only fires on `hew_reply` OOM where the
                //              copy-into-libc-buffer step failed and the
                //              runtime fell through to publish null with
                //              len=0 — see runtime
                //              `reply_channel.rs:426-440`). ──
                fn_ctx.builder.position_at_end(payload_invalid_bb);
                fn_ctx
                    .builder
                    .build_call(
                        free_fn,
                        &[reply_ptr_val.into(), reply_len_val.into()],
                        "hew_reply_payload_free_invalid_call",
                    )
                    .llvm_ctx("hew_reply_payload_free invalid-path call")?;
                // AskError::PayloadSizeMismatch = 7 (see Err-branch
                // discriminant catalog comment below).
                const ASKERR_PAYLOAD_SIZE_MISMATCH: u64 = 7;
                let askerr_payload_size_mismatch =
                    i32_ty.const_int(ASKERR_PAYLOAD_SIZE_MISMATCH, false);
                let error_local_invalid =
                    composite_dest_local(error_dest, "hew_lambda_actor_ask payload-invalid")?;
                let (error_tag_ptr_invalid, error_tag_ty_invalid) =
                    place_pointer(fn_ctx, Place::MachineTag(error_local_invalid))?;
                let error_tag_int_ty_invalid = match error_tag_ty_invalid {
                    BasicTypeEnum::IntType(t) => t,
                    other => {
                        return Err(CodegenError::FailClosed(format!(
                            "hew_lambda_actor_ask AskError tag projection \
                             (payload-invalid) must be integer, got {other:?}"
                        )));
                    }
                };
                let askerr_payload_widened = match askerr_payload_size_mismatch
                    .get_type()
                    .get_bit_width()
                    .cmp(&error_tag_int_ty_invalid.get_bit_width())
                {
                    std::cmp::Ordering::Equal => askerr_payload_size_mismatch,
                    std::cmp::Ordering::Less => fn_ctx
                        .builder
                        .build_int_z_extend(
                            askerr_payload_size_mismatch,
                            error_tag_int_ty_invalid,
                            "lambda_ask_err_payload_zext",
                        )
                        .llvm_ctx("lambda ask err payload-invalid zext")?,
                    std::cmp::Ordering::Greater => fn_ctx
                        .builder
                        .build_int_truncate(
                            askerr_payload_size_mismatch,
                            error_tag_int_ty_invalid,
                            "lambda_ask_err_payload_trunc",
                        )
                        .llvm_ctx("lambda ask err payload-invalid trunc")?,
                };
                fn_ctx
                    .builder
                    .build_store(error_tag_ptr_invalid, askerr_payload_widened)
                    .llvm_ctx("store hew_lambda_actor_ask AskError tag (payload-invalid)")?;
                emit_result_err(fn_ctx, d, error_dest)?;
                fn_ctx
                    .builder
                    .build_unconditional_branch(merge_bb)
                    .llvm_ctx("hew_lambda_actor_ask payload-invalid branch to merge")?;

                // ── Err branch: map SendError → AskError, store
                //              Result::Err(AskError::<variant>). The
                //              runtime guarantees `*reply_out` is null
                //              and `*reply_len_out` is 0 on every failure
                //              path, so we do NOT load through reply_ptr
                //              and do NOT call `hew_reply_payload_free`
                //              (libc::free(null) is a no-op but skipping
                //              keeps the Err path minimal). ──
                fn_ctx.builder.position_at_end(err_bb);
                // Status → AskError tag mapping. The AskError enum's
                // discriminants come from `hew-types/src/builtin_enums.rs`:
                //   0=NoError, 1=NodeNotRunning, 2=RoutingFailed,
                //   3=EncodeFailed, 4=SendFailed, 5=Timeout,
                //   6=ConnectionDropped, 7=PayloadSizeMismatch,
                //   8=WorkerAtCapacity, 9=ActorStopped, 10=MailboxFull,
                //   11=OrphanedAsk, 12=NoRunnableWork, 13=DecodeFailure.
                // SendError discriminants (`hew-runtime/src/duplex.rs`):
                //   1=Closed, 2=Full, 3=ActorStopped, 4=DoubleClose,
                //   5=OrphanedAsk.
                // Mapping (load-bearing; matches runtime's typed-error
                // contract):
                //   Closed       (1) → SendFailed (4)  — closed before send
                //   Full         (2) → MailboxFull (10)
                //   ActorStopped (3) → ActorStopped (9)
                //   DoubleClose  (4) → SendFailed (4)  — handle released
                //   OrphanedAsk  (5) → OrphanedAsk (11)
                // Unknown statuses fall through to SendFailed (4) — a
                // catch-all that surfaces a typed error rather than
                // silent UB. A `switch` is the structural fit (LLVM
                // lowers to a jump table or a sequence of icmps depending
                // on density).
                const ASKERR_SEND_FAILED: u64 = 4;
                const ASKERR_ACTOR_STOPPED: u64 = 9;
                const ASKERR_MAILBOX_FULL: u64 = 10;
                const ASKERR_ORPHANED_ASK: u64 = 11;
                let askerr_send_failed = i32_ty.const_int(ASKERR_SEND_FAILED, false);
                let askerr_actor_stopped = i32_ty.const_int(ASKERR_ACTOR_STOPPED, false);
                let askerr_mailbox_full = i32_ty.const_int(ASKERR_MAILBOX_FULL, false);
                let askerr_orphaned_ask = i32_ty.const_int(ASKERR_ORPHANED_ASK, false);
                // Chain of selects: start with the catch-all (SendFailed)
                // and override for each known status. Equivalent to a
                // switch but avoids splitting the err_bb into more blocks.
                let one = i32_ty.const_int(1, false);
                let two = i32_ty.const_int(2, false);
                let three = i32_ty.const_int(3, false);
                let five = i32_ty.const_int(5, false);
                let is_full = fn_ctx
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        status_val,
                        two,
                        "lambda_ask_is_full",
                    )
                    .llvm_ctx("lambda ask is-full cmp")?;
                let is_stopped = fn_ctx
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        status_val,
                        three,
                        "lambda_ask_is_stopped",
                    )
                    .llvm_ctx("lambda ask is-stopped cmp")?;
                let is_orphaned = fn_ctx
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        status_val,
                        five,
                        "lambda_ask_is_orphaned",
                    )
                    .llvm_ctx("lambda ask is-orphaned cmp")?;
                let _ = one; // closed (1) and DoubleClose (4) both → SendFailed catch-all
                let after_full = fn_ctx
                    .builder
                    .build_select(
                        is_full,
                        askerr_mailbox_full,
                        askerr_send_failed,
                        "lambda_ask_err_after_full",
                    )
                    .llvm_ctx("lambda ask err select full")?
                    .into_int_value();
                let after_stopped = fn_ctx
                    .builder
                    .build_select(
                        is_stopped,
                        askerr_actor_stopped,
                        after_full,
                        "lambda_ask_err_after_stopped",
                    )
                    .llvm_ctx("lambda ask err select stopped")?
                    .into_int_value();
                let askerr_tag = fn_ctx
                    .builder
                    .build_select(
                        is_orphaned,
                        askerr_orphaned_ask,
                        after_stopped,
                        "lambda_ask_err_askerr_tag",
                    )
                    .llvm_ctx("lambda ask err select orphaned")?
                    .into_int_value();
                // Store the AskError tag into the error_dest local's
                // MachineTag slot, then bind Result::Err(error_dest)
                // into the user's `dest`. The AskError type is a pure
                // unit-variant enum so its only field is the tag — no
                // payload copy beyond the tag store is needed.
                let error_local = composite_dest_local(error_dest, "hew_lambda_actor_ask Err")?;
                let (error_tag_ptr, error_tag_ty) =
                    place_pointer(fn_ctx, Place::MachineTag(error_local))?;
                let error_tag_int_ty = match error_tag_ty {
                    BasicTypeEnum::IntType(t) => t,
                    other => {
                        return Err(CodegenError::FailClosed(format!(
                            "hew_lambda_actor_ask AskError tag projection must be \
                             integer, got {other:?}"
                        )));
                    }
                };
                let askerr_tag_widened = match askerr_tag
                    .get_type()
                    .get_bit_width()
                    .cmp(&error_tag_int_ty.get_bit_width())
                {
                    std::cmp::Ordering::Equal => askerr_tag,
                    std::cmp::Ordering::Less => fn_ctx
                        .builder
                        .build_int_z_extend(askerr_tag, error_tag_int_ty, "lambda_ask_err_zext")
                        .llvm_ctx("lambda ask err zext")?,
                    std::cmp::Ordering::Greater => fn_ctx
                        .builder
                        .build_int_truncate(askerr_tag, error_tag_int_ty, "lambda_ask_err_trunc")
                        .llvm_ctx("lambda ask err trunc")?,
                };
                fn_ctx
                    .builder
                    .build_store(error_tag_ptr, askerr_tag_widened)
                    .llvm_ctx("store hew_lambda_actor_ask AskError tag")?;
                emit_result_err(fn_ctx, d, error_dest)?;
                fn_ctx
                    .builder
                    .build_unconditional_branch(merge_bb)
                    .llvm_ctx("hew_lambda_actor_ask err branch to merge")?;

                // ── Merge: subsequent instructions resume here. ──
                fn_ctx.builder.position_at_end(merge_bb);
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_lambda_actor_release(actor: *mut HewLambdaActorHandle) -> i32.
        // The CallRuntimeAbi surface for this symbol mirrors the drop-
        // dispatch path in `lower_drop_runtime` (the `Drop` instr is
        // the canonical close site for handle ownership; CallRuntimeAbi
        // is the producer surface for the same release call when a
        // future slice (e.g. explicit `.release()` on a handle) wants
        // it inline). Result discarded.
        F::LambdaActorRelease => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_lambda_actor_release): expected 1 arg \
                     (actor_handle), got {}",
                    args.len()
                )));
            }
            let handle = load_duplex_handle(fn_ctx, args[0], "hew_lambda_actor_release arg0")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[handle.into()],
                "hew_lambda_actor_release_call",
                "hew_lambda_actor_release call",
            )?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_lambda_actor_release returns i32 (discarded); \
                     producer must not supply dest={d:?}"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_lambda_actor_new(mailbox_capacity: usize, shape: i32,
        //                      body_fn: *const HewLambdaActorBody,
        //                      state: *mut c_void,
        //                      state_drop: *const HewLambdaActorStateDrop)
        //   -> *mut HewLambdaActorHandle
        // (`hew-runtime/src/lambda_actor.rs:821`). The body_fn /
        // state_drop pointers are function pointers and cannot be
        // expressed as `Place` args — the spawn-site lowering uses the
        // dedicated `Terminator::MakeLambdaActor` (mirrors
        // `Terminator::MakeGenerator`); this CallRuntimeAbi arm is a
        // fail-closed sentinel so the producer surface and codegen
        // surface stay in lock-step (the M2 allowlist accepts the
        // symbol; codegen rejects it from the wrong producer surface).
        F::LambdaActorNew => {
            return Err(CodegenError::FailClosed(format!(
                "Instr::CallRuntimeAbi(hew_lambda_actor_new): the lambda-actor \
                 construction surface is `Terminator::MakeLambdaActor` (function- \
                 pointer args cannot be expressed as MIR `Place` values); routing \
                 `hew_lambda_actor_new` through CallRuntimeAbi is a producer-side \
                 error. args.len()={}, dest={dest:?}",
                args.len()
            )));
        }
        // ── Bytes value ABI ───────────────────────────────────────────────
        //
        // hew_bytes_push(triple: &mut BytesTriple, byte: u8) -> void.
        // args[0]: Place::Local(N) for a bytes local. Pass the alloca address
        // directly so the runtime can write back ptr/offset/len after CoW/grow.
        // args[1]: Hew API accepts i32 for convenience; truncate to u8 at the
        // runtime ABI boundary.
        F::BytesLen => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_len): expected 1 arg (bytes), got {}",
                    args.len()
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_len arg0 must be a `bytes` receiver; got {:?}",
                    place_resolved_ty(fn_ctx, args[0])?
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_bytes_len returns a length; producer must supply a dest".into(),
                )
            })?;
            let (triple_ptr, _triple_ty) = place_pointer(fn_ctx, args[0])?;
            let len_val = fn_ctx.call_runtime_basic(
                symbol,
                &[triple_ptr.into()],
                "hew_bytes_len_call",
                "hew_bytes_len call",
            )?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, len_val)
                .llvm_ctx("hew_bytes_len store")?;
            let _ = (i32_ty, i64_ty, ptr_ty);
        }
        // hew_bytes_slice(ptr, offset, len, start, end) -> BytesTriple. A `bytes`
        // receiver is a stack-resident triple; unpack it field-by-field, then land
        // the classified aggregate return (RegisterPair on SysV/AAPCS, sret on
        // MSVC/wasm32) into the dest slot via `store_classified_bytes_return`.
        F::BytesSlice => {
            if args.len() != 3 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_slice): expected 3 args \
                     (bytes, start, end), got {}",
                    args.len()
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_slice arg0 must be a `bytes` receiver; got {:?}",
                    place_resolved_ty(fn_ctx, args[0])?
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_bytes_slice returns bytes; producer must supply a dest".into(),
                )
            })?;
            let (triple_ptr, triple_ty) = place_pointer(fn_ctx, args[0])?;
            let BasicTypeEnum::StructType(triple_struct_ty) = triple_ty else {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_slice: bytes receiver alloca has non-struct type {triple_ty:?}; \
                     expected the `{{ptr, i32, i32}}` BytesTriple layout"
                )));
            };
            let data_ptr_gep = fn_ctx
                .builder
                .build_struct_gep(triple_struct_ty, triple_ptr, 0, "bytes_slice_ptr_gep")
                .llvm_ctx("hew_bytes_slice ptr GEP")?;
            let data_ptr = fn_ctx
                .builder
                .build_load(ptr_ty, data_ptr_gep, "bytes_slice_ptr")
                .llvm_ctx("hew_bytes_slice ptr load")?
                .into_pointer_value();
            let offset_gep = fn_ctx
                .builder
                .build_struct_gep(triple_struct_ty, triple_ptr, 1, "bytes_slice_offset_gep")
                .llvm_ctx("hew_bytes_slice offset GEP")?;
            let offset_val = fn_ctx
                .builder
                .build_load(i32_ty, offset_gep, "bytes_slice_offset")
                .llvm_ctx("hew_bytes_slice offset load")?
                .into_int_value();
            let len_gep = fn_ctx
                .builder
                .build_struct_gep(triple_struct_ty, triple_ptr, 2, "bytes_slice_len_gep")
                .llvm_ctx("hew_bytes_slice len GEP")?;
            let len_val = fn_ctx
                .builder
                .build_load(i32_ty, len_gep, "bytes_slice_len")
                .llvm_ctx("hew_bytes_slice len load")?
                .into_int_value();
            let start_val = load_int_arg(fn_ctx, args[1], i64_ty, "hew_bytes_slice start")?;
            let end_val = load_int_arg(fn_ctx, args[2], i64_ty, "hew_bytes_slice end")?;
            // Obtain dest_ptr before the call so we can pass it as the out-pointer.
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            if !matches!(dest_ty, BasicTypeEnum::StructType(_)) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_slice dest must be a bytes struct slot, got {dest_ty:?}"
                )));
            }
            // Canonical `hew_bytes_slice(ptr, offset, len, start, end) ->
            // BytesTriple`, classified per target by the R5 ABI classifier:
            //   - SysV/AAPCS: the 16-byte BytesTriple returns in a register pair
            //     ([2 x i64]); store the 16-byte aggregate into the dest
            //     {ptr,i32,i32} slot (identical byte layout).
            //   - Windows x64 MSVC: returns indirectly; pass dest as the sret
            //     pointer and the runtime writes the triple in place.
            // Replaces the `_raw` out-pointer twin that faked MSVC's sret ABI.
            let bytes_triple_ty = bytes_triple_llvm_ty(fn_ctx);
            let triple = fn_ctx.llvm_mod.get_triple();
            let triple_str = triple.as_str().to_string_lossy();
            let (fv, return_abi) = crate::abi_class::declare_aggregate_return(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                fn_ctx.target_data,
                &triple_str,
                "hew_bytes_slice",
                bytes_triple_ty,
                &[
                    ptr_ty.into(),
                    i32_ty.into(),
                    i32_ty.into(),
                    i64_ty.into(),
                    i64_ty.into(),
                ],
            )?;
            let non_sret_args = [
                data_ptr.into(),
                offset_val.into(),
                len_val.into(),
                start_val.into(),
                end_val.into(),
            ];
            store_classified_bytes_return(
                fn_ctx,
                fv,
                return_abi,
                &non_sret_args,
                dest_ptr,
                "hew_bytes_slice",
            )?;
            let _ = (i8_ty, i64_ty);
        }
        F::BytesPush => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_push): expected 2 args \
                     (bytes, byte), got {}",
                    args.len()
                )));
            }
            let (bytes_slot, _bytes_ty) = place_pointer(fn_ctx, args[0])?;
            // The byte param is now typed u8 (i8 in LLVM); load directly as i8.
            let byte_i8 = load_int_arg(fn_ctx, args[1], i8_ty, "hew_bytes_push byte")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[bytes_slot.into(), byte_i8.into()],
                "hew_bytes_push_call",
                "hew_bytes_push call",
            )?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_push returns void; producer must not supply dest={d:?}"
                )));
            }
            let _ = (i64_ty, ptr_ty);
        }
        // hew_bytes_index(ptr: *mut u8, offset: u32, len: u32, index: i64) -> u8
        // (`hew-runtime/src/bytes.rs:773`). The dedicated `bytes` element getter
        // for both the `b[i]` indexing sugar (MIR `lower_bytes_index`) and the
        // `bytes.get(i)` method (`impl bytes` in `std/io.hew`).
        //
        // A `bytes` value is a stack-resident `BytesTriple { ptr, offset, len }`,
        // NOT a `*mut HewVec`, so it must NOT route through the Vec getter (which
        // loads a heap-Vec `ptr` from arg0 via `load_duplex_handle`). The triple
        // is passed BY VALUE field-by-field: GEP the triple alloca for `ptr`
        // (field 0), `offset` (field 1, i32), and `len` (field 2, i32), then
        // hand them to the runtime alongside the i64 index. The runtime
        // bounds-checks and aborts on OOB (boundary-fail-closed); no compiler-
        // emitted trap CFG. The dest int type is widened from the runtime's i8
        // return (i8 for `b[i]`'s `u8` dest; i32 for `bytes.get`'s `i32` dest).
        F::BytesIndex => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_index): expected 2 args \
                     (bytes, index), got {}",
                    args.len()
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_index arg0 must be a `bytes` receiver; got {:?}",
                    place_resolved_ty(fn_ctx, args[0])?
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_bytes_index returns a byte; producer must supply a dest".into(),
                )
            })?;
            // Unpack the BytesTriple alloca: {ptr, i32 offset, i32 len}.
            let (triple_ptr, triple_ty) = place_pointer(fn_ctx, args[0])?;
            let BasicTypeEnum::StructType(triple_struct_ty) = triple_ty else {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_index: bytes receiver alloca has non-struct type {triple_ty:?}; \
                     expected the `{{ptr, i32, i32}}` BytesTriple layout"
                )));
            };
            let data_ptr_gep = fn_ctx
                .builder
                .build_struct_gep(triple_struct_ty, triple_ptr, 0, "bytes_index_ptr_gep")
                .llvm_ctx("hew_bytes_index ptr GEP")?;
            let data_ptr = fn_ctx
                .builder
                .build_load(ptr_ty, data_ptr_gep, "bytes_index_ptr")
                .llvm_ctx("hew_bytes_index ptr load")?
                .into_pointer_value();
            let offset_gep = fn_ctx
                .builder
                .build_struct_gep(triple_struct_ty, triple_ptr, 1, "bytes_index_offset_gep")
                .llvm_ctx("hew_bytes_index offset GEP")?;
            let offset_val = fn_ctx
                .builder
                .build_load(i32_ty, offset_gep, "bytes_index_offset")
                .llvm_ctx("hew_bytes_index offset load")?
                .into_int_value();
            let len_gep = fn_ctx
                .builder
                .build_struct_gep(triple_struct_ty, triple_ptr, 2, "bytes_index_len_gep")
                .llvm_ctx("hew_bytes_index len GEP")?;
            let len_val = fn_ctx
                .builder
                .build_load(i32_ty, len_gep, "bytes_index_len")
                .llvm_ctx("hew_bytes_index len load")?
                .into_int_value();
            let index_val = load_int_arg(fn_ctx, args[1], i64_ty, "hew_bytes_index index")?;
            let byte_val = fn_ctx.call_runtime_int(
                symbol,
                &[
                    data_ptr.into(),
                    offset_val.into(),
                    len_val.into(),
                    index_val.into(),
                ],
                "hew_bytes_index_call",
                "hew_bytes_index call",
            )?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            let BasicTypeEnum::IntType(dest_int_ty) = dest_ty else {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_index dest must be integer-shaped (u8 / i32); got {dest_ty:?}"
                )));
            };
            // Zero-extend the u8 byte to the dest width (no-op when dest is i8).
            let store_val = fn_ctx
                .builder
                .build_int_z_extend_or_bit_cast(byte_val, dest_int_ty, "bytes_index_zext")
                .llvm_ctx("hew_bytes_index zext")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, store_val)
                .llvm_ctx("hew_bytes_index store")?;
            let _ = (i8_ty, i64_ty, ptr_ty);
        }
        // hew_bytes_pop(&mut BytesTriple) -> i64. The receiver is a stack-
        // resident triple; pass its alloca ADDRESS so the runtime writes back
        // the shrunken triple after CoW. Returns the popped byte; the runtime
        // aborts on an empty buffer (the spec pop signature has no Option).
        F::BytesPop => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_pop): expected 1 arg (bytes), got {}",
                    args.len()
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_pop arg0 must be a `bytes` receiver; got {:?}",
                    place_resolved_ty(fn_ctx, args[0])?
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_bytes_pop returns a byte; producer must supply a dest".into(),
                )
            })?;
            let (triple_ptr, _triple_ty) = place_pointer(fn_ctx, args[0])?;
            let byte_val = fn_ctx.call_runtime_int(
                symbol,
                &[triple_ptr.into()],
                "hew_bytes_pop_call",
                "hew_bytes_pop call",
            )?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            let BasicTypeEnum::IntType(dest_int_ty) = dest_ty else {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_pop dest must be integer-shaped; got {dest_ty:?}"
                )));
            };
            // The runtime returns i64; truncate to the dest width (no-op for
            // an i64 dest, narrows to i8 for a u8 dest).
            let store_val = fn_ctx
                .builder
                .build_int_truncate_or_bit_cast(byte_val, dest_int_ty, "bytes_pop_trunc")
                .llvm_ctx("hew_bytes_pop trunc")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, store_val)
                .llvm_ctx("hew_bytes_pop store")?;
            let _ = (i8_ty, i32_ty, ptr_ty);
        }
        // hew_bytes_set(&mut BytesTriple, index: i64, byte: u8). Pass the
        // receiver alloca address (write-back after CoW), the i64 index, and the
        // byte truncated to u8. Void return; the runtime aborts on OOB.
        F::BytesSet => {
            if args.len() != 3 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_set): expected 3 args \
                     (bytes, index, byte), got {}",
                    args.len()
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_set arg0 must be a `bytes` receiver; got {:?}",
                    place_resolved_ty(fn_ctx, args[0])?
                )));
            }
            let (triple_ptr, _triple_ty) = place_pointer(fn_ctx, args[0])?;
            let index_val = load_int_arg(fn_ctx, args[1], i64_ty, "hew_bytes_set index")?;
            let byte_i8 = load_int_arg(fn_ctx, args[2], i8_ty, "hew_bytes_set byte")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[triple_ptr.into(), index_val.into(), byte_i8.into()],
                "hew_bytes_set_call",
                "hew_bytes_set call",
            )?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_set returns void; producer must not supply dest={d:?}"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_bytes_is_empty(*const BytesTriple) -> bool. Pass the triple alloca
        // address (the by-pointer bytes-consumer convention); the runtime reads
        // the len field.
        F::BytesIsEmpty => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_is_empty): expected 1 arg (bytes), got {}",
                    args.len()
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_is_empty arg0 must be a `bytes` receiver; got {:?}",
                    place_resolved_ty(fn_ctx, args[0])?
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_bytes_is_empty returns bool; producer must supply a dest".into(),
                )
            })?;
            let (triple_ptr, _triple_ty) = place_pointer(fn_ctx, args[0])?;
            let bool_val = fn_ctx.call_runtime_int(
                symbol,
                &[triple_ptr.into()],
                "hew_bytes_is_empty_call",
                "hew_bytes_is_empty call",
            )?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            // The runtime returns Rust `bool` (i1 at the C ABI); widen into the
            // i8 bool dest.
            let store_val = zext_bool_i1_to_dest(fn_ctx, bool_val, dest_ty, "hew_bytes_is_empty")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, store_val)
                .llvm_ctx("hew_bytes_is_empty store")?;
            let _ = (i8_ty, i32_ty, i64_ty, ptr_ty);
        }
        // hew_bytes_contains(*const BytesTriple, byte: u8) -> bool. Pass the
        // triple alloca address and the byte truncated to u8.
        F::BytesContains => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_contains): expected 2 args \
                     (bytes, byte), got {}",
                    args.len()
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_contains arg0 must be a `bytes` receiver; got {:?}",
                    place_resolved_ty(fn_ctx, args[0])?
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_bytes_contains returns bool; producer must supply a dest".into(),
                )
            })?;
            let (triple_ptr, _triple_ty) = place_pointer(fn_ctx, args[0])?;
            let byte_i8 = load_int_arg(fn_ctx, args[1], i8_ty, "hew_bytes_contains byte")?;
            let bool_val = fn_ctx.call_runtime_int(
                symbol,
                &[triple_ptr.into(), byte_i8.into()],
                "hew_bytes_contains_call",
                "hew_bytes_contains call",
            )?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            // The runtime returns Rust `bool` (i1 at the C ABI); widen into the
            // i8 bool dest.
            let store_val = zext_bool_i1_to_dest(fn_ctx, bool_val, dest_ty, "hew_bytes_contains")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, store_val)
                .llvm_ctx("hew_bytes_contains store")?;
            let _ = (i32_ty, i64_ty, ptr_ty);
        }
        // hew_bytes_clear(&mut BytesTriple). Pass the receiver alloca address so
        // the runtime releases the buffer ref and writes back the empty triple.
        F::BytesClear => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_clear): expected 1 arg (bytes), got {}",
                    args.len()
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_clear arg0 must be a `bytes` receiver; got {:?}",
                    place_resolved_ty(fn_ctx, args[0])?
                )));
            }
            let (triple_ptr, _triple_ty) = place_pointer(fn_ctx, args[0])?;
            fn_ctx.call_runtime_void(
                symbol,
                &[triple_ptr.into()],
                "hew_bytes_clear_call",
                "hew_bytes_clear call",
            )?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_clear returns void; producer must not supply dest={d:?}"
                )));
            }
            let _ = (i8_ty, i32_ty, i64_ty, ptr_ty);
        }
        // hew_bytes_append(&mut dst, src_ptr, src_offset, src_len). Pass the
        // destination alloca address (write-back after CoW/grow) plus the source
        // triple unpacked field-by-field (the source is borrowed, never freed).
        F::BytesAppend => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_bytes_append): expected 2 args \
                     (bytes, other), got {}",
                    args.len()
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_append arg0 must be a `bytes` receiver; got {:?}",
                    place_resolved_ty(fn_ctx, args[0])?
                )));
            }
            if !matches!(place_resolved_ty(fn_ctx, args[1])?, ResolvedTy::Bytes) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_append arg1 must be a `bytes` source; got {:?}",
                    place_resolved_ty(fn_ctx, args[1])?
                )));
            }
            // arg0: destination receiver — pass the alloca address for write-back.
            let (dst_ptr, _dst_ty) = place_pointer(fn_ctx, args[0])?;
            // arg1: source triple — unpack {ptr, offset, len} field-by-field.
            let (src_triple_ptr, src_triple_ty) = place_pointer(fn_ctx, args[1])?;
            let BasicTypeEnum::StructType(src_struct_ty) = src_triple_ty else {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_append: source alloca has non-struct type {src_triple_ty:?}; \
                     expected the `{{ptr, i32, i32}}` BytesTriple layout"
                )));
            };
            let src_data_gep = fn_ctx
                .builder
                .build_struct_gep(src_struct_ty, src_triple_ptr, 0, "bytes_append_src_ptr_gep")
                .llvm_ctx("hew_bytes_append src ptr GEP")?;
            let src_data_ptr = fn_ctx
                .builder
                .build_load(ptr_ty, src_data_gep, "bytes_append_src_ptr")
                .llvm_ctx("hew_bytes_append src ptr load")?
                .into_pointer_value();
            let src_offset_gep = fn_ctx
                .builder
                .build_struct_gep(
                    src_struct_ty,
                    src_triple_ptr,
                    1,
                    "bytes_append_src_offset_gep",
                )
                .llvm_ctx("hew_bytes_append src offset GEP")?;
            let src_offset_val = fn_ctx
                .builder
                .build_load(i32_ty, src_offset_gep, "bytes_append_src_offset")
                .llvm_ctx("hew_bytes_append src offset load")?
                .into_int_value();
            let src_len_gep = fn_ctx
                .builder
                .build_struct_gep(src_struct_ty, src_triple_ptr, 2, "bytes_append_src_len_gep")
                .llvm_ctx("hew_bytes_append src len GEP")?;
            let src_len_val = fn_ctx
                .builder
                .build_load(i32_ty, src_len_gep, "bytes_append_src_len")
                .llvm_ctx("hew_bytes_append src len load")?
                .into_int_value();
            fn_ctx.call_runtime_void(
                symbol,
                &[
                    dst_ptr.into(),
                    src_data_ptr.into(),
                    src_offset_val.into(),
                    src_len_val.into(),
                ],
                "hew_bytes_append_call",
                "hew_bytes_append call",
            )?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_bytes_append returns void; producer must not supply dest={d:?}"
                )));
            }
            let _ = (i8_ty, i64_ty);
        }
        // Actor link/monitor builtins.
        //
        // hew_actor_link(parent, child) -> void
        //   Statement-position (dest=None): emit the void call, done.
        //   Value-needed   (dest=Some(result_local: Result<(),LinkError>)):
        //     call void → `emit_result_ok(dest, None)` writes tag=0 (Ok, no
        //     payload). hew_actor_link is infallible at the runtime today, so
        //     Ok(()) is the only shape. Err arms are reserved for future partition policies.
        //
        // hew_actor_monitor(watcher, target) -> i64 (ref_id)
        //   Statement-position (dest=None): emit the call, discard i64 return.
        //   Value-needed   (dest=Some(ref_id_local: i64)):
        //     call i64 → store into the i64 dest. A subsequent MIR RecordInit
        //     (emitted by the MIR producer) assembles MonitorRef{ref_id} from
        //     the raw i64 local.  This handler only stores the primitive.
        //
        // boundary-fail-closed (P0): both shapes are BitCopy — they do not
        // reach the heap-owning composite spine. Arity violations are
        // fail-closed with a diagnostic naming the symbol.
        F::ActorLink => {
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
            let llvm_args: [BasicMetadataValueEnum; 2] = [parent.into(), child.into()];
            fn_ctx.call_runtime_void(
                symbol,
                &llvm_args,
                "hew_actor_link_call",
                "hew_actor_link call",
            )?;
            // Value-needed: synthesise Ok(()) into the Result<(),LinkError> dest.
            // hew_actor_link is void/infallible; tag=0, no payload.
            if let Some(d) = dest {
                emit_result_ok(fn_ctx, d, None)?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        F::ActorMonitor => {
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
            let llvm_args: [BasicMetadataValueEnum; 2] = [watcher.into(), target.into()];
            // Call hew_actor_monitor → i64 ref_id. In statement position (dest=None)
            // the return is discarded. In value position (dest=Some(ref_id_local))
            // the i64 is stored directly into the i64-typed dest; the subsequent
            // MIR RecordInit assembles MonitorRef{ref_id} from that local.
            let ref_id = fn_ctx.call_runtime_int(
                symbol,
                &llvm_args,
                "hew_actor_monitor_call",
                "hew_actor_monitor call",
            )?;
            if let Some(d) = dest {
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, d)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, ref_id)
                    .llvm_ctx("hew_actor_monitor store ref_id")?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_actor_demonitor(ref_id: i64) -> void. Cancels a previously-registered
        // monitor. The auto-drop path for a MonitorRef VALUE (scope-exit, the
        // common case) goes through RuntimeDropDescriptor::MonitorRefClose →
        // lower_drop_runtime in llvm.rs (struct-field GEP + load), bypassing this
        // arm. This `CallRuntimeAbi` arm lowers the DIRECT call in the body of the
        // stdlib `impl MonitorRef { fn close(self) { hew_actor_demonitor(self.ref_id) } }`
        // (std/link_monitor.hew): a program that `import std::link_monitor`s lowers
        // that inherent close body, whose `unsafe` block calls the symbol with a
        // plain i64 ref_id and reaches here. (It is absent from the checked-MIR
        // corpus — see the `hew_actor_demonitor` EXPECTED_UNCOVERED pin — because
        // no corpus fixture imports std::link_monitor and lowers close.)
        F::ActorDemonitor => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_actor_demonitor): expected 1 arg \
                     (ref_id: i64), got {}",
                    args.len()
                )));
            }
            // arg0: ref_id (i64). Loaded from the alloca the MIR lowerer
            // allocated for the plain i64 value expression.
            let ref_id = load_int_arg(fn_ctx, args[0], i64_ty, "demonitor_ref_id")?;
            let llvm_args: [BasicMetadataValueEnum; 1] = [ref_id.into()];
            fn_ctx.call_runtime_void(
                symbol,
                &llvm_args,
                "hew_actor_demonitor_call",
                "hew_actor_demonitor call",
            )?;
            // void return; dest is always None (the close body is a void call).
            let _ = (i32_ty, ptr_ty, dest);
        }
        // hew_node_monitor(target_pid: i64) -> i64. Positive returns are ref ids;
        // negative returns encode MonitorError as `-(variant + 1)`. Value
        // position constructs Result<MonitorRef, MonitorError>; statement
        // position discards the setup result.
        F::NodeMonitor => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_node_monitor): expected 1 arg \
                     (target_pid: i64), got {}",
                    args.len()
                )));
            }
            let target_pid = load_int_arg(fn_ctx, args[0], i64_ty, "node_monitor_target")?;
            let llvm_args: [BasicMetadataValueEnum; 1] = [target_pid.into()];
            let setup_result = fn_ctx.call_runtime_int(
                symbol,
                &llvm_args,
                "hew_node_monitor_call",
                "hew_node_monitor call",
            )?;
            if let Some(d) = dest {
                // MonitorError::NodeNotRunning is variant 0 and is the defensive
                // mapping for an impossible zero return.
                emit_signed_setup_result(
                    fn_ctx,
                    setup_result,
                    d,
                    true,
                    0,
                    "hew_node_monitor_result",
                )?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_node_monitor_recv(ref_id: i64, timeout_ms: i64) -> i64.
        // Blocks for the distributed monitor's terminal signal and returns the
        // carried down-reason. Both args are BitCopy i64; the i64 reason is
        // stored into the i64 dest in value position.
        F::NodeMonitorRecv => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_node_monitor_recv): expected 2 args \
                     (ref_id: i64, timeout_ms: i64), got {}",
                    args.len()
                )));
            }
            let ref_id = load_int_arg(fn_ctx, args[0], i64_ty, "node_monitor_recv_ref")?;
            let timeout = load_int_arg(fn_ctx, args[1], i64_ty, "node_monitor_recv_timeout")?;
            let llvm_args: [BasicMetadataValueEnum; 2] = [ref_id.into(), timeout.into()];
            let reason = fn_ctx.call_runtime_int(
                symbol,
                &llvm_args,
                "hew_node_monitor_recv_call",
                "hew_node_monitor_recv call",
            )?;
            if let Some(d) = dest {
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, d)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, reason)
                    .llvm_ctx("hew_node_monitor_recv store reason")?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_node_link_remote(target_pid: i64, policy_tag: i64) -> i64.
        // Positive returns are internal link ref ids; negative returns encode
        // LinkError as `-(variant + 1)`. The immediate Hew-visible result is
        // Result<(), LinkError>; EXIT arrives asynchronously when the remote dies.
        F::LinkRemote => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_node_link_remote): expected 2 args \
                     (target_pid: i64, policy_tag: i64), got {}",
                    args.len()
                )));
            }
            let target_pid = load_int_arg(fn_ctx, args[0], i64_ty, "node_link_target")?;
            let policy_tag = load_int_arg(fn_ctx, args[1], i64_ty, "node_link_policy")?;
            let llvm_args: [BasicMetadataValueEnum; 2] = [target_pid.into(), policy_tag.into()];
            let setup_result = fn_ctx.call_runtime_int(
                symbol,
                &llvm_args,
                "hew_node_link_remote_call",
                "hew_node_link_remote call",
            )?;
            if let Some(d) = dest {
                // LinkError::NodeNotRunning is variant 2 and is the defensive
                // mapping for an impossible zero return.
                emit_signed_setup_result(
                    fn_ctx,
                    setup_result,
                    d,
                    false,
                    2,
                    "hew_node_link_remote_result",
                )?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_actor_self() -> *mut HewActor (`hew-runtime/src/actor.rs`). Reads
        // the live actor from the per-dispatch thread-local — the borrowed
        // handle the current `receive fn` runs on. Emitted for `this` used as a
        // value (`HirExprKind::ActorSelf`): a self-send (`this.go()`) lowers its
        // receiver through here, then `Terminator::Send` loads the handle back
        // from `dest`. The returned `*mut HewActor` is a BORROW (no ownership
        // transfer), so the `LocalPid` dest slot carries no drop obligation.
        F::ActorSelf => {
            if !args.is_empty() {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_actor_self): expected 0 args, got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_actor_self returns the self-handle; producer must supply a dest".into(),
                )
            })?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            // The `LocalPid` handle slot is a `ptr` alloca — the actor handle is
            // stored directly (the same representation `load_duplex_handle`
            // reads back when `Terminator::Send` consumes this Place).
            match dest_ty {
                BasicTypeEnum::PointerType(_) => {}
                other => {
                    return Err(CodegenError::FailClosed(format!(
                        "hew_actor_self: dest {dest_place:?} resolves to non-pointer type \
                         {other:?}; expected `ptr` (the LocalPid handle alloca)"
                    )));
                }
            }
            let self_ptr = fn_ctx.call_runtime_ptr(
                symbol,
                &[],
                "hew_actor_self_call",
                "hew_actor_self call",
            )?;
            fn_ctx
                .builder
                .build_store(dest_ptr, self_ptr)
                .llvm_ctx("hew_actor_self store")?;
            let _ = (i32_ty, ptr_ty);
        }
        // hew_supervisor_stop(sup: *mut HewSupervisor) -> void
        // (`hew-runtime/src/supervisor.rs:1944`). Graceful shutdown: void return.
        // The `supervisor_stop(sup)` builtin passes a single supervisor handle;
        // the producer always supplies `dest: None`.
        F::SupervisorStop => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_supervisor_stop): expected 1 arg \
                     (sup), got {}",
                    args.len()
                )));
            }
            let sup_ptr = load_duplex_handle(fn_ctx, args[0], "supervisor_stop_arg0")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[sup_ptr.into()],
                "hew_supervisor_stop_call",
                "hew_supervisor_stop call",
            )?;
            // Void return — producer supplies dest: None; fail-closed if not.
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_supervisor_stop is void; producer must not supply \
                     dest={d:?}"
                )));
            }
            let _ = (i32_ty, i64_ty);
        }
        // hew_supervisor_restart_await_blocking(sup: *mut HewSupervisor, key: u32)
        // -> void (`hew-runtime/src/supervisor.rs`). The contextless
        // `await_restart` path: blocks until the child slot is Live or
        // permanently Dead. The producer supplies [sup, key] and dest: None.
        F::SupervisorRestartAwaitBlocking => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_supervisor_restart_await_blocking): \
                     expected 2 args (sup, key), got {}",
                    args.len()
                )));
            }
            let sup_ptr = load_duplex_handle(fn_ctx, args[0], "restart_await_blocking_sup")?;
            let key = load_place_as_basic(fn_ctx, args[1], "restart_await_blocking_key")?
                .into_int_value();
            // The MIR producer emits the key as an i64 const; the ABI is u32.
            let key_u32 = fn_ctx
                .builder
                .build_int_truncate(key, i32_ty, "restart_await_blocking_key_u32")
                .llvm_ctx("restart-await-blocking key truncate")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[sup_ptr.into(), key_u32.into()],
                "hew_supervisor_restart_await_blocking_call",
                "hew_supervisor_restart_await_blocking call",
            )?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_supervisor_restart_await_blocking is void; producer must \
                     not supply dest={d:?}"
                )));
            }
            let _ = i64_ty;
        }
        // `hew_duplex_close` is only called from the Drop ritual
        // (`lower_drop`); reaching it via `Instr::CallRuntimeAbi`
        // means a producer mis-routed a destructor through the
        // generic call path. Fail-closed.
        F::DuplexClose => {
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
        //
        F::VecLen => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_vec_len): expected 1 arg (vec), got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_len: producer must supply a dest place for the length".into(),
                )
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            // `bytes` operand (B-2): a `bytes` value is a stack-resident
            // `BytesTriple { ptr, offset, len }`, NOT a `*mut HewVec`, so the
            // `hew_vec_len(*mut HewVec)` C ABI cannot apply. Route a bytes
            // receiver to `hew_bytes_len(*const BytesTriple) -> i64`, passing the
            // triple alloca's ADDRESS (a plain `ptr`) — the uniform by-pointer
            // bytes-param convention. By value the
            // `{ptr,i32,i32}` arg is not reliably ABI-portable (LLVM's 3-register
            // small-struct classification vs Rust's repr(C) two-register pair).
            // Checker-authority: branch on `args[0]`'s `ResolvedTy::Bytes`.
            if matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
                let (triple_ptr, _triple_ty) = place_pointer(fn_ctx, args[0])?;
                let len_val = fn_ctx.call_runtime_basic(
                    "hew_bytes_len",
                    &[triple_ptr.into()],
                    "hew_bytes_len_call",
                    "hew_bytes_len call",
                )?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, len_val)
                    .llvm_ctx("bytes.len store")?;
                let _ = (i32_ty, ptr_ty);
                return Ok(());
            }
            let vec_ptr = load_duplex_handle(fn_ctx, args[0], "hew_vec_len arg0")?;
            let len_val = fn_ctx.call_runtime_basic(
                symbol,
                &[vec_ptr.into()],
                "hew_vec_len_call",
                "hew_vec_len call",
            )?;
            fn_ctx
                .builder
                .build_store(dest_ptr, len_val)
                .llvm_ctx("hew_vec_len store")?;
            let _ = (i32_ty, ptr_ty);
        }
        // hew_vec_get_bool(v: *mut HewVec, index: i64) -> bool (i1 ABI)
        // hew_vec_get_i32(v: *mut HewVec, index: i64) -> i32
        // hew_vec_get_i64(v: *mut HewVec, index: i64) -> i64
        // hew_vec_get_f64(v: *mut HewVec, index: i64) -> f64
        // hew_vec_get_ptr(v: *mut HewVec, index: i64) -> *mut c_void
        // hew_vec_get_str(v: *mut HewVec, index: i64) -> *const c_char
        //
        // All variants share the same ABI shape: load vec ptr from arg0, load
        // index i64 from arg1, call, store result into dest. The result type
        // differs per variant and is encoded in the function return type from
        // `intern_runtime_decl`.
        F::VecGet(
            VecGetElem::Bool
            | VecGetElem::F32
            | VecGetElem::I8
            | VecGetElem::U8
            | VecGetElem::I16
            | VecGetElem::U16
            | VecGetElem::I32
            | VecGetElem::I64
            | VecGetElem::F64
            | VecGetElem::Ptr
            | VecGetElem::Str,
        ) => {
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
                .llvm_ctx_with(|| format!("{symbol} call"))?;
            let result_val = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed(format!("{symbol} returned void")))?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(format!("{symbol}: producer must supply a dest place"))
            })?;
            // Closure-pair element (`fns[i]` on a `Vec<fn(...)>`): the
            // returned pointer is the slot's pair box; copy the 16-byte
            // pair out (a borrow — the vec keeps the box and the env).
            if symbol == "hew_vec_get_ptr"
                && matches!(
                    place_resolved_ty(fn_ctx, dest_place)?,
                    ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }
                )
            {
                emit_unbox_closure_pair(fn_ctx, result_val.into_pointer_value(), dest_place)?;
                let _ = (i32_ty, ptr_ty);
                return Ok(());
            }
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            let store_val = if symbol == "hew_vec_get_bool" {
                zext_bool_i1_to_dest(fn_ctx, result_val.into_int_value(), dest_ty, symbol)?
            } else {
                result_val
            };
            fn_ctx
                .builder
                .build_store(dest_ptr, store_val)
                .llvm_ctx_with(|| format!("{symbol} store"))?;
            let _ = (i32_ty, ptr_ty);
        }
        // ── Vec<T> layout-descriptor element getter (subscript / for-in) ──
        //
        // hew_vec_get_layout(v: *mut HewVec, index: i64,
        //                    layout: *const HewTypeLayout) -> *const c_void
        //
        // Emitted for `xs[i]` and `for x in xs` where the element type is a
        // BitCopy Named value record or a Tuple — any type whose elements are
        // stored inline at a stride determined by the layout descriptor (not
        // the fixed 8-byte pointer stride used by `hew_vec_get_ptr`).
        //
        // The layout descriptor is synthesised from the dest-place's type
        // (same as the `lower_layout_vec_direct_call` path for `.get()`).
        // The runtime returns a `*const c_void` pointing into the vec buffer;
        // codegen loads the full element value through that pointer into dest.
        F::VecGet(VecGetElem::Layout) => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_vec_get_layout): expected 2 args \
                     (vec, index), got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_get_layout returns an element; producer must supply a dest".into(),
                )
            })?;
            let vec_ptr = load_duplex_handle(fn_ctx, args[0], "hew_vec_get_layout arg0")?;
            let index_val = load_int_arg(fn_ctx, args[1], i64_ty, "hew_vec_get_layout arg1")?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            let layout_ptr = crate::layout::layout_descriptor_ptr(fn_ctx, dest_ty, "get")?;
            let fv = crate::layout::get_or_declare_layout_vec_runtime(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                "hew_vec_get_layout",
            )?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), index_val.into(), layout_ptr.into()],
                    "hew_vec_get_layout_call",
                )
                .llvm_ctx("hew_vec_get_layout call")?;
            let raw_ptr = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_vec_get_layout returned void".into()))?
                .into_pointer_value();
            let loaded = fn_ctx
                .builder
                .build_load(dest_ty, raw_ptr, "hew_vec_get_layout_load")
                .llvm_ctx("hew_vec_get_layout load")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, loaded)
                .llvm_ctx("hew_vec_get_layout store")?;
            let _ = (i32_ty, ptr_ty);
        }
        // hew_vec_get_owned(v: *const HewVec, index: i64) -> *const c_void
        //
        // W5.016: emitted for `xs[i]` / `for x in xs` where the element is an
        // owned (non-Copy) record/enum/tuple. The runtime returns a BORROWED
        // pointer into the live buffer (no clone/drop); codegen loads the full
        // element value through it into dest. The borrowed element stays owned
        // by the Vec — the dest is NOT scheduled for an independent drop (the
        // for-in binding is a borrow, F5 discipline).
        F::VecGet(VecGetElem::Owned) => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_vec_get_owned): expected 2 args \
                     (vec, index), got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_get_owned returns an element; producer must supply a dest".into(),
                )
            })?;
            let vec_ptr = load_duplex_handle(fn_ctx, args[0], "hew_vec_get_owned arg0")?;
            let index_val = load_int_arg(fn_ctx, args[1], i64_ty, "hew_vec_get_owned arg1")?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            let fv =
                get_or_declare_owned_vec_runtime(fn_ctx.ctx, fn_ctx.llvm_mod, "hew_vec_get_owned")?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), index_val.into()],
                    "hew_vec_get_owned_call",
                )
                .llvm_ctx("hew_vec_get_owned call")?;
            let raw_ptr = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_vec_get_owned returned void".into()))?
                .into_pointer_value();
            let loaded = fn_ctx
                .builder
                .build_load(dest_ty, raw_ptr, "hew_vec_get_owned_load")
                .llvm_ctx("hew_vec_get_owned load")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, loaded)
                .llvm_ctx("hew_vec_get_owned store")?;
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
        // Every VecSliceElem variant shares the arm; the per-element
        // symbol is already in `symbol` (the family's c_symbol).
        F::VecSliceRange(_) => {
            if args.len() != 3 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 3 args (vec, start, end), got {}",
                    args.len()
                )));
            }
            let vec_ptr = load_duplex_handle(fn_ctx, args[0], &format!("{symbol} arg0"))?;
            let start_val = load_int_arg(fn_ctx, args[1], i64_ty, &format!("{symbol} arg1"))?;
            let end_val = load_int_arg(fn_ctx, args[2], i64_ty, &format!("{symbol} arg2"))?;
            let mut llvm_args: Vec<BasicMetadataValueEnum> =
                vec![vec_ptr.into(), start_val.into(), end_val.into()];
            if symbol == "hew_vec_slice_range_layout" {
                // DEDUP-TODO: share this hidden layout-pointer synthesis with
                // `llvm.rs::lower_layout_vec_direct_call` so the descriptor
                // slice ABI has one codegen helper.
                let vec_resolved_ty = place_resolved_ty(fn_ctx, args[0])?.clone();
                let elem_hew_ty = match &vec_resolved_ty {
                    ResolvedTy::Named {
                        name,
                        args: vec_args,
                        ..
                    } if name == "Vec" && vec_args.len() == 1 => vec_args[0].clone(),
                    other => {
                        return Err(CodegenError::FailClosed(format!(
                            "hew_vec_slice_range_layout: arg0 must be Vec<T>, got {other:?}"
                        )));
                    }
                };
                let layout_elem_ty = layout_vec_element_needs_descriptor(fn_ctx, &elem_hew_ty)?
                    .ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "hew_vec_slice_range_layout: element type {elem_hew_ty:?} \
                             is not a layout-descriptor-backed record/tuple"
                        ))
                    })?;
                let layout_ptr =
                    crate::layout::layout_descriptor_ptr(fn_ctx, layout_elem_ty, "slice")?;
                llvm_args.push(layout_ptr.into());
            }
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
                    &llvm_args,
                    &format!("{symbol}_call"),
                )
                .llvm_ctx_with(|| format!("{symbol} call"))?;
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
                .llvm_ctx_with(|| format!("{symbol} store"))?;
            let _ = (i32_ty, ptr_ty);
        }
        // String runtime calls — concat plus W3 collections-sugar S2 index/slice.
        //
        // These arms follow the Vec single-element-getter / slice-range
        // shape: load the `*const c_char` value from a `ptr`-typed
        // String alloca (string locals are represented as `ptr` in
        // LLVM — see `basic_type_for(ResolvedTy::String)`), pass any
        // scalar arguments, and store the return into the producer-allocated
        // dest. The runtime entries themselves enforce invalid-input bounds.
        //
        // hew_string_concat(a: *const c_char, b: *const c_char) -> *mut c_char.
        F::StringConcat => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_string_concat): expected 2 args (a, b), got {}",
                    args.len()
                )));
            }
            let lhs_ptr = load_duplex_handle(fn_ctx, args[0], "hew_string_concat arg0")?;
            let rhs_ptr = load_duplex_handle(fn_ctx, args[1], "hew_string_concat arg1")?;
            let result_val = fn_ctx.call_runtime_basic(
                symbol,
                &[lhs_ptr.into(), rhs_ptr.into()],
                "hew_string_concat_call",
                "hew_string_concat call",
            )?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_string_concat: producer must supply a dest place".into(),
                )
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result_val)
                .llvm_ctx("hew_string_concat store")?;
            let _ = i32_ty;
        }
        F::ObserveReadU64 => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_observe_read_u64): expected 1 arg (name), got {}",
                    args.len()
                )));
            }
            let name_ptr = load_duplex_handle(fn_ctx, args[0], "hew_observe_read_u64 arg0")?;
            let result_val = fn_ctx.call_runtime_basic(
                symbol,
                &[name_ptr.into()],
                "hew_observe_read_u64_call",
                "hew_observe_read_u64 call",
            )?;
            if let Some(dest_place) = dest {
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, result_val)
                    .llvm_ctx("hew_observe_read_u64 store")?;
            }
            let _ = i32_ty;
        }
        F::ObserveScrape | F::ObserveSeries => {
            let (call_name, call_ctx, store_ctx) = if call.family() == F::ObserveScrape {
                (
                    "hew_observe_scrape_call",
                    "hew_observe_scrape call",
                    "hew_observe_scrape store",
                )
            } else {
                (
                    "hew_observe_series_call",
                    "hew_observe_series call",
                    "hew_observe_series store",
                )
            };
            if !args.is_empty() {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 0 args, got {}",
                    args.len()
                )));
            }
            let result_val = fn_ctx.call_runtime_basic(symbol, &[], call_name, call_ctx)?;
            if let Some(dest_place) = dest {
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, result_val)
                    .llvm_ctx(store_ctx)?;
            }
            let _ = i32_ty;
        }
        F::ObserveBarrier => {
            if !args.is_empty() {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_observe_barrier): expected 0 args, got {}",
                    args.len()
                )));
            }
            let result_val = fn_ctx.call_runtime_basic(
                symbol,
                &[],
                "hew_observe_barrier_call",
                "hew_observe_barrier call",
            )?;
            if let Some(dest_place) = dest {
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, result_val)
                    .llvm_ctx("hew_observe_barrier store")?;
            }
            let _ = i32_ty;
        }
        // User-metric scalar emit path (#1862). `std::metrics` reaches these
        // through its `extern "C"` block; the bodies live in
        // `hew-runtime/src/metrics.rs`. The register entry points take a
        // `*const c_char` name and return the `i64` slot handle; the mutators
        // take an `i64` handle plus an `i64` delta or an `f64` observation and
        // return void.
        F::MetricCounterRegister | F::MetricGaugeRegister | F::MetricHistogramRegisterSimple => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 1 arg (name), got {}",
                    args.len()
                )));
            }
            let name_ptr = load_duplex_handle(fn_ctx, args[0], "metric register name")?;
            let result_val = fn_ctx.call_runtime_basic(
                symbol,
                &[name_ptr.into()],
                "metric_register_call",
                "metric register call",
            )?;
            if let Some(dest_place) = dest {
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, result_val)
                    .llvm_ctx("metric register store")?;
            }
            let _ = i32_ty;
        }
        F::MetricCounterInc | F::MetricGaugeInc | F::MetricGaugeDec => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 1 arg (handle), got {}",
                    args.len()
                )));
            }
            let handle = load_int_arg(fn_ctx, args[0], i64_ty, "metric mutate handle")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[handle.into()],
                "metric_mutate_call",
                "metric mutate call",
            )?;
            let _ = i32_ty;
        }
        F::MetricCounterAdd | F::MetricGaugeSet | F::MetricGaugeAdd => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 2 args (handle, n), got {}",
                    args.len()
                )));
            }
            let handle = load_int_arg(fn_ctx, args[0], i64_ty, "metric mutate handle")?;
            let n = load_int_arg(fn_ctx, args[1], i64_ty, "metric mutate value")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[handle.into(), n.into()],
                "metric_mutate2_call",
                "metric mutate2 call",
            )?;
            let _ = i32_ty;
        }
        F::MetricHistogramRecord => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 2 args (handle, value), got {}",
                    args.len()
                )));
            }
            let handle = load_int_arg(fn_ctx, args[0], i64_ty, "histogram record handle")?;
            let value = load_math_f64_arg(fn_ctx, args[1], "histogram record value", symbol)?;
            fn_ctx.call_runtime_void(
                symbol,
                &[handle.into(), value.into()],
                "histogram_record_call",
                "histogram record call",
            )?;
            let _ = i32_ty;
        }
        F::StringCharCount => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_string_char_count): expected 1 arg (s), got {}",
                    args.len()
                )));
            }
            let s_ptr = load_duplex_handle(fn_ctx, args[0], "hew_string_char_count arg0")?;
            let result_val = fn_ctx.call_runtime_basic(
                symbol,
                &[s_ptr.into()],
                "hew_string_char_count_call",
                "hew_string_char_count call",
            )?;
            if let Some(dest_place) = dest {
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, result_val)
                    .llvm_ctx("hew_string_char_count store")?;
            }
            let _ = (i64_ty, ptr_ty);
        }
        // `impl duration` receiver methods (`hew-runtime/src/io_time.rs`). Each
        // takes the i64-backed duration (nanoseconds) and returns the converted
        // count: `nanos`/`micros`/`millis`/`secs`/`mins`/`hours`/`abs` produce
        // an `i64` stored straight into the dest; `is_zero` produces the C `i32`
        // boolean, compared `!= 0` to an `i1` and zero-extended into the `i8`
        // bool dest (mirrors predicate stores for i1-to-bool slots).
        F::DurationNanos
        | F::DurationMicros
        | F::DurationMillis
        | F::DurationSecs
        | F::DurationMins
        | F::DurationHours
        | F::DurationAbs
        | F::DurationIsZero => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected 1 arg (duration), got {}",
                    args.len()
                )));
            }
            let duration = load_int_arg(fn_ctx, args[0], i64_ty, symbol)?;
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[duration.into()], &format!("{symbol}_call"))
                .llvm_ctx_with(|| format!("{symbol} call"))?;
            if let Some(dest_place) = dest {
                let result = call.try_as_basic_value().basic().ok_or_else(|| {
                    CodegenError::FailClosed(format!("{symbol} returned void"))
                })?;
                let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
                let store_val = if symbol == "hew_duration_is_zero" {
                    // i32 C boolean -> i1 (`!= 0`) -> i8 bool dest.
                    let i32_result = result.into_int_value();
                    let predicate = fn_ctx
                        .builder
                        .build_int_compare(
                            IntPredicate::NE,
                            i32_result,
                            i32_ty.const_zero(),
                            &format!("{symbol}_pred"),
                        )
                        .llvm_ctx_with(|| format!("{symbol} nonzero compare"))?;
                    zext_bool_i1_to_dest(fn_ctx, predicate, dest_ty, symbol)?
                } else {
                    result
                };
                fn_ctx
                    .builder
                    .build_store(dest_ptr, store_val)
                    .llvm_ctx_with(|| format!("{symbol} store"))?;
            }
            let _ = (i8_ty, ptr_ty);
        }
        // `impl instant` methods (`hew-runtime/src/io_time.rs`). `instant` is
        // i64-backed (a monotonic nanosecond timestamp), so every argument and
        // result is a bare `i64`: `now()` takes no args and returns the clock;
        // `elapsed(now)` and `duration_since(now, earlier)` take 1 / 2 i64
        // timestamps and return an i64-backed `duration`. Load each i64 arg,
        // call the extern, store the i64 result straight into the dest.
        F::InstantNow | F::InstantElapsed | F::InstantDurationSince => {
            let expected = if call.family() == F::InstantNow {
                0
            } else if call.family() == F::InstantElapsed {
                1
            } else {
                2
            };
            if args.len() != expected {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected {expected} i64 arg(s), got {}",
                    args.len()
                )));
            }
            let mut call_args = Vec::with_capacity(args.len());
            for &arg in args {
                call_args.push(load_int_arg(fn_ctx, arg, i64_ty, symbol)?.into());
            }
            let fv = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                symbol,
            )?;
            let call = fn_ctx
                .builder
                .build_call(fv, &call_args, &format!("{symbol}_call"))
                .llvm_ctx_with(|| format!("{symbol} call"))?;
            if let Some(dest_place) = dest {
                let result = call.try_as_basic_value().basic().ok_or_else(|| {
                    CodegenError::FailClosed(format!("{symbol} returned void"))
                })?;
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, result)
                    .llvm_ctx_with(|| format!("{symbol} store"))?;
            }
            let _ = (i8_ty, ptr_ty);
        }
        F::StringIndex => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_string_index): expected 2 args (s, i), got {}",
                    args.len()
                )));
            }
            let s_ptr = load_duplex_handle(fn_ctx, args[0], "hew_string_index arg0")?;
            let i_val = load_int_arg(fn_ctx, args[1], i64_ty, "hew_string_index arg1")?;
            let result_val = fn_ctx.call_runtime_basic(
                symbol,
                &[s_ptr.into(), i_val.into()],
                "hew_string_index_call",
                "hew_string_index call",
            )?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_string_index: producer must supply a dest place".into(),
                )
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result_val)
                .llvm_ctx("hew_string_index store")?;
            let _ = ptr_ty;
        }
        // hew_string_slice_codepoints(s: *const c_char, start: i64, end: i64)
        //   -> *mut c_char (fresh owned slice)
        F::StringSliceCodepoints => {
            if args.len() != 3 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_string_slice_codepoints): expected 3 args \
                     (s, start, end), got {}",
                    args.len()
                )));
            }
            let s_ptr = load_duplex_handle(fn_ctx, args[0], "hew_string_slice_codepoints arg0")?;
            let start_val =
                load_int_arg(fn_ctx, args[1], i64_ty, "hew_string_slice_codepoints arg1")?;
            let end_val =
                load_int_arg(fn_ctx, args[2], i64_ty, "hew_string_slice_codepoints arg2")?;
            let result_val = fn_ctx.call_runtime_basic(
                symbol,
                &[s_ptr.into(), start_val.into(), end_val.into()],
                "hew_string_slice_codepoints_call",
                "hew_string_slice_codepoints call",
            )?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_string_slice_codepoints: producer must supply a dest place".into(),
                )
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result_val)
                .llvm_ctx("hew_string_slice_codepoints store")?;
            let _ = i32_ty;
        }
        // hew_task_new() -> *mut HewTask
        // No args. dest: Place holding the task pointer.
        F::TaskNew => {
            if !args.is_empty() {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_new): expected 0 args, got {}",
                    args.len()
                )));
            }
            let task_ptr = fn_ctx.call_runtime_basic(
                symbol,
                &[],
                "hew_task_new_call",
                "hew_task_new call",
            )?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_task_new: producer must supply a dest place for the task ptr".into(),
                )
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, task_ptr)
                .llvm_ctx("hew_task_new store")?;
            let _ = (i32_ty, ptr_ty);
        }
        F::TaskScopeNew => {
            if !args.is_empty() {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_scope_new): expected 0 args, got {}",
                    args.len()
                )));
            }
            let scope_ptr = fn_ctx.call_runtime_basic(
                symbol,
                &[],
                "hew_task_scope_new_call",
                "hew_task_scope_new call",
            )?;
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed("hew_task_scope_new requires a dest".into())
            })?;
            let (dest_ptr, _) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, scope_ptr)
                .llvm_ctx("hew_task_scope_new store")?;
            let _ = (i32_ty, ptr_ty);
        }
        F::TaskScopeSetCurrent => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_scope_set_current): expected 1 arg, got {}",
                    args.len()
                )));
            }
            let scope_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_scope_set_current arg0")?;
            let call = fn_ctx.call_runtime(
                symbol,
                &[scope_ptr.into()],
                "hew_task_scope_set_current_call",
                "hew_task_scope_set_current call",
            )?;
            if let Some(dest_place) = dest {
                let prev = call.try_as_basic_value().basic().ok_or_else(|| {
                    CodegenError::FailClosed("hew_task_scope_set_current returned void".into())
                })?;
                let (dest_ptr, _) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, prev)
                    .llvm_ctx("hew_task_scope_set_current store")?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        F::TaskScopeDestroy | F::TaskScopeJoinAll => {
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
                .llvm_ctx_with(|| format!("{symbol} call"))?;
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "{symbol} returns void; producer must not supply dest={d:?}"
                )));
            }
            let _ = (i32_ty, ptr_ty);
        }
        F::TaskScopeSpawn => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_scope_spawn): expected 2 args, got {}",
                    args.len()
                )));
            }
            let scope_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_scope_spawn arg0")?;
            let task_ptr = load_duplex_handle(fn_ctx, args[1], "hew_task_scope_spawn arg1")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[scope_ptr.into(), task_ptr.into()],
                "hew_task_scope_spawn_call",
                "hew_task_scope_spawn call",
            )?;
            let _ = (i32_ty, ptr_ty);
        }
        F::TaskScopeCancelAfterNs => {
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
            fn_ctx.call_runtime_void(
                symbol,
                &[scope_ptr.into(), nanos.into()],
                "hew_task_scope_cancel_after_ns_call",
                "hew_task_scope_cancel_after_ns call",
            )?;
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
        F::TaskSpawnThread => {
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
        F::TaskAwaitBlocking => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_await_blocking): expected 1 arg \
                     (task), got {}",
                    args.len()
                )));
            }
            let task_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_await_blocking arg0")?;
            let call = fn_ctx.call_runtime(
                symbol,
                &[task_ptr.into()],
                "hew_task_await_blocking_call",
                "hew_task_await_blocking call",
            )?;
            // Result pointer — optional; void-result tasks may pass dest: None.
            if let Some(dest_place) = dest {
                let result_val = call.try_as_basic_value().basic().ok_or_else(|| {
                    CodegenError::FailClosed("hew_task_await_blocking returned void".into())
                })?;
                let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, result_val)
                    .llvm_ctx("hew_task_await_blocking store")?;
            }
            let _ = (i32_ty, ptr_ty);
        }
        // hew_task_get_result(task: *mut HewTask) -> *mut c_void
        // args[0]: task ptr. dest: Place for the result pointer.
        // Must be called after hew_task_await_blocking (task is Done).
        F::TaskGetResult => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_get_result): expected 1 arg \
                     (task), got {}",
                    args.len()
                )));
            }
            let task_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_get_result arg0")?;
            let result_val = fn_ctx.call_runtime_basic(
                symbol,
                &[task_ptr.into()],
                "hew_task_get_result_call",
                "hew_task_get_result call",
            )?;
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
                .llvm_ctx("hew_task_get_result store")?;
            let _ = (i32_ty, ptr_ty);
        }
        // hew_task_free(task: *mut HewTask) -> void
        // args[0]: task ptr. dest: None — frees the task allocation.
        F::TaskFree => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_task_free): expected 1 arg (task), got {}",
                    args.len()
                )));
            }
            let task_ptr = load_duplex_handle(fn_ctx, args[0], "hew_task_free arg0")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[task_ptr.into()],
                "hew_task_free_call",
                "hew_task_free call",
            )?;
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
        // ABI bridge — how the canonical symbol crosses the C ABI per target:
        //   The 16-byte `ChildLookupResult` straddles the SysV-register-pair /
        //   MSVC-indirect fault line. The historical trap 206 came from declaring
        //   the field-accurate struct WITHOUT a matching ABI attribute, letting
        //   LLVM pick indirect-sret on MSVC (caller read a stale x8/RCX slot →
        //   spurious tag → trap 206 or stale handle → AV).
        //
        //   The R5 classifier removes the gap: `declare_aggregate_return` declares
        //   the canonical `hew_supervisor_child_get` register-pair (`[2 x i64]`) on
        //   SysV/AAPCS and `void(ptr sret(ChildLookupResult) noalias, ...)` on
        //   Windows x64 MSVC, so the declared carrier and the attribute always
        //   agree. The `_raw` out-pointer twin the old comment described is being
        //   retired with the rest of the `_raw` family.
        //
        //   `key` is `u32` in the runtime (i32 in LLVM); MIR emits i64 for the
        //   slot index (ConstI64). Truncate to i32 before the call.
        //
        // WASM: `uses_wasm_excluded_symbol` gates this symbol before WASM
        //   emission; supervisor tree requires the native scheduler runtime.
        // `hew_supervisor_child_get` (actor child) and `hew_supervisor_nested_get`
        // (child supervisor) share an identical C ABI — `(ptr sup, u32 key) ->
        // ChildLookupResult` — and identical struct-return handling. The only
        // difference is the runtime symbol, which `symbol` (derived from the
        // call's family) already carries, so both families ride this one arm.
        F::SupervisorChildGet | F::SupervisorNestedGet | F::SupervisorPoolChildGet => {
            // child_get/nested_get take 2 args (sup, key); pool_child_get takes 3
            // (sup, pool_key, index). All three return the same ChildLookupResult
            // aggregate, so the ABI handling below is shared; only the arg list
            // and the declared param types differ.
            let is_pool = matches!(call.family(), F::SupervisorPoolChildGet);
            let expected_args = if is_pool { 3 } else { 2 };
            if args.len() != expected_args {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi({symbol}): expected {expected_args} args, got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "{symbol}: producer must supply a dest place \
                     (the __HewChildLookupResult alloca)"
                ))
            })?;

            // arg0: supervisor handle — ptr-typed (ActorHandle or actor-derived ptr).
            let sup_ptr = load_duplex_handle(fn_ctx, args[0], "supervisor_child_get sup")?;

            // arg1: slot/pool key — i64 in MIR (ConstI64); truncate to i32.
            let key_i64 = load_int_arg(fn_ctx, args[1], i64_ty, "supervisor_child_get key_i64")?;
            let key_i32 = fn_ctx
                .builder
                .build_int_truncate(key_i64, i32_ty, "supervisor_child_get key_i32")
                .llvm_ctx("supervisor_child_get key truncate")?;
            // pool_child_get arg2: the member index. Pass the full i64 through
            // untruncated -- `hew_supervisor_pool_child_get`'s `index` param is
            // `u64` specifically so the runtime can bounds-check the caller's
            // real index before any narrowing cast. Truncating to i32 here (as
            // this code used to) let an index like `2^32` silently wrap back
            // into `[0, member_count)` and alias an unrelated live member
            // instead of trapping OOB (hew-lang/hew#2244) -- passing the wide
            // value through removes the wraparound at its source instead of
            // moving it further down the pipeline.
            let index_i64 = if is_pool {
                Some(load_int_arg(
                    fn_ctx,
                    args[2],
                    i64_ty,
                    "supervisor_pool_child_get index_i64",
                )?)
            } else {
                None
            };

            // Declare the canonical `hew_supervisor_child_get` with its true
            // `ChildLookupResult` aggregate return, classified per target by the
            // R5 ABI classifier:
            //   - SysV/AAPCS (linux/darwin): the 16-byte aggregate is a register
            //     pair → declared `[2 x i64]`, no sret. word0 = packed
            //     (tag,reason,pad); word1 = handle. This is exactly the
            //     register-pair shape the Rust callee returns for a 16-byte
            //     `#[repr(C)]` aggregate — the classifier selects it instead of
            //     the old hand-encoded `{i64,i64}`.
            //   - Windows x64 MSVC: the aggregate is returned INDIRECTLY → the
            //     classifier declares `void(ptr sret(ChildLookupResult), ...)`
            //     and the caller reads the result from its own slot. This is the
            //     correct ABI the `_raw` out-pointer twin existed to fake;
            //     `sret(T)` makes the canonical symbol right on MSVC too.
            // The historical trap 206 was declaring the field-accurate struct
            // WITHOUT a matching attribute, letting LLVM pick indirect-sret while
            // the caller read a register pair. The classifier removes that gap:
            // the declared carrier and the attribute always agree.
            let child_result_ty = fn_ctx.ctx.struct_type(
                &[
                    fn_ctx.ctx.i8_type().into(),               // tag
                    fn_ctx.ctx.i8_type().into(),               // reason
                    fn_ctx.ctx.i8_type().array_type(6).into(), // _pad
                    ptr_ty.into(),                             // handle
                ],
                false,
            );
            let triple = fn_ctx.llvm_mod.get_triple();
            let triple_str = triple.as_str().to_string_lossy();
            let param_tys: Vec<inkwell::types::BasicMetadataTypeEnum> = if is_pool {
                // Third param is the member index, passed as the full i64 —
                // not truncated to i32 like the supervisor/pool key — so the
                // runtime can bounds-check the caller's real index before any
                // narrowing cast (hew-lang/hew#2244). Must match
                // `hew_supervisor_pool_child_get`'s `index: u64` ABI param.
                vec![ptr_ty.into(), i32_ty.into(), i64_ty.into()]
            } else {
                vec![ptr_ty.into(), i32_ty.into()]
            };
            let (fv, return_abi) = crate::abi_class::declare_aggregate_return(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                fn_ctx.target_data,
                &triple_str,
                symbol,
                child_result_ty,
                &param_tys,
            )?;
            // Argument list for the register-pair (no sret) path.
            let call_args: Vec<inkwell::values::BasicMetadataValueEnum> = match index_i64 {
                Some(idx) => vec![sup_ptr.into(), key_i32.into(), idx.into()],
                None => vec![sup_ptr.into(), key_i32.into()],
            };
            let (tag_i64, handle_i64) = match return_abi {
                crate::abi_class::AggregateReturnAbi::RegisterPair { carrier } => {
                    let call_site = fn_ctx
                        .builder
                        .build_call(fv, &call_args, "hew_supervisor_child_get_call")
                        .llvm_ctx("hew_supervisor_child_get call")?;
                    let pair = call_site
                        .try_as_basic_value()
                        .basic()
                        .ok_or_else(|| {
                            CodegenError::FailClosed(
                                "hew_supervisor_child_get returned void unexpectedly".into(),
                            )
                        })?
                        .into_array_value();
                    let _ = carrier;
                    // word0: packed (tag,reason,_pad) — tag in the low byte.
                    let word0 = fn_ctx
                        .builder
                        .build_extract_value(pair, 0, "child_word0")
                        .llvm_ctx("extract child word0")?
                        .into_int_value();
                    let word1 = fn_ctx
                        .builder
                        .build_extract_value(pair, 1, "child_word1")
                        .llvm_ctx("extract child word1")?
                        .into_int_value();
                    let tag_i8 = fn_ctx
                        .builder
                        .build_int_truncate(word0, fn_ctx.ctx.i8_type(), "child_tag_i8")
                        .llvm_ctx("trunc tag")?;
                    let tag_i64 = fn_ctx
                        .builder
                        .build_int_z_extend(tag_i8, i64_ty, "child_tag_i64")
                        .llvm_ctx("zext tag")?;
                    (tag_i64, word1)
                }
                crate::abi_class::AggregateReturnAbi::Sret => {
                    // Caller allocates the result slot; pass its address as the
                    // hidden sret first argument; read tag (field 0) and handle
                    // (field 3) from the slot after the call.
                    let result_slot = fn_ctx
                        .builder
                        .build_alloca(child_result_ty, "child_result_sret")
                        .llvm_ctx("supervisor_child_get sret alloca")?;
                    // RC10: bracket the sret result slot. Its entire live range is
                    // this match arm — written by the sret call, read by the two
                    // GEP-loads below, then dead. The slot is SAFE to bracket: the
                    // range is intra-block, it never crosses a suspend, and its
                    // address escapes only to `hew_supervisor_child_get`, which
                    // fills the slot synchronously during the call and does not
                    // retain the pointer. (LESSONS: boundary-fail-closed — the
                    // bracket is a real live range, not a guess.)
                    crate::llvm::emit_lifetime_start(
                        fn_ctx.ctx,
                        fn_ctx.llvm_mod,
                        &fn_ctx.builder,
                        result_slot,
                        "child_result_sret_lt_start",
                    )?;
                    let sret_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                        match index_i64 {
                            Some(idx) => vec![
                                result_slot.into(),
                                sup_ptr.into(),
                                key_i32.into(),
                                idx.into(),
                            ],
                            None => {
                                vec![result_slot.into(), sup_ptr.into(), key_i32.into()]
                            }
                        };
                    fn_ctx
                        .builder
                        .build_call(fv, &sret_args, "hew_supervisor_child_get_sret_call")
                        .llvm_ctx("hew_supervisor_child_get (sret) call")?;
                    let tag_field = fn_ctx
                        .builder
                        .build_struct_gep(child_result_ty, result_slot, 0, "child_sret_tag_gep")
                        .llvm_ctx("child sret tag GEP")?;
                    let tag_i8 = fn_ctx
                        .builder
                        .build_load(fn_ctx.ctx.i8_type(), tag_field, "child_sret_tag")
                        .llvm_ctx("child sret tag load")?
                        .into_int_value();
                    let tag_i64 = fn_ctx
                        .builder
                        .build_int_z_extend(tag_i8, i64_ty, "child_sret_tag_i64")
                        .llvm_ctx("zext sret tag")?;
                    let handle_field = fn_ctx
                        .builder
                        .build_struct_gep(child_result_ty, result_slot, 3, "child_sret_handle_gep")
                        .llvm_ctx("child sret handle GEP")?;
                    let handle_ptr = fn_ctx
                        .builder
                        .build_load(ptr_ty, handle_field, "child_sret_handle")
                        .llvm_ctx("child sret handle load")?
                        .into_pointer_value();
                    // RC10: last read of the sret slot is the handle load above
                    // (`tag_i64`/`handle_ptr` are now plain SSA values); the slot
                    // is dead from here. End its lifetime before the ptrtoint so
                    // the storage is free to reuse/colour.
                    crate::llvm::emit_lifetime_end(
                        fn_ctx.ctx,
                        fn_ctx.llvm_mod,
                        &fn_ctx.builder,
                        result_slot,
                        "child_result_sret_lt_end",
                    )?;
                    let handle_i64 = fn_ctx
                        .builder
                        .build_ptr_to_int(handle_ptr, i64_ty, "child_sret_handle_i64")
                        .llvm_ctx("child sret handle ptrtoint")?;
                    (tag_i64, handle_i64)
                }
            };

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
                .llvm_ctx("supervisor_child_get dest tag GEP")?;
            fn_ctx
                .builder
                .build_store(tag_field_ptr, tag_i64)
                .llvm_ctx("supervisor_child_get tag store")?;
            // Store handle into field 1 of the dest alloca.
            let handle_field_ptr = fn_ctx
                .builder
                .build_struct_gep(dest_struct_ty, dest_ptr, 1, "dest_handle_field_ptr")
                .llvm_ctx("supervisor_child_get dest handle GEP")?;
            fn_ctx
                .builder
                .build_store(handle_field_ptr, handle_i64)
                .llvm_ctx("supervisor_child_get handle store")?;
            let _ = (i32_ty, ptr_ty);
        }

        // hew_supervisor_pool_len(sup: *mut HewSupervisor, pool_key: u32) -> i64
        // The static-pool member count. Producer supplies [sup, pool_key] and an
        // i64 dest.
        F::SupervisorPoolLen => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_supervisor_pool_len): expected 2 args \
                     (sup, pool_key), got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_supervisor_pool_len: producer must supply an i64 dest".to_string(),
                )
            })?;
            let sup_ptr = load_duplex_handle(fn_ctx, args[0], "supervisor_pool_len sup")?;
            let key_i64 = load_int_arg(fn_ctx, args[1], i64_ty, "supervisor_pool_len key_i64")?;
            let key_i32 = fn_ctx
                .builder
                .build_int_truncate(key_i64, i32_ty, "supervisor_pool_len key_i32")
                .llvm_ctx("supervisor_pool_len key truncate")?;
            let len_val = fn_ctx.call_runtime_int(
                symbol,
                &[sup_ptr.into(), key_i32.into()],
                "hew_supervisor_pool_len_call",
                "hew_supervisor_pool_len call",
            )?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, len_val)
                .llvm_ctx("hew_supervisor_pool_len store")?;
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
        F::RegexMatch => {
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
                .llvm_ctx("hew_regex_match text load")?
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
                    .llvm_ctx("hew_regex_match GEP")?
            };
            let handle = fn_ctx
                .builder
                .build_load(ptr_ty, slot_ptr, "regex_handle")
                .llvm_ctx("hew_regex_match handle load")?
                .into_pointer_value();
            // Call hew_regex_match(handle, text) -> i32.
            let llvm_args: [BasicMetadataValueEnum; 2] = [handle.into(), text_ptr.into()];
            let result_i32 = fn_ctx.call_runtime_basic(
                symbol,
                &llvm_args,
                "hew_regex_match_call",
                "hew_regex_match call",
            )?;
            // Store i32 into dest.
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, result_i32)
                .llvm_ctx("hew_regex_match store")?;
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
        F::RegexCapture => {
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
                .llvm_ctx("hew_regex_capture text load")?
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
                    .llvm_ctx("hew_regex_capture GEP")?
            };
            let handle = fn_ctx
                .builder
                .build_load(ptr_ty, slot_ptr, "regex_cap_handle")
                .llvm_ctx("hew_regex_capture handle load")?
                .into_pointer_value();
            // arg2: capture_idx — i64.
            let cap_idx = load_int_arg(fn_ctx, args[2], i64_ty, "hew_regex_capture cap_idx")?;
            // Call hew_regex_capture(handle, text, cap_idx) -> *mut c_char.
            let llvm_args: [BasicMetadataValueEnum; 3] =
                [handle.into(), text_ptr.into(), cap_idx.into()];
            let cap_ptr = fn_ctx.call_runtime_ptr(
                symbol,
                &llvm_args,
                "hew_regex_capture_call",
                "hew_regex_capture call",
            )?;
            // Convert *mut c_char → i64 (ptrtoint) so the capture place (ResolvedTy::I64)
            // receives the pointer bit-pattern. The MIR null-check (IntCmp Eq 0) fires on
            // null return (group did not participate → branch to next arm, fail-closed).
            let cap_as_i64 = fn_ctx
                .builder
                .build_ptr_to_int(cap_ptr, i64_ty, "regex_cap_as_i64")
                .llvm_ctx("hew_regex_capture ptrtoint")?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, cap_as_i64)
                .llvm_ctx("hew_regex_capture store")?;
        }

        // hew_regex_handle(literal_id: i64) -> Pattern
        //
        // Value-position regex literals load the precompiled source handle and
        // clone it into the `Pattern.handle` field. Each resource value owns its
        // clone, so scope-exit close never frees the shared literal table entry.
        //
        // args[0]: literal_id (ConstI64 local; GEP index into @hew_regex_handles)
        // dest:    regex.Pattern record local (receives the cloned handle field)
        F::RegexHandle => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "Instr::CallRuntimeAbi(hew_regex_handle): expected 1 arg \
                     (literal_id), got {}",
                    args.len()
                )));
            }
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_regex_handle: producer must supply a dest place for Pattern"
                        .into(),
                )
            })?;
            // arg0: literal_id — ConstI64 → GEP index into @hew_regex_handles.
            let lit_id = load_int_arg(fn_ctx, args[0], i64_ty, "hew_regex_handle lit_id")?;
            let handle_arr_global =
                fn_ctx
                    .llvm_mod
                    .get_global("hew_regex_handles")
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "hew_regex_handle: @hew_regex_handles global not found — \
                             regex_literals must be non-empty in the pipeline to emit the global"
                                .into(),
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
                        "regex_handle_slot",
                    )
                    .llvm_ctx("hew_regex_handle GEP")?
            };
            let handle = fn_ctx
                .builder
                .build_load(ptr_ty, slot_ptr, "regex_handle")
                .llvm_ctx("hew_regex_handle load")?
                .into_pointer_value();
            let clone_fn = fn_ctx.llvm_mod.get_function("hew_regex_clone").ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_regex_handle: hew_regex_clone declaration not found".into(),
                )
            })?;
            let cloned = fn_ctx
                .builder
                .build_call(clone_fn, &[handle.into()], "regex_literal_clone")
                .llvm_ctx("hew_regex_handle clone")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_regex_handle: clone returned void".into())
                })?
                .into_pointer_value();
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest_place)?;
            let dest_struct_ty = dest_ty.into_struct_type();
            let handle_ptr = fn_ctx
                .builder
                .build_struct_gep(dest_struct_ty, dest_ptr, 0, "regex_pattern_handle_ptr")
                .llvm_ctx("hew_regex_handle Pattern.handle GEP")?;
            fn_ctx
                .builder
                .build_store(handle_ptr, cloned)
                .llvm_ctx("hew_regex_handle store")?;
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
        F::RegexFreeCapture => {
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
                .llvm_ctx("hew_regex_free_capture inttoptr")?;
            fn_ctx.call_runtime_void(
                symbol,
                &[cap_ptr.into()],
                "hew_regex_free_capture_call",
                "hew_regex_free_capture call",
            )?;
        }

        // Admitted but not wired (no codegen lowering arm yet), plus the
        // pre-staged intercept families whose calls ride `Terminator::Call`
        // and can never legally arrive as `Instr::CallRuntimeAbi` (no
        // non-pre-staged `RuntimeCallFamily` maps to their symbol, so
        // `RuntimeCall::new` refuses them at construction — this arm is
        // their unreachable-by-construction backstop). Exhaustively
        // enumerated, NO wildcard: a new family cannot land without an
        // explicit lowering decision here (`parity-or-tracked-gap`,
        // LESSONS P0 `match-fail-closed`).
        F::ActorAsk
        | F::ActorAskWithChannel
        | F::ActorCooperate
        | F::ActorSendById
        | F::ActorSpawn
        | F::ActorUnlink
        | F::AutoMutexAlloc
        | F::AutoMutexFree
        | F::AutoMutexLock
        | F::AutoMutexUnlock
        // `hew_bytes_get` rides a `Terminator::Call` whose callee codegen
        // intercepts to build `Option<u8>` inline; it is never emitted as an
        // `Instr::CallRuntimeAbi`, so it has no MIR-ABI lowering arm here and
        // fails closed if it ever reaches this dispatch, like `TaskSetResult`.
        | F::BytesGet
        | F::CancelTokenIsRequested
        | F::CancelTokenRelease
        | F::CancelTokenRetain
        | F::ChannelRecvLayout
        | F::ChannelSendLayout
        | F::ChannelTryRecvLayout
        | F::ChannelSenderClose
        | F::ChannelReceiverClose
        | F::DuplexClone
        | F::DuplexPayloadFree
        | F::DuplexTrySend
        | F::DynBoxAlloc
        | F::DynBoxFree
        | F::HashMapContainsKeyLayout
        | F::HashMapFreeLayout
        | F::HashMapGetLayout
        | F::HashMapInsertLayout
        | F::HashMapKeysLayout
        | F::HashMapLenLayout
        | F::HashMapNew
        | F::HashMapNewWithLayout
        | F::HashMapRemoveLayout
        | F::HashMapValuesLayout
        | F::HashSetContainsLayout
        | F::HashSetFreeLayout
        | F::HashSetInsertLayout
        | F::HashSetIsEmptyLayout
        | F::HashSetLenLayout
        | F::HashSetNew
        | F::HashSetNewWithLayout
        | F::HashSetRemoveLayout
        | F::LambdaBodyAllocReplyBuf
        | F::LambdaActorClone
        | F::LambdaActorDowngrade
        | F::LambdaDrainAll
        | F::LambdaActorWeakClone
        | F::LambdaActorWeakDrop
        | F::MathIntrinsic(_)
        // User metrics (#1862): the labelled `*Vec` registration and the
        // bucketed histogram registration take raw C arrays (`*const i64` /
        // `*const *const c_char` plus a length). A Hew `extern "C"` declaration
        // marshals a `Vec<T>` to a single `*mut HewVec`, which does not match
        // that `(ptr, len)` ABI, so these stay fail-closed until a HewVec-shaped
        // ABI lands. The scalar surface has a real arm below.
        | F::MetricHistogramRegister
        | F::MetricVecRegister
        | F::MetricVecWith
        | F::NodeLookup
        | F::RcNew
        | F::RegexCompile
        | F::RemotePidSend
        | F::ReplyChannelCancel
        | F::ReplyChannelFree
        | F::ReplyChannelNew
        | F::ReplyPayloadFree
        | F::ReplyWait
        | F::SelectFirst
        | F::SinkClose
        | F::SinkPeerClosed
        | F::ActorGenSinkComplete
        | F::ActorGenSinkRegister
        | F::SinkWrite(_)
        | F::SinkTryWrite(_)
        | F::StreamClose
        | F::StreamNextLayout
        | F::StreamSendLayout
        | F::StreamTryNextLayout
        // `hew_string_get` rides a `Terminator::Call` whose callee codegen
        // intercepts to build `Option<char>` inline; it is never emitted as an
        // `Instr::CallRuntimeAbi`, so it has no MIR-ABI lowering arm here and
        // fails closed if it ever reaches this dispatch, like `BytesGet`.
        // The sentinel-wrapping inspectors (`find`/`char_at`/`codepoint_at_utf8`)
        // ride the same intercepted `Terminator::Call` path (D46).
        | F::StringCharAt
        | F::StringCharAtUtf8
        | F::StringFind
        | F::StringGet
        | F::TcpAttachLocal
        | F::TaskCompleteThreaded
        | F::TaskCompletionObserve
        | F::TaskCompletionUnobserve
        | F::TaskGetEnv
        | F::TaskGetError
        | F::TaskSetEnv
        // `hew_task_set_result` is synthesized directly by the codegen task
        // wrapper (`get_or_create_task_wrapper`), never emitted as an
        // `Instr::CallRuntimeAbi` — so it has no MIR-ABI lowering arm here and
        // fails closed if it ever reaches this dispatch, exactly like its
        // sibling `TaskCompleteThreaded`.
        | F::TaskSetResult
        | F::VtableDispatchPanicOnOob => {
            return Err(CodegenError::FailClosed(format!(
                "Instr::CallRuntimeAbi(symbol={symbol:?}, family={:?}): codegen has no \
                 lowering arm for this runtime family; wire a per-family arm or leave \
                 the producer fail-closed until the substrate lane lands",
                call.family(),
            )));
        }
    }
    Ok(())
}

// ===========================================================================
// Runtime-ABI declaration interning + catalog-FFI predeclaration (god-module carve)
// ===========================================================================
// Pure relocation from llvm.rs: `intern_runtime_decl` (the runtime-ABI
// signature table), the catalog-FFI declaration helpers, extern-record return
// classification, and stdlib-catalog / extern predeclaration. Byte-identical
// IR before and after.

/// Intern a runtime-ABI function declaration for `symbol` in `llvm_mod`.
///
/// Returns the cached `FunctionValue` on repeat lookups so multiple
/// `Instr::CallRuntimeAbi` sites share one `declare` line. The signature
/// table is hard-coded from `hew-runtime/src/duplex.rs` and pinned by the
/// E4 plan §D1-D3 (revised 2026-05-15). Most signatures use 64-bit integer
/// widths for `usize`-typed args; the `usize`/`size_t` params of the actor
/// FFI surface (spawn/send) use the target-correct width via `runtime_size_ty`
/// so wasm32 (`size_t = i32`) links and runs against the wasm32 runtime.
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
pub(crate) fn intern_runtime_decl<'ctx>(
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
    let i8_ty = ctx.i8_type();
    let i16_ty = ctx.i16_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    // Target-correct `size_t`/`usize` width: i32 on wasm32, i64 on native.
    // Used for the actor FFI `usize` size params so the codegen declaration
    // matches the runtime's real C ABI (see `runtime_size_ty`).
    let size_ty = runtime_size_ty(ctx, llvm_mod);
    let fn_ty = match symbol {
        // hew_actor_cooperate() -> c_int
        // (`hew-runtime/src/scheduler.rs`, `scheduler_wasm.rs`). Decrements
        // the current actor's reductions budget and yields when exhausted.
        // The return value is a scheduler signal; cooperate-site injection
        // discards it.
        // hew_actor_ask(actor: *mut HewActor, msg_type: i32,
        //               data: *mut c_void, size: usize) -> *mut c_void
        // The `size` param is `usize` in the runtime C ABI: i32 on wasm32,
        // i64 on native 64-bit targets. Use `size_ty` to match exactly.
        "hew_actor_ask" => ptr_ty.fn_type(
            &[ptr_ty.into(), i32_ty.into(), ptr_ty.into(), size_ty.into()],
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
        // The `size` param is `usize` — use `size_ty` for correct width.
        "hew_actor_ask_with_channel" => i32_ty.fn_type(
            &[
                ptr_ty.into(),
                i32_ty.into(),
                ptr_ty.into(),
                size_ty.into(),
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
        // hew_string_equals(a: *const c_char, b: *const c_char) -> i32
        // (`hew-runtime/src/string.rs`). Returns 1 when equal, 0 otherwise.
        "hew_string_equals" => i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_string_hash_fnv1a(s: *const c_char) -> i64
        // (`hew-runtime/src/string.rs`). FNV-1a-64 over the NUL-terminated
        // payload — the hash twin of `hew_string_equals`. Called by the
        // per-record hash thunk when descending into a `string` key field.
        "hew_string_hash_fnv1a" => i64_ty.fn_type(&[ptr_ty.into()], false),
        // hew_string_compare(a: *const c_char, b: *const c_char) -> i32
        // (`hew-runtime/src/string.rs`). strcmp-style: negative when a < b,
        // 0 when equal, positive when a > b.  Used by string ordering (`<`,
        // `<=`, `>`, `>=`) in the IntCmp codegen arm.
        "hew_string_compare" => i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_string_concat(a: *const c_char, b: *const c_char) -> *mut c_char
        // (`hew-runtime/src/string.rs`). Returns a fresh owned string.
        "hew_string_concat" => ptr_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        "hew_actor_register_type" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        "hew_register_handler_name" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i32_ty.into(), ptr_ty.into()], false),
        "hew_observe_read_u64" => i64_ty.fn_type(&[ptr_ty.into()], false),
        "hew_observe_scrape" => ptr_ty.fn_type(&[], false),
        "hew_observe_series" => ptr_ty.fn_type(&[], false),
        "hew_observe_barrier" => i64_ty.fn_type(&[], false),
        // User-metric scalar emit path (#1862). Bodies in
        // `hew-runtime/src/metrics.rs`. A name comes in as `*const c_char` and
        // the slot handle goes out as `i64` (>= 0 valid, -1 on failure); the
        // mutators take an `i64` handle (plus an `i64` delta or an `f64`
        // observation) and return void. The labelled `*Vec` and bucketed
        // histogram registration take raw C arrays and are not reached through
        // this declared-call path.
        "hew_metric_counter_register"
        | "hew_metric_gauge_register"
        | "hew_metric_histogram_register_simple" => i64_ty.fn_type(&[ptr_ty.into()], false),
        "hew_metric_counter_inc" | "hew_metric_gauge_inc" | "hew_metric_gauge_dec" => {
            ctx.void_type().fn_type(&[i64_ty.into()], false)
        }
        "hew_metric_counter_add" | "hew_metric_gauge_set" | "hew_metric_gauge_add" => {
            ctx.void_type().fn_type(&[i64_ty.into(), i64_ty.into()], false)
        }
        "hew_metric_histogram_record" => ctx
            .void_type()
            .fn_type(&[i64_ty.into(), ctx.f64_type().into()], false),
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
        // hew_reply_channel_is_orphaned(ch: *mut HewReplyChannel) -> i32
        // (`hew-runtime/src/reply_channel.rs`). Returns 1 if mailbox teardown
        // marked the channel orphaned, 0 otherwise. The suspending-ask resume
        // edge reads this on a null reply to bind AskError::OrphanedAsk (matching
        // the blocking-ask path), instead of the TLS last-error slot which never
        // carries the channel-local orphaned fact. Read BEFORE freeing the ref.
        "hew_reply_channel_is_orphaned" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // hew_reply_channel_free(ch: *mut HewReplyChannel) -> void
        // (`hew-runtime/src/reply_channel.rs:409`). Releases one
        // reference; frees the channel when the refcount reaches zero.
        "hew_reply_channel_free" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        "hew_reply_channel_retain" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_reply_channel_set_parked_waiter(ch: *mut HewReplyChannel,
        // actor: *mut HewActor) -> void (`hew-runtime/src/reply_channel.rs`).
        // Records that the waiter is a parked continuation belonging to `actor`
        // so a reply wakes it via `enqueue_resume` (W6.010 suspendable ask).
        "hew_reply_channel_set_parked_waiter" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_reply_channel_set_reply_drop_fn(ch: *mut HewReplyChannel,
        //   drop_fn: void(*)(void*) | null) -> void
        // (`hew-runtime/src/reply_channel.rs`). Registers the reply type R's
        // typed destructor on the channel; the free leg runs it on a
        // delivered-but-never-consumed reply (await timeout / cancel) before
        // freeing the byte-copied buffer, releasing any heap embedded in R
        // instead of leaking it (#1739). A null thunk (bit-copy R) keeps the
        // prior buffer-free-alone behaviour.
        "hew_reply_channel_set_reply_drop_fn" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // ── NEW-6b await-deadline wiring (hew-runtime/src/await_cancel.rs,
        // reply_channel.rs, timer_periodic.rs) ───────────────────────────────
        // hew_reply_channel_set_await_cancel(ch: *mut HewReplyChannel,
        //   reg: *mut HewAwaitCancel) -> void (reply_channel.rs:167). Attaches
        // the shared cancel/deadline registration to the reply channel; the
        // channel retains it and calls `hew_await_cancel_complete` on a reply
        // deposit (cancelling the timer — the deadline-vs-reply arbiter).
        "hew_reply_channel_set_await_cancel" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_reply_channel_cancel_cleanup(source: *mut c_void, status: i32)
        // -> void (reply_channel.rs:193). The cleanup callback handed to
        // `hew_await_cancel_new`; on deadline expiry it tombstones the reply
        // channel so a late replier releases its sender ref. Declared so codegen
        // can take its address; never called directly by codegen.
        "hew_reply_channel_cancel_cleanup" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i32_ty.into()], false),
        // hew_await_cancel_new(actor: *mut HewActor, cleanup: fn(ptr,i32),
        //   source: *mut c_void) -> *mut HewAwaitCancel (await_cancel.rs:195).
        // Allocates the one-shot cancel/deadline registration (refs=1, snapshots
        // the current scope cancel token).
        "hew_await_cancel_new" => {
            ptr_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_await_cancel_schedule_deadline_ms(reg: *mut HewAwaitCancel,
        //   tw: *mut HewTimerWheel, delay_ms: u64) -> i32 (await_cancel.rs:320).
        // Schedules the registration to time out after `delay_ms`; returns 0 on
        // success, -1 if invalid or no longer pending.
        "hew_await_cancel_schedule_deadline_ms" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), i64_ty.into()], false)
        }
        // hew_await_cancel_complete(reg) -> i32 (await_cancel.rs:283). One-shot
        // CAS Pending→Completed; cancels the deadline timer if it wins. The
        // resume edge calls this to claim the reply before checking status.
        "hew_await_cancel_complete" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // hew_await_cancel_cancel(reg, status: i32, wake_actor: i32) -> i32
        // (await_cancel.rs:299). Forces a terminal Cancelled/TimedOut transition;
        // used on the abandon-cleanup edge to disarm the timer.
        "hew_await_cancel_cancel" => {
            i32_ty.fn_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false)
        }
        // hew_await_cancel_status(reg) -> i32 (await_cancel.rs:268). Reads the
        // terminal state (Pending=0, Completed=1, Cancelled=2, TimedOut=3).
        "hew_await_cancel_status" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // hew_await_cancel_free(reg) -> void (await_cancel.rs:245). Releases one
        // registration reference (frees at zero).
        "hew_await_cancel_free" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_global_timer_wheel() -> *mut HewTimerWheel (timer_periodic.rs).
        // Returns the process-wide timer wheel, lazily creating it and starting
        // the 1ms ticker thread. The deadline schedule target.
        "hew_global_timer_wheel" => ptr_ty.fn_type(&[], false),
        // hew_actor_schedule_periodic(actor: *mut HewActor, msg_type: i32,
        //   interval_ms: u64) -> *mut c_void (timer_periodic.rs:309 native,
        // timer_periodic_wasm.rs:228 wasm32 — identical C signature on both
        // targets; wasm32 delivery is cooperative via `hew_wasm_timer_tick`).
        // Arms the `#[every(duration)]` recurring zero-payload self-send of
        // `msg_type`. Returns the timer handle, or null when arming fails
        // (null actor, zero interval, or ticker startup failure — the runtime
        // records the detail in its last-error slot). Spawn-site codegen
        // checks the handle and traps fail-closed; the handle is otherwise
        // unconsumed because cancellation is actor-lifetime-scoped
        // (`hew_actor_free` → `cancel_all_timers_for_actor`).
        "hew_actor_schedule_periodic" => {
            ptr_ty.fn_type(&[ptr_ty.into(), i32_ty.into(), i64_ty.into()], false)
        }
        "hew_reply_channel_signal_ready" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // ── suspendable-callee driver (Terminator::SuspendingCallClosure) ─────
        // The continuation-handle verbs the driver calls to run a closure-invoke
        // callee coroutine (`hew-runtime/src/cont.rs`). Identical declarations to
        // the actor-dispatch trampoline's get-or-declare locals (llvm.rs ~26563).
        // hew_cont_frame_alloc(size: u64) -> ptr (cont.rs:134). Size-headered
        // coro-frame allocator (always `u64` size on all targets, incl. wasm32);
        // generator companions allocate through it so their drop free is the
        // symmetric `hew_cont_frame_free` partner.
        "hew_cont_frame_alloc" => ptr_ty.fn_type(&[i64_ty.into()], false),
        // hew_gen_coro_destroy(companion: *mut c_void) -> void (cont.rs). Single
        // teardown owner of a generator value: destroys the coro frame (handle at
        // companion offset 0) then frees the companion. Null-safe.
        "hew_gen_coro_destroy" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_cont_resume(handle: *mut c_void) -> void (cont.rs:216).
        "hew_cont_resume" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_cont_done(handle: *mut c_void) -> bool (cont.rs:238). `true` once
        // the coroutine reached its final suspend (`coro.done`); the generator
        // consumer (`Instr::GeneratorNext`) reads it as the done/None sentinel.
        // The C return is `bool` (i1), null-safe (a null handle reports done).
        "hew_cont_done" => ctx.bool_type().fn_type(&[ptr_ty.into()], false),
        // hew_cont_poll(handle: *mut c_void, out_value: *mut c_void) -> ResumePoll
        // (cont.rs:271). `ResumePoll` is `#[repr(i32)]` (Pending=0, Ready=1), so
        // the C return is i32; the driver reads Ready as "callee completed".
        "hew_cont_poll" => i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_cont_destroy(handle: *mut c_void) -> void (cont.rs:296). The SOLE
        // teardown owner of the child coro frame; null-safe.
        "hew_cont_destroy" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_actor_park_lifecycle_cont(actor: *mut HewActor, cont: *mut c_void)
        // -> bool (scheduler.rs). Parks a SUSPENDED `init`/`#[on(start)]`
        // continuation against a freshly-spawned (Idle) actor so the scheduler's
        // resume re-entry drives it to completion. Returns true when parked; false
        // (and destroys the handle) when refused. The C return is `bool` (i1).
        "hew_actor_park_lifecycle_cont" => {
            ctx.bool_type().fn_type(&[ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_context_install(ctx: *mut c_void) -> *mut c_void
        // (execution_context.rs). Installs a codegen-built execution context as the
        // current canonical context and returns the previous one. The lifecycle
        // spawn site brackets the hook ramp call with install/restore so a
        // suspending hook's `hew_actor_self()` resolves to the spawned actor.
        "hew_context_install" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        // hew_context_restore(prev: *mut c_void) -> void (execution_context.rs).
        // Restores the context `hew_context_install` returned; pairs 1:1.
        "hew_context_restore" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_context_reply_channel_swap_push(ch: *mut c_void) -> void
        // (`hew-runtime/src/execution_context.rs`). Opens a SCOPED reply-channel
        // swap: installs `ch` as the current execution context's reply channel,
        // clears the consumed bit, and pushes the saved outer pointer + consumed
        // bit onto the per-worker swap stack. The driver brackets each
        // SYNCHRONOUS child-advancing call (ramp / resume) with push/pop so the
        // child closure coroutine's `Return` arm (`hew_get_reply_channel` +
        // `hew_reply`) deposits onto the driver-owned channel rather than the
        // ambient actor channel, AND the child's consumed-bit flip is isolated
        // from the outer dispatch's fallback/orphan-reply routing.
        "hew_context_reply_channel_swap_push" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_context_reply_channel_swap_pop() -> void
        // (`hew-runtime/src/execution_context.rs`). Closes the innermost swap on
        // the NORMAL-return edge: restores the outer reply-channel pointer +
        // consumed bit. The crash/trap edge instead unwinds the whole stack from
        // the scheduler (`reply_channel_swap_unwind`), so restoration is
        // structurally guaranteed on every exit path.
        "hew_context_reply_channel_swap_pop" => ctx.void_type().fn_type(&[], false),
        // ── NEW-1 read slot (non-blocking `await conn.read()`) ───────────────
        // hew_read_slot_new() -> *mut HewReadSlot
        // (`hew-runtime/src/read_slot.rs`). Box-allocates a fresh one-shot read
        // slot with one creator ref; the suspending-read ramp frees it on the
        // resume / register-failure / abandon edges.
        "hew_read_slot_new" => ptr_ty.fn_type(&[], false),
        // hew_conn_await_read(conn: c_int, actor: *mut HewActor,
        //                     slot: *mut HewReadSlot) -> c_int
        // (`hew-runtime/src/transport.rs`). Registers the parked continuation +
        // the slot with the reactor's resume-mode. `conn` is pointer-shaped on
        // the spine (Connection is `#[opaque]` → bare ptr, like every TCP ABI);
        // the runtime reads it back as `c_int`. Returns 0 on success, -1 on
        // failure (the ramp binds the empty-bytes register-error edge).
        "hew_conn_await_read" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_read_slot_take(slot: *mut HewReadSlot) -> BytesTriple
        // (`hew-runtime/src/read_slot.rs`). Takes the deposited `bytes` value out
        // of the slot (ownership transfer), or an empty triple if no Data deposit.
        // Returns the AAPCS/SysV two-eightbyte `[2 x i64]` the Rust
        // `#[repr(C)] BytesTriple` uses — reconstructed into the `{ptr,i32,i32}`
        // dest by a raw 16-byte store (the register-pair aggregate-return shape).
        "hew_read_slot_take" => i64_ty.array_type(2).fn_type(&[ptr_ty.into()], false),
        // hew_read_slot_set_await_cancel(slot: *mut HewReadSlot,
        //   reg: *mut HewAwaitCancel) -> void (read_slot.rs:275). Attaches the
        // common cancel/deadline registration to the read slot; read-complete
        // deposits call `hew_await_cancel_complete` through this handle.
        "hew_read_slot_set_await_cancel" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_read_slot_cancel_cleanup(source: *mut c_void, status: i32) -> void
        // (read_slot.rs:300). Cleanup callback passed to `hew_await_cancel_new`;
        // on deadline expiry it marks the read slot TimedOut and detaches it from
        // the reactor so a later readiness event cannot wake the caller.
        "hew_read_slot_cancel_cleanup" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i32_ty.into()], false),
        // hew_read_slot_status(slot: *mut HewReadSlot) -> i32. Reads the current
        // read-slot terminal state; the read deadline resume path uses the
        // await-cancel status as authority, but this remains part of the ABI.
        "hew_read_slot_status" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // hew_read_slot_cancel(slot: *mut HewReadSlot) -> void
        // (`hew-runtime/src/read_slot.rs`). The abandon / register-failure edge:
        // a still-pending reactor deposit is dropped instead of waking + the
        // buffer is not kept alive past teardown.
        "hew_read_slot_cancel" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_read_slot_free(slot: *mut HewReadSlot) -> void
        // (`hew-runtime/src/read_slot.rs`). Releases one ref; frees the slot when
        // the last ref drops (and releases a still-present Data buffer).
        "hew_read_slot_free" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // ── NEW-2 suspending listener accept (non-blocking `await accept()`) ──
        // hew_listener_await_accept(listener: c_int, actor: *mut HewActor,
        //                           slot: *mut HewReadSlot) -> c_int
        // (`hew-runtime/src/transport.rs`). Registers the parked continuation +
        // the slot with the reactor's accept-mode. `listener` is pointer-shaped
        // on the spine (Listener is `#[opaque]` → bare ptr, like every TCP ABI);
        // the runtime reads it back as `c_int`. Returns 0 on success, -1 on
        // failure (the ramp binds the invalid-connection register-error edge).
        "hew_listener_await_accept" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_read_slot_take_handle(slot: *mut HewReadSlot) -> i64
        // (`hew-runtime/src/read_slot.rs`). Takes the deposited i64 `Connection`
        // handle out of the slot, or the invalid sentinel (`-1`) if no Data
        // deposit. Declared returning `ptr` so the accept ramp reinterprets the
        // 64-bit return (in rax) as the pointer-shaped `Connection` on the spine,
        // exactly as the blocking `hew_tcp_accept` return is reinterpreted.
        "hew_read_slot_take_handle" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        // ── NEW-7 suspending typed-stream consumer ───────────────────────────
        // hew_stream_await_next(stream: *mut HewStream, actor: *mut HewActor,
        //                       slot: *mut HewReadSlot) -> i32
        // (`hew-runtime/src/stream.rs`). Registers the parked consumer
        // continuation + slot with the stream's channel core. Returns
        // STREAM_AWAIT_READY (1, bind immediately) or STREAM_AWAIT_SUSPEND (0,
        // park). Handles are opaque ptrs on the spine.
        "hew_stream_await_next" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_stream_detach_await(stream: *mut HewStream, slot: *mut HewReadSlot)
        //   -> void (`hew-runtime/src/stream.rs`). The abandon edge: releases the
        // channel core's in-flight ref on the slot.
        "hew_stream_detach_await" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // ── NEW-7 suspending typed-stream producer ───────────────────────────
        // hew_stream_await_send(sink: *mut HewSink, actor: *mut HewActor,
        //                       slot: *mut HewReadSlot, data: *const BytesTriple)
        //   -> i32 (`hew-runtime/src/stream.rs`). Registers the parked producer
        // continuation + slot with the sink's channel core (or pushes + binds
        // immediately). `data` is passed by pointer (the bytes-triple alloca
        // address), like `hew_sink_write_bytes`. Returns STREAM_AWAIT_READY (1)
        // or STREAM_AWAIT_SUSPEND (0).
        "hew_stream_await_send" => i32_ty.fn_type(
            &[ptr_ty.into(), ptr_ty.into(), ptr_ty.into(), ptr_ty.into()],
            false,
        ),
        // Layout-generic sibling of `hew_stream_await_send` (receive-gen-fn
        // pump forward, ANY witness-describable element — not just
        // bytes/string): `hew_stream_await_send_layout(sink, actor, slot,
        // data: *const c_void, layout: *const HewVecElemLayout) -> i32`
        // (`hew-runtime/src/stream.rs`). Same STREAM_AWAIT_READY/SUSPEND
        // return convention as `hew_stream_await_send`.
        "hew_stream_await_send_layout" => i32_ty.fn_type(
            &[
                ptr_ty.into(),
                ptr_ty.into(),
                ptr_ty.into(),
                ptr_ty.into(),
                ptr_ty.into(),
            ],
            false,
        ),
        // hew_sink_detach_await(sink: *mut HewSink, slot: *mut HewReadSlot) ->
        //   void (`hew-runtime/src/stream.rs`). The abandon edge for a parked
        // producer: releases the core's in-flight ref + drops the pending item.
        "hew_sink_detach_await" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // ── NEW-4 std/channel suspending receive ─────────────────────────────
        // hew_channel_await_recv(rx: *mut HewChannelReceiver,
        //                        actor: *mut HewActor, slot: *mut HewReadSlot)
        //   -> i32 (`hew-runtime/src/channel.rs`). Registers the parked consumer
        // continuation + slot with the channel core (or binds immediately).
        // Returns STREAM_AWAIT_READY (1) or STREAM_AWAIT_SUSPEND (0).
        "hew_channel_await_recv" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_channel_recv_cancel_cleanup(source: *mut c_void, status: i32) -> void
        // (`hew-runtime/src/channel.rs`). HewAwaitCleanup callback: cancels the
        // read slot and detaches the channel core's in-flight consumer reference.
        // ABI is identical to `hew_read_slot_cancel_cleanup` (the general read
        // callback). Passed as `cleanup` to `hew_await_cancel_new` for recv deadlines.
        "hew_channel_recv_cancel_cleanup"
        // hew_stream_recv_cancel_cleanup(source: *mut c_void, status: i32) -> void
        // (`hew-runtime/src/stream.rs`). Same callback ABI for stream recv deadlines.
        | "hew_stream_recv_cancel_cleanup" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i32_ty.into()], false),
        // hew_channel_detach_recv(rx: *mut HewChannelReceiver,
        //                         slot: *mut HewReadSlot) -> void. The abandon
        // edge: releases the channel core's in-flight ref on the slot.
        "hew_channel_detach_recv" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_channel_poll(rx, callback: fn(*mut c_void), userdata,
        //   release: fn(*mut c_void)) -> u64 (`hew-runtime/src/channel.rs`).
        // Registers a select{} readiness poll; fires `callback(userdata)` when
        // the channel becomes readable, or `release(userdata)` if the poll is
        // cancelled first (the observer-reference ownership handoff). Returns a
        // non-zero pending-read id (0 = failure).
        "hew_channel_poll" => i64_ty.fn_type(
            &[ptr_ty.into(), ptr_ty.into(), ptr_ty.into(), ptr_ty.into()],
            false,
        ),
        // hew_channel_cancel_pending_read(rx, id) -> void. Withdraws a losing
        // select arm's channel poll.
        "hew_channel_cancel_pending_read" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // ── Layout-witness element entries (generic element width) ──────────
        // i32 sym(handle: ptr, out: ptr, witness: *const HewVecElemLayout)
        // (`hew-runtime/src/channel.rs` / `stream.rs`). The runtime decodes one
        // element directly into `out` — the Option<T> Some payload slot — per
        // the witness's ownership kind. rc 1 → Some; rc 0 → None (out
        // untouched; a None-tagged Option never reads its payload slot).
        "hew_channel_recv_layout"
        | "hew_channel_try_recv_layout"
        | "hew_stream_next_layout"
        | "hew_stream_pop_layout"
        | "hew_stream_try_next_layout" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // void sym(handle: ptr, data: ptr, witness: *const HewVecElemLayout):
        // the typed-serialise send side. The element at `data` is deep-copied
        // into the queue envelope (clone thunk for layout-managed elements);
        // the caller keeps its value.
        "hew_channel_send_layout" | "hew_stream_send_layout" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false),
        // hew_select_first(channels: *mut *mut HewReplyChannel, count: i32,
        //                  timeout_ms: i32) -> i32
        // (`hew-runtime/src/reply_channel.rs:484`). Returns the winning
        // channel index, or -1 on timeout (-1 timeout means wait
        // indefinitely; any non-negative value enforces a deadline).
        "hew_select_first" => i32_ty.fn_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false),
        // hew_select_ready_index(channels: *mut *mut HewReplyChannel,
        //                        count: i32) -> i32
        // (`hew-runtime/src/reply_channel.rs`). The non-blocking first-ready
        // scan the suspending-select resume edge calls once to find the winning
        // arm; returns the winner index, or -1 if no channel is ready (a
        // deadline wake).
        "hew_select_ready_index" => i32_ty.fn_type(&[ptr_ty.into(), i32_ty.into()], false),
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
        // hew_actor_demonitor(ref_id: u64) -> void
        // (`hew-runtime/src/monitor.rs:281`). Cancels a monitor by ref_id.
        // Called from the MonitorRef::close drop ritual (RuntimeDropDescriptor::MonitorRefClose).
        // Native-only: link/monitor/demonitor are OS-thread-dependent and not
        // supported on wasm32 (basic actors — spawn/send/receive/ask — are).
        "hew_actor_demonitor" => ctx.void_type().fn_type(&[i64_ty.into()], false),
        // hew_node_monitor(target_pid: i64) -> i64
        // (`hew-runtime/src/hew_node.rs`). Positive returns are ref ids; negative
        // returns encode MonitorError as `-(variant + 1)`.
        // codegen-offset-mirror-drift: must match the runtime signature.
        "hew_node_monitor" => i64_ty.fn_type(&[i64_ty.into()], false),
        // hew_node_monitor_recv(ref_id: i64, timeout_ms: i64) -> i64
        // (`hew-runtime/src/dist_monitor.rs`). Blocks until the monitor's terminal
        // signal arrives (or timeout_ms elapses) and returns the carried
        // down-reason. codegen-offset-mirror-drift: must match the runtime
        // `#[no_mangle]` signature.
        "hew_node_monitor_recv" => i64_ty.fn_type(&[i64_ty.into(), i64_ty.into()], false),
        // hew_node_link_remote(target_pid: i64, policy_tag: i64) -> i64
        // (`hew-runtime/src/hew_node.rs`). Establishes a cross-node link: the
        // calling actor links the remote actor `target_pid` with the
        // `PartitionPolicy` discriminant `policy_tag`. Positive returns are
        // internal link ref ids; negative returns encode LinkError as
        // `-(variant + 1)`. The local actor is resolved internally (like
        // hew_node_monitor / hew_actor_self), so no node/self argument.
        // codegen-offset-mirror-drift: must match the runtime `#[no_mangle]`
        // signature.
        "hew_node_link_remote" => i64_ty.fn_type(&[i64_ty.into(), i64_ty.into()], false),
        // hew_actor_send_by_id(actor_id: u64, msg_type: i32, data: *mut c_void,
        //                      size: usize) -> c_int (`hew-runtime/src/actor.rs:2489`).
        // `size` is `usize`/`size_t` → target-correct width (i32 on wasm32).
        // hew_actor_send_by_id(pid, dispatch, msg_type, data, size). `dispatch`
        // is the target actor TYPE's dispatch global — the `(dispatch, msg_type)`
        // cross-node serialize codec key on the remote path (null/unused for a
        // purely local send). codegen-offset-mirror-drift: must match the runtime
        // `#[no_mangle]` signature.
        "hew_actor_send_by_id" => i32_ty.fn_type(
            &[
                i64_ty.into(),
                ptr_ty.into(),
                i32_ty.into(),
                ptr_ty.into(),
                size_ty.into(),
            ],
            false,
        ),
        // hew_tcp_attach_local(conn: c_int, actor: *mut HewActor,
        //                      on_data_type: i32, on_close_type: i32) -> c_int
        // (`hew-runtime/src/transport.rs`). The active-mode `conn.attach(handler)`
        // surface lowers here: codegen synthesises the two msg_id args from the
        // concrete actor's protocol descriptor (`emit_tcp_attach_local_call`).
        // `conn` is declared pointer-shaped — `Connection` is an `#[opaque]`
        // handle that lowers to a bare `ptr` (W3.020), and every other TCP ABI
        // (`hew_tcp_accept`/`hew_tcp_write`/…) likewise hands the `c_int` fd to
        // the runtime in a pointer-shaped register; the runtime reads it back as
        // `c_int`. Declaring it `i32` here (the lone outlier) made codegen pass a
        // `ptr` value to an `i32` parameter and fail LLVM verification.
        "hew_tcp_attach_local" => i32_ty.fn_type(
            &[ptr_ty.into(), ptr_ty.into(), i32_ty.into(), i32_ty.into()],
            false,
        ),
        // hew_node_api_ask(pid, dispatch, msg_type, data, size, timeout_ms,
        //                  reply_size). `dispatch` is the target actor TYPE's
        // dispatch global — the `(dispatch, msg_type)` codec key for both the
        // request encode and the reply decode (codegen-offset-mirror-drift: must
        // match the runtime `#[no_mangle]` signature).
        "hew_node_api_ask" => ptr_ty.fn_type(
            &[
                i64_ty.into(),
                ptr_ty.into(),
                i32_ty.into(),
                ptr_ty.into(),
                i64_ty.into(),
                i64_ty.into(),
                i64_ty.into(),
            ],
            false,
        ),
        "hew_node_ask_take_last_error" => i32_ty.fn_type(&[], false),
        // hew_node_api_ask_async(pid, dispatch, msg_type, data, size,
        //                        timeout_ms, caller_actor) -> *mut c_void
        // (`hew-runtime/src/hew_node.rs`). NEW-5 suspendable submit: parks the
        // caller's coroutine on the reply table and returns an opaque pending
        // handle (null on setup failure). `caller_actor` is the `hew_actor_self`
        // pointer the wire reply resumes through `enqueue_resume`. `dispatch`
        // keys the request encode.
        "hew_node_api_ask_async" => ptr_ty.fn_type(
            &[
                i64_ty.into(),
                ptr_ty.into(),
                i32_ty.into(),
                ptr_ty.into(),
                i64_ty.into(),
                i64_ty.into(),
                ptr_ty.into(),
            ],
            false,
        ),
        // hew_node_api_ask_finish(pending_handle, dispatch, msg_type, reply_size)
        //   -> *mut c_void (`hew-runtime/src/hew_node.rs`). Drains the reply
        // deposited for a suspended remote ask on the resume edge; null is the
        // typed-failure sentinel (read via hew_node_ask_take_last_error).
        // `dispatch` keys the reply decode.
        "hew_node_api_ask_finish" => {
            ptr_ty.fn_type(
                &[ptr_ty.into(), ptr_ty.into(), i32_ty.into(), i64_ty.into()],
                false,
            )
        }
        // hew_node_api_ask_cancel(pending_handle) -> void
        // (`hew-runtime/src/hew_node.rs`). Abandons a suspended remote ask on
        // the `coro.destroy` cleanup edge, removing the pending entry.
        "hew_node_api_ask_cancel" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_actor_ask_take_last_error() -> i32
        // (`hew-runtime/src/actor.rs:150`). Reads and clears the thread-local
        // error discriminant set by a failed `hew_actor_ask` (null return).
        // Returns an `AskError` discriminant as an `i32`.
        "hew_actor_ask_take_last_error" => i32_ty.fn_type(&[], false),
        "hew_actor_state_lock_acquire" => i32_ty.fn_type(&[ptr_ty.into()], false),
        "hew_actor_state_lock_release" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // hew_actor_spawn(state: *mut c_void, state_size: usize,
        //                 dispatch: HewDispatchFn) -> *mut HewActor
        // (`hew-runtime/src/actor.rs:1968`). `state_size` is `usize`/`size_t`
        // → target-correct width (i32 on wasm32).
        "hew_actor_spawn" => ptr_ty.fn_type(&[ptr_ty.into(), size_ty.into(), ptr_ty.into()], false),
        // hew_auto_mutex_alloc() -> *mut HewAutoMutex
        // (`hew-runtime/src/auto_mutex.rs`). Allocates one opaque
        // mutex handle the compiler stashes in the closure-env (or
        // generator-state) lock-slot tail; one call per populated
        // slot at env materialisation.
        "hew_auto_mutex_alloc" => ptr_ty.fn_type(&[], false),
        // hew_auto_mutex_free(mtx: *mut HewAutoMutex) -> void
        // (`hew-runtime/src/auto_mutex.rs`). Frees one handle.
        // Idempotent on null. Compiler emits one call per `_alloc`
        // at env destructor time; LIFO drop stream guarantees every
        // matching `_unlock` precedes the `_free` for the same handle.
        "hew_auto_mutex_free" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_auto_mutex_lock(mtx: *mut HewAutoMutex) -> void.
        // Compiler emits BEFORE each cross-suspend mutable access on
        // the shared capture; suspend points sit OUTSIDE the bracket.
        "hew_auto_mutex_lock" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_auto_mutex_unlock(mtx: *mut HewAutoMutex) -> void.
        // Compiler emits immediately AFTER the access completes.
        "hew_auto_mutex_unlock" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
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
        // hew_actor_self() -> *mut HewActor (`hew-runtime/src/actor.rs`). Reads
        // the current actor from the thread-local execution context. Used by the
        // suspendable-caller ask (W6.010) to register the parked-continuation
        // waiter; the thread-local read stays valid across a suspend/resume,
        // unlike the spilled `ctx` parameter.
        "hew_actor_self" => ptr_ty.fn_type(&[], false),
        // hew_reply(ch: *mut HewReplyChannel, value: *mut c_void, size: usize) -> bool
        // `size` is `usize` — use `size_ty` for correct width on wasm32 vs native.
        "hew_reply" => ctx
            .bool_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into(), size_ty.into()], false),
        "hew_sched_init" => i32_ty.fn_type(&[], false),
        // Native lifecycle tail entry points. `hew_sched_shutdown` closes and
        // joins scheduler workers; `hew_runtime_cleanup_after_main` then checks
        // that post-worker cleanup is safe before entering the canonical runtime
        // cleanup chain.
        "hew_sched_shutdown" | "hew_runtime_cleanup_after_main" => {
            ctx.void_type().fn_type(&[], false)
        }
        // hew_sched_run() -> void (`hew-runtime/src/scheduler_wasm.rs`).
        // Host/re-entrant cooperative drain: runs runnable actors to completion
        // without owning scheduler shutdown or actor cleanup.
        "hew_sched_run" => ctx.void_type().fn_type(&[], false),
        // hew_wasm_runtime_exit() -> void (`hew-runtime/src/scheduler_wasm.rs`).
        // Standalone-WASM main-return epilogue: drains runnable actors, shuts
        // down scheduler state, and runs the runtime cleanup chain before WASI
        // process return. WASI has no runtime-owned atexit hook to fill this gap.
        "hew_wasm_runtime_exit" => ctx.void_type().fn_type(&[], false),
        // hew_shutdown_initiate{,_implicit}(drain_timeout_ms: i64) -> void
        // (`hew-runtime/src/shutdown.rs:183`). Non-blocking — sets the
        // shutdown phase to QUIESCE and spawns an orchestrator thread that
        // drains remaining actor messages then calls `hew_sched_shutdown`.
        // Pass 0 to use the default 5-second drain timeout.
        // Pass negative to skip the drain (immediate terminate).
        // Safe to call on an uninitialized scheduler: `drain_is_idle()`
        // returns `true` immediately when no scheduler is present, so the
        // orchestrator completes in ~20 ms (two idle polls at 10 ms each).
        "hew_shutdown_initiate" | "hew_shutdown_initiate_implicit" => {
            ctx.void_type().fn_type(&[i64_ty.into()], false)
        }
        // hew_shutdown_wait() -> i32
        // (`hew-runtime/src/shutdown.rs:231`). Blocks until `PHASE_DONE`
        // (returns 0). Returns -1 if shutdown was never initiated and -2
        // if the shutdown worker panicked. Paired with
        // either initiate entry point; emitted by the implicit main-exit drain
        // epilogue immediately after its implicit initiate call.
        "hew_shutdown_wait" => i32_ty.fn_type(&[], false),
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
        // hew_duplex_close_half(half: *mut c_void, direction: i32) -> i32
        // (`hew-runtime/src/duplex.rs:1496`). Closes one direction of a
        // Duplex's dual queue selected by `direction`
        // (HewDuplexDirection::Send=0, Recv=1; pinned i32 ABI per
        // `hew-runtime/src/duplex.rs:765`). The runtime's AtomicBool
        // double-close guard makes re-entry safe across all three Drop
        // contexts (sync return / async cancel / actor shutdown).
        // The MIR Place variant (`SendHalf` vs `RecvHalf`) carries the
        // direction; codegen materialises the discriminant at the
        // `lower_drop` call site (see direction special-case there).
        "hew_duplex_close_half" => i32_ty.fn_type(&[ptr_ty.into(), i32_ty.into()], false),
        // hew_duplex_send_half(d: *mut HewDuplexHandle) -> *mut HewSendHalfHandle
        // hew_duplex_recv_half(d: *mut HewDuplexHandle) -> *mut HewRecvHalfHandle
        // (`hew-runtime/src/duplex.rs:1189`, `:1235`). Consume the unified
        // handle and return the direction-only half pointer (null on a lost
        // consume/close race; the caller honours the consume contract). The
        // half pointer is the call's value, written into the MIR half Place.
        "hew_duplex_send_half" | "hew_duplex_recv_half" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        // hew_send_half_send(half, msg: *const u8, len: usize) -> i32
        // hew_send_half_try_send(half, msg, len) -> i32
        // (`hew-runtime/src/duplex.rs:1279`, `:1320`). Same ABI shape as
        // hew_duplex_send; the i32 is the SendError discriminant.
        "hew_send_half_send" | "hew_send_half_try_send" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), i64_ty.into()], false)
        }
        // hew_recv_half_recv(half, out_ptr: *mut *mut u8, out_len: *mut usize) -> i32
        // hew_recv_half_try_recv(half, out_ptr, out_len) -> i32
        // hew_duplex_recv(d, out_ptr, out_len) -> i32
        // hew_duplex_try_recv(d, out_ptr, out_len) -> i32
        // (`hew-runtime/src/duplex.rs:1362`, `:1421`, `:953`, `:1031`). The
        // payload is written to the out-params; the i32 is the RecvError status.
        "hew_recv_half_recv" | "hew_recv_half_try_recv" | "hew_duplex_recv"
        | "hew_duplex_try_recv" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_duplex_payload_free(ptr: *mut u8, len: usize) -> void
        // (`hew-runtime/src/duplex.rs`). Releases a recv'd payload buffer; the
        // recv codegen arm calls it after copying the bytes into Result::Ok.
        "hew_duplex_payload_free" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_lambda_actor_release(actor: *mut HewLambdaActorHandle) -> i32
        // (`hew-runtime/src/lambda_actor.rs:411`). Same signature shape
        // as hew_duplex_close — one ptr arg, i32 result discarded.
        "hew_lambda_actor_release" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // hew_lambda_actor_send(actor: *mut HewLambdaActorHandle,
        //                       msg: *const u8, len: usize) -> i32
        // (`hew-runtime/src/lambda_actor.rs:918`). Same ABI shape as
        // hew_duplex_send — handle ptr, message buffer ptr, length;
        // i32 result is SendError discriminant (discarded by codegen
        // arm; reply/result-bridging is a follow-on slice).
        "hew_lambda_actor_send" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), i64_ty.into()], false)
        }
        // hew_lambda_actor_weak_send(weak: *mut HewLambdaActorWeakHandle,
        //                            msg: *const u8, len: usize) -> i32
        // (`hew-runtime/src/lambda_actor.rs:1443`). The weak self-send for
        // recursive lambda bodies: upgrades just long enough to dispatch,
        // returns SendError::ActorStopped when the external strong count
        // is zero (§5.9 ratification 2 — no resurrection).
        "hew_lambda_actor_weak_send" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), i64_ty.into()], false)
        }
        // hew_lambda_actor_downgrade(actor) -> *mut HewLambdaActorWeakHandle
        // (`hew-runtime/src/lambda_actor.rs:1410`). Used by the
        // MakeLambdaActor env back-fill for the weak self-capture field.
        "hew_lambda_actor_downgrade" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        // hew_lambda_actor_weak_drop(weak) -> i32
        // (`hew-runtime/src/lambda_actor.rs:1529`). Releases the weak
        // handle; called by the synthesized env dropper.
        "hew_lambda_actor_weak_drop" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // hew_lambda_actor_ask(actor: *mut HewLambdaActorHandle,
        //                      msg: *const u8, len: usize,
        //                      reply_out: *mut *mut u8,
        //                      reply_len_out: *mut usize) -> i32
        // (`hew-runtime/src/lambda_actor.rs:1000`). 5-arg ABI; out-
        // params are caller-provided slots that the runtime writes the
        // reply buffer pointer/length into. i32 result is SendError
        // discriminant (discarded).
        "hew_lambda_actor_ask" => i32_ty.fn_type(
            &[
                ptr_ty.into(),
                ptr_ty.into(),
                i64_ty.into(),
                ptr_ty.into(),
                ptr_ty.into(),
            ],
            false,
        ),
        // hew_lambda_actor_new(mailbox_capacity: usize, shape: i32,
        //                      body_fn: *const HewLambdaActorBody,
        //                      state: *mut c_void,
        //                      state_drop: *const HewLambdaActorStateDrop)
        //   -> *mut HewLambdaActorHandle
        // (`hew-runtime/src/lambda_actor.rs:821`). Constructed via
        // `Terminator::MakeLambdaActor` (function-pointer args cannot
        // be expressed as MIR `Place` values); the codegen arm for the
        // terminator declares this signature via `intern_runtime_decl`.
        "hew_lambda_actor_new" => ptr_ty.fn_type(
            &[
                i64_ty.into(),
                i32_ty.into(),
                ptr_ty.into(),
                ptr_ty.into(),
                ptr_ty.into(),
            ],
            false,
        ),
        // hew_lambda_body_alloc_reply_buf(len: usize) -> *mut u8
        // (`hew-runtime/src/lambda_actor.rs`). Allocates a Box-backed
        // reply buffer of `len` bytes that the lambda body writes its
        // reply into; the runtime pairs this with the internal
        // `free_body_reply_buf` after `hew_reply` copies the bytes into
        // the libc-allocated published reply. The synthesised body's
        // Ask-return epilogue calls this once per ask to allocate the
        // reply slot.
        "hew_lambda_body_alloc_reply_buf" => ptr_ty.fn_type(&[i64_ty.into()], false),
        // hew_lambda_drain_all(timeout_ms: i64) -> i32
        // (`hew-runtime/src/lambda_actor.rs`). Codegen calls this from
        // `main`'s exit epilogue when the module synthesises any lambda-
        // actor body fn (i.e. emits `Terminator::MakeLambdaActor`). Blocks
        // until every lambda-actor dispatch thread has exited or the
        // timeout elapses (5 s default when `timeout_ms == 0`). Returns
        // 0 on clean drain, 1 on timeout. Safe to call when no lambda
        // actors were spawned (returns immediately).
        "hew_lambda_drain_all" => ctx.i32_type().fn_type(&[i64_ty.into()], false),
        // hew_reply_payload_free(ptr: *mut u8, len: usize) -> void
        // (`hew-runtime/src/reply_channel.rs:500`). Frees the libc-
        // allocated reply buffer the runtime wrote into the ask's
        // `*reply_out` slot. Paired with the ask reply-decode in
        // codegen's `hew_lambda_actor_ask` arm; null-safe per libc.
        "hew_reply_payload_free" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_cancel_token_release(token: *mut HewCancellationToken) -> void
        // (`hew-runtime/src/task_scope.rs:182`). Drops one owned token
        // reference. Observation paths do not call this; elaborated drop plans
        // call it at normal resource cleanup points.
        "hew_cancel_token_release" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_stream_close(stream: *mut HewStream) -> void
        // (`hew-runtime/src/stream.rs:1576`). Drops the owned stream handle;
        // null-safe. Emitted by elaborated drop plans for `Stream<T>` locals.
        "hew_stream_close" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_sink_close(sink: *mut HewSink) -> void
        // (`hew-runtime/src/stream.rs:1623`). Drops the owned sink handle;
        // null-safe. Emitted by elaborated drop plans for `Sink<T>` locals.
        "hew_sink_close" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_vec_len(v: *mut HewVec) -> i64
        // (`hew-runtime/src/vec.rs:649`). Returns the element count as i64.
        "hew_vec_len" => i64_ty.fn_type(&[ptr_ty.into()], false),
        // hew_vec_equals_thunk(lhs: *const HewVec, rhs: *const HewVec, eq_fn) -> i32
        // (`hew-runtime/src/vec.rs`). Compares element-by-element with a
        // codegen-emitted equality thunk; never falls back to raw byte compare.
        "hew_vec_equals_thunk" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_bytes_len(triple: *const BytesTriple) -> i64
        // (`hew-runtime/src/bytes.rs`). The canonical `bytes.len()` entry: a
        // `bytes` value is a `BytesTriple`, not a `*mut HewVec`. Passed BY
        // POINTER (the caller's triple alloca address) — the uniform by-pointer
        // bytes-param convention; by value the
        // `{ptr,i32,i32}` arg is not reliably ABI-portable. Returns `len` as i64.
        "hew_bytes_len" => i64_ty.fn_type(&[ptr_ty.into()], false),
        // hew_bytes_slice(ptr, offset, len, start, end) -> BytesTriple.
        // Returns the same two-eightbyte `[2 x i64]` boundary type used by the
        // migrated bytes producers; codegen stores it into the `{ptr,i32,i32}`
        // destination slot.
        "hew_bytes_slice" => i64_ty.array_type(2).fn_type(
            &[
                ptr_ty.into(),
                i32_ty.into(),
                i32_ty.into(),
                i64_ty.into(),
                i64_ty.into(),
            ],
            false,
        ),
        // hew_bytes_push(triple: &mut BytesTriple, byte: u8) -> void.
        // The receiver is the address of the stack-resident BytesTriple, not a
        // loaded heap Vec pointer.
        "hew_bytes_push" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i8_ty.into()], false),
        // hew_bytes_index(ptr: *mut u8, offset: u32, len: u32, index: i64) -> u8
        // (`hew-runtime/src/bytes.rs:773`). The dedicated `bytes` element getter:
        // a `bytes` value is a `BytesTriple { ptr, offset, len }`, NOT a
        // `*mut HewVec`, so the `hew_vec_get_i32(*mut HewVec, i64)` ABI cannot
        // apply. The triple fields are passed BY VALUE (the runtime takes the
        // unpacked `ptr`/`offset`/`len`, not a triple pointer) so the small-struct
        // ABI classification ambiguity never arises. The runtime bounds-checks
        // `index` and aborts on OOB (boundary-fail-closed). Returns the byte as u8.
        "hew_bytes_index" => i8_ty.fn_type(
            &[ptr_ty.into(), i32_ty.into(), i32_ty.into(), i64_ty.into()],
            false,
        ),
        // hew_bytes_pop(triple: &mut BytesTriple) -> i64. The receiver is passed
        // by the address of its stack-resident triple (write-back after CoW);
        // returns the popped byte as i64.
        "hew_bytes_pop" => i64_ty.fn_type(&[ptr_ty.into()], false),
        // hew_bytes_set(triple: &mut BytesTriple, index: i64, byte: u8) -> void.
        // Receiver by address (write-back after CoW); index i64, byte i8.
        "hew_bytes_set" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into(), i8_ty.into()], false),
        // hew_bytes_is_empty(triple: *const BytesTriple) -> bool. Receiver by
        // address; Rust/C bool crosses the ABI as i1.
        "hew_bytes_is_empty" => ctx.bool_type().fn_type(&[ptr_ty.into()], false),
        // hew_bytes_contains(triple: *const BytesTriple, byte: u8) -> bool.
        // Receiver by address, byte i8; returns i1.
        "hew_bytes_contains" => ctx
            .bool_type()
            .fn_type(&[ptr_ty.into(), i8_ty.into()], false),
        // hew_bytes_clear(triple: &mut BytesTriple) -> void. Receiver by address
        // (releases the buffer ref and writes back the empty triple).
        "hew_bytes_clear" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_bytes_append(dst: &mut BytesTriple, src_ptr, src_offset, src_len)
        // -> void. Destination by address (write-back after CoW/grow); the
        // source triple is unpacked into (ptr, i32 offset, i32 len).
        "hew_bytes_append" => ctx.void_type().fn_type(
            &[ptr_ty.into(), ptr_ty.into(), i32_ty.into(), i32_ty.into()],
            false,
        ),
        // hew_vec_get_bool(v: *mut HewVec, index: i64) -> bool
        // Rust/C bool crosses the ABI as i1; Hew bool locals are stored as i8.
        "hew_vec_get_bool" => ctx
            .bool_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_vec_get_i32(v: *mut HewVec, index: i64) -> i32
        // (`hew-runtime/src/vec.rs:394`). Bounds-checked by the MIR emitter
        // before this call; the runtime also aborts on OOB as defence-in-depth.
        "hew_vec_get_i8" | "hew_vec_get_u8" => {
            i8_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false)
        }
        "hew_vec_get_i16" | "hew_vec_get_u16" => {
            i16_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false)
        }
        "hew_vec_get_i32" => i32_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // hew_vec_get_i64(v: *mut HewVec, index: i64) -> i64
        // (`hew-runtime/src/vec.rs:411`).
        "hew_vec_get_i64" => i64_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        "hew_vec_get_f32" => ctx
            .f32_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into()], false),
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
        // Pointer-element Vec mutators (`hew-runtime/src/vec.rs`): used by
        // the closure-pair Vec marshalling (boxed-pair elements).
        // hew_vec_push_ptr(v, val: *mut c_void); hew_vec_set_ptr(v, i, val);
        // hew_vec_pop_ptr(v) -> *mut c_void.
        "hew_vec_push_ptr" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        "hew_vec_set_ptr" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into(), ptr_ty.into()], false),
        "hew_vec_pop_ptr" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        // hew_vec_get_str(v: *mut HewVec, index: i64) -> *const c_char
        // (`hew-runtime/src/vec.rs`). Returns a retained/header-aware owner;
        // callers that bind it must balance the owner with hew_string_drop.
        "hew_vec_get_str" => ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        // C-3 Vec<T> range-slice: hew_vec_slice_range_T(v, start, end) ->
        // *mut HewVec<T>. Caller owns the freshly-allocated result; drop
        // elaboration frees it via the existing hew_vec_free path.
        // (`hew-runtime/src/vec.rs` — added in this slice.)
        "hew_vec_slice_range_i32"
        | "hew_vec_slice_range_i64"
        | "hew_vec_slice_range_f64"
        | "hew_vec_slice_range_bytesize"
        | "hew_vec_slice_range_ptr"
        | "hew_vec_slice_range_str" => {
            ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false)
        }
        "hew_vec_slice_range_owned" => {
            ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false)
        }
        "hew_vec_slice_range_layout" => {
            ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into(), ptr_ty.into()], false)
        }
        // W3 collections-sugar S2 — fail-closed codepoint indexing / slicing.
        //
        // hew_string_index(s: *const c_char, index: i64) -> i32 (codepoint).
        //   Aborts on null / invalid UTF-8 / negative / OOB.
        // hew_string_slice_codepoints(s: *const c_char, start: i64, end: i64)
        //   -> *mut c_char (fresh owned slice).
        //   Aborts on invalid bounds / null / invalid UTF-8.
        // Drop ownership: returned string follows the existing
        // hew_string_drop / String value-class discipline.
        "hew_string_char_count" => i32_ty.fn_type(&[ptr_ty.into()], false),
        "hew_string_index" => i32_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        "hew_string_slice_codepoints" => {
            ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false)
        }
        // hew_rc_new(data: *const u8, size: i64, align: i64, drop_fn: *const fn) -> *mut u8.
        // `align` is the payload's ABI alignment so the runtime can over-align
        // the allocation rather than guessing from the source address.
        "hew_rc_new" => {
            ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into(), ptr_ty.into()], false)
        }
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
        // hew_supervisor_add_child_supervisor_with_init(
        //     parent: *mut HewSupervisor, child: *mut HewSupervisor,
        //     init_fn: SupervisorInitFn) -> c_int
        // (`hew-runtime/src/supervisor.rs:3664`). Attaches an already-started
        // child supervisor (returned by its own bootstrap) to `parent` and
        // records the bootstrap as the restart init_fn for subtree escalation.
        // Sets the child's parent back-pointer and unregisters it from the
        // top-level shutdown list. Returns 0 on success, -1 on null/self.
        "hew_supervisor_add_child_supervisor_with_init" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
        }
        // hew_supervisor_set_child_state_clone(sup: *mut HewSupervisor,
        //                                      child_index: c_int,
        //                                      state_clone_fn: HewStateCloneFn) -> void
        // (`hew-runtime/src/supervisor.rs:3807-3829`). Registers the
        // codegen-synthesised `__hew_state_clone_<Actor>` against a child
        // slot so the supervisor restart consumer can deep-clone the
        // pristine `init_state` template before each restart spawn.
        // W2.002 Stage 2 emits this immediately after the matching
        // `hew_supervisor_add_child_spec` call.
        "hew_supervisor_set_child_state_clone" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i32_ty.into(), ptr_ty.into()], false),
        // hew_supervisor_set_child_state_drop(sup: *mut HewSupervisor,
        //                                     child_index: c_int,
        //                                     state_drop_fn: unsafe extern "C" fn(*mut c_void))
        //                                     -> void
        // (`hew-runtime/src/supervisor.rs:3722-3760`). Paired sibling of
        // `hew_supervisor_set_child_state_clone`; emitted alongside it.
        // Without the drop registration, the runtime's `libc::free`
        // fallback for `state_drop_fn=None` would bytewise-free
        // owned-heap wrappers — leaking deep contents (and aliasing
        // them into a use-after-free once the clone path is wired).
        "hew_supervisor_set_child_state_drop" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i32_ty.into(), ptr_ty.into()], false),
        // hew_supervisor_set_child_lifecycle(sup: *mut HewSupervisor,
        //                                    child_index: c_int,
        //                                    lifecycle_fn: HewLifecycleFn) -> void
        // (`hew-runtime/src/supervisor.rs`). Stores the codegen-emitted
        // `__hew_lifecycle_<Actor>` wrapper against a child slot for
        // parity/back-fill. Same `(ptr, i32, ptr) -> void` shape as the state
        // setters. The INITIAL-spawn lifecycle fire reads the literal-carried
        // pointer inside `add_child_spec`; this setter does not re-fire.
        "hew_supervisor_set_child_lifecycle" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i32_ty.into(), ptr_ty.into()], false),
        // hew_supervisor_set_child_init_fn(sup: *mut HewSupervisor,
        //                                  child_index: c_int,
        //                                  init_fn: HewChildInitFn,
        //                                  config: *mut c_void,
        //                                  config_size: usize) -> void
        // (`hew-runtime/src/supervisor.rs:5320`). The v0.6 init-closure restart
        // model per-child setter: adopts the supervisor-owned config buffer once
        // (idempotent on the same pointer) and installs the per-child init thunk.
        // The literal carrier inside `add_child_spec` is the load-bearing path
        // for the INITIAL spawn; codegen emits this setter for back-fill /
        // out-of-tree-caller symmetry with the other per-child setters. Shape:
        // `(ptr sup, i32 child_index, ptr init_fn, ptr config, i64 config_size)
        // -> void`.
        "hew_supervisor_set_child_init_fn" => ctx.void_type().fn_type(
            &[
                ptr_ty.into(),
                i32_ty.into(),
                ptr_ty.into(),
                ptr_ty.into(),
                i64_ty.into(),
            ],
            false,
        ),
        // hew_supervisor_set_config_drop_fn(sup: *mut HewSupervisor,
        //                                   drop_fn: unsafe extern "C" fn(*mut c_void)) -> void
        // (`hew-runtime/src/supervisor.rs`). Registers the config struct's
        // `__hew_record_drop_inplace_<T>` so teardown releases the config
        // buffer's owned inner fields before the flat free. Shape:
        // `(ptr sup, ptr drop_fn) -> void`.
        "hew_supervisor_set_config_drop_fn" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
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
        // hew_supervisor_restart_await_blocking(sup: *mut HewSupervisor, key: u32)
        // -> void (`hew-runtime/src/supervisor.rs`). The contextless
        // `await_restart` path: blocks the calling thread until the child slot is
        // Live or permanently Dead.
        "hew_supervisor_restart_await_blocking" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i32_ty.into()], false),
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
        // hew_supervisor_pool_add_slot(sup: *mut HewSupervisor, name: *const c_char,
        //     strategy: c_int, max_members: usize) -> c_int
        // (`hew-runtime/src/supervisor.rs`). Allocates a pool slot and returns its
        // pool_key (0-based, disjoint from the static child index space). The
        // bootstrap calls it once per `pool` declaration before spawning members.
        "hew_supervisor_pool_add_slot" => i32_ty.fn_type(
            &[ptr_ty.into(), ptr_ty.into(), i32_ty.into(), size_ty.into()],
            false,
        ),
        // hew_supervisor_pool_member_add_static(sup: *mut HewSupervisor,
        //     pool_key: u32, static_idx: u32) -> c_int
        // (`hew-runtime/src/supervisor.rs`). Binds a static child (registered via
        // add_child_spec into children[]) as pool member `static_idx`. The
        // accessor resolves the member through the live static slot, so a restart
        // re-resolves automatically. Returns 0 on success.
        "hew_supervisor_pool_member_add_static" => i32_ty.fn_type(
            &[ptr_ty.into(), i32_ty.into(), i32_ty.into()],
            false,
        ),
        // hew_supervisor_pool_child_get(sup: *mut HewSupervisor, pool_key: u32,
        //     index: u32) -> ChildLookupResult
        // (`hew-runtime/src/supervisor.rs`). The pool analogue of
        // `hew_supervisor_child_get`; same `{ i64, i64 }` by-value ABI (see that
        // arm's note). Resolves pool member `index` through its live static slot
        // (static-backed pools) — out-of-range → Dead(UnknownSlot).
        "hew_supervisor_pool_child_get" => {
            let result_ty = ctx.struct_type(&[i64_ty.into(), i64_ty.into()], false);
            result_ty.fn_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false)
        }
        // hew_supervisor_pool_len(sup: *mut HewSupervisor, pool_key: u32) -> i64
        // (`hew-runtime/src/supervisor.rs`). Returns the pool's member count
        // (the fixed static-member count for a static pool), or -1 on bad key.
        "hew_supervisor_pool_len" => i64_ty.fn_type(&[ptr_ty.into(), i32_ty.into()], false),
        // hew_task_new() -> *mut HewTask
        // (`hew-runtime/src/task_scope.rs:214`). Box-allocates a HewTask
        // in the Ready state. Producer calls this to obtain a task handle
        // before calling hew_task_spawn_thread.
        "hew_task_new" => ptr_ty.fn_type(&[], false),
        "hew_task_complete_threaded" => ctx.void_type().fn_type(&[ptr_ty.into()], false),
        // hew_task_set_result(task: *mut HewTask, result: *mut c_void, size: usize)
        //   -> void (`hew-runtime/src/task_scope.rs`). Deep-copies `size` bytes of
        // the value representation at `result` into a malloc'd buffer the task
        // owns until consumed; the codegen task wrapper calls it to publish a
        // value-returning task's body result before `hew_task_complete_threaded`.
        // `size` is target-correct `usize` (i32 on wasm32, i64 on native) to
        // match the runtime's real C ABI.
        "hew_task_set_result" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into(), size_ty.into()], false),
        "hew_task_get_error" => i32_ty.fn_type(&[ptr_ty.into()], false),
        "hew_task_get_env" => ptr_ty.fn_type(&[ptr_ty.into()], false),
        "hew_task_scope_new" => ptr_ty.fn_type(&[], false),
        // hew_task_scope_cancel / _detach_completion(scope: *mut HewTaskScope)
        // -> void. `cancel` flips the scope's cancellation flag (driving every
        // child to Done); `detach_completion` is the SuspendingScopeDeadline
        // abandon-edge seam (the per-child join observers self-reclaim under the
        // scope cancel + join_all the teardown runs).
        "hew_task_scope_destroy"
        | "hew_task_scope_join_all"
        | "hew_task_scope_cancel"
        | "hew_task_scope_detach_completion" => {
            ctx.void_type().fn_type(&[ptr_ty.into()], false)
        }
        // hew_task_scope_completion_observe(scope: *mut HewTaskScope,
        //   reg: *mut HewAwaitCancel, actor: *mut HewActor) -> i32
        // (`hew-runtime/src/task_scope.rs`). The wait-ALL completion arm of the
        // SuspendingScopeDeadline arbiter: wires the scope's child join onto the
        // shared first-ready arbiter. Returns SCOPE_JOIN_READY (1) when the scope
        // already joined (bind immediately) or SCOPE_JOIN_SUSPEND (0) after
        // parking one counting observer per outstanding child.
        "hew_task_scope_completion_observe" => {
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
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
        // hew_task_await_suspend(scope: *mut HewTaskScope, task: *mut HewTask,
        //                        actor: *mut HewActor, slot: *mut HewReadSlot)
        //   -> i32 (`hew-runtime/src/task_scope.rs`). Registers the parked
        // continuation as a task-completion observer (or binds immediately when
        // the task is already Done). The observer callback wakes the actor via
        // `enqueue_resume`. Returns TASK_AWAIT_READY (1) when the bind can
        // proceed immediately, or TASK_AWAIT_SUSPEND (0) after parking. The
        // task-completion analogue of `hew_channel_await_recv`.
        "hew_task_await_suspend" => i32_ty.fn_type(
            &[ptr_ty.into(), ptr_ty.into(), ptr_ty.into(), ptr_ty.into()],
            false,
        ),
        // hew_task_detach_await(scope: *mut HewTaskScope, task: *mut HewTask,
        //                       slot: *mut HewReadSlot) -> void
        // (`hew-runtime/src/task_scope.rs`). The abandon edge: detaches the
        // completion observer registered by `hew_task_await_suspend` so a
        // racing `Done` drops its wake against a freed continuation.
        "hew_task_detach_await" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false),
        // hew_supervisor_restart_await_suspend(sup: *mut HewSupervisor, key: u32,
        //                                      actor: *mut HewActor,
        //                                      slot: *mut HewReadSlot) -> i32
        // (`hew-runtime/src/supervisor.rs`). The cooperative `await_restart`
        // observer: pre-park state check (Live/Dead → READY 1; Transient →
        // park + SUSPEND 0) and registers the parked continuation against the
        // supervisor restart-await waiter list. `notify_restart` wakes it via
        // `enqueue_resume`. The supervisor-restart analogue of
        // `hew_task_await_suspend`.
        "hew_supervisor_restart_await_suspend" => i32_ty.fn_type(
            &[ptr_ty.into(), i32_ty.into(), ptr_ty.into(), ptr_ty.into()],
            false,
        ),
        // hew_supervisor_restart_await_detach(sup: *mut HewSupervisor,
        //                                     slot: *mut HewReadSlot) -> void
        // (`hew-runtime/src/supervisor.rs`). The abandon edge: removes the
        // waiter registered by `hew_supervisor_restart_await_suspend` so a
        // racing `notify_restart` drops its wake against a freed continuation.
        "hew_supervisor_restart_await_detach" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
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
        // hew_actor_set_state_clone(actor: *mut HewActor,
        //                           state_clone_fn: HewStateCloneFn) -> void
        // (`hew-runtime/src/actor.rs:3131-3141`). Registers the
        // codegen-synthesised `__hew_state_clone_<Actor>` C-ABI fn on a
        // freshly spawned actor handle. The slot is consumed by the
        // supervisor restart path (`hew-runtime/src/supervisor.rs:1199-
        // 1294` `hew_actor_spawn_opts_adopt`) to deep-clone the pristine
        // `init_state` before each restart spawn. W2.002 Stage 2 emits
        // this at the post-merge of `emit_spawn_actor`. The runtime
        // null-tolerates via `cabi_guard!(actor.is_null())` so an OOM
        // spawn returning null is harmless even without an extra codegen
        // null guard at the call site.
        "hew_actor_set_state_clone" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        // hew_actor_set_state_drop(actor: *mut HewActor,
        //                          state_drop_fn: unsafe extern "C" fn(*mut c_void)) -> void
        // (`hew-runtime/src/actor.rs:3097-3108`). Paired sibling of
        // `hew_actor_set_state_clone`; emitted alongside it so the
        // runtime invokes the codegen-synthesised `__hew_state_drop_
        // <Actor>` body at actor shutdown instead of bytewise
        // `libc::free` of the wrapper (which would leak any owned-
        // heap state fields). Paired-emission is load-bearing: clone
        // without drop leaks; drop without clone aliases into UAF.
        "hew_actor_set_state_drop" => ctx
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
        // hew_cancel_token_is_requested(token: *mut HewCancellationToken) -> i32
        // (`hew-runtime/src/task_scope.rs:272`). Returns nonzero when the token
        // (or any ancestor) has been signalled. Codegen emits this from the
        // checker-authoritative `Instr::CancellationTokenIsCancelled` expression
        // observation. The path borrows the handle; ownership remains with the
        // source local and its existing drop path.
        "hew_cancel_token_is_requested" => i32_ty.fn_type(&[ptr_ty.into()], false),
        // ── W5.005 (F1b): memory-intrinsic floor allocator (hew-runtime/src/mem.rs) ──
        // hew_alloc(size: u64, align: u64) -> *mut u8 (`mem.rs:113`). Returns
        // null on an invalid `(size, align)` layout; aborts on genuine OOM.
        "hew_alloc" => ptr_ty.fn_type(&[i64_ty.into(), i64_ty.into()], false),
        // hew_realloc(ptr: *mut u8, old_size: u64, new_size: u64, align: u64)
        // -> *mut u8 (`mem.rs:149`). The 4-arg form carries `old_size` so the
        // runtime can reconstruct the old `Layout`. Null `ptr` is a fresh
        // alloc; null return leaves the old block intact.
        "hew_realloc" => ptr_ty.fn_type(
            &[ptr_ty.into(), i64_ty.into(), i64_ty.into(), i64_ty.into()],
            false,
        ),
        // hew_dealloc(ptr: *mut u8, size: u64, align: u64) (`mem.rs:209`).
        // Frees a block produced by `hew_alloc`/`hew_realloc` with the same
        // `(size, align)`. Returns void.
        "hew_dealloc" => ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false),
        other => {
            return Err(CodegenError::FailClosed(format!(
                "intern_runtime_decl: codegen has no LLVM signature for runtime \
                 symbol {other:?}; the symbol is an admitted runtime-ABI entry \
                 (a non-pre-staged `RuntimeCallFamily`) but no codegen arm wires it — \
                 extend the signature table or leave the producer fail-closed"
            )));
        }
    };
    let fv = llvm_mod.add_function(symbol, fn_ty, Some(Linkage::External));
    decls.insert(symbol.to_string(), fv);
    Ok(fv)
}

impl<'a, 'ctx> FnCtx<'a, 'ctx> {
    /// Declare-then-call the runtime symbol `sym`, returning the raw
    /// `CallSiteValue`. This is the single seam that collapses the canonical
    /// runtime-call idiom
    /// (`intern_runtime_decl` → `build_call` → `llvm_ctx`) restated by hand at
    /// ~160 sites. `name` is the `build_call` SSA-value name — inkwell embeds it
    /// verbatim into the IR, so it MUST be threaded per-site (never derived from
    /// `sym`, which diverges the `.ll`). `err_ctx` is the `llvm_ctx` context for
    /// the LLVM-error path, also preserved per-site.
    ///
    /// Void-returning calls use this directly (or `call_runtime_void`); the
    /// typed variants (`call_runtime_basic`/`_int`/`_ptr`) layer the
    /// fail-closed "returned void" unwrap so that guard lives in ONE place.
    pub(crate) fn call_runtime(
        &self,
        sym: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
        err_ctx: &'static str,
    ) -> CodegenResult<CallSiteValue<'ctx>> {
        let callee = intern_runtime_decl(
            self.ctx,
            self.llvm_mod,
            &mut self.runtime_decls.borrow_mut(),
            sym,
        )?;
        self.builder
            .build_call(callee, args, name)
            .llvm_ctx(err_ctx)
    }

    /// Declare-then-call a void-returning runtime symbol, discarding the result.
    pub(crate) fn call_runtime_void(
        &self,
        sym: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
        err_ctx: &'static str,
    ) -> CodegenResult<()> {
        self.call_runtime(sym, args, name, err_ctx)?;
        Ok(())
    }

    /// Declare-then-call a value-returning runtime symbol and extract its basic
    /// value, failing closed if the runtime fn returned void. This is the single
    /// authority for the "typed runtime call returned void" fail-closed guard
    /// that was hand-copied at every value-returning site.
    pub(crate) fn call_runtime_basic(
        &self,
        sym: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
        err_ctx: &'static str,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.call_runtime(sym, args, name, err_ctx)?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed(format!("{sym} returned void")))
    }

    /// `call_runtime_basic` + `.into_int_value()`.
    pub(crate) fn call_runtime_int(
        &self,
        sym: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
        err_ctx: &'static str,
    ) -> CodegenResult<IntValue<'ctx>> {
        Ok(self
            .call_runtime_basic(sym, args, name, err_ctx)?
            .into_int_value())
    }

    /// `call_runtime_basic` + `.into_pointer_value()`.
    pub(crate) fn call_runtime_ptr(
        &self,
        sym: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
        err_ctx: &'static str,
    ) -> CodegenResult<PointerValue<'ctx>> {
        Ok(self
            .call_runtime_basic(sym, args, name, err_ctx)?
            .into_pointer_value())
    }
}

pub(crate) fn fn_type_for_return<'ctx>(
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

fn basic_type_signature_label(ty: BasicTypeEnum<'_>) -> String {
    match ty {
        BasicTypeEnum::ArrayType(t) => t.print_to_string().to_string(),
        BasicTypeEnum::FloatType(t) => t.print_to_string().to_string(),
        BasicTypeEnum::IntType(t) => t.print_to_string().to_string(),
        BasicTypeEnum::PointerType(t) => t.print_to_string().to_string(),
        BasicTypeEnum::StructType(t) => t.print_to_string().to_string(),
        BasicTypeEnum::VectorType(t) => t.print_to_string().to_string(),
        BasicTypeEnum::ScalableVectorType(t) => t.print_to_string().to_string(),
    }
}

fn metadata_type_signature_label(ty: BasicMetadataTypeEnum<'_>) -> String {
    match ty {
        BasicMetadataTypeEnum::ArrayType(t) => t.print_to_string().to_string(),
        BasicMetadataTypeEnum::FloatType(t) => t.print_to_string().to_string(),
        BasicMetadataTypeEnum::IntType(t) => t.print_to_string().to_string(),
        BasicMetadataTypeEnum::PointerType(t) => t.print_to_string().to_string(),
        BasicMetadataTypeEnum::StructType(t) => t.print_to_string().to_string(),
        BasicMetadataTypeEnum::VectorType(t) => t.print_to_string().to_string(),
        BasicMetadataTypeEnum::ScalableVectorType(t) => t.print_to_string().to_string(),
        BasicMetadataTypeEnum::MetadataType(t) => t.print_to_string().to_string(),
    }
}

fn return_type_signature_label(return_ty: Option<BasicTypeEnum<'_>>) -> String {
    return_ty
        .map(basic_type_signature_label)
        .unwrap_or_else(|| "void".to_string())
}

fn ensure_catalog_ffi_signature_matches<'ctx>(
    symbol: &str,
    existing: FunctionValue<'ctx>,
    return_ty: Option<BasicTypeEnum<'ctx>>,
    param_tys: &[BasicMetadataTypeEnum<'ctx>],
) -> CodegenResult<()> {
    let fn_ty = existing.get_type();
    let existing_param_tys = fn_ty.get_param_types();
    let expected_params = param_tys
        .iter()
        .copied()
        .map(metadata_type_signature_label)
        .collect::<Vec<_>>();
    let actual_params = existing_param_tys
        .iter()
        .copied()
        .map(metadata_type_signature_label)
        .collect::<Vec<_>>();
    let expected_return = return_type_signature_label(return_ty);
    let actual_return = return_type_signature_label(fn_ty.get_return_type());

    if expected_params != actual_params || expected_return != actual_return {
        return Err(CodegenError::FailClosed(format!(
            "catalog FFI declaration for `{symbol}` conflicts with existing LLVM declaration: \
             expected ({}) -> {}, got ({}) -> {}",
            expected_params.join(", "),
            expected_return,
            actual_params.join(", "),
            actual_return
        )));
    }

    Ok(())
}

fn get_or_declare_catalog_ffi<'ctx>(
    llvm_mod: &LlvmModule<'ctx>,
    symbol: &str,
    fn_ty: FunctionType<'ctx>,
    return_ty: Option<BasicTypeEnum<'ctx>>,
    param_tys: &[BasicMetadataTypeEnum<'ctx>],
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(existing) = llvm_mod.get_function(symbol) {
        ensure_catalog_ffi_signature_matches(symbol, existing, return_ty, param_tys)?;
        return Ok(existing);
    }
    Ok(llvm_mod.add_function(symbol, fn_ty, Some(Linkage::External)))
}

fn builtin_type_to_llvm<'ctx>(
    ctx: &'ctx Context,
    target_data: &TargetData,
    ty: BuiltinTy,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<BasicTypeEnum<'ctx>> {
    resolve_ty(ctx, target_data, &ty.to_resolved(), record_layouts)
}

fn builtin_return_to_llvm<'ctx>(
    ctx: &'ctx Context,
    target_data: &TargetData,
    ty: BuiltinTy,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<Option<BasicTypeEnum<'ctx>>> {
    match ty {
        BuiltinTy::Unit | BuiltinTy::Never => Ok(None),
        other => Ok(Some(builtin_type_to_llvm(
            ctx,
            target_data,
            other,
            record_layouts,
        )?)),
    }
}

pub(crate) fn declare_print_runtime<'ctx>(
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

/// Runtime C-ABI return width (in bits) for catalog FFI symbols whose runtime
/// signature returns a *narrower* integer than the Hew-facing catalog type
/// declares.
///
/// WHY: the stdlib catalog records the **Hew-facing** type of each shim (e.g.
/// `string.find -> i64`), but several runtime functions actually return `i32`
/// (`hew_string_find`, `hew_string_length`, `hew_string_char_at`). Declaring
/// the LLVM extern with the Hew-facing width
/// (i64) does not match the C ABI: a `-> i32` C function leaves the upper 32
/// bits of the return register undefined, so reading a 64-bit result yields
/// garbage (the `-1` not-found sentinel surfaces as `4294967295`). The LLVM
/// `declare` must match the runtime's real C signature; the call boundary then
/// sign-extends the true i32 result to the Hew-facing i64. (Quit-walls #2/#3,
/// new-user audit 2026-06-02.)
///
/// WHEN OBSOLETE: when the catalog grows a dedicated "runtime ABI width" field
/// per entry (or the runtime functions are widened to i64). Until then this
/// table is the single source of truth bridging the Hew type and the C ABI.
///
/// WHAT THE REAL SOLUTION LOOKS LIKE: a `runtime_abi_return` field on
/// `BuiltinEntry` so the width travels with the catalog row rather than a
/// codegen-side lookup keyed by symbol name.
fn runtime_ffi_return_abi_bits(symbol: &str) -> Option<u32> {
    match symbol {
        // `*const c_char -> i32`: -1 not-found sentinel must sign-extend to i64.
        "hew_string_find" => Some(32),
        // `*const c_char -> i32` length / char code: non-negative in practice,
        // but the i64-declared call still reads undefined high bits. Declaring
        // the true i32 and sign-extending is the ABI-correct path.
        "hew_string_length" | "hew_string_char_at" => Some(32),
        // UTF-8 helpers also return i32 (with -1 sentinel on OOB/invalid): must
        // round-trip to the Hew-facing i64 as signed.
        "hew_string_char_count" | "hew_string_char_at_utf8" => Some(32),
        // `hew_duration_is_zero(i64) -> i32` C boolean (`hew-runtime/src/
        // io_time.rs`). The catalog row's Hew-facing type is `bool` (`i8`), but
        // the runtime register is `i32`; declare the true width so the call
        // boundary reads the correct 32-bit result before narrowing to bool.
        "hew_duration_is_zero" => Some(32),
        _ => None,
    }
}

/// Per-parameter runtime C-ABI integer width (in bits) for catalog FFI symbols
/// whose runtime parameter width differs from the Hew-facing catalog type.
///
/// Mirror of `runtime_ffi_return_abi_bits` for the argument direction:
/// `hew_string_char_at(s, idx: i32)` takes an `i32`, but the catalog declares
/// the index parameter as `i64`. Declaring the extern with the Hew-facing width
/// passes a 64-bit value where the C function reads a 32-bit register; the call
/// boundary truncates the i64 argument down to the declared i32. Shared runtime
/// symbols can also use a wider runtime width than one Hew-facing overload, such
/// as `hew_uint_to_string(n: u32)` serving both `u16` and `u32` catalog entries.
/// Returns `None` to keep the catalog-declared width when there is no override.
///
/// `param_idx` is zero-based over the catalog `entry.params` slice.
pub(crate) fn runtime_ffi_param_abi_bits(symbol: &str, param_idx: usize) -> Option<u32> {
    match (symbol, param_idx) {
        // `hew_string_char_at(s: ptr, idx: i32) -> i32`.
        ("hew_string_char_at", 1) => Some(32),
        // `hew_string_char_at_utf8(s: ptr, index: i32) -> i32`.
        ("hew_string_char_at_utf8", 1) => Some(32),
        // `hew_uint_to_string(n: u32) -> ptr`.
        ("hew_uint_to_string", 0) => Some(32),
        _ => None,
    }
}

/// Reconcile an integer value to a target integer type.
///
/// This is the single width-reconciliation primitive for the extern call
/// boundary: it bridges the Hew-facing integer width of an argument/result
/// against the runtime C function's declared LLVM integer width. Signed values
/// sign-extend when widening so narrow negative sentinels stay negative;
/// unsigned values zero-extend so high-bit magnitudes stay positive.
/// Non-integer types and equal-width integers pass through unchanged.
///
/// Returns the (possibly converted) value as a `BasicValueEnum`.
pub(crate) fn reconcile_int_width<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    value: inkwell::values::BasicValueEnum<'ctx>,
    target_ty: BasicTypeEnum<'ctx>,
    signed: bool,
    what: &str,
) -> CodegenResult<inkwell::values::BasicValueEnum<'ctx>> {
    let (BasicTypeEnum::IntType(target_int), inkwell::values::BasicValueEnum::IntValue(src_int)) =
        (target_ty, value)
    else {
        // Non-integer (pointer/struct/float) — nothing to reconcile.
        return Ok(value);
    };
    let src_width = src_int.get_type().get_bit_width();
    let dest_width = target_int.get_bit_width();
    if src_width == dest_width {
        return Ok(value);
    }
    if src_width > dest_width {
        return Ok(fn_ctx
            .builder
            .build_int_truncate(src_int, target_int, "ffi_arg_trunc")
            .llvm_ctx_with(|| format!("FFI {what} truncate"))?
            .into());
    }
    let converted = if signed {
        fn_ctx
            .builder
            .build_int_s_extend(src_int, target_int, "ffi_sext")
            .llvm_ctx_with(|| format!("FFI {what} sign-extend"))?
    } else {
        fn_ctx
            .builder
            .build_int_z_extend(src_int, target_int, "ffi_zext")
            .llvm_ctx_with(|| format!("FFI {what} zero-extend"))?
    };
    Ok(converted.into())
}

pub(crate) fn reconcile_int_width_signed<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    value: inkwell::values::BasicValueEnum<'ctx>,
    target_ty: BasicTypeEnum<'ctx>,
    what: &str,
) -> CodegenResult<inkwell::values::BasicValueEnum<'ctx>> {
    reconcile_int_width(fn_ctx, value, target_ty, true, what)
}

pub(crate) fn declare_catalog_ffi<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    target_data: &TargetData,
    entry: &BuiltinEntry,
    symbol: &str,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<FnSymbol<'ctx>> {
    if is_bool_vec_runtime_symbol(symbol) {
        let value = get_or_declare_bool_vec_runtime(ctx, llvm_mod, symbol)?;
        let return_ty = match entry.return_ty {
            BuiltinTy::Unit | BuiltinTy::Never => ctx.i8_type().into(),
            _ => builtin_type_to_llvm(ctx, target_data, entry.return_ty, record_layouts)?,
        };
        return Ok(FnSymbol::Real {
            value,
            return_ty,
            returns_unit: matches!(entry.return_ty, BuiltinTy::Unit | BuiltinTy::Never),
            extern_record_ret: None,
            extern_malloc_string_ret: false,
        });
    }
    let mut param_tys: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(entry.params.len());
    for (idx, param) in entry.params.iter().enumerate() {
        // Every `bytes` parameter lowers to a plain `ptr` — the address of the
        // caller's `{ptr, i32, i32}` BytesTriple alloca. A 16-byte triple passed
        // by value is not reliably ABI-portable in any argument position, so the
        // runtime always reads the triple through the pointer. The call edge
        // passes the alloca address for any `ptr`-declared bytes param.
        if matches!(param, BuiltinTy::Bytes) {
            param_tys.push(ctx.ptr_type(AddressSpace::default()).into());
            continue;
        }
        let llvm_param = match runtime_ffi_param_abi_bits(symbol, idx) {
            Some(32) => ctx.i32_type().into(),
            Some(bits) => {
                return Err(CodegenError::FailClosed(format!(
                    "runtime FFI param ABI width {bits} for `{symbol}` arg {idx} is unsupported"
                )));
            }
            None => builtin_type_to_llvm(ctx, target_data, *param, record_layouts)?,
        };
        param_tys.push(metadata_type_from_basic(llvm_param));
    }
    // A `-> bytes` producer returns a `#[repr(C)] BytesTriple` (16 bytes, two
    // eightbytes). Classify the return per target via the R5 ABI classifier:
    // RegisterPair on SysV/AAPCS, sret (Indirect) on MSVC/wasm32. The call edge
    // dispatches on the shared `ExternRecordRet` decision (RegisterPair →
    // Carrier, Sret → sret), replacing the per-symbol `{symbol}_raw` void+out-ptr
    // twin that hand-faked the MSVC/wasm32 sret ABI. (LESSONS:
    // aggregate-abi-by-classifier-not-per-symbol.)
    if matches!(entry.return_ty, BuiltinTy::Bytes) {
        let bytes_triple_ty = crate::llvm::bytes_triple_llvm_ty(ctx);
        let triple = llvm_mod.get_triple();
        let triple_str = triple.as_str().to_string_lossy();
        let (value, return_abi) = crate::abi_class::declare_aggregate_return(
            ctx,
            llvm_mod,
            target_data,
            &triple_str,
            symbol,
            bytes_triple_ty,
            &param_tys,
        )?;
        let extern_record_ret = match return_abi {
            crate::abi_class::AggregateReturnAbi::RegisterPair { .. } => ExternRecordRet::Carrier {
                pointee: bytes_triple_ty,
            },
            crate::abi_class::AggregateReturnAbi::Sret => ExternRecordRet::Sret {
                pointee: bytes_triple_ty,
            },
        };
        return Ok(FnSymbol::Real {
            value,
            return_ty: bytes_triple_ty.into(),
            returns_unit: false,
            extern_record_ret: Some(extern_record_ret),
            extern_malloc_string_ret: false,
        });
    }
    // Declare the extern at the runtime's true C-ABI return width when it is
    // narrower than the Hew-facing catalog type. The call boundary
    // (`Terminator::Call` → `FnSymbol::Real`) reconciles the narrow result up to
    // the Hew dest width with sign-extension. See `runtime_ffi_return_abi_bits`.
    let return_ty = match runtime_ffi_return_abi_bits(symbol) {
        Some(32) => Some(ctx.i32_type().into()),
        Some(bits) => {
            return Err(CodegenError::FailClosed(format!(
                "runtime FFI ABI width {bits} for `{symbol}` is unsupported"
            )));
        }
        None => builtin_return_to_llvm(ctx, target_data, entry.return_ty, record_layouts)?,
    };
    let fn_ty = fn_type_for_return(ctx, return_ty, &param_tys);
    let value = get_or_declare_catalog_ffi(llvm_mod, symbol, fn_ty, return_ty, &param_tys)?;
    Ok(FnSymbol::Real {
        value,
        return_ty: return_ty.unwrap_or_else(|| ctx.i8_type().into()),
        returns_unit: return_ty.is_none(),
        extern_record_ret: None,
        extern_malloc_string_ret: false,
    })
}

pub(crate) fn predeclare_stdlib_catalog<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    target_data: &TargetData,
    fn_symbols: &mut FnSymbolMap<'ctx>,
    record_layouts: &RecordLayoutMap<'ctx>,
) -> CodegenResult<()> {
    for entry in stdlib_catalog::entries() {
        match entry.linkage {
            BuiltinLinkage::RuntimeFfiShim { symbol }
            | BuiltinLinkage::ToStringShim { symbol }
            | BuiltinLinkage::StringCloneShim { symbol } => {
                let symbol_entry =
                    declare_catalog_ffi(ctx, llvm_mod, target_data, entry, symbol, record_layouts)?;
                fn_symbols.insert(entry.name.to_string(), symbol_entry);
                fn_symbols.insert(symbol.to_string(), symbol_entry);
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
            BuiltinLinkage::CompilerIntrinsic { .. } => {
                // Numeric `math.*` intrinsics declare NO LLVM extern here: they
                // route through builtin method-rewrites and never reach a
                // `Terminator::Call` (they are excluded from MIR's
                // `module_fn_names`). Any attempt to actually lower a call to a
                // `CompilerIntrinsic` is fail-closed upstream at MIR
                // (`NotYetImplemented`, `hew-mir/src/lower.rs`); this arm is an
                // intentional no-op, NOT a silent emit path. Memory-floor
                // `mem.*` intrinsics use `CalleeNameDispatchOnly` linkage and a
                // synthesized body (see `emit_floor_intrinsic_body`), so they
                // never reach this arm.
            }
            BuiltinLinkage::CalleeNameDispatchOnly => {
                // No LLVM extern: the call is intercepted in
                // `Terminator::Call` lowering by callee name. fn_symbols
                // is not populated; codegen never reaches the
                // `FnSymbol::Real` arm for this catalog row.
            }
            BuiltinLinkage::LayoutDescriptorSymbol { .. } => {
                // W4.001 Stage C0b: descriptor symbols are runtime statics,
                // not callables. C0b is checker-visible artifacts only —
                // no LLVM extern is declared here. Stage C call-site
                // materialisation is what takes the descriptor's address
                // (via `hew-cabi::map::hew_layout_*` extern statics).
            }
            BuiltinLinkage::NodeRegisterByPid {
                register_symbol,
                pid_accessor,
            } => {
                // Declare both LLVM functions manually: the catalog param list
                // is a placeholder and cannot be fed through `declare_catalog_ffi`
                // (which would produce the wrong LLVM types for the LocalPid<T>
                // argument).
                let ptr_ty = ctx.ptr_type(AddressSpace::default());
                let i64_ty = ctx.i64_type();
                let i32_ty = ctx.i32_type();

                // hew_actor_pid(actor: *mut HewActor) -> u64
                let pid_fn_ty = i64_ty.fn_type(&[ptr_ty.into()], false);
                let pid_fn = llvm_mod.get_function(pid_accessor).unwrap_or_else(|| {
                    llvm_mod.add_function(pid_accessor, pid_fn_ty, Some(Linkage::External))
                });

                // hew_node_api_register_by_pid(name: *const c_char, pid: u64) -> c_int
                let reg_fn_ty = i32_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false);
                let register_fn = llvm_mod.get_function(register_symbol).unwrap_or_else(|| {
                    llvm_mod.add_function(register_symbol, reg_fn_ty, Some(Linkage::External))
                });

                fn_symbols.insert(
                    entry.name.to_string(),
                    FnSymbol::NodeRegisterPid {
                        register_fn,
                        pid_fn,
                    },
                );
            }
        }
    }
    Ok(())
}

/// Returns `true` if `struct_ty` has any floating-point or SIMD-vector leaf
/// field (recursively, through nested structs and arrays). A `bytes` field is
/// `{ptr, i32, i32}` — integer/pointer only, so it is NOT a float leaf.
///
/// Such aggregates fall into the SysV SSE / AAPCS64 HFA return classes that the
/// size-only [`crate::abi_class::classify_aggregate`] does not model; the extern
/// record-return path fails closed on them rather than misrouting a
/// float-bearing aggregate as an INTEGER-class register pair (which AAPCS64
/// would return in `d`-registers, not memory / GP registers).
pub(crate) fn struct_has_float_or_vector_leaf(struct_ty: StructType<'_>) -> bool {
    struct_ty
        .get_field_types()
        .into_iter()
        .any(basic_ty_has_float_or_vector_leaf)
}

fn basic_ty_has_float_or_vector_leaf(ty: BasicTypeEnum<'_>) -> bool {
    match ty {
        BasicTypeEnum::FloatType(_)
        | BasicTypeEnum::VectorType(_)
        | BasicTypeEnum::ScalableVectorType(_) => true,
        BasicTypeEnum::StructType(s) => struct_has_float_or_vector_leaf(s),
        BasicTypeEnum::ArrayType(a) => basic_ty_has_float_or_vector_leaf(a.get_element_type()),
        BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_) => false,
    }
}

/// Classify how a user `extern "C"` function's `#[repr(C)]` RECORD return
/// (`struct_ty`) crosses the C-ABI boundary and DECLARE the LLVM function with
/// the matching carrier/sret shape. Returns `Some((fn_value, abi))` when the
/// return needs a carrier or sret, or `None` when the record is a Direct
/// single-field aggregate whose bare struct return is already correct (the
/// caller then falls through to the generic declaration).
///
/// This is the generic-user-extern consumer of the ABI classifier that #2399
/// was missing: previously only three hardcoded runtime symbols were classified,
/// and every user extern record return was declared with a bare
/// `fn_type_for_return` — no size check, no attribute — silently miscompiling
/// any record whose C-ABI return crossing differs from LLVM's bare aggregate
/// lowering.
///
/// `param_tys` are the function's declared non-return parameters (already
/// lowered); the sret arm prepends the hidden result pointer, the carrier arms
/// leave them unchanged.
///
/// # Errors
///
/// Fails closed for a float/SIMD-bearing record (SSE/HFA classes unmodelled), a
/// wasm32 record return (no runtime probe proves that ABI), an unmodelled
/// target (propagated from the classifier), or a RegisterPair record with a
/// sub-eightbyte tail whose carrier store would overflow the destination slot.
pub(crate) fn classify_extern_record_return<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    target_data: &TargetData,
    name: &str,
    struct_ty: StructType<'ctx>,
    param_tys: &[BasicMetadataTypeEnum<'ctx>],
) -> CodegenResult<Option<(FunctionValue<'ctx>, ExternRecordRet<'ctx>)>> {
    use crate::abi_class::AbiClass;

    // Fail-closed pre-check: SysV SSE / AAPCS64 HFA classes are not modelled.
    if struct_has_float_or_vector_leaf(struct_ty) {
        return Err(CodegenError::FailClosed(format!(
            "extern `{name}` returns a `#[repr(C)]` record containing a float/SIMD field; \
             the SysV SSE / AAPCS64 HFA aggregate-return classes are not modelled. Return \
             the record through an out-pointer parameter, or split the float fields out. \
             (LESSONS: boundary-fail-closed)"
        )));
    }

    let triple = llvm_mod.get_triple();
    let triple_str = triple.as_str().to_string_lossy();

    // Fail-closed pre-check: wasm32. `classify_aggregate`'s wasm32 arm is
    // size-only and would misclassify a multi-field <=8-byte aggregate; no
    // compiled-binary probe exercises the wasm32 extern-record-return ABI, so
    // fail closed rather than emit an unproven lowering.
    if triple_str.starts_with("wasm32") {
        return Err(CodegenError::FailClosed(format!(
            "extern `{name}` returns a `#[repr(C)]` record on wasm32; the wasm32 extern \
             record-return ABI is not modelled. Return the record through an out-pointer \
             parameter. (LESSONS: boundary-fail-closed)"
        )));
    }

    let class = crate::abi_class::classify_aggregate(struct_ty, target_data, &triple_str)?;
    match class {
        AbiClass::Indirect => {
            // void @name(ptr sret(struct) noalias, params...). The caller
            // allocates the result slot and passes its address as arg 0.
            let ptr_ty = ctx.ptr_type(AddressSpace::default());
            let mut sret_params: Vec<BasicMetadataTypeEnum<'ctx>> =
                Vec::with_capacity(param_tys.len() + 1);
            sret_params.push(ptr_ty.into());
            sret_params.extend_from_slice(param_tys);
            let fn_ty = ctx.void_type().fn_type(&sret_params, false);
            let value = llvm_mod
                .get_function(name)
                .unwrap_or_else(|| llvm_mod.add_function(name, fn_ty, Some(Linkage::External)));
            crate::abi_class::apply_return_attrs(ctx, value, class, struct_ty.into(), 0)?;
            Ok(Some((value, ExternRecordRet::Sret { pointee: struct_ty })))
        }
        AbiClass::RegisterPair => {
            // [N x i64] @name(params...): LLVM returns the carrier in the same
            // N-register pair the Rust #[repr(C)] INTEGER-class aggregate uses.
            // The raw carrier store at the call site is byte-exact only when the
            // carrier's width equals the record's ABI size — i.e. a whole number
            // of eightbytes. A sub-eightbyte tail (9-15 B) would overflow the
            // destination slot, so fail closed.
            let size = target_data.get_abi_size(&struct_ty);
            let eightbytes = size.div_ceil(8);
            if eightbytes * 8 != size {
                return Err(CodegenError::FailClosed(format!(
                    "extern `{name}` returns a {size}-byte record classified RegisterPair \
                     with a sub-eightbyte tail; the register-pair carrier store is byte-exact \
                     only for whole-eightbyte sizes. Pad the record to a multiple of 8 bytes \
                     or return it through an out-pointer. (LESSONS: boundary-fail-closed)"
                )));
            }
            let n = u32::try_from(eightbytes).map_err(|_| {
                CodegenError::FailClosed(format!(
                    "extern `{name}` record size {size} overflows the eightbyte carrier count"
                ))
            })?;
            let carrier = ctx.i64_type().array_type(n);
            let fn_ty = carrier.fn_type(param_tys, false);
            let value = llvm_mod
                .get_function(name)
                .unwrap_or_else(|| llvm_mod.add_function(name, fn_ty, Some(Linkage::External)));
            Ok(Some((
                value,
                ExternRecordRet::Carrier { pointee: struct_ty },
            )))
        }
        AbiClass::Direct => {
            if struct_ty.count_fields() <= 1 {
                // Single-field aggregate: the field occupies the whole return, so
                // the bare struct return matches the C ABI. Natural — fall through.
                return Ok(None);
            }
            // Direct multi-field: the C ABI coerces the <=8-byte aggregate into
            // ONE INTEGER register while a bare struct return splits the fields.
            // Declare an iN carrier (N = size*8) so LLVM returns it in one
            // register. Direct sizes are exactly 1|2|4|8, so the carrier width
            // equals the record's ABI size and the raw store is byte-exact;
            // any other size reaching a Direct multi-field class is a
            // classifier-contract violation — fail closed.
            let size = target_data.get_abi_size(&struct_ty);
            let carrier = match size {
                1 => ctx.i8_type(),
                2 => ctx.i16_type(),
                4 => ctx.i32_type(),
                8 => ctx.i64_type(),
                other => {
                    return Err(CodegenError::FailClosed(format!(
                        "extern `{name}` returns a {other}-byte record classified Direct \
                         multi-field; the coerced-int carrier is defined only for sizes \
                         1/2/4/8. (LESSONS: boundary-fail-closed)"
                    )));
                }
            };
            let fn_ty = carrier.fn_type(param_tys, false);
            let value = llvm_mod
                .get_function(name)
                .unwrap_or_else(|| llvm_mod.add_function(name, fn_ty, Some(Linkage::External)));
            Ok(Some((
                value,
                ExternRecordRet::Carrier { pointee: struct_ty },
            )))
        }
        AbiClass::CoercedInt { .. } => {
            // `classify_aggregate` never produces CoercedInt today; the Direct
            // multi-field arm above handles the coerced-int case directly. If a
            // future classifier arm returns CoercedInt it needs its own carrier
            // wiring here rather than a silent emit.
            Err(CodegenError::FailClosed(format!(
                "extern `{name}` classified CoercedInt; the extern record-return path does \
                 not wire that classifier arm. (LESSONS: boundary-fail-closed)"
            )))
        }
    }
}

/// Emit the `Terminator::Call` edge for a user extern whose `#[repr(C)]`
/// record return was classified by [`classify_extern_record_return`]. The
/// declaration and this call edge share the one [`ExternRecordRet`] decision so
/// they cannot disagree (LESSONS `codegen-call-matches-callee-arity`).
///
/// - [`ExternRecordRet::Sret`]: the caller's destination slot IS the result
///   slot — its address is PREPENDED as argument 0 (sret is the first
///   parameter; the callee writes the record through it, so there is no
///   post-call store). Because of the hidden leading parameter, every declared
///   parameter type sits at `idx + 1` relative to the Hew argument list — the
///   int-width reconciliation must read the shifted index or every width
///   reconciliation silently misaligns (LESSONS `ffi-abi-width-mirror`; the
///   `recordret_big_mixed` probe pins this).
/// - [`ExternRecordRet::Carrier`]: the call returns an `[N x i64]` / `iN`
///   carrier whose byte width equals the record's ABI size (checked at
///   declaration); store the carrier value raw into the destination slot — the
///   byte layouts match, reconstructing the record (the documented
///   `store_classified_bytes_return` register-pair mechanism).
pub(crate) fn emit_extern_record_return_call<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    callee: &str,
    value: FunctionValue<'ctx>,
    record_ret_abi: ExternRecordRet<'ctx>,
    args: &[Place],
    dest: Option<&Place>,
) -> CodegenResult<()> {
    let (is_sret, pointee) = match record_ret_abi {
        ExternRecordRet::Sret { pointee } => (true, pointee),
        ExternRecordRet::Carrier { pointee } => (false, pointee),
    };
    // The record return is an owned aggregate: `hew-mir` always assigns a
    // tracked dest for owned/aggregate returns, so a missing dest is a
    // producer regression — fail closed rather than silently discarding
    // (mirrors the generic arm's aggregate-discard guard).
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "Call to record-returning extern `{callee}` must carry a Terminator::Call dest \
             (the {} return needs a destination slot)",
            if is_sret { "sret" } else { "carrier" }
        ))
    })?;
    let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
    if dest_ty != BasicTypeEnum::StructType(pointee) {
        return Err(CodegenError::FailClosed(format!(
            "Call dest type {dest_ty:?} does not match extern `{callee}`'s classified record \
             return {pointee:?} (LESSONS: codegen-call-matches-callee-arity)"
        )));
    }
    let declared_param_tys = value.get_type().get_param_types();
    // The sret pointer occupies declared param 0, shifting every Hew argument's
    // declared type one slot to the right.
    let param_index_shift = usize::from(is_sret);
    let mut arg_vals: Vec<inkwell::values::BasicMetadataValueEnum> =
        Vec::with_capacity(args.len() + param_index_shift);
    if is_sret {
        arg_vals.push(metadata_value_from_basic(dest_ptr.into()));
    }
    for (idx, arg) in args.iter().enumerate() {
        let (arg_ptr, arg_ty) = place_pointer(fn_ctx, *arg)?;
        // By-pointer bytes param (e.g. `hew_tls_write_result`'s `data: bytes`):
        // the callee declares this parameter as `ptr` while the Hew argument is a
        // `bytes` value (a `{ptr, i32, i32}` alloca). Pass the alloca ADDRESS so
        // the runtime reads the triple through the pointer — the same branch as
        // the generic arm, read at the SHIFTED declared index. Every `bytes`
        // param is `ptr`-declared, so this branch fires for all of them.
        if matches!(
            declared_param_tys.get(idx + param_index_shift),
            Some(BasicMetadataTypeEnum::PointerType(_))
        ) && matches!(arg_ty, BasicTypeEnum::StructType(_))
            && matches!(place_resolved_ty(fn_ctx, *arg)?, ResolvedTy::Bytes)
        {
            arg_vals.push(metadata_value_from_basic(arg_ptr.into()));
            continue;
        }
        let loaded = fn_ctx
            .builder
            .build_load(arg_ty, arg_ptr, "call_arg")
            .llvm_ctx("record-return call arg load")?;
        // Reconcile each loaded argument against the callee's *declared* LLVM
        // parameter type at the SHIFTED index — same rule as the generic arm
        // (a Hew i64 local feeding a declared i32 param must narrow, signed
        // for signed operands).
        let reconciled = match declared_param_tys.get(idx + param_index_shift) {
            Some(BasicMetadataTypeEnum::IntType(param_int)) => {
                let arg_resolved_ty = place_resolved_ty(fn_ctx, *arg)?;
                reconcile_int_width(
                    fn_ctx,
                    loaded,
                    (*param_int).into(),
                    !is_unsigned_integer_ty(arg_resolved_ty),
                    "argument",
                )?
            }
            _ => loaded,
        };
        arg_vals.push(metadata_value_from_basic(reconciled));
    }
    let call_site = fn_ctx
        .builder
        .build_call(value, &arg_vals, "call_result")
        .llvm_ctx("record-return extern call")?;
    if is_sret {
        // The callee wrote the record through the sret pointer — no store.
        // The sret(pointee)+noalias attributes live on the DECLARATION;
        // LLVM's `CallBase::paramHasAttr` falls back to the callee declaration
        // for direct calls (the shipped `store_classified_bytes_return` Sret
        // arm relies on the same mechanism — mirror it, no call-site plumbing).
        let _ = call_site;
        return Ok(());
    }
    // Carrier: store the returned [N x i64] / iN raw into the dest record slot.
    // Byte width equals the record's ABI size (enforced at declaration), so the
    // raw store is the exact documented reconstruction.
    let carrier_val = call_site.try_as_basic_value().basic().ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "record-returning extern `{callee}` returned void unexpectedly (carrier class)"
        ))
    })?;
    fn_ctx
        .builder
        .build_store(dest_ptr, carrier_val)
        .llvm_ctx("record-return carrier store")?;
    Ok(())
}

/// Predeclare every user-authored `extern "<abi>" { fn ...; }` symbol as an
/// LLVM external function so `Terminator::Call` lookups by name resolve
/// transparently. The symbol is satisfied at link time — by `hew-runtime`
/// for stable JIT-ABI symbols, or by a sibling stdlib staticlib that the
/// driver adds to the native link line.
///
/// Must run BEFORE user-fn declaration so that an extern fn whose name
/// happens to collide with a user fn (a checker-rejected case in practice)
/// would still get a deterministic registration order; in normal flow the
/// two sets are disjoint and the insertion order does not matter.
pub(crate) fn predeclare_extern_decls<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    fn_symbols: &mut FnSymbolMap<'ctx>,
    extern_decls: &[hew_mir::model::ExternDecl],
    record_layouts: &RecordLayoutMap<'ctx>,
    target_data: &TargetData,
) -> CodegenResult<()> {
    for decl in extern_decls {
        if fn_symbols.contains_key(&decl.name) {
            // Already registered as a stdlib-catalog shim (e.g. user redeclares
            // a runtime symbol that the catalog already wired with the correct
            // signature). Skip to preserve the catalog's typed FnSymbol entry.
            continue;
        }
        if is_bool_vec_runtime_symbol(&decl.name) {
            let value = get_or_declare_bool_vec_runtime(ctx, llvm_mod, &decl.name)?;
            let return_ty = if matches!(decl.return_ty, ResolvedTy::Unit) {
                ctx.i8_type().into()
            } else {
                resolve_ty(ctx, target_data, &decl.return_ty, record_layouts)?
            };
            fn_symbols.insert(
                decl.name.clone(),
                FnSymbol::Real {
                    value,
                    return_ty,
                    returns_unit: matches!(decl.return_ty, ResolvedTy::Unit),
                    extern_record_ret: None,
                    extern_malloc_string_ret: false,
                },
            );
            continue;
        }

        // ── bytes boundary: one unconditional rule ───────────────────────────
        //
        // A `-> bytes` return is classified by the R5 aggregate ABI classifier
        // below (no per-symbol allowlist; it fails closed on an unmodelled
        // target). Every `bytes` PARAM lowers to a plain `ptr` — the address of
        // the caller's `{ptr, i32, i32}` BytesTriple alloca — regardless of the
        // symbol. A 16-byte triple passed by value is not reliably ABI-portable
        // in any argument position, so the runtime always reads it through the
        // pointer. There is no unclassified `bytes` ABI to gate: a bytes param is
        // always `ptr`, a bytes return is always classified.
        // LESSONS: `boundary-fail-closed` (P0), `inline-collection-param-aliases-by-pointer`.

        let mut param_tys: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(decl.param_tys.len());
        for ty in &decl.param_tys {
            // A `bytes` parameter lowers to a plain `ptr` (the address of the
            // caller's BytesTriple alloca); the generic Call path passes the
            // alloca address rather than the loaded struct.
            if matches!(ty, ResolvedTy::Bytes) {
                param_tys.push(ctx.ptr_type(AddressSpace::default()).into());
                continue;
            }
            param_tys.push(metadata_type_from_basic(resolve_ty(
                ctx,
                target_data,
                ty,
                record_layouts,
            )?));
        }
        // A `-> bytes` extern returns a `#[repr(C)] BytesTriple` classified per
        // target by the R5 ABI classifier (RegisterPair on SysV/AAPCS, sret on
        // MSVC/wasm32). The call edge dispatches on the shared `ExternRecordRet`
        // decision (RegisterPair → Carrier, Sret → sret), replacing the
        // `{name}_raw` void+out-ptr twin. (LESSONS:
        // aggregate-abi-by-classifier-not-per-symbol.)
        if matches!(decl.return_ty, ResolvedTy::Bytes) {
            let bytes_triple_ty = crate::llvm::bytes_triple_llvm_ty(ctx);
            let triple = llvm_mod.get_triple();
            let triple_str = triple.as_str().to_string_lossy();
            let (value, return_abi) = crate::abi_class::declare_aggregate_return(
                ctx,
                llvm_mod,
                target_data,
                &triple_str,
                &decl.name,
                bytes_triple_ty,
                &param_tys,
            )?;
            let extern_record_ret = match return_abi {
                crate::abi_class::AggregateReturnAbi::RegisterPair { .. } => {
                    ExternRecordRet::Carrier {
                        pointee: bytes_triple_ty,
                    }
                }
                crate::abi_class::AggregateReturnAbi::Sret => ExternRecordRet::Sret {
                    pointee: bytes_triple_ty,
                },
            };
            fn_symbols.insert(
                decl.name.clone(),
                FnSymbol::Real {
                    value,
                    return_ty: bytes_triple_ty.into(),
                    returns_unit: false,
                    extern_record_ret: Some(extern_record_ret),
                    extern_malloc_string_ret: false,
                },
            );
            continue;
        }
        // ── extern record-return C-ABI classification (#2399) ─────────────────
        //
        // A user extern returning a `#[repr(C)]` record (an LLVM StructType —
        // EXCLUDING `bytes`, handled by the classified branch above, and
        // `Unit`) routes its return through the ABI classifier. LLVM's bare
        // aggregate-return ABI diverges from the C ABI whenever the aggregate
        // exceeds the direct-return threshold (>16 B → indirect/sret) OR
        // multiple fields share an eightbyte (the C ABI packs them into one
        // INTEGER register while a bare struct return splits them). Opaque
        // handles (`type X {}` → `ptr`), `Vec`/`Stream`/`Sink` returns, and
        // scalar returns are not StructTypes and fall through untouched.
        if !matches!(decl.return_ty, ResolvedTy::Bytes | ResolvedTy::Unit) {
            if let BasicTypeEnum::StructType(struct_ty) =
                resolve_ty(ctx, target_data, &decl.return_ty, record_layouts)?
            {
                if let Some((value, extern_record_ret)) = classify_extern_record_return(
                    ctx,
                    llvm_mod,
                    target_data,
                    &decl.name,
                    struct_ty,
                    &param_tys,
                )? {
                    fn_symbols.insert(
                        decl.name.clone(),
                        FnSymbol::Real {
                            value,
                            return_ty: struct_ty.into(),
                            returns_unit: false,
                            extern_record_ret: Some(extern_record_ret),
                            extern_malloc_string_ret: false,
                        },
                    );
                    continue;
                }
                // `None` → Direct single-field record: the field occupies the
                // whole return, no eightbyte-packing divergence, so the bare
                // struct return is already C-ABI-correct. Fall through to the
                // generic declaration below (unchanged).
            }
        }
        let return_ty = if matches!(decl.return_ty, ResolvedTy::Unit) {
            None
        } else {
            Some(resolve_ty(
                ctx,
                target_data,
                &decl.return_ty,
                record_layouts,
            )?)
        };
        let fn_ty = fn_type_for_return(ctx, return_ty, &param_tys);
        let value = llvm_mod
            .get_function(&decl.name)
            .unwrap_or_else(|| llvm_mod.add_function(&decl.name, fn_ty, Some(Linkage::External)));
        fn_symbols.insert(
            decl.name.clone(),
            FnSymbol::Real {
                value,
                return_ty: return_ty.unwrap_or_else(|| ctx.i8_type().into()),
                returns_unit: return_ty.is_none(),
                extern_record_ret: None,
                extern_malloc_string_ret: decl.malloc_string_return,
            },
        );
    }
    Ok(())
}
