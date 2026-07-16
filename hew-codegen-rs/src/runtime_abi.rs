//! Runtime-ABI call lowering for the LLVM backend.
//!
//! Pure relocation (R4 god-module carve) of the runtime-call lowering edge out
//! of llvm.rs: [`lower_call_runtime_abi`] — the single dispatch that lowers
//! every `Instr::CallRuntimeAbi` to its C-ABI extern declaration and call,
//! covering the entire `RuntimeCallFamily` surface — plus the two bytes-ABI
//! consumer-convention predicates ([`is_bytes_by_pointer_consumer`],
//! [`is_bytes_struct_expansion_consumer`]) that the catalog FFI declaration
//! and the call edge both consult.
//!
//! Mirrors the `crate::coro` carve: shared codegen context and the per-family
//! emit helpers are imported from `crate::llvm`; `crate::llvm` calls back via
//! `crate::runtime_abi::lower_call_runtime_abi`. The per-family arm bodies live
//! verbatim here — no behaviour change; every emitted `.ll` is byte-identical.

use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, IntValue};
use inkwell::{AddressSpace, IntPredicate};

use hew_mir::Place;
use hew_types::ResolvedTy;

#[allow(unused_imports)]
use crate::llvm::*;

/// Runtime consumers that take a `bytes` value BY POINTER (`*const BytesTriple`)
/// rather than by value.
///
/// WHY: a 16-byte `BytesTriple {ptr, i32, i32}` passed by value as a NON-first
/// argument loses its second eightbyte (offset/len) at the C-ABI boundary in
/// the current codegen — the in-register small-struct coercion is only reliable
/// for a single/first by-value struct argument (e.g. `hew_bytes_to_string`).
/// These consumers all take the bytes value after a leading handle argument
/// (`conn`/`sink`), so codegen passes the address of the caller's triple alloca
/// (a plain `ptr`, always ABI-portable) and the runtime reads through it —
/// exactly the proven `hew_bytes_push(&mut BytesTriple, ..)` pattern. The Hew
/// surface still declares these params as `bytes`; only the C-ABI lowering
/// differs. WHEN OBSOLETE: when codegen emits the correct C-ABI coercion for a
/// by-value struct in any argument position (e.g. via the `byval`/coerced-int
/// frontend lowering), these can revert to by-value `bytes` params.
pub(crate) fn is_bytes_by_pointer_consumer(symbol: &str) -> bool {
    matches!(
        symbol,
        "hew_tcp_write"
            | "hew_sink_write_bytes"
            | "hew_sink_try_write_bytes"
            | "hew_bytes_to_string"
            | "hew_bytes_len"
            | "hew_file_write_bytes"
            // TCP broadcast: `message: bytes` passed as *const BytesTriple.
            // (Previous *const c_char signature ignored `offset` — only worked
            // when offset==0. Fixed to take *const BytesTriple.)
            | "hew_tcp_broadcast_except"
            // TLS write bridge: `data: bytes` passed as *const BytesTriple.
            // (Previous (*const u8, usize) pair ignored `offset`; fixed.)
            | "hew_tls_write_result"
            // Ed25519 sign: all _hew wrappers that accept `bytes` inputs pass
            // them as *const BytesTriple (by-pointer consumer convention).
            | "hew_ed25519_public_key_from_pkcs8_hew"
            | "hew_ed25519_sign_hew"
            | "hew_ed25519_verify_hew"
            // QUIC send: `data: bytes` passed as *const BytesTriple.
            | "hew_quic_stream_send"
            | "hew_quic_stream_send_timeout_hew"
            // compress: all _hew wrappers accept `bytes` inputs via *const BytesTriple.
            | "hew_gzip_compress_hew"
            | "hew_gzip_decompress_hew"
            | "hew_deflate_compress_hew"
            | "hew_deflate_decompress_hew"
            | "hew_zlib_compress_hew"
            | "hew_zlib_decompress_hew"
            // msgpack: `bytes` inputs passed as *const BytesTriple.
            | "hew_msgpack_to_json_hew"
            | "hew_msgpack_encode_bytes_hew"
    )
}

/// Runtime consumers that take a `bytes` value BY STRUCT EXPANSION — the Rust
/// side expects `(ptr: *mut u8, offset: u32, len: u32)` as three separate
/// parameters matching the `#[repr(C)] BytesTriple` field layout.
///
/// WHY this is a separate category: these functions take `bytes` as the FIRST
/// (or only) argument. On AArch64 and SysV x86-64, a `{ptr, i32, i32}` struct
/// passed by value as the first argument expands into three registers matching
/// the Rust `(ptr, offset, len)` signature exactly. This is confirmed correct
/// for the listed symbols; do NOT list a symbol here unless the Rust impl
/// literally receives `(ptr: *mut u8, offset: u32, len: u32)` (or repeated
/// triples for multiple `bytes` params).
///
/// WHEN OBSOLETE: same as `is_bytes_by_pointer_consumer` — once codegen emits
/// proper `byval`/coerced-int lowering for struct params this distinction
/// collapses. The list is an explicit registry so unknown symbols fail closed
/// rather than silently mismatching.
pub(crate) fn is_bytes_struct_expansion_consumer(symbol: &str) -> bool {
    matches!(
        symbol,
        // crypto: input `bytes` params expand to (ptr, offset, len).
        "hew_sha256_hew"
            | "hew_sha384_hew"
            | "hew_sha512_hew"
            | "hew_hmac_sha256_hew"
            | "hew_constant_time_eq_hew"
            // encrypt: `key: bytes` and optional `ciphertext: bytes` each expand.
            | "hew_encrypt_seal_base64_hew"
            | "hew_encrypt_open_hew"
            | "hew_encrypt_try_open_hew"
            | "hew_encrypt_must_open_hew"
    )
}

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
            // triple alloca's ADDRESS (a plain `ptr`) — the by-pointer bytes-
            // consumer convention (`is_bytes_by_pointer_consumer`). By value the
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

        // Allowlisted but not wired (no codegen lowering arm yet), plus the
        // pre-staged intercept families whose calls ride `Terminator::Call`
        // and can never legally arrive as `Instr::CallRuntimeAbi` (their
        // symbols are not in `MIR_EMITTER_RUNTIME_SYMBOLS`, so
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
