//! Integer/float arithmetic helpers for the LLVM backend.
//!
//! Pure relocation of the already-free arithmetic helpers out of [`crate::llvm`]
//! (R4 god-module carve). The per-instruction integer/float binary lowering
//! still lives in `crate::llvm::lower_instruction`'s match arms; only the
//! standalone helpers those arms call live here:
//!
//! - overflow / saturating intrinsic-name selection
//!   ([`overflow_intrinsic_name`], [`saturating_intrinsic_name`]),
//! - the signed/unsigned saturation bound ([`saturating_bound`]),
//! - the width-changing saturating cast lowering
//!   ([`lower_saturating_width_cast`]),
//! - the `std.mem` floor-intrinsic body synthesis ([`FloorIntrinsic`],
//!   [`emit_floor_intrinsic_body`]).
//!
//! Cross-module references stay `crate::`-qualified exactly as `crate::coro`
//! does: this module imports the shared codegen types/helpers from
//! `crate::llvm`, and `crate::llvm` calls back into `crate::arith::*`.

use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue};
use inkwell::IntPredicate;

use hew_mir::{IntArithOp, IntSignedness, Place, RawMirFunction};
use hew_types::ResolvedTy;

use crate::llvm::{
    expect_int_type, intern_runtime_decl, place_pointer, primitive_to_llvm, CodegenError,
    CodegenResult, FnCtx, LlvmResultExt, RuntimeDeclMap,
};

pub(crate) fn overflow_intrinsic_name(op: IntArithOp, signed: IntSignedness) -> &'static str {
    match (op, signed) {
        (IntArithOp::Add, IntSignedness::Signed) => "llvm.sadd.with.overflow",
        (IntArithOp::Add, IntSignedness::Unsigned) => "llvm.uadd.with.overflow",
        (IntArithOp::Sub, IntSignedness::Signed) => "llvm.ssub.with.overflow",
        (IntArithOp::Sub, IntSignedness::Unsigned) => "llvm.usub.with.overflow",
        (IntArithOp::Mul, IntSignedness::Signed) => "llvm.smul.with.overflow",
        (IntArithOp::Mul, IntSignedness::Unsigned) => "llvm.umul.with.overflow",
    }
}

/// Return the `llvm.{s,u}{add,sub}.sat` base name for Add/Sub.
/// LLVM does not provide `llvm.smul.sat`; callers must keep the
/// `.with.overflow` + select path for Mul. Panics on Mul to enforce the
/// call-site invariant (the `IntArithSaturating` arm only routes Add/Sub here).
pub(crate) fn saturating_intrinsic_name(op: IntArithOp, signed: IntSignedness) -> &'static str {
    match (op, signed) {
        (IntArithOp::Add, IntSignedness::Signed) => "llvm.sadd.sat",
        (IntArithOp::Add, IntSignedness::Unsigned) => "llvm.uadd.sat",
        (IntArithOp::Sub, IntSignedness::Signed) => "llvm.ssub.sat",
        (IntArithOp::Sub, IntSignedness::Unsigned) => "llvm.usub.sat",
        (IntArithOp::Mul, _) => {
            panic!("saturating_intrinsic_name: llvm.smul.sat does not exist; use overflow path for Mul")
        }
    }
}

pub(crate) fn saturating_bound<'ctx>(
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
                    .llvm_ctx("saturating add sign compare")?,
                IntArithOp::Sub => fn_ctx
                    .builder
                    .build_int_compare(IntPredicate::SLT, rhs_v, zero, "sat_sub_negative")
                    .llvm_ctx("saturating sub sign compare")?,
                IntArithOp::Mul => {
                    let lhs_neg = fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::SLT, lhs_v, zero, "sat_mul_lhs_neg")
                        .llvm_ctx("saturating mul lhs sign compare")?;
                    let rhs_neg = fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::SLT, rhs_v, zero, "sat_mul_rhs_neg")
                        .llvm_ctx("saturating mul rhs sign compare")?;
                    fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::EQ, lhs_neg, rhs_neg, "sat_mul_same_sign")
                        .llvm_ctx("saturating mul same-sign compare")?
                }
            };
            Ok(fn_ctx
                .builder
                .build_select(choose_max, max, min, "sat_signed_bound")
                .llvm_ctx("saturating bound select")?
                .into_int_value())
        }
    }
}

/// Saturating integer-to-integer width conversion.
///
/// Both `from_ty` and `to_ty` must be checker-admitted integers.
/// The source value is clamped to `[to_ty::MIN, to_ty::MAX]` before
/// the narrowing (or widening) cast.
///
/// Algorithm overview:
///
/// - **Widening** (`src_bits < dest_bits`): every source value is already
///   representable in the wider target — no clamping needed.  Just extend
///   (sext for signed sources, zext for unsigned sources).
///
/// - **Same-width** (`src_bits == dest_bits`) or **Narrowing**
///   (`src_bits > dest_bits`): the target bounds fit in the source-width
///   type, so work entirely in the source integer type.  Emit two
///   `cmp + select` pairs to clamp, then truncate / bit-cast as needed.
///
///   Bounds computation:
///   - Signed target  k bits: MIN = -(2^(k-1)),  MAX = 2^(k-1) - 1
///   - Unsigned target k bits: MIN = 0,           MAX = 2^k - 1
///
///   Represented as constants in the *source* integer type; they fit
///   because src_bits >= dest_bits in this path.
///
///   Comparisons use the *source* signedness (we are asking "does the
///   source value fall outside the target range?").
pub(crate) fn lower_saturating_width_cast(
    fn_ctx: &FnCtx<'_, '_>,
    dest: Place,
    src: Place,
    from_ty: &ResolvedTy,
    to_ty: &ResolvedTy,
) -> CodegenResult<()> {
    if !from_ty.is_integer() || !to_ty.is_integer() {
        return Err(CodegenError::FailClosed(format!(
            "SaturatingWidthCast requires integer types; got {} → {}",
            from_ty.user_facing(),
            to_ty.user_facing()
        )));
    }

    let (src_ptr, src_storage) = place_pointer(fn_ctx, src)?;
    let (dest_ptr, dest_storage) = place_pointer(fn_ctx, dest)?;
    let src_int_ty = expect_int_type(src_storage, "saturating width cast source")?;
    let dest_int_ty = expect_int_type(dest_storage, "saturating width cast dest")?;

    let expected_src = primitive_to_llvm(fn_ctx.ctx, fn_ctx.target_data, from_ty)?;
    let expected_dest = primitive_to_llvm(fn_ctx.ctx, fn_ctx.target_data, to_ty)?;
    if src_storage != expected_src {
        return Err(CodegenError::FailClosed(format!(
            "SaturatingWidthCast source storage {src_storage:?} disagrees with from_ty {}",
            from_ty.user_facing()
        )));
    }
    if dest_storage != expected_dest {
        return Err(CodegenError::FailClosed(format!(
            "SaturatingWidthCast dest storage {dest_storage:?} disagrees with to_ty {}",
            to_ty.user_facing()
        )));
    }

    let src_v = fn_ctx
        .builder
        .build_load(src_int_ty, src_ptr, "sat_wcast_src")
        .llvm_ctx("saturating width cast source load")?
        .into_int_value();

    let src_bits = src_int_ty.get_bit_width();
    let dest_bits = dest_int_ty.get_bit_width();
    let to_signed = to_ty.is_signed_integer();
    let from_signed = from_ty.is_signed_integer();

    let result: BasicValueEnum<'_> = if src_bits < dest_bits {
        // Widening: every source value fits in the target — no clamping.
        // Extend using source signedness (sext for signed, zext for unsigned).
        if from_signed {
            fn_ctx
                .builder
                .build_int_s_extend(src_v, dest_int_ty, "sat_wcast_sext")
                .llvm_ctx("saturating width cast sign extend")?
                .into()
        } else {
            fn_ctx
                .builder
                .build_int_z_extend(src_v, dest_int_ty, "sat_wcast_zext")
                .llvm_ctx("saturating width cast zero extend")?
                .into()
        }
    } else if src_bits == dest_bits && from_signed != to_signed {
        // Same-width sign change: the target range is NOT a subset of the source
        // range when interpreted in the source signedness, so we cannot compute
        // clamping bounds in the source type.  Each direction needs only ONE clamp:
        //
        //   signed → unsigned (e.g. i32 → u32):
        //     Target range [0, 2^k - 1].  Every non-negative source value fits
        //     exactly; negative source values clamp to 0.  No upper clamp is
        //     needed because 2^k - 1 is not representable as a signed k-bit value.
        //     Emit: max(src, 0) via SGT + select, then let the store bitcast.
        //
        //   unsigned → signed (e.g. u32 → i32):
        //     Target range [-(2^(k-1)), 2^(k-1)-1].  Source is always >= 0 so no
        //     lower clamp is needed.  Clamp high at 2^(k-1)-1 using UGT so the
        //     comparison sees the unsigned magnitude correctly.
        //     Emit: min(src, 2^(k-1)-1) via UGT + select, then let the store bitcast.
        if from_signed {
            // signed → unsigned: clamp low at 0 only.
            let zero = src_int_ty.const_zero();
            let below_zero = fn_ctx
                .builder
                .build_int_compare(IntPredicate::SLT, src_v, zero, "sat_swcast_below_zero")
                .llvm_ctx("saturating same-width sign cast below-zero compare")?;
            fn_ctx
                .builder
                .build_select(below_zero, zero, src_v, "sat_swcast_clamped")
                .llvm_ctx("saturating same-width sign cast clamp low select")?
                .into_int_value()
                .into()
        } else {
            // unsigned → signed: clamp high at 2^(k-1)-1 only.
            // The constant is representable as an unsigned k-bit value.
            let signed_max_val: u64 = (1_u64 << (dest_bits - 1)) - 1;
            let signed_max = src_int_ty.const_int(signed_max_val, false);
            let above_max = fn_ctx
                .builder
                .build_int_compare(IntPredicate::UGT, src_v, signed_max, "sat_swcast_above_max")
                .llvm_ctx("saturating same-width sign cast above-max compare")?;
            fn_ctx
                .builder
                .build_select(above_max, signed_max, src_v, "sat_swcast_clamped")
                .llvm_ctx("saturating same-width sign cast clamp high select")?
                .into_int_value()
                .into()
        }
    } else {
        // Narrowing (src_bits > dest_bits): clamp in the source integer type, then
        // truncate.  The target bounds always fit in the source type here because
        // src_bits > dest_bits.
        let (min_const, max_const) = if to_signed {
            // Signed target k bits: MIN = -(2^(k-1)), MAX = 2^(k-1) - 1
            let min_val: i64 = -1_i64 << (dest_bits - 1);
            let max_val: i64 = (1_i64 << (dest_bits - 1)) - 1;
            (
                src_int_ty.const_int(min_val as u64, true),
                src_int_ty.const_int(max_val as u64, true),
            )
        } else {
            // Unsigned target k bits: MIN = 0, MAX = 2^k - 1
            // dest_bits < src_bits <= 64, so the shift is safe.
            let max_val: u64 = (1_u64 << dest_bits) - 1;
            (
                src_int_ty.const_zero(),
                src_int_ty.const_int(max_val, false),
            )
        };

        // Clamp below: if src < min → use min.
        // Compare using source signedness.
        let below_min = if from_signed {
            fn_ctx
                .builder
                .build_int_compare(IntPredicate::SLT, src_v, min_const, "sat_wcast_below_min")
                .llvm_ctx("saturating width cast below-min compare")?
        } else {
            fn_ctx
                .builder
                .build_int_compare(IntPredicate::ULT, src_v, min_const, "sat_wcast_below_min")
                .llvm_ctx("saturating width cast below-min compare")?
        };
        let clamped_low = fn_ctx
            .builder
            .build_select(below_min, min_const, src_v, "sat_wcast_clamp_low")
            .llvm_ctx("saturating width cast clamp low select")?
            .into_int_value();

        // Clamp above: if clamped_low > max → use max.
        let above_max = if from_signed {
            fn_ctx
                .builder
                .build_int_compare(
                    IntPredicate::SGT,
                    clamped_low,
                    max_const,
                    "sat_wcast_above_max",
                )
                .llvm_ctx("saturating width cast above-max compare")?
        } else {
            fn_ctx
                .builder
                .build_int_compare(
                    IntPredicate::UGT,
                    clamped_low,
                    max_const,
                    "sat_wcast_above_max",
                )
                .llvm_ctx("saturating width cast above-max compare")?
        };
        let clamped = fn_ctx
            .builder
            .build_select(above_max, max_const, clamped_low, "sat_wcast_clamped")
            .llvm_ctx("saturating width cast clamp high select")?
            .into_int_value();

        fn_ctx
            .builder
            .build_int_truncate(clamped, dest_int_ty, "sat_wcast_trunc")
            .llvm_ctx("saturating width cast truncate")?
            .into()
    };

    fn_ctx
        .builder
        .build_store(dest_ptr, result)
        .llvm_ctx("saturating width cast store")?;
    Ok(())
}

/// The memory-intrinsic floor (`mem.*`, W5.005 / F1b) for which codegen
/// synthesizes a trampoline body directly from the catalog id threaded on
/// `RawMirFunction::intrinsic_id`.
///
/// This enum is the central fail-closed authority (D343): an `intrinsic_id`
/// that does not parse to one of these variants is a hard `CodegenError`, not
/// a silent empty-body no-op. The catalog (`stdlib_catalog`) and the
/// `std.mem` floor surface are the source of truth for the id strings; this
/// `match` is the codegen-side counterpart and must stay in lockstep with
/// them (the `unknown_floor_intrinsic_id_fails_closed` test guards the gap).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FloorIntrinsic {
    Alloc,
    Realloc,
    Dealloc,
    PtrOffset,
    PtrCopy,
}

impl FloorIntrinsic {
    pub(crate) fn from_catalog_id(id: &str) -> Option<Self> {
        match id {
            "mem.alloc" => Some(Self::Alloc),
            "mem.realloc" => Some(Self::Realloc),
            "mem.dealloc" => Some(Self::Dealloc),
            "mem.ptr_offset" => Some(Self::PtrOffset),
            "mem.ptr_copy" => Some(Self::PtrCopy),
            _ => None,
        }
    }
}

/// Synthesize the trampoline body for a `#[intrinsic("mem.*")]` memory-floor
/// function (W5.005 / F1b, Decision 4 Option A).
///
/// The MIR `blocks` of these functions are a bodyless placeholder — the
/// source declaration is `fn alloc(..) -> *mut u8 {}`. Codegen discards that
/// placeholder and emits the real lowering here, driven by the typed
/// `intrinsic` id threaded from HIR. Calls to `mem$alloc` (etc.) dispatch
/// normally to this defined function; the body is the substrate.
///
/// Pointer args/returns are opaque `ptr`; the integer args are u64-width
/// (`i64` in LLVM). Byte-level monomorphic (A612): `ptr_offset`/`ptr_copy`
/// count raw bytes — the caller has already multiplied by the descriptor
/// size, so no element-type scaling happens here.
///
/// Fail-closed (D343): the caller rejects an unrecognised id before reaching
/// this function, so every arm maps to a concrete lowering — there is no
/// silent empty-body path. A param-arity mismatch between the stub signature
/// and the lowering is also a hard error.
pub(crate) fn emit_floor_intrinsic_body<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    llvm_fn: FunctionValue<'ctx>,
    func: &RawMirFunction,
    intrinsic: FloorIntrinsic,
) -> CodegenResult<()> {
    let builder = ctx.create_builder();
    let entry = ctx.append_basic_block(llvm_fn, "entry");
    builder.position_at_end(entry);
    let mut decls = RuntimeDeclMap::new();
    let i8_ty = ctx.i8_type();

    let param = |i: u32| -> CodegenResult<BasicValueEnum<'ctx>> {
        llvm_fn.get_nth_param(i).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "floor intrinsic `{}` ({intrinsic:?}) has no LLVM parameter at index {i}; \
                 the `std.mem` stub signature and the codegen lowering disagree",
                func.name
            ))
        })
    };

    match intrinsic {
        FloorIntrinsic::Alloc => {
            let size = param(0)?;
            let align = param(1)?;
            let f = intern_runtime_decl(ctx, llvm_mod, &mut decls, "hew_alloc")?;
            let call = builder
                .build_call(f, &[size.into(), align.into()], "mem_alloc")
                .llvm_ctx("mem.alloc hew_alloc call")?;
            let ret = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_alloc returned void; expected ptr".into())
            })?;
            builder.build_return(Some(&ret)).llvm_ctx("mem.alloc ret")?;
        }
        FloorIntrinsic::Realloc => {
            let ptr = param(0)?;
            let old_size = param(1)?;
            let new_size = param(2)?;
            let align = param(3)?;
            let f = intern_runtime_decl(ctx, llvm_mod, &mut decls, "hew_realloc")?;
            let call = builder
                .build_call(
                    f,
                    &[ptr.into(), old_size.into(), new_size.into(), align.into()],
                    "mem_realloc",
                )
                .llvm_ctx("mem.realloc hew_realloc call")?;
            let ret = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_realloc returned void; expected ptr".into())
            })?;
            builder
                .build_return(Some(&ret))
                .llvm_ctx("mem.realloc ret")?;
        }
        FloorIntrinsic::Dealloc => {
            let ptr = param(0)?;
            let size = param(1)?;
            let align = param(2)?;
            let f = intern_runtime_decl(ctx, llvm_mod, &mut decls, "hew_dealloc")?;
            builder
                .build_call(f, &[ptr.into(), size.into(), align.into()], "mem_dealloc")
                .llvm_ctx("mem.dealloc hew_dealloc call")?;
            builder
                .build_return(Some(&i8_ty.const_zero()))
                .llvm_ctx("mem.dealloc ret")?;
        }
        FloorIntrinsic::PtrOffset => {
            let base = param(0)?.into_pointer_value();
            let byte_offset = param(1)?.into_int_value();
            // SAFETY: i8 in-bounds GEP by a raw byte count — this primitive IS
            // Rust's `<*mut T>::add` (inbounds). The result must stay within the
            // same allocation `base` points into (one-past-end permitted);
            // anything else is UB. Bounds are the caller's obligation (A605 —
            // unsafe primitives; the only callers are compiler-authored
            // containers). Cross-allocation/wrapping offset is a separate future
            // primitive, intentionally not provided here (YAGNI).
            let gep = unsafe {
                builder
                    .build_in_bounds_gep(i8_ty, base, &[byte_offset], "mem_ptr_offset")
                    .llvm_ctx("mem.ptr_offset i8 gep")?
            };
            builder
                .build_return(Some(&gep))
                .llvm_ctx("mem.ptr_offset ret")?;
        }
        FloorIntrinsic::PtrCopy => {
            let dst = param(0)?.into_pointer_value();
            let src = param(1)?.into_pointer_value();
            let byte_count = param(2)?.into_int_value();
            builder
                .build_memcpy(dst, 1, src, 1, byte_count)
                .llvm_ctx("mem.ptr_copy memcpy")?;
            builder
                .build_return(Some(&i8_ty.const_zero()))
                .llvm_ctx("mem.ptr_copy ret")?;
        }
    }
    Ok(())
}
