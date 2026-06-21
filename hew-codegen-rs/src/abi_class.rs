//! Aggregate C-ABI classification for the LLVM backend.
//!
//! Replaces the hand-encoded, per-symbol, per-target aggregate-passing decisions
//! (the three `bytes`-ABI allowlists + the `_raw` out-pointer symbol family) with
//! ONE classifier keyed on `(struct type, target)`. The classifier decides how an
//! aggregate crosses the C-ABI boundary — passed/returned directly, in a register
//! pair, coerced to an integer, or indirectly via a hidden pointer (`sret` for
//! returns, `byval` for arguments) — and [`apply_return_attrs`]/[`apply_arg_attrs`]
//! attach the matching LLVM attributes so LLVM's own target lowering does the work
//! the frontend currently reverse-engineers by hand.
//!
//! ## Why this exists
//!
//! Today an aggregate like `BytesTriple { ptr, u32, u32 }` (16 bytes) or
//! `ChildLookupResult { u8, u8, [u8;6], ptr }` (16 bytes) is passed by a
//! per-symbol convention: hand-declared `{ i64, i64 }` register-pair returns on
//! SysV/AAPCS, and a duplicate `_raw` void+out-pointer symbol to dodge the
//! Windows x64 MSVC sret ABI. Both 16-byte aggregates sit exactly on the
//! SysV-register-pair / MSVC-indirect fault line — the line the `_raw` family was
//! born to straddle. A real `sret(T)`/`byval(T)` attribute makes the canonical
//! symbol correct on every target, deleting the duplicate family.
//!
//! ## Fail-closed posture
//!
//! [`classify_aggregate`] returns `Err(CodegenError::FailClosed)` for any target
//! whose aggregate ABI is not modelled. It NEVER guesses. An unmodelled target is
//! a build error, not a silent miscompile.
//!
//! ## ABI references
//!
//! - System V AMD64 (`x86_64-unknown-linux-gnu`): §3.2.3 argument passing — an
//!   aggregate ≤ 16 bytes with no unaligned/SSE fields is classified INTEGER and
//!   returned/passed in up to two registers (`rax:rdx` / two arg registers);
//!   larger is MEMORY (caller-allocated, hidden pointer). Mirrors
//!   `rustc_codegen_llvm`'s `cast_target` / `PassMode::Indirect`.
//! - AArch64 AAPCS (`aarch64-apple-darwin`): a "small" aggregate ≤ 16 bytes is
//!   passed in up to two consecutive registers (`x0:x1`); larger is passed
//!   indirectly. Same 16-byte boundary as SysV for our aggregates.
//! - Windows x64 (`*-windows-msvc`): an aggregate is passed/returned in a single
//!   register ONLY if its size is exactly 1, 2, 4, or 8 bytes; EVERY other size
//!   (including 16) is passed/returned INDIRECTLY via a caller-allocated hidden
//!   pointer. This is `WinX86_64ABIInfo::classify` in Clang. The 16-byte
//!   aggregates therefore go indirect on MSVC where they go register-pair on
//!   SysV/AAPCS — the exact divergence the `_raw` family compensates for.
//! - wasm32 (`wasm32-unknown-unknown`): the C ABI passes an aggregate that is not
//!   a single scalar indirectly (by pointer). LLVM's wasm backend lowers a
//!   by-value struct argument/return via a pointer when it is not a single
//!   element. We classify multi-field aggregates as indirect on wasm32.

// SHIM(R5): the classification layer lands additively. The return path is wired
// (`declare_aggregate_return` drives `hew_supervisor_child_get`'s classified
// return in `runtime_abi.rs`); the argument path (`apply_arg_attrs`) and the
// `CoercedInt` variant land with the by-value `bytes`-arg migration. Until every
// helper has a production consumer the not-yet-wired ones read as dead code.
// WHEN OBSOLETE: removed the commit the last consumer (the `bytes`-arg migration)
// lands.
#![allow(dead_code)]

use inkwell::attributes::AttributeLoc;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::targets::TargetData;
use inkwell::types::{AnyTypeEnum, StructType};
use inkwell::values::FunctionValue;

use crate::llvm::{CodegenError, CodegenResult};

/// The C-ABI classification of an aggregate for a concrete target.
///
/// Returns and arguments share the bucket vocabulary; the SITE (return vs arg)
/// decides which LLVM attribute an `Indirect` class lowers to (`sret` vs
/// `byval`). The classifier is pure — it makes no LLVM edits; the
/// `apply_*_attrs` helpers do.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AbiClass {
    /// The aggregate fits in a single register and is passed/returned by value
    /// as its natural LLVM struct type (no attribute, no coercion). Used for
    /// 1/2/4/8-byte aggregates on every target.
    Direct,
    /// The aggregate fits in two registers and is passed/returned by value as a
    /// `[2 x i64]` (or the natural two-eightbyte struct) — SysV/AAPCS ≤ 16-byte
    /// aggregates. No `sret`/`byval`; LLVM lowers the by-value struct into the
    /// register pair.
    RegisterPair,
    /// The aggregate is coerced to a single integer of `bits` width and passed
    /// by value as that integer. Reserved for sub-eightbyte aggregates that the
    /// SysV INTEGER class packs into one register; not currently produced for
    /// the 16-byte corpus but modelled for completeness.
    ///
    /// SHIM(R5): never produced by `classify_aggregate` yet — the runtime-symbol
    /// corpus (BytesTriple, ChildLookupResult) has no sub-eightbyte aggregate
    /// that coerces to a single packed integer. WHEN OBSOLETE: when a corpus
    /// symbol returns/takes a 3-7-byte aggregate the SysV INTEGER class packs
    /// into one register, this variant carries it. WHAT: a real coerced-int
    /// classification arm in `classify_aggregate` keyed on the eightbyte
    /// classification, not just total size.
    CoercedInt { bits: u32 },
    /// The aggregate is passed/returned INDIRECTLY via a caller-allocated hidden
    /// pointer. At a return site this lowers to an `sret(T)` parameter
    /// (`noalias`); at an argument site to a `byval(T)` parameter. Used for
    /// every >16-byte aggregate (all targets) and every aggregate larger than a
    /// single register on Windows x64 MSVC (size != 1/2/4/8) and wasm32.
    Indirect,
}

/// The byte size above which SysV / AAPCS pass an aggregate indirectly.
const SMALL_AGGREGATE_MAX: u64 = 16;

/// Returns `true` when `triple` names the Windows x64 MSVC environment, whose
/// aggregate ABI differs from SysV/AAPCS (every non-{1,2,4,8}-byte aggregate is
/// indirect). Matches on the `-msvc` environment suffix, not the arch, because
/// only the MSVC environment carries this rule.
fn is_windows_msvc(triple: &str) -> bool {
    triple.contains("windows-msvc") || triple.ends_with("-msvc")
}

/// Returns `true` for any Windows triple. Used to keep `*-windows-gnu`
/// (Win64-GNU / MinGW) OUT of the SysV/AAPCS arm: Win64-GNU uses the Win64
/// aggregate ABI (indirect for size ∉ {1,2,4,8}), NOT SysV. Hew targets
/// Windows via MSVC, so Win64-GNU is an unsupported target and must fail
/// closed rather than be misclassified as SysV.
fn is_windows(triple: &str) -> bool {
    triple.contains("windows") || triple.contains("-win32")
}

/// Returns `true` when `triple` names a wasm32 target.
fn is_wasm32(triple: &str) -> bool {
    triple.starts_with("wasm32")
}

/// Classify how `struct_ty` crosses the C-ABI boundary on `triple`.
///
/// `target_data` supplies the ABI size (the size operand is the only datum the
/// register-pair-vs-indirect decision needs for our corpus). `triple` selects
/// the per-target rule.
///
/// # Errors
///
/// Returns [`CodegenError::FailClosed`] if `triple` names a target whose
/// aggregate ABI is not modelled. The classifier never guesses an ABI.
pub(crate) fn classify_aggregate(
    struct_ty: StructType<'_>,
    target_data: &TargetData,
    triple: &str,
) -> CodegenResult<AbiClass> {
    let size = target_data.get_abi_size(&struct_ty);

    // Windows x64 MSVC: a single-register class ONLY for size exactly 1/2/4/8;
    // every other size (notably 16) is indirect. This is the rule the `_raw`
    // family exists to satisfy — the canonical symbol with `sret(T)`/`byval(T)`
    // replaces it.
    if is_windows_msvc(triple) {
        return Ok(match size {
            1 | 2 | 4 | 8 => AbiClass::Direct,
            _ => AbiClass::Indirect,
        });
    }

    // wasm32: a multi-field aggregate is passed/returned by pointer. A
    // single-element aggregate degenerates to its element (Direct). The wasm C
    // ABI does not have the SysV two-register class.
    if is_wasm32(triple) {
        return Ok(match size {
            1 | 2 | 4 | 8 => AbiClass::Direct,
            _ => AbiClass::Indirect,
        });
    }

    // SysV AMD64 (x86_64-*-linux-gnu / -darwin) and AArch64 AAPCS
    // (aarch64-*-darwin / -linux): aggregates ≤ 16 bytes go in up to two
    // registers; larger go indirect. We treat both arches under the one
    // small-aggregate rule because our entire corpus (BytesTriple,
    // ChildLookupResult — both 16 bytes, two eightbytes, no SSE/float fields)
    // classifies identically on SysV and AAPCS.
    if is_sysv_or_aapcs(triple) {
        return Ok(match size {
            0 => AbiClass::Direct, // empty aggregate — degenerate, no register used
            1 | 2 | 4 | 8 => AbiClass::Direct,
            n if n <= SMALL_AGGREGATE_MAX => AbiClass::RegisterPair,
            _ => AbiClass::Indirect,
        });
    }

    Err(CodegenError::FailClosed(format!(
        "classify_aggregate: aggregate ABI for target `{triple}` is not modelled \
         (size {size}). Model the target's aggregate-passing rule before emitting \
         a by-value aggregate for it — the classifier never guesses an ABI. \
         (LESSONS: boundary-fail-closed)"
    )))
}

/// Returns `true` for the SysV AMD64 and AArch64 AAPCS targets whose 16-byte
/// small-aggregate register-pair rule the classifier applies. Conservative: only
/// the architectures whose aggregate ABI is proven against the corpus are
/// admitted; anything else falls through to the fail-closed arm.
fn is_sysv_or_aapcs(triple: &str) -> bool {
    let is_x86_64 = triple.starts_with("x86_64-") || triple.starts_with("amd64-");
    let is_aarch64 = triple.starts_with("aarch64-") || triple.starts_with("arm64-");
    // -gnu / -darwin / -musl / bare (no environment) on these arches use the
    // SysV/AAPCS small-aggregate rule. EXCLUDE every Windows triple: MSVC was
    // already handled above (Indirect), and `*-windows-gnu` (Win64-GNU/MinGW)
    // uses the Win64 aggregate ABI, NOT SysV — admitting it here would emit a
    // register-pair where the platform expects indirect. Hew targets Windows
    // via MSVC, so Win64-GNU falls through to the fail-closed arm.
    (is_x86_64 || is_aarch64) && !is_windows(triple)
}

/// Look up an LLVM enum attribute kind id by name, failing closed if this LLVM
/// build does not expose it. Mirrors `coro.rs`'s `presplitcoroutine` lookup.
fn enum_kind_id(name: &str) -> CodegenResult<u32> {
    let id = inkwell::attributes::Attribute::get_named_enum_kind_id(name);
    if id == 0 {
        return Err(CodegenError::FailClosed(format!(
            "`{name}` is not a known LLVM attribute in this build — the aggregate \
             ABI lowering requires it. (LESSONS: boundary-fail-closed)"
        )));
    }
    Ok(id)
}

/// Attach the return-site ABI attributes for `class` to `fn_value`'s sret
/// parameter when the return is indirect.
///
/// For an [`AbiClass::Indirect`] return the caller allocates the result slot and
/// passes its address as a hidden FIRST parameter; this helper marks that
/// parameter `sret(pointee_ty)` + `noalias`. `pointee_ty` is the aggregate's LLVM
/// struct type (the sret pointee — required in opaque-pointer mode where the
/// pointer itself carries no pointee type). For Direct/RegisterPair/CoercedInt
/// returns this is a no-op (the value is returned by value).
///
/// `sret_param_index` is the 0-based index of the hidden sret pointer parameter
/// in the declared function (0 when sret is the only/first param).
pub(crate) fn apply_return_attrs<'ctx>(
    ctx: &'ctx Context,
    fn_value: FunctionValue<'ctx>,
    class: AbiClass,
    pointee_ty: AnyTypeEnum<'ctx>,
    sret_param_index: u32,
) -> CodegenResult<()> {
    if class != AbiClass::Indirect {
        return Ok(());
    }
    let sret_id = enum_kind_id("sret")?;
    let noalias_id = enum_kind_id("noalias")?;
    let sret_attr = ctx.create_type_attribute(sret_id, pointee_ty);
    let noalias_attr = ctx.create_enum_attribute(noalias_id, 0);
    fn_value.add_attribute(AttributeLoc::Param(sret_param_index), sret_attr);
    fn_value.add_attribute(AttributeLoc::Param(sret_param_index), noalias_attr);
    Ok(())
}

/// Attach the argument-site ABI attribute for `class` to `fn_value`'s parameter
/// at `param_index` when the argument is passed indirectly.
///
/// For an [`AbiClass::Indirect`] argument the caller passes the address of a
/// caller-owned copy and the callee receives it `byval(pointee_ty)` — LLVM emits
/// the ABI-correct copy. For Direct/RegisterPair/CoercedInt arguments this is a
/// no-op (the value is passed by value / in registers).
pub(crate) fn apply_arg_attrs<'ctx>(
    ctx: &'ctx Context,
    fn_value: FunctionValue<'ctx>,
    class: AbiClass,
    pointee_ty: AnyTypeEnum<'ctx>,
    param_index: u32,
) -> CodegenResult<()> {
    if class != AbiClass::Indirect {
        return Ok(());
    }
    let byval_id = enum_kind_id("byval")?;
    let byval_attr = ctx.create_type_attribute(byval_id, pointee_ty);
    fn_value.add_attribute(AttributeLoc::Param(param_index), byval_attr);
    Ok(())
}

/// Expose the aggregate's ABI size for callers that need it independently of the
/// class (e.g. to choose the `[N x i64]` register-pair carrier shape). Pure
/// passthrough to `TargetData::get_abi_size`.
pub(crate) fn aggregate_abi_size(struct_ty: StructType<'_>, target_data: &TargetData) -> u64 {
    target_data.get_abi_size(&struct_ty)
}

/// How a classified aggregate RETURN is declared at the LLVM boundary, and how
/// the caller reads the result. Returned by [`declare_aggregate_return`] so the
/// declaration edge and the call edge agree on one decision.
#[derive(Debug, Clone, Copy)]
pub(crate) enum AggregateReturnAbi<'ctx> {
    /// The aggregate is returned by value in a register pair. The declared
    /// function returns `carrier` (an `[N x i64]` matching the aggregate's
    /// eightbyte count); the caller extracts each eightbyte from the returned
    /// value. No sret parameter.
    RegisterPair {
        carrier: inkwell::types::ArrayType<'ctx>,
    },
    /// The aggregate is returned indirectly. The declared function returns void
    /// with a leading `sret(aggregate)` pointer parameter; the caller allocates
    /// the result slot, passes its address, and reads the aggregate from it.
    Sret,
}

/// Declare (or fetch) `symbol` as an aggregate-returning runtime function whose
/// return ABI is chosen by [`classify_aggregate`] for `triple`, attaching the
/// sret attribute when indirect.
///
/// `aggregate_ty` is the true LLVM struct type of the returned aggregate (e.g.
/// `ChildLookupResult` / `BytesTriple`). `non_sret_params` are the function's
/// declared parameters EXCLUDING any sret pointer (the helper prepends the sret
/// pointer parameter itself when the class is indirect).
///
/// Returns the `FunctionValue` and an [`AggregateReturnAbi`] telling the caller
/// how to issue the call and read the result.
///
/// # Errors
///
/// Fails closed if the target's aggregate ABI is unmodelled, if the aggregate is
/// classified `Direct`/`CoercedInt` (not yet produced for the runtime corpus —
/// add the arm when a corpus symbol needs it), or if the required LLVM attribute
/// is unavailable in this build.
pub(crate) fn declare_aggregate_return<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &inkwell::module::Module<'ctx>,
    target_data: &TargetData,
    triple: &str,
    symbol: &str,
    aggregate_ty: StructType<'ctx>,
    non_sret_params: &[inkwell::types::BasicMetadataTypeEnum<'ctx>],
) -> CodegenResult<(FunctionValue<'ctx>, AggregateReturnAbi<'ctx>)> {
    let class = classify_aggregate(aggregate_ty, target_data, triple)?;
    let ptr_ty = ctx.ptr_type(inkwell::AddressSpace::default());
    let i64_ty = ctx.i64_type();
    match class {
        AbiClass::RegisterPair => {
            // Carrier is an `[N x i64]` matching the aggregate's eightbyte count,
            // which LLVM returns in the same N-register pair the Rust callee uses
            // for a `#[repr(C)]` aggregate of this size (SysV/AAPCS INTEGER class).
            let size = target_data.get_abi_size(&aggregate_ty);
            let eightbytes = u32::try_from(size.div_ceil(8)).map_err(|_| {
                CodegenError::FailClosed(format!(
                    "declare_aggregate_return: aggregate `{symbol}` size {size} overflows the \
                     eightbyte-count carrier computation"
                ))
            })?;
            let carrier = i64_ty.array_type(eightbytes);
            let fn_ty = carrier.fn_type(non_sret_params, false);
            let fv = llvm_mod
                .get_function(symbol)
                .unwrap_or_else(|| llvm_mod.add_function(symbol, fn_ty, Some(Linkage::External)));
            Ok((fv, AggregateReturnAbi::RegisterPair { carrier }))
        }
        AbiClass::Indirect => {
            // void(ptr sret(aggregate) noalias, non_sret_params...).
            let mut params: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
                Vec::with_capacity(non_sret_params.len() + 1);
            params.push(ptr_ty.into());
            params.extend_from_slice(non_sret_params);
            let fn_ty = ctx.void_type().fn_type(&params, false);
            let fv = llvm_mod
                .get_function(symbol)
                .unwrap_or_else(|| llvm_mod.add_function(symbol, fn_ty, Some(Linkage::External)));
            apply_return_attrs(ctx, fv, class, aggregate_ty.into(), 0)?;
            Ok((fv, AggregateReturnAbi::Sret))
        }
        AbiClass::Direct | AbiClass::CoercedInt { .. } => Err(CodegenError::FailClosed(format!(
            "declare_aggregate_return: aggregate `{symbol}` classified {class:?} on `{triple}` — \
             the Direct/CoercedInt return path is not yet wired for the runtime corpus. Add the \
             arm when a corpus symbol needs it. (LESSONS: boundary-fail-closed)"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::targets::{CodeModel, RelocMode};
    use inkwell::targets::{InitializationConfig, Target, TargetTriple};
    use inkwell::OptimizationLevel;

    /// One-time LLVM target init so cross-target `TargetData` can be built on any
    /// host. Mirrors `llvm.rs::initialise_llvm_targets`.
    fn init_targets() {
        use std::sync::OnceLock;
        static INIT: OnceLock<()> = OnceLock::new();
        INIT.get_or_init(|| {
            Target::initialize_all(&InitializationConfig::default());
        });
    }

    /// Build a `TargetData` for an arbitrary triple on this host (works for
    /// cross targets because `Target::initialize_all` registers every backend).
    fn target_data_for(triple: &str) -> inkwell::targets::TargetData {
        init_targets();
        let tt = TargetTriple::create(triple);
        let target =
            Target::from_triple(&tt).unwrap_or_else(|e| panic!("from_triple({triple}): {e:?}"));
        let machine = target
            .create_target_machine(
                &tt,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap_or_else(|| panic!("create_target_machine({triple}) returned None"));
        machine.get_target_data()
    }

    /// The `BytesTriple { ptr, u32, u32 }` layout — 16 bytes on 64-bit targets.
    fn bytes_triple<'c>(ctx: &'c Context) -> StructType<'c> {
        let ptr = ctx.ptr_type(inkwell::AddressSpace::default());
        let i32t = ctx.i32_type();
        ctx.struct_type(&[ptr.into(), i32t.into(), i32t.into()], false)
    }

    /// The `ChildLookupResult { u8, u8, [u8;6], ptr }` layout — 16 bytes.
    fn child_lookup<'c>(ctx: &'c Context) -> StructType<'c> {
        let i8t = ctx.i8_type();
        let pad = i8t.array_type(6);
        let ptr = ctx.ptr_type(inkwell::AddressSpace::default());
        ctx.struct_type(&[i8t.into(), i8t.into(), pad.into(), ptr.into()], false)
    }

    /// An 8-byte single-eightbyte aggregate `{ i32, i32 }`.
    fn pair_i32<'c>(ctx: &'c Context) -> StructType<'c> {
        let i32t = ctx.i32_type();
        ctx.struct_type(&[i32t.into(), i32t.into()], false)
    }

    /// A >16-byte aggregate `{ ptr, ptr, ptr }` (24 bytes).
    fn big_three_ptr<'c>(ctx: &'c Context) -> StructType<'c> {
        let ptr = ctx.ptr_type(inkwell::AddressSpace::default());
        ctx.struct_type(&[ptr.into(), ptr.into(), ptr.into()], false)
    }

    const SYSV: &str = "x86_64-unknown-linux-gnu";
    const AARCH64_DARWIN: &str = "aarch64-apple-darwin";
    const WIN_MSVC: &str = "x86_64-pc-windows-msvc";
    const WASM32: &str = "wasm32-unknown-unknown";

    #[test]
    fn bytes_triple_is_register_pair_on_sysv() {
        let ctx = Context::create();
        let td = target_data_for(SYSV);
        assert_eq!(
            td.get_abi_size(&bytes_triple(&ctx)),
            16,
            "BytesTriple must be 16 bytes on SysV"
        );
        assert_eq!(
            classify_aggregate(bytes_triple(&ctx), &td, SYSV).unwrap(),
            AbiClass::RegisterPair
        );
    }

    #[test]
    fn bytes_triple_is_register_pair_on_aarch64_darwin() {
        let ctx = Context::create();
        let td = target_data_for(AARCH64_DARWIN);
        assert_eq!(
            classify_aggregate(bytes_triple(&ctx), &td, AARCH64_DARWIN).unwrap(),
            AbiClass::RegisterPair
        );
    }

    #[test]
    fn bytes_triple_is_indirect_on_windows_msvc() {
        let ctx = Context::create();
        let td = target_data_for(WIN_MSVC);
        // The deletion proof: the 16-byte aggregate goes INDIRECT on MSVC where
        // it is a register pair on SysV/AAPCS — exactly why the `_raw` family
        // exists. `sret(T)` makes the canonical symbol correct here.
        assert_eq!(
            classify_aggregate(bytes_triple(&ctx), &td, WIN_MSVC).unwrap(),
            AbiClass::Indirect
        );
    }

    #[test]
    fn child_lookup_is_register_pair_on_aarch64_darwin() {
        let ctx = Context::create();
        let td = target_data_for(AARCH64_DARWIN);
        assert_eq!(td.get_abi_size(&child_lookup(&ctx)), 16);
        assert_eq!(
            classify_aggregate(child_lookup(&ctx), &td, AARCH64_DARWIN).unwrap(),
            AbiClass::RegisterPair
        );
    }

    #[test]
    fn child_lookup_is_indirect_on_windows_msvc() {
        let ctx = Context::create();
        let td = target_data_for(WIN_MSVC);
        assert_eq!(
            classify_aggregate(child_lookup(&ctx), &td, WIN_MSVC).unwrap(),
            AbiClass::Indirect
        );
    }

    #[test]
    fn eight_byte_pair_is_direct_everywhere() {
        let ctx = Context::create();
        for triple in [SYSV, AARCH64_DARWIN, WIN_MSVC, WASM32] {
            let td = target_data_for(triple);
            assert_eq!(td.get_abi_size(&pair_i32(&ctx)), 8, "{triple}");
            assert_eq!(
                classify_aggregate(pair_i32(&ctx), &td, triple).unwrap(),
                AbiClass::Direct,
                "8-byte aggregate must be Direct on {triple}"
            );
        }
    }

    #[test]
    fn over_16_byte_aggregate_is_indirect_on_sysv() {
        let ctx = Context::create();
        let td = target_data_for(SYSV);
        assert!(td.get_abi_size(&big_three_ptr(&ctx)) > 16);
        // NEGATIVE: a >16-byte aggregate must NOT be a register pair.
        let class = classify_aggregate(big_three_ptr(&ctx), &td, SYSV).unwrap();
        assert_eq!(class, AbiClass::Indirect);
        assert_ne!(class, AbiClass::RegisterPair);
    }

    #[test]
    fn over_16_byte_aggregate_is_indirect_on_aarch64() {
        let ctx = Context::create();
        let td = target_data_for(AARCH64_DARWIN);
        assert_eq!(
            classify_aggregate(big_three_ptr(&ctx), &td, AARCH64_DARWIN).unwrap(),
            AbiClass::Indirect
        );
    }

    #[test]
    fn bytes_triple_is_indirect_on_wasm32() {
        let ctx = Context::create();
        let td = target_data_for(WASM32);
        // wasm32 is 32-bit: BytesTriple { ptr(4), u32(4), u32(4) } = 12 bytes,
        // a multi-field aggregate the wasm C ABI passes by pointer.
        let class = classify_aggregate(bytes_triple(&ctx), &td, WASM32).unwrap();
        assert_eq!(class, AbiClass::Indirect, "wasm32 BytesTriple class");
        assert_ne!(class, AbiClass::RegisterPair);
    }

    #[test]
    fn unmodelled_target_fails_closed() {
        let ctx = Context::create();
        // Use SysV TargetData (size only); the triple drives the rule. A target
        // the classifier does not model must fail closed, never guess.
        let td = target_data_for(SYSV);
        let err = classify_aggregate(bytes_triple(&ctx), &td, "sparc64-unknown-linux-gnu")
            .expect_err("an unmodelled target must fail closed");
        match err {
            CodegenError::FailClosed(msg) => {
                assert!(msg.contains("not modelled"), "msg: {msg}");
            }
            other => panic!("expected FailClosed, got {other:?}"),
        }
    }

    #[test]
    fn win64_gnu_fails_closed_not_classified_sysv() {
        // `x86_64-pc-windows-gnu` (Win64-GNU / MinGW) uses the Win64 aggregate
        // ABI (indirect for size ∉ {1,2,4,8}), NOT SysV. It is an unsupported
        // target (Hew targets Windows via MSVC), so it must fail closed — NOT
        // be admitted to the SysV register-pair arm. This is the cross-eco ABI
        // correctness fix: an x86_64 non-MSVC Windows triple is no longer
        // misclassified as SysV-like.
        let ctx = Context::create();
        let td = target_data_for(SYSV); // size only; the triple drives the rule
        assert!(
            !is_sysv_or_aapcs("x86_64-pc-windows-gnu"),
            "Win64-GNU must NOT be admitted to the SysV/AAPCS arm"
        );
        let err = classify_aggregate(bytes_triple(&ctx), &td, "x86_64-pc-windows-gnu")
            .expect_err("Win64-GNU must fail closed, not classify as SysV");
        match err {
            CodegenError::FailClosed(msg) => {
                assert!(msg.contains("not modelled"), "msg: {msg}");
            }
            other => panic!("expected FailClosed for Win64-GNU, got {other:?}"),
        }
    }

    // ── Stage 0 spike, kept as a permanent regression ──────────────────────
    // Proves `create_type_attribute(sret/byval, %T)` round-trips through this
    // inkwell build's IR text and that the module verifies — the de-risk gate
    // the plan's Stage 0 requires before any production migration.

    #[test]
    fn sret_attr_round_trips_and_verifies() {
        let ctx = Context::create();
        let llvm_mod = ctx.create_module("sret_spike");
        let ptr = ctx.ptr_type(inkwell::AddressSpace::default());
        // void @indirect_ret(ptr sret(%BytesTriple) noalias %0, ptr %1)
        let triple_ty = bytes_triple(&ctx);
        let fn_ty = ctx.void_type().fn_type(&[ptr.into(), ptr.into()], false);
        let fv = llvm_mod.add_function("indirect_ret", fn_ty, None);
        apply_return_attrs(&ctx, fv, AbiClass::Indirect, triple_ty.into(), 0).expect("apply sret");
        // Give it a trivial body so the module verifies.
        let entry = ctx.append_basic_block(fv, "entry");
        let builder = ctx.create_builder();
        builder.position_at_end(entry);
        builder.build_return(None).unwrap();

        let ir = llvm_mod.print_to_string().to_string();
        assert!(ir.contains("sret("), "IR must contain sret(...):\n{ir}");
        assert!(ir.contains("noalias"), "IR must contain noalias:\n{ir}");
        assert!(llvm_mod.verify().is_ok(), "sret module must verify:\n{ir}");
    }

    #[test]
    fn byval_attr_round_trips_and_verifies() {
        let ctx = Context::create();
        let llvm_mod = ctx.create_module("byval_spike");
        let ptr = ctx.ptr_type(inkwell::AddressSpace::default());
        let triple_ty = bytes_triple(&ctx);
        // void @indirect_arg(ptr %handle, ptr byval(%BytesTriple) %1)
        let fn_ty = ctx.void_type().fn_type(&[ptr.into(), ptr.into()], false);
        let fv = llvm_mod.add_function("indirect_arg", fn_ty, None);
        apply_arg_attrs(&ctx, fv, AbiClass::Indirect, triple_ty.into(), 1).expect("apply byval");
        let entry = ctx.append_basic_block(fv, "entry");
        let builder = ctx.create_builder();
        builder.position_at_end(entry);
        builder.build_return(None).unwrap();

        let ir = llvm_mod.print_to_string().to_string();
        assert!(ir.contains("byval("), "IR must contain byval(...):\n{ir}");
        assert!(llvm_mod.verify().is_ok(), "byval module must verify:\n{ir}");
    }

    #[test]
    fn register_pair_return_emits_no_sret() {
        // NEGATIVE: a RegisterPair class must NOT attach sret — the historical
        // trap-206 shape (a register-pair return wrongly forced indirect, or an
        // indirect return wrongly emitted as a register pair) is what this guards.
        let ctx = Context::create();
        let llvm_mod = ctx.create_module("regpair_spike");
        let i64t = ctx.i64_type();
        let ptr = ctx.ptr_type(inkwell::AddressSpace::default());
        let ret_ty = ctx.struct_type(&[i64t.into(), i64t.into()], false);
        let fn_ty = ret_ty.fn_type(&[ptr.into()], false);
        let fv = llvm_mod.add_function("regpair_ret", fn_ty, None);
        let triple_ty = bytes_triple(&ctx);
        apply_return_attrs(&ctx, fv, AbiClass::RegisterPair, triple_ty.into(), 0)
            .expect("register-pair return is a no-op");
        let entry = ctx.append_basic_block(fv, "entry");
        let builder = ctx.create_builder();
        builder.position_at_end(entry);
        builder.build_return(Some(&ret_ty.const_zero())).unwrap();

        let ir = llvm_mod.print_to_string().to_string();
        assert!(
            !ir.contains("sret("),
            "RegisterPair return must NOT emit sret:\n{ir}"
        );
        assert!(llvm_mod.verify().is_ok());
    }
}
