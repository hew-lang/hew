//! Cross-node payload serialization codec emission for the LLVM backend.
//!
//! Pure relocation (god-module carve) of the CBOR / wire-text serialize and
//! deserialize thunk emitters out of llvm.rs: the `__hew_cbor_serialize_*` /
//! `__hew_cbor_deserialize_*` C-ABI thunk pair emission, the wire-text
//! descriptor builders, and their layout-walk helpers. The byte-format
//! authority lives in `hew-runtime`; the layout-walk authority lives here.
//! `crate::llvm`'s module-build path constructs the pipeline and calls into
//! these emitters; this module imports the shared codegen context and helpers
//! from `crate::llvm`.
//!
//! No behaviour change — every emitted `.ll` is byte-identical before and
//! after the move.

use inkwell::context::Context;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

use hew_mir::{EnumLayout, RecordLayout};
use hew_runtime::internal::types::HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH;
use hew_types::{BuiltinType, ResolvedTy, WireLayoutTable, WireTextFormat};

#[allow(unused_imports)]
use crate::llvm::*;

// ===========================================================================
// Cross-node payload serialization thunks (gap #31)
// ===========================================================================
//
// For every actor-message type that can cross a process boundary, codegen emits
// a `__hew_cbor_serialize_<key>` / `__hew_cbor_deserialize_<key>` C-ABI thunk
// pair. The thunk WALKS the value's layout in IR (the single source of truth for
// field order and offsets) and calls the runtime codec primitives
// (`hew-runtime/src/cbor_serial.rs`) to build / read the wire bytes. The
// byte-format authority lives in the runtime; the layout-walk authority lives
// here. A module-init constructor registers each pair by its `msg_type`
// discriminant so the receive path can find the decoder.
//
// Accepted subset == the `Serializable` floor the checker already enforces:
// scalars, `string`, `bytes`, records (positional fields), enums (a tag + the
// active variant's positional fields). Any other shape fails closed at codegen.

/// Stable, symbol-safe key for a serializable message type, used as the
/// `__hew_cbor_serialize_<key>` / `__hew_cbor_deserialize_<key>` suffix. Reuses
/// the canonical structural `ResolvedTy` encoder so two distinct shapes never
/// collide on one thunk.
pub(crate) fn xnode_codec_key(ty: &ResolvedTy) -> String {
    hew_hir::mangle_resolved_ty(ty)
}

/// Declare a runtime codec primitive by name with the given LLVM signature.
pub(crate) fn declare_codec_prim<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    name: &str,
    fn_ty: inkwell::types::FunctionType<'ctx>,
) -> FunctionValue<'ctx> {
    let _ = ctx;
    llvm_mod
        .get_function(name)
        .unwrap_or_else(|| llvm_mod.add_function(name, fn_ty, Some(Linkage::External)))
}

/// Resolve a `Named { name, args }` to the layout-map registration key:
/// the plain (short) name for a monomorphic type, the `mangle`d name for a
/// generic instantiation. Mirrors `user_record_layout_key` / `lookup_enum_layout`.
fn xnode_registry_key(
    name: &str,
    args: &[ResolvedTy],
    records: &[RecordLayout],
    enums: &[EnumLayout],
) -> String {
    let short = short_name(name);
    if args.is_empty() {
        // Prefer an exact registered name; else the short form.
        if records.iter().any(|r| r.name == name) || enums.iter().any(|e| e.name == name) {
            return name.to_string();
        }
        short.to_string()
    } else {
        // Shorten the spine for the full-outer-name candidate too: registration
        // keys on bare args, so a qualified payload must be normalised here or
        // the `full` probe never matches and only the short form would.
        let full = mangle_with_shortened_args(name, args);
        if records.iter().any(|r| r.name == full) || enums.iter().any(|e| e.name == full) {
            return full;
        }
        mangle_with_shortened_args(short, args)
    }
}

/// Emit inline drop calls for any heap-owning fields written by the decode walk
/// into `dst`.  Used on the decode-failure path (Fix 3b): the destination struct
/// was zero-initialised before the walk, so every field the walk did NOT reach is
/// still null/zero and the null-safe drop helpers (`hew_string_drop`,
/// `hew_bytes_drop`) short-circuit safely.
///
/// WHY here and not in a shared runtime helper: the field layout is statically
/// known from the type, so a static walk is cheaper than a runtime descriptor
/// lookup and avoids adding a new FFI surface for what is a narrow failure path.
/// WHEN-OBSOLETE: if Hew gains a general GC or type-descriptor RTTI, delegate
/// there instead.
/// WHAT: only String and Bytes carry inline heap pointers that the decode walk
/// allocates; scalars/unit/bool/int/float are value types — no drop needed.
/// Named records and enums delegate to the existing
/// `__hew_record_drop_inplace_*` / `__hew_enum_drop_inplace_*` helpers (which
/// are also null-field-safe because Hew drop helpers guard against null).
#[allow(clippy::too_many_arguments)]
fn emit_de_drop_owned<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    ty: &ResolvedTy,
    dst: PointerValue<'ctx>,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> CodegenResult<()> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    match ty {
        // Scalars: no heap allocation in the decode walk — nothing to drop.
        ResolvedTy::Bool
        | ResolvedTy::I8
        | ResolvedTy::U8
        | ResolvedTy::I16
        | ResolvedTy::U16
        | ResolvedTy::I32
        | ResolvedTy::U32
        | ResolvedTy::Char
        | ResolvedTy::I64
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize
        | ResolvedTy::Duration
        | ResolvedTy::F32
        | ResolvedTy::F64
        | ResolvedTy::Unit
        | ResolvedTy::Never => Ok(()),

        // String: `hew_cbor_de_string` stores a malloc'd C string pointer.
        // `hew_string_drop(null)` is a no-op (guards with `cabi_guard!`), so
        // calling it unconditionally on a zero-inited (i.e. null) field is safe.
        ResolvedTy::String => {
            let drop_str = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_string_drop",
                ctx.void_type().fn_type(&[ptr_ty.into()], false),
            );
            // Load the *mut c_char pointer stored at `dst`.
            let s = builder
                .build_load(ptr_ty, dst, "de_drop_str_load")
                .llvm_ctx("de drop string load")?
                .into_pointer_value();
            builder
                .build_call(drop_str, &[s.into()], "de_drop_str")
                .llvm_ctx("de drop string call")?;
            Ok(())
        }

        // Bytes: stored as a `{ ptr, i32, i32 }` triple where the first word is
        // the data pointer (null when zero-inited or on decode failure before the
        // field is written).  `hew_bytes_drop(null)` returns immediately.
        ResolvedTy::Bytes => {
            let drop_bytes = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_bytes_drop",
                ctx.void_type().fn_type(&[ptr_ty.into()], false),
            );
            // GEP to the first field (data ptr) of the triple.
            let triple_ty = ctx.struct_type(
                &[ptr_ty.into(), ctx.i32_type().into(), ctx.i32_type().into()],
                false,
            );
            let data_ptr_slot = builder
                .build_struct_gep(triple_ty, dst, 0, "de_drop_bytes_slot")
                .llvm_ctx("de drop bytes gep")?;
            let data_ptr = builder
                .build_load(ptr_ty, data_ptr_slot, "de_drop_bytes_load")
                .llvm_ctx("de drop bytes load")?
                .into_pointer_value();
            builder
                .build_call(drop_bytes, &[data_ptr.into()], "de_drop_bytes")
                .llvm_ctx("de drop bytes call")?;
            Ok(())
        }

        // Named record: delegate to the codegen-emitted per-type in-place drop
        // helper (`__hew_record_drop_inplace_<name>`).  That helper walks the
        // struct's owned fields and drops them; null fields are safe.
        ResolvedTy::Named { name, args, .. } => {
            let key = xnode_registry_key(name, args, pipeline_records, enum_layouts);
            if let Some(el) = enum_layouts.iter().find(|e| e.name == key) {
                let drop_fn = get_or_declare_enum_drop_inplace(ctx, llvm_mod, &key);
                if el.is_indirect {
                    // An indirect (recursive) enum's `dst` holds a heap-node
                    // POINTER: `emit_de_enum_cbor` stored the `hew_alloc`'d node
                    // there and decoded the tagged union INTO the node. The
                    // in-place drop reads its argument AS the tagged-union node,
                    // so it must run on the NODE — invoking it on `dst` would
                    // reinterpret the pointer bits as the discriminant and drop
                    // garbage, and would ALSO leak the node (nothing frees it).
                    // Load the node, drop its owned payload in place, then
                    // `hew_dealloc` the node exactly once (paired with the
                    // codec's `hew_alloc`). Every fail path leaves the node
                    // well-defined: the known-tag walk writes every field, and
                    // the unknown-tag path zeroes the node before latching, so
                    // the structural drop never reads uninitialised bytes (#2208).
                    let node = builder
                        .build_load(ptr_ty, dst, "de_drop_enum_node")
                        .llvm_ctx("de drop indirect enum node load")?
                        .into_pointer_value();
                    builder
                        .build_call(drop_fn, &[node.into()], "de_drop_enum_node")
                        .llvm_ctx("de drop indirect enum node call")?;
                    let layout = machine_layouts.get(&key).ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "indirect-enum decode cleanup: enum `{key}` has no \
                             MachineCodegenLayout — codec/layout registration drift"
                        ))
                    })?;
                    let node_size = layout.outer_struct.size_of().ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "indirect-enum decode cleanup: enum `{key}` outer struct \
                             has no size"
                        ))
                    })?;
                    // Matches `emit_de_enum_cbor`'s `hew_alloc(size, 8)`; the
                    // runtime allocator asserts the (size, align) pair matches.
                    let align = i64_ty.const_int(8, false);
                    let dealloc = declare_codec_prim(
                        ctx,
                        llvm_mod,
                        "hew_dealloc",
                        ctx.void_type()
                            .fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false),
                    );
                    builder
                        .build_call(
                            dealloc,
                            &[node.into(), node_size.into(), align.into()],
                            "de_drop_enum_node_free",
                        )
                        .llvm_ctx("de drop indirect enum node dealloc")?;
                } else {
                    // Inline enum: `dst` IS the tagged-union storage.
                    builder
                        .build_call(drop_fn, &[dst.into()], "de_drop_enum")
                        .llvm_ctx("de drop enum call")?;
                }
            } else if record_layouts.contains_key(&key) {
                // Record: delegate to the per-record in-place drop helper.
                let drop_fn = get_or_declare_record_drop_inplace(ctx, llvm_mod, &key);
                builder
                    .build_call(drop_fn, &[dst.into()], "de_drop_record")
                    .llvm_ctx("de drop record call")?;
            }
            // If neither layout is found (e.g. a type not yet registered), the
            // decode walk would have already failed with a codegen error before
            // reaching this path — no drop needed.
            Ok(())
        }

        // JUSTIFIED: unsupported compound/runtime-only types should have failed in
        // `emit_de_value_cbor` or at the Serializable boundary before cleanup emission.
        // If one reaches cleanup, fail closed instead of silently skipping fields
        // that may own heap allocations in a future lowering.
        other => Err(CodegenError::FailClosed(format!(
            "cross-node decode cleanup: unsupported value type {other:?} \
             (outside the Serializable cleanup floor)"
        ))),
    }
}

// ── CBOR wire-body codec ─────────────────────────────────────────────────────
//
// The wire `.encode()` / `.decode()` path lowers to a `__hew_cbor_serialize_*`
// / `__hew_cbor_deserialize_*` thunk pair whose bodies are emitted here. The
// ABI is identical to the bespoke serialize/deserialize thunks above, so the
// existing `lower_wire_codec_instr` call shape is reused verbatim; only the
// runtime primitives differ. A wire struct encodes as a CBOR map keyed by each
// field's `@N` wire tag (the compatibility authority); the decoder selects
// fields by the same key, tolerates absent/extra keys per forward-compat, and
// fails closed on any type mismatch or truncation. The map framing is the one
// structural difference from the flat positional walk above.

/// The CBOR map key for field `field_name` (declaration index `field_idx`) of
/// record `record_key`. A `#[wire]` struct keys by its `@N` tag (the
/// compatibility authority recorded in the wire layout table); a record with no
/// wire layout (e.g. a hand-built fixture) falls back to a 1-based positional
/// key. The encoder and decoder both call this with the same inputs, so the two
/// directions always agree regardless of which branch is taken.
fn cbor_field_key(
    wire_layouts: &WireLayoutTable,
    record_key: &str,
    field_name: &str,
    field_idx: usize,
) -> u64 {
    let probe = |name: &str| -> Option<u64> {
        wire_layouts
            .get(name)
            .and_then(|entry| entry.fields.iter().find(|f| f.name == field_name))
            .map(|f| u64::from(f.tag))
    };
    if let Some(tag) = probe(record_key) {
        return tag;
    }
    // Records are usually keyed by the short name; try it if the key was qualified.
    let short = short_name(record_key);
    if short != record_key {
        if let Some(tag) = probe(short) {
            return tag;
        }
    }
    field_idx as u64 + 1
}

/// Declare-or-fetch the CBOR serialize thunk:
/// `ptr __hew_cbor_serialize_<key>(ptr value, ptr out_len)`. Same ABI as the
/// bespoke serialize thunk so `lower_wire_codec_instr` calls it identically.
pub(crate) fn get_or_declare_cbor_serialize_thunk<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    key: &str,
) -> FunctionValue<'ctx> {
    let sym = format!("__hew_cbor_serialize_{key}");
    if let Some(fv) = llvm_mod.get_function(&sym) {
        return fv;
    }
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    llvm_mod.add_function(
        &sym,
        ptr_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        Some(Linkage::Internal),
    )
}

/// Declare-or-fetch the CBOR deserialize thunk:
/// `ptr __hew_cbor_deserialize_<key>(ptr data, usize len, ptr out_struct_size)`.
/// Returns a `libc::malloc`'d reconstructed value (or null on decode failure),
/// matching the bespoke deserialize thunk ABI.
pub(crate) fn get_or_declare_cbor_deserialize_thunk<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    key: &str,
) -> FunctionValue<'ctx> {
    let sym = format!("__hew_cbor_deserialize_{key}");
    if let Some(fv) = llvm_mod.get_function(&sym) {
        return fv;
    }
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let size_ty = runtime_size_ty(ctx, llvm_mod);
    llvm_mod.add_function(
        &sym,
        ptr_ty.fn_type(&[ptr_ty.into(), size_ty.into(), ptr_ty.into()], false),
        Some(Linkage::Internal),
    )
}

/// Emit the per-field ENCODE walk for a value of type `ty` rooted at
/// `value_ptr`, emitting CBOR items onto the runtime `CborSerBuf` handle `buf`.
/// The caller has already emitted the map key for this value (if it sits inside
/// a struct map); this routine emits exactly one CBOR item. Recurses into nested
/// records as nested maps. Fails closed on any shape outside the supported
/// wire-body floor.
#[allow(clippy::too_many_arguments)]
fn emit_ser_value_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    ty: &ResolvedTy,
    value_ptr: PointerValue<'ctx>,
    buf: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_ty = ctx.void_type();
    let i64_ty = ctx.i64_type();
    match ty {
        ResolvedTy::Bool
        | ResolvedTy::I8
        | ResolvedTy::U8
        | ResolvedTy::I16
        | ResolvedTy::U16
        | ResolvedTy::I32
        | ResolvedTy::U32
        | ResolvedTy::Char
        | ResolvedTy::I64
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize
        | ResolvedTy::Duration => {
            let llvm_ty = resolve_ty(ctx, target_data, ty, record_layouts)?;
            let int_ty = llvm_ty.into_int_type();
            let raw = builder
                .build_load(int_ty, value_ptr, "cbor_ser_scalar")
                .llvm_ctx("cbor ser scalar load")?
                .into_int_value();
            if matches!(ty, ResolvedTy::Bool) {
                let i8_ty = ctx.i8_type();
                let as_i8 = if int_ty.get_bit_width() == 8 {
                    raw
                } else {
                    builder
                        .build_int_truncate(raw, i8_ty, "cbor_ser_bool_trunc")
                        .llvm_ctx("cbor ser bool trunc")?
                };
                let prim = declare_codec_prim(
                    ctx,
                    llvm_mod,
                    "hew_cbor_ser_bool",
                    void_ty.fn_type(&[ptr_ty.into(), i8_ty.into()], false),
                );
                builder
                    .build_call(prim, &[buf.into(), as_i8.into()], "cbor_ser_bool")
                    .llvm_ctx("cbor ser bool")?;
            } else {
                let unsigned = matches!(
                    ty,
                    ResolvedTy::U8
                        | ResolvedTy::U16
                        | ResolvedTy::U32
                        | ResolvedTy::U64
                        | ResolvedTy::Usize
                        | ResolvedTy::Char
                );
                let widened = if int_ty.get_bit_width() == 64 {
                    raw
                } else if unsigned {
                    builder
                        .build_int_z_extend(raw, i64_ty, "cbor_ser_zext")
                        .llvm_ctx("cbor ser zext")?
                } else {
                    builder
                        .build_int_s_extend(raw, i64_ty, "cbor_ser_sext")
                        .llvm_ctx("cbor ser sext")?
                };
                let prim_name = if unsigned {
                    "hew_cbor_ser_u64"
                } else {
                    "hew_cbor_ser_i64"
                };
                let prim = declare_codec_prim(
                    ctx,
                    llvm_mod,
                    prim_name,
                    void_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
                );
                builder
                    .build_call(prim, &[buf.into(), widened.into()], "cbor_ser_int")
                    .llvm_ctx("cbor ser int")?;
            }
            Ok(())
        }
        ResolvedTy::F32 | ResolvedTy::F64 => {
            let llvm_ty = resolve_ty(ctx, target_data, ty, record_layouts)?;
            let f = builder
                .build_load(llvm_ty, value_ptr, "cbor_ser_float")
                .llvm_ctx("cbor ser float load")?
                .into_float_value();
            let f64v = if matches!(ty, ResolvedTy::F64) {
                f
            } else {
                builder
                    .build_float_ext(f, ctx.f64_type(), "cbor_ser_fext")
                    .llvm_ctx("cbor ser fext")?
            };
            let prim = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_cbor_ser_f64",
                void_ty.fn_type(&[ptr_ty.into(), ctx.f64_type().into()], false),
            );
            builder
                .build_call(prim, &[buf.into(), f64v.into()], "cbor_ser_f64")
                .llvm_ctx("cbor ser f64")?;
            Ok(())
        }
        ResolvedTy::String => {
            let s = builder
                .build_load(ptr_ty, value_ptr, "cbor_ser_string_ptr")
                .llvm_ctx("cbor ser string load")?
                .into_pointer_value();
            let prim = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_cbor_ser_string",
                void_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
            );
            builder
                .build_call(prim, &[buf.into(), s.into()], "cbor_ser_string")
                .llvm_ctx("cbor ser string")?;
            Ok(())
        }
        ResolvedTy::Bytes => {
            let i32_ty = ctx.i32_type();
            let triple_ty = ctx.struct_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false);
            let data_ptr = builder
                .build_struct_gep(triple_ty, value_ptr, 0, "cbor_ser_bytes_ptr")
                .llvm_ctx("cbor ser bytes ptr gep")?;
            let data = builder
                .build_load(ptr_ty, data_ptr, "cbor_ser_bytes_data")
                .llvm_ctx("cbor ser bytes data load")?
                .into_pointer_value();
            let off_ptr = builder
                .build_struct_gep(triple_ty, value_ptr, 1, "cbor_ser_bytes_off")
                .llvm_ctx("cbor ser bytes off gep")?;
            let off = builder
                .build_load(i32_ty, off_ptr, "cbor_ser_bytes_offv")
                .llvm_ctx("cbor ser bytes off load")?
                .into_int_value();
            let len_ptr = builder
                .build_struct_gep(triple_ty, value_ptr, 2, "cbor_ser_bytes_len")
                .llvm_ctx("cbor ser bytes len gep")?;
            let len = builder
                .build_load(i32_ty, len_ptr, "cbor_ser_bytes_lenv")
                .llvm_ctx("cbor ser bytes len load")?
                .into_int_value();
            let prim = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_cbor_ser_bytes",
                void_ty.fn_type(
                    &[ptr_ty.into(), ptr_ty.into(), i32_ty.into(), i32_ty.into()],
                    false,
                ),
            );
            builder
                .build_call(
                    prim,
                    &[buf.into(), data.into(), off.into(), len.into()],
                    "cbor_ser_bytes",
                )
                .llvm_ctx("cbor ser bytes")?;
            Ok(())
        }
        ResolvedTy::Named {
            name,
            args,
            builtin,
            ..
        } => match builtin {
            Some(BuiltinType::Vec) => {
                let elem = args.first().ok_or_else(|| {
                    CodegenError::FailClosed("wire CBOR serialize: Vec<T> missing element".into())
                })?;
                emit_ser_vec_cbor(
                    ctx,
                    llvm_mod,
                    builder,
                    func,
                    elem,
                    value_ptr,
                    buf,
                    wire_layouts,
                    record_layouts,
                    machine_layouts,
                    pipeline_records,
                    enum_layouts,
                    target_data,
                )
            }
            Some(BuiltinType::Option) => emit_ser_option_cbor(
                ctx,
                llvm_mod,
                builder,
                func,
                name,
                args,
                value_ptr,
                buf,
                wire_layouts,
                record_layouts,
                machine_layouts,
                pipeline_records,
                enum_layouts,
                target_data,
            ),
            _ => emit_ser_named_cbor(
                ctx,
                llvm_mod,
                builder,
                func,
                name,
                args,
                value_ptr,
                buf,
                wire_layouts,
                record_layouts,
                machine_layouts,
                pipeline_records,
                enum_layouts,
                target_data,
            ),
        },
        other => Err(CodegenError::FailClosed(format!(
            "wire CBOR serialize: unsupported value type {other:?} \
             (outside the supported wire-body floor)"
        ))),
    }
}

/// Emit the CBOR encode walk for a `Named` type. A registered record encodes as
/// a CBOR map keyed by each field's wire tag; a registered enum encodes under
/// the map-of-one shape (`emit_ser_enum_cbor`).
#[allow(clippy::too_many_arguments)]
fn emit_ser_named_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    name: &str,
    args: &[ResolvedTy],
    value_ptr: PointerValue<'ctx>,
    buf: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let key = xnode_registry_key(name, args, pipeline_records, enum_layouts);
    // Enum: encode under the map-of-one shape.
    if let (Some(layout), Some(el)) = (
        machine_layouts.get(&key),
        enum_layouts.iter().find(|e| e.name == key),
    ) {
        return emit_ser_enum_cbor(
            ctx,
            llvm_mod,
            builder,
            func,
            &key,
            layout,
            el,
            value_ptr,
            buf,
            wire_layouts,
            record_layouts,
            machine_layouts,
            pipeline_records,
            enum_layouts,
            target_data,
        );
    }
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_ty = ctx.void_type();
    let i64_ty = ctx.i64_type();
    if let (Some(struct_ty), Some(rl)) = (
        record_layouts.get(&key).copied(),
        pipeline_records.iter().find(|r| r.name == key),
    ) {
        let begin = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_ser_begin_map",
            void_ty.fn_type(&[ptr_ty.into()], false),
        );
        builder
            .build_call(begin, &[buf.into()], "cbor_ser_begin_map")
            .llvm_ctx("cbor ser begin_map")?;
        let key_prim = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_ser_key_u64",
            void_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        );
        for (idx, fty) in rl.field_tys.iter().enumerate() {
            let field_name = rl.field_names.get(idx).map_or("", String::as_str);
            let tag = cbor_field_key(wire_layouts, &key, field_name, idx);
            let tag_const = i64_ty.const_int(tag, false);
            builder
                .build_call(
                    key_prim,
                    &[buf.into(), tag_const.into()],
                    &format!("cbor_ser_key_{idx}"),
                )
                .llvm_ctx("cbor ser key")?;
            let field_ptr = builder
                .build_struct_gep(
                    struct_ty,
                    value_ptr,
                    idx as u32,
                    &format!("cbor_ser_field_{idx}"),
                )
                .llvm_ctx("cbor ser field gep")?;
            emit_ser_value_cbor(
                ctx,
                llvm_mod,
                builder,
                func,
                fty,
                field_ptr,
                buf,
                wire_layouts,
                record_layouts,
                machine_layouts,
                pipeline_records,
                enum_layouts,
                target_data,
            )?;
        }
        let end = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_ser_end_map",
            void_ty.fn_type(&[ptr_ty.into()], false),
        );
        builder
            .build_call(end, &[buf.into()], "cbor_ser_end_map")
            .llvm_ctx("cbor ser end_map")?;
        return Ok(());
    }
    Err(CodegenError::FailClosed(format!(
        "wire CBOR serialize: named type `{name}` (key `{key}`) is not a \
         registered record layout"
    )))
}

/// Emit the CBOR DECODE walk for a value of type `ty`, reading from the runtime
/// `CborDeReader` handle `reader` and storing the reconstructed value into
/// `dst`. The caller has already selected this value's map key (if any). Fails
/// closed on any shape outside the supported wire-body floor.
#[allow(clippy::too_many_arguments)]
fn emit_de_value_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    ty: &ResolvedTy,
    dst: PointerValue<'ctx>,
    reader: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    match ty {
        ResolvedTy::Bool
        | ResolvedTy::I8
        | ResolvedTy::U8
        | ResolvedTy::I16
        | ResolvedTy::U16
        | ResolvedTy::I32
        | ResolvedTy::U32
        | ResolvedTy::Char
        | ResolvedTy::I64
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize
        | ResolvedTy::Duration => {
            let llvm_ty = resolve_ty(ctx, target_data, ty, record_layouts)?;
            let int_ty = llvm_ty.into_int_type();
            if matches!(ty, ResolvedTy::Bool) {
                let i8_ty = ctx.i8_type();
                let prim = declare_codec_prim(
                    ctx,
                    llvm_mod,
                    "hew_cbor_de_bool",
                    i8_ty.fn_type(&[ptr_ty.into()], false),
                );
                let v = builder
                    .build_call(prim, &[reader.into()], "cbor_de_bool")
                    .llvm_ctx("cbor de bool")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_bool void".into()))?
                    .into_int_value();
                let stored = if int_ty.get_bit_width() == 8 {
                    v
                } else {
                    builder
                        .build_int_z_extend(v, int_ty, "cbor_de_bool_zext")
                        .llvm_ctx("cbor de bool zext")?
                };
                builder
                    .build_store(dst, stored)
                    .llvm_ctx("cbor de bool store")?;
            } else if matches!(ty, ResolvedTy::Char) {
                // `char` reads through a dedicated primitive that validates the
                // decoded scalar is a Unicode scalar value (rejecting surrogates
                // and values above U+10FFFF) and fails closed otherwise. It
                // returns the codepoint widened to i64; truncate to the i32
                // `char` store (lossless — a scalar value is below 2^21).
                let prim = declare_codec_prim(
                    ctx,
                    llvm_mod,
                    "hew_cbor_de_char",
                    i64_ty.fn_type(&[ptr_ty.into()], false),
                );
                let wide = builder
                    .build_call(prim, &[reader.into()], "cbor_de_char")
                    .llvm_ctx("cbor de char")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_char void".into()))?
                    .into_int_value();
                let narrow = builder
                    .build_int_truncate(wide, int_ty, "cbor_de_char_trunc")
                    .llvm_ctx("cbor de char trunc")?;
                builder
                    .build_store(dst, narrow)
                    .llvm_ctx("cbor de char store")?;
            } else {
                // Every fixed-width integer reads through the range-checked
                // primitive: a CBOR integer outside the target type's bounds
                // (over-max, under-min, or negative for an unsigned target) is
                // rejected as a decode failure instead of being truncated to a
                // fabricated value (CLAUDE.md §2). The reader returns the in-range
                // value's exact low-64 bit pattern, so the narrowing truncation
                // below is lossless.
                let unsigned = matches!(
                    ty,
                    ResolvedTy::U8
                        | ResolvedTy::U16
                        | ResolvedTy::U32
                        | ResolvedTy::U64
                        | ResolvedTy::Usize
                );
                let i32_ty = ctx.i32_type();
                let width = int_ty.get_bit_width();
                let prim = declare_codec_prim(
                    ctx,
                    llvm_mod,
                    "hew_cbor_de_int_checked",
                    i64_ty.fn_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false),
                );
                let bits_arg = i32_ty.const_int(u64::from(width), false);
                let signed_arg = i32_ty.const_int(u64::from(!unsigned), false);
                let wide = builder
                    .build_call(
                        prim,
                        &[reader.into(), bits_arg.into(), signed_arg.into()],
                        "cbor_de_int",
                    )
                    .llvm_ctx("cbor de int")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_int_checked void".into()))?
                    .into_int_value();
                let narrow = if width == 64 {
                    wide
                } else {
                    builder
                        .build_int_truncate(wide, int_ty, "cbor_de_int_trunc")
                        .llvm_ctx("cbor de int trunc")?
                };
                builder
                    .build_store(dst, narrow)
                    .llvm_ctx("cbor de int store")?;
            }
            Ok(())
        }
        ResolvedTy::F32 | ResolvedTy::F64 => {
            let prim = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_cbor_de_f64",
                ctx.f64_type().fn_type(&[ptr_ty.into()], false),
            );
            let f = builder
                .build_call(prim, &[reader.into()], "cbor_de_f64")
                .llvm_ctx("cbor de f64")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_f64 void".into()))?
                .into_float_value();
            let stored = if matches!(ty, ResolvedTy::F64) {
                f
            } else {
                builder
                    .build_float_trunc(f, ctx.f32_type(), "cbor_de_ftrunc")
                    .llvm_ctx("cbor de ftrunc")?
            };
            builder
                .build_store(dst, stored)
                .llvm_ctx("cbor de float store")?;
            Ok(())
        }
        ResolvedTy::String => {
            let prim = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_cbor_de_string",
                ptr_ty.fn_type(&[ptr_ty.into()], false),
            );
            let s = builder
                .build_call(prim, &[reader.into()], "cbor_de_string")
                .llvm_ctx("cbor de string")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_string void".into()))?
                .into_pointer_value();
            builder
                .build_store(dst, s)
                .llvm_ctx("cbor de string store")?;
            Ok(())
        }
        ResolvedTy::Bytes => {
            let i32_ty = ctx.i32_type();
            let len_slot = builder
                .build_alloca(i32_ty, "cbor_de_bytes_len")
                .llvm_ctx("cbor de bytes len alloca")?;
            let read_prim = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_cbor_de_bytes",
                ptr_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
            );
            let raw = builder
                .build_call(
                    read_prim,
                    &[reader.into(), len_slot.into()],
                    "cbor_de_bytes",
                )
                .llvm_ctx("cbor de bytes")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_bytes void".into()))?
                .into_pointer_value();
            let len = builder
                .build_load(i32_ty, len_slot, "cbor_de_bytes_lenv")
                .llvm_ctx("cbor de bytes len load")?
                .into_int_value();
            // Classified `hew_bytes_from_static(ptr, len) -> BytesTriple`,
            // landed in the dest triple slot per the target's aggregate ABI.
            crate::runtime_abi::emit_classified_bytes_return_call_raw(
                ctx,
                llvm_mod,
                target_data,
                builder,
                "hew_bytes_from_static",
                &[ptr_ty.into(), i32_ty.into()],
                &[raw.into(), len.into()],
                dst,
            )?;
            let free_prim = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_ser_free_bytes",
                ctx.void_type().fn_type(&[ptr_ty.into()], false),
            );
            builder
                .build_call(free_prim, &[raw.into()], "cbor_de_bytes_free_raw")
                .llvm_ctx("cbor de bytes free raw")?;
            Ok(())
        }
        ResolvedTy::Named {
            name,
            args,
            builtin,
            ..
        } => match builtin {
            Some(BuiltinType::Vec) => {
                let elem = args.first().ok_or_else(|| {
                    CodegenError::FailClosed("wire CBOR deserialize: Vec<T> missing element".into())
                })?;
                emit_de_vec_cbor(
                    ctx,
                    llvm_mod,
                    builder,
                    func,
                    elem,
                    dst,
                    reader,
                    wire_layouts,
                    record_layouts,
                    machine_layouts,
                    pipeline_records,
                    enum_layouts,
                    target_data,
                )
            }
            Some(BuiltinType::Option) => emit_de_option_cbor(
                ctx,
                llvm_mod,
                builder,
                func,
                name,
                args,
                dst,
                reader,
                wire_layouts,
                record_layouts,
                machine_layouts,
                pipeline_records,
                enum_layouts,
                target_data,
            ),
            _ => emit_de_named_cbor(
                ctx,
                llvm_mod,
                builder,
                func,
                name,
                args,
                dst,
                reader,
                wire_layouts,
                record_layouts,
                machine_layouts,
                pipeline_records,
                enum_layouts,
                target_data,
            ),
        },
        other => Err(CodegenError::FailClosed(format!(
            "wire CBOR deserialize: unsupported value type {other:?} \
             (outside the supported wire-body floor)"
        ))),
    }
}

/// Emit the CBOR decode walk for a `Named` type into `dst`. A registered record
/// enters a CBOR map, selects each field by its wire tag, decodes it, then exits
/// the map (tolerating extra keys); a registered enum decodes the
/// map-of-one shape (`emit_de_enum_cbor`).
#[allow(clippy::too_many_arguments)]
fn emit_de_named_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    name: &str,
    args: &[ResolvedTy],
    dst: PointerValue<'ctx>,
    reader: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let key = xnode_registry_key(name, args, pipeline_records, enum_layouts);
    // Enum: decode the map-of-one shape.
    if let (Some(layout), Some(el)) = (
        machine_layouts.get(&key),
        enum_layouts.iter().find(|e| e.name == key),
    ) {
        return emit_de_enum_cbor(
            ctx,
            llvm_mod,
            builder,
            func,
            &key,
            layout,
            el,
            dst,
            reader,
            wire_layouts,
            record_layouts,
            machine_layouts,
            pipeline_records,
            enum_layouts,
            target_data,
        );
    }
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_ty = ctx.void_type();
    let i64_ty = ctx.i64_type();
    if let (Some(struct_ty), Some(rl)) = (
        record_layouts.get(&key).copied(),
        pipeline_records.iter().find(|r| r.name == key),
    ) {
        let enter = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_de_enter_map",
            void_ty.fn_type(&[ptr_ty.into()], false),
        );
        builder
            .build_call(enter, &[reader.into()], "cbor_de_enter_map")
            .llvm_ctx("cbor de enter_map")?;
        let select = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_de_select_key",
            void_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        );
        for (idx, fty) in rl.field_tys.iter().enumerate() {
            let field_name = rl.field_names.get(idx).map_or("", String::as_str);
            let tag = cbor_field_key(wire_layouts, &key, field_name, idx);
            let tag_const = i64_ty.const_int(tag, false);
            builder
                .build_call(
                    select,
                    &[reader.into(), tag_const.into()],
                    &format!("cbor_de_select_{idx}"),
                )
                .llvm_ctx("cbor de select_key")?;
            let field_ptr = builder
                .build_struct_gep(struct_ty, dst, idx as u32, &format!("cbor_de_field_{idx}"))
                .llvm_ctx("cbor de field gep")?;
            emit_de_value_cbor(
                ctx,
                llvm_mod,
                builder,
                func,
                fty,
                field_ptr,
                reader,
                wire_layouts,
                record_layouts,
                machine_layouts,
                pipeline_records,
                enum_layouts,
                target_data,
            )?;
        }
        let exit = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_de_exit_map",
            void_ty.fn_type(&[ptr_ty.into()], false),
        );
        builder
            .build_call(exit, &[reader.into()], "cbor_de_exit_map")
            .llvm_ctx("cbor de exit_map")?;
        return Ok(());
    }
    Err(CodegenError::FailClosed(format!(
        "wire CBOR deserialize: named type `{name}` (key `{key}`) is not a \
         registered record layout"
    )))
}

/// Wire tag for an enum variant: the schema-stable tag from the enum's wire
/// layout (matched by variant name), falling back to the positional index when
/// no explicit layout entry exists. Mirrors `cbor_field_key` for record fields:
/// the wire tag — not the in-memory discriminant — is the cross-version
/// compatibility authority, so the map key / unit integer is driven from it.
fn cbor_variant_tag(
    wire_layouts: &WireLayoutTable,
    enum_key: &str,
    variant_name: &str,
    variant_idx: usize,
) -> u64 {
    let probe = |name: &str| -> Option<u64> {
        wire_layouts
            .get(name)
            .and_then(|entry| entry.variants.iter().find(|(n, _)| n == variant_name))
            .map(|(_, tag)| u64::from(*tag))
    };
    if let Some(tag) = probe(enum_key) {
        return tag;
    }
    let short = short_name(enum_key);
    if short != enum_key {
        if let Some(tag) = probe(short) {
            return tag;
        }
    }
    variant_idx as u64
}

/// Resolve a wire struct field's JSON/YAML key name. An explicit `json_name` /
/// `yaml_name` override wins; otherwise the source field name is folded through
/// the type-level `json_case` / `yaml_case` policy (if any); otherwise the source
/// name is used verbatim. The DECODE side reads the same descriptor, so the key
/// the encoder writes is exactly the key the decoder looks for.
fn wire_text_field_name(
    field: &hew_types::WireFieldLayout,
    case: Option<hew_parser::ast::NamingCase>,
    format: WireTextFormat,
) -> String {
    let explicit = match format {
        WireTextFormat::Json => field.json_name.as_deref(),
        WireTextFormat::Yaml => field.yaml_name.as_deref(),
    };
    if let Some(name) = explicit {
        return name.to_string();
    }
    match case {
        Some(c) => apply_naming_case(&field.name, c),
        None => field.name.clone(),
    }
}

/// Apply a [`hew_parser::ast::NamingCase`] to a snake_case-or-mixed source
/// identifier, producing the cased key string. Words are split on `_` and on
/// lower→upper boundaries so both `user_id` and `userId` normalise consistently.
fn apply_naming_case(name: &str, case: hew_parser::ast::NamingCase) -> String {
    use hew_parser::ast::NamingCase;
    let words = split_identifier_words(name);
    if words.is_empty() {
        return name.to_string();
    }
    match case {
        NamingCase::SnakeCase => words.join("_"),
        NamingCase::ScreamingSnake => words
            .iter()
            .map(|w| w.to_uppercase())
            .collect::<Vec<_>>()
            .join("_"),
        NamingCase::KebabCase => words.join("-"),
        NamingCase::CamelCase => {
            let mut out = String::new();
            for (i, w) in words.iter().enumerate() {
                if i == 0 {
                    out.push_str(w);
                } else {
                    out.push_str(&capitalize(w));
                }
            }
            out
        }
        NamingCase::PascalCase => words.iter().map(|w| capitalize(w)).collect(),
    }
}

/// Split a source identifier into lowercase words on `_`/`-` separators and on
/// lower→upper case boundaries.
fn split_identifier_words(name: &str) -> Vec<String> {
    let mut words = Vec::new();
    let mut current = String::new();
    let mut prev_lower = false;
    for ch in name.chars() {
        if ch == '_' || ch == '-' {
            if !current.is_empty() {
                words.push(std::mem::take(&mut current));
            }
            prev_lower = false;
            continue;
        }
        if ch.is_uppercase() && prev_lower && !current.is_empty() {
            words.push(std::mem::take(&mut current));
        }
        for lc in ch.to_lowercase() {
            current.push(lc);
        }
        prev_lower = ch.is_lowercase() || ch.is_ascii_digit();
    }
    if !current.is_empty() {
        words.push(current);
    }
    words
}

/// Capitalize the first character of a lowercase word.
fn capitalize(word: &str) -> String {
    let mut chars = word.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

/// Build the per-type text-codec DESCRIPTOR: a compact JSON schema string that
/// maps each wire tag (`@N`) to its JSON/YAML key name, recursively for nested
/// wire structs/enums, vectors, and options. The runtime `wire_text` bridge
/// parses this to drive the CBOR↔text transcode. The schema mirrors EXACTLY the
/// shapes the binary CBOR codec walk produces (struct = tag-keyed map, enum =
/// integer tag / map-of-one), so the two stay in lockstep.
///
/// `format` selects the JSON or YAML key naming (the override + case policy may
/// differ between the two). A type outside the supported wire-body floor emits an
/// `{"k":"opaque"}` node, which the bridge fails closed on — never a fabricated
/// transcode.
pub(crate) fn build_text_descriptor(
    ty: &ResolvedTy,
    format: WireTextFormat,
    wire_layouts: &WireLayoutTable,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> String {
    let mut visiting: std::collections::HashSet<String> = std::collections::HashSet::new();
    build_text_descriptor_node(
        ty,
        format,
        wire_layouts,
        pipeline_records,
        enum_layouts,
        &mut visiting,
    )
}

/// Recursive descriptor-node builder. `visiting` breaks layout cycles (a
/// recursive record/enum) by emitting `opaque` on a back-edge — the wire-body
/// floor does not include recursive text types, so this fails closed rather than
/// looping forever.
fn build_text_descriptor_node(
    ty: &ResolvedTy,
    format: WireTextFormat,
    wire_layouts: &WireLayoutTable,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    visiting: &mut std::collections::HashSet<String>,
) -> String {
    match ty {
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::Isize
        | ResolvedTy::Duration => r#"{"k":"i64"}"#.to_string(),
        ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::Usize
        | ResolvedTy::Char => r#"{"k":"u64"}"#.to_string(),
        ResolvedTy::F32 | ResolvedTy::F64 => r#"{"k":"f64"}"#.to_string(),
        ResolvedTy::Bool => r#"{"k":"bool"}"#.to_string(),
        ResolvedTy::String => r#"{"k":"str"}"#.to_string(),
        ResolvedTy::Bytes => r#"{"k":"bytes"}"#.to_string(),
        ResolvedTy::Named {
            name,
            args,
            builtin,
            ..
        } => match builtin {
            Some(BuiltinType::Vec) => {
                let elem = args.first().map_or_else(
                    || r#"{"k":"opaque"}"#.to_string(),
                    |e| {
                        build_text_descriptor_node(
                            e,
                            format,
                            wire_layouts,
                            pipeline_records,
                            enum_layouts,
                            visiting,
                        )
                    },
                );
                format!(r#"{{"k":"vec","e":{elem}}}"#)
            }
            Some(BuiltinType::Option) => {
                let elem = args.first().map_or_else(
                    || r#"{"k":"opaque"}"#.to_string(),
                    |e| {
                        build_text_descriptor_node(
                            e,
                            format,
                            wire_layouts,
                            pipeline_records,
                            enum_layouts,
                            visiting,
                        )
                    },
                );
                format!(r#"{{"k":"opt","e":{elem}}}"#)
            }
            _ => build_text_descriptor_named(
                name,
                args,
                format,
                wire_layouts,
                pipeline_records,
                enum_layouts,
                visiting,
            ),
        },
        _ => r#"{"k":"opaque"}"#.to_string(),
    }
}

/// Build the descriptor for a `Named` record or enum: a struct node (tag-keyed
/// field list) or an enum node (variant list), mirroring the CBOR codec's
/// record/enum resolution (`xnode_registry_key` + `cbor_field_key` /
/// `cbor_variant_tag`).
fn build_text_descriptor_named(
    name: &str,
    args: &[ResolvedTy],
    format: WireTextFormat,
    wire_layouts: &WireLayoutTable,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    visiting: &mut std::collections::HashSet<String>,
) -> String {
    let key = xnode_registry_key(name, args, pipeline_records, enum_layouts);
    if !visiting.insert(key.clone()) {
        // Recursive back-edge: a recursive text type is outside the floor.
        return r#"{"k":"opaque"}"#.to_string();
    }
    let case = wire_layouts.get(&key).and_then(|e| match format {
        WireTextFormat::Json => e.json_case,
        WireTextFormat::Yaml => e.yaml_case,
    });
    let result = if let Some(el) = enum_layouts.iter().find(|e| e.name == key) {
        // Enum: list each variant's tag, source name, and payload descriptors.
        let variants: Vec<String> = el
            .variants
            .iter()
            .enumerate()
            .map(|(idx, variant)| {
                let tag = cbor_variant_tag(wire_layouts, &key, &variant.name, idx);
                let payload: Vec<String> = variant
                    .field_tys
                    .iter()
                    .map(|fty| {
                        build_text_descriptor_node(
                            fty,
                            format,
                            wire_layouts,
                            pipeline_records,
                            enum_layouts,
                            visiting,
                        )
                    })
                    .collect();
                format!(
                    r#"{{"t":{tag},"n":{name},"p":[{payload}]}}"#,
                    name = json_string_literal(&variant.name),
                    payload = payload.join(",")
                )
            })
            .collect();
        format!(r#"{{"k":"enum","v":[{}]}}"#, variants.join(","))
    } else if let Some(rl) = pipeline_records.iter().find(|r| r.name == key) {
        // Struct: list each field's tag, JSON/YAML key name, and field descriptor.
        let fields: Vec<String> = rl
            .field_tys
            .iter()
            .enumerate()
            .map(|(idx, fty)| {
                let field_name = rl.field_names.get(idx).map_or("", String::as_str);
                let tag = cbor_field_key(wire_layouts, &key, field_name, idx);
                // Resolve the text key name from the wire layout (override + case).
                let text_name = wire_layouts
                    .get(&key)
                    .and_then(|e| e.fields.iter().find(|f| f.name == field_name))
                    .map_or_else(
                        || field_name.to_string(),
                        |f| wire_text_field_name(f, case, format),
                    );
                let desc = build_text_descriptor_node(
                    fty,
                    format,
                    wire_layouts,
                    pipeline_records,
                    enum_layouts,
                    visiting,
                );
                format!(
                    r#"{{"t":{tag},"n":{name},"d":{desc}}}"#,
                    name = json_string_literal(&text_name)
                )
            })
            .collect();
        format!(r#"{{"k":"struct","f":[{}]}}"#, fields.join(","))
    } else {
        // Not a registered record or enum: outside the text floor.
        r#"{"k":"opaque"}"#.to_string()
    };
    visiting.remove(&key);
    result
}

/// Serialize a string as a JSON string literal (quoted, with the JSON escapes the
/// descriptor parser expects). Used for field/variant key names embedded in the
/// descriptor schema.
fn json_string_literal(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// [`hew_mir::HeapOwnershipLayouts`] view over the standalone pipeline
/// record/enum layout slices the CBOR thunk emitter carries (it has no
/// `FnCtx`). Resolves user record / enum members through [`xnode_registry_key`]
/// so routing the CBOR element probe through the single
/// `hew_mir::ty_heap_ownership` authority resolves both heap leaves and enum
/// indirectness from the same layout.
struct CborHeapLayouts<'a> {
    pipeline_records: &'a [RecordLayout],
    enum_layouts: &'a [EnumLayout],
}

impl hew_mir::HeapOwnershipLayouts for CborHeapLayouts<'_> {
    fn record_field_tys(&self, name: &str, args: &[ResolvedTy]) -> Option<Vec<ResolvedTy>> {
        let key = xnode_registry_key(name, args, self.pipeline_records, self.enum_layouts);
        self.pipeline_records
            .iter()
            .find(|r| r.name == key)
            .map(|r| r.field_tys.clone())
    }

    fn enum_variant_field_tys(
        &self,
        name: &str,
        args: &[ResolvedTy],
    ) -> Option<Vec<Vec<ResolvedTy>>> {
        let key = xnode_registry_key(name, args, self.pipeline_records, self.enum_layouts);
        self.enum_layouts
            .iter()
            .find(|e| e.name == key)
            .map(|e| e.variants.iter().map(|v| v.field_tys.clone()).collect())
    }

    fn enum_is_indirect(&self, name: &str, args: &[ResolvedTy]) -> bool {
        let key = xnode_registry_key(name, args, self.pipeline_records, self.enum_layouts);
        self.enum_layouts
            .iter()
            .any(|layout| layout.name == key && layout.is_indirect)
    }
}

/// Backing-store kind a decoded CBOR array needs for its element type.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum CborVecElemKind {
    /// Scalar BitCopy element (bool / int / float / char / duration): a plain
    /// `hew_vec_new_with_elem_size` buffer filled by byte-copy
    /// `hew_vec_push_generic` and read through `hew_vec_get_generic`. The
    /// constructed vec has a NULL layout descriptor (`hew_vec_new_*`), so the
    /// generic (non-layout-aware) accessors are the matching ABI.
    Plain,
    /// `string` element: a `hew_vec_new_str` vec filled by copy-in
    /// `hew_vec_push_str`. `hew_vec_free` releases every element string.
    Str,
    /// Heap-free record/enum element: a *layout-aware* BitCopy vec
    /// (`hew_vec_new_with_layout` / `hew_vec_push_layout` / `hew_vec_get_layout`,
    /// `ownership_kind = Plain`). This is the SAME ABI `Vec::new` constructs for
    /// a `Vec<record>` (`VecCtor::BitCopyLayout`), so the codec MUST use the
    /// layout-aware accessors — the generic ones abort on a layout-aware vec
    /// (`Vec layout-aware operation is not implemented`). The descriptor is a
    /// thunk-less `{size, align, ownership_kind=0}` matching `layout_descriptor_ptr`.
    LayoutBitCopy,
    /// Heap-owning record / enum element that resolves an owned clone/drop thunk
    /// key: an OWNED (thunk-bearing) Vec (`hew_vec_new_with_elem_layout` /
    /// `hew_vec_push_owned_move` / `hew_vec_get_owned`,
    /// `ownership_kind = LayoutManaged`). This is the SAME ABI `Vec::new` builds
    /// for a `Vec<owned>` (`VecCtor::Owned`), and the descriptor global
    /// dedup-collapses onto the constructor's via the single
    /// `owned_elem_layout_descriptor_ptr` authority. `hew_vec_free` walks the
    /// per-element `drop_fn`, so a partially decoded vec releases exactly once.
    /// Nested-collection elements (`Vec<Vec<…>>`) are excluded — that base ABI is
    /// NYI (see the `Defer` arm) — so this covers records/enums with owned scalar
    /// leaf fields (`string` today).
    Owned,
    /// Element outside the supported wire-body floor (bytes / indirect enum /
    /// `Option`-with-heap / nested collection `Vec<Vec>`/`Vec<HashMap>`/
    /// `Vec<HashSet>` / no resolvable owned thunk key): the codec fails closed
    /// (a failable sub-shape, not a silent partial).
    Defer,
}

/// Owns the record field-type table the wire codec reconstructs from the
/// threaded `pipeline_records`, so an [`OwnedElemRegistries`] borrow can be handed
/// to the single owned-descriptor authority (`owned_elem_thunk_key` /
/// `owned_elem_layout_descriptor_ptr`). The reconstructed map is byte-identical to
/// the one `build_module` gives `FnCtx` (both are `pipeline.record_layouts` keyed
/// by name), so the codec's owned-element resolution can never disagree with the
/// constructor's.
struct CborOwnedElemRegistries<'a, 'ctx> {
    record_field_resolved_tys: std::collections::HashMap<String, Vec<ResolvedTy>>,
    enum_layouts: &'a [EnumLayout],
    machine_layouts: &'a MachineLayoutMap<'ctx>,
}

impl<'ctx> CborOwnedElemRegistries<'_, 'ctx> {
    fn registries(&self) -> OwnedElemRegistries<'_, 'ctx> {
        OwnedElemRegistries {
            enum_layouts: self.enum_layouts,
            machine_layouts: self.machine_layouts,
            record_field_resolved_tys: &self.record_field_resolved_tys,
        }
    }
}

fn cbor_owned_elem_registries<'a, 'ctx>(
    pipeline_records: &[RecordLayout],
    enum_layouts: &'a [EnumLayout],
    machine_layouts: &'a MachineLayoutMap<'ctx>,
) -> CborOwnedElemRegistries<'a, 'ctx> {
    CborOwnedElemRegistries {
        record_field_resolved_tys: pipeline_records
            .iter()
            .map(|r| (r.name.clone(), r.field_tys.clone()))
            .collect(),
        enum_layouts,
        machine_layouts,
    }
}

/// Classify a CBOR Vec element type into the backing-store kind its codec needs.
/// Both encode and decode branch on the kind: scalar elements use the generic
/// (null-layout) accessors, record/enum elements use the layout-aware BitCopy
/// accessors (matching `Vec::new`'s `VecCtor::BitCopyLayout` construction), and
/// heap-owning compounds fail closed.
pub(crate) fn cbor_vec_elem_kind(
    elem: &ResolvedTy,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    machine_layouts: &MachineLayoutMap<'_>,
) -> CborVecElemKind {
    // Reach the single owned-descriptor authority via the same registries
    // `Vec::new` uses (reconstructed from the threaded `pipeline_records`).
    let owned_regs = cbor_owned_elem_registries(pipeline_records, enum_layouts, machine_layouts);
    let regs = owned_regs.registries();
    match elem {
        ResolvedTy::Bool
        | ResolvedTy::I8
        | ResolvedTy::U8
        | ResolvedTy::I16
        | ResolvedTy::U16
        | ResolvedTy::I32
        | ResolvedTy::U32
        | ResolvedTy::I64
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize
        | ResolvedTy::Char
        | ResolvedTy::Duration
        | ResolvedTy::F32
        | ResolvedTy::F64 => CborVecElemKind::Plain,
        ResolvedTy::String => CborVecElemKind::Str,
        // Non-`Option` builtins stay fail-closed. A nested collection element
        // (`Vec<Vec<…>>`, `Vec<HashMap>`, `Vec<HashSet>`) would need the base
        // language's recursive owned clone/drop thunk synthesis for a
        // collection-of-collections, which is NOT implemented yet: the checker
        // already rejects the analogous `Vec<record-with-a-collection-field>` with
        // "recursive owned clone/drop thunk synthesis … not implemented yet", and a
        // direct `Vec<Vec<string>>` (which the checker does not yet gate) aborts at
        // runtime with a missing element-layout descriptor even OUTSIDE the codec.
        // Admitting it here would emit codec code that runtime-panics rather than
        // fails closed, so the codec keeps `Defer` until the base owned nested-Vec
        // ABI lands. `HashMap`/`HashSet` values additionally have no
        // `emit_ser_value_cbor` arm; every other builtin (`Generator` / `Result` /
        // `Range` / `Rc` / …) has no codec support.
        ResolvedTy::Named {
            builtin: Some(b), ..
        } if !matches!(b, BuiltinType::Option) => CborVecElemKind::Defer,
        // `Option<T>` element: a heap-free payload (`Option<i64>`) is the
        // layout-aware BitCopy vec; a heap-owning payload (`Option<string>`) stays
        // fail-closed. Option resolves an enum thunk key (it lowers to an enum),
        // but its null-encoding + owned-element ABI is a distinct, unproven shape —
        // out of scope for this floor, so it does NOT take the `Owned` path the
        // user-record/enum arm below does.
        ResolvedTy::Named {
            builtin: Some(BuiltinType::Option),
            ..
        } => {
            let ownership = hew_mir::ty_heap_ownership(
                elem,
                &CborHeapLayouts {
                    pipeline_records,
                    enum_layouts,
                },
            );
            if ownership.owns_heap || ownership.via_indirection {
                CborVecElemKind::Defer
            } else {
                CborVecElemKind::LayoutBitCopy
            }
        }
        // User record / enum elements: route both heap ownership and enum
        // indirectness through the single `hew_mir::ty_heap_ownership`
        // authority. A heap-free record/enum element is the
        // layout-aware BitCopy vec `Vec::new` constructs (`hew_vec_new_with_layout`).
        // A heap-OWNING record/enum element takes the OWNED (thunk-bearing) Vec ABI
        // — the same authority `Vec::new`'s `VecCtor::Owned` uses — provided it
        // resolves an owned thunk key; a heap-owning element with no resolvable key
        // (and every indirect enum, whose pointer ABI is a separate store family)
        // stays fail-closed.
        ResolvedTy::Named { .. } => {
            let ownership = hew_mir::ty_heap_ownership(
                elem,
                &CborHeapLayouts {
                    pipeline_records,
                    enum_layouts,
                },
            );
            if ownership.via_indirection {
                CborVecElemKind::Defer
            } else if ownership.owns_heap {
                if crate::thunks::owned_elem_thunk_key(regs, elem).is_some() {
                    CborVecElemKind::Owned
                } else {
                    CborVecElemKind::Defer
                }
            } else {
                CborVecElemKind::LayoutBitCopy
            }
        }
        _ => CborVecElemKind::Defer,
    }
}

/// Emit (or reuse) a thunk-less `HewTypeLayout` (`{size, align, ownership_kind=0}`)
/// global for a layout-aware BitCopy Vec element, returning a pointer suitable
/// for the `hew_vec_*_layout` accessor family.
///
/// This mirrors `crate::layout::layout_descriptor_ptr` EXACTLY (same struct
/// shape, same `__hew_layout_{label}_{size}_{align}_plain` dedup name, same
/// `ownership_kind = 0`, same `target_data`-driven size/align) so a descriptor
/// the codec stamps and a descriptor `Vec::new` stamps for the same element
/// collapse onto one global and pass the runtime's
/// `validate_bitcopy_layout_operation` equality check. The codec path has no
/// `FnCtx`, so it takes the raw `ctx`/`llvm_mod`/`target_data` directly.
fn codec_bitcopy_layout_descriptor_ptr<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    target_data: &TargetData,
    elem_llvm_ty: BasicTypeEnum<'ctx>,
    label: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    let (size, align) = abi_size_align(elem_llvm_ty, Some(target_data))?;
    let usize_ty = ctx.ptr_sized_int_type(target_data, None);
    let i8_ty = ctx.i8_type();
    let layout_ty = ctx.struct_type(&[usize_ty.into(), usize_ty.into(), i8_ty.into()], false);
    let global_name = format!("__hew_layout_{label}_{size}_{align}_plain");
    if let Some(global) = llvm_mod.get_global(&global_name) {
        return Ok(global.as_pointer_value());
    }
    let init = layout_ty.const_named_struct(&[
        usize_ty.const_int(size, false).into(),
        usize_ty.const_int(u64::from(align), false).into(),
        i8_ty.const_zero().into(),
    ]);
    let global = llvm_mod.add_global(layout_ty, None, &global_name);
    global.set_constant(true);
    global.set_linkage(Linkage::Private);
    global.set_initializer(&init);
    Ok(global.as_pointer_value())
}

/// Encode a `Vec<T>` field as a CBOR array. Encode is uniform across element
/// kinds: `hew_vec_get_generic` returns the element slot for both BitCopy and
/// `string` vecs (neither is layout-aware), and `emit_ser_value_cbor` loads it
/// (the `String` arm dereferences the slot). Owned-compound elements fail
/// closed, symmetric with `emit_de_vec_cbor` (which cannot reconstruct them).
#[allow(clippy::too_many_arguments)]
fn emit_ser_vec_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    elem_ty: &ResolvedTy,
    value_ptr: PointerValue<'ctx>,
    buf: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let kind = cbor_vec_elem_kind(elem_ty, pipeline_records, enum_layouts, machine_layouts);
    if kind == CborVecElemKind::Defer {
        return Err(CodegenError::FailClosed(format!(
            "wire CBOR serialize: Vec<{elem_ty:?}> has an element outside the \
             supported wire-body floor (bytes / indirect enum / Option-with-heap / \
             nested collection / HashMap or HashSet element / no resolvable owned \
             thunk key)"
        )));
    }
    let owned = kind == CborVecElemKind::Owned;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_ty = ctx.void_type();
    let i64_ty = ctx.i64_type();

    // A heap-free record/enum element is held in a LAYOUT-AWARE BitCopy vec
    // (`Vec::new` → `VecCtor::BitCopyLayout`), so its element slots must be read
    // through `hew_vec_get_layout` with a matching descriptor. The generic
    // accessor (`hew_vec_get_generic`) aborts on a layout-aware vec. Scalar /
    // string vecs carry a NULL layout and use the generic accessor.
    let layout_ptr = if kind == CborVecElemKind::LayoutBitCopy {
        let elem_llvm = resolve_ty(ctx, target_data, elem_ty, record_layouts)?;
        Some(codec_bitcopy_layout_descriptor_ptr(
            ctx,
            llvm_mod,
            target_data,
            elem_llvm,
            "wire_vec",
        )?)
    } else {
        None
    };

    // An owned (heap-owning) element rides an OWNED vec
    // (`ownership_kind = LayoutManaged`), whose slots are borrowed through
    // `hew_vec_get_owned`. Build the owned descriptor via the SAME single
    // authority `Vec::new` uses so the global dedup-collapses onto the
    // constructor's (and it validates the element resolves an owned thunk key at
    // encode too, symmetric with decode). The element is encoded in place from
    // its borrowed slot exactly as the LayoutBitCopy arm does.
    if owned {
        let elem_llvm = resolve_ty(ctx, target_data, elem_ty, record_layouts)?;
        let regs = cbor_owned_elem_registries(pipeline_records, enum_layouts, machine_layouts);
        crate::layout::owned_elem_layout_descriptor_ptr(
            ctx,
            llvm_mod,
            target_data,
            regs.registries(),
            elem_ty,
            elem_llvm,
            "wire_vec",
        )?;
    }

    let vec_ptr = builder
        .build_load(ptr_ty, value_ptr, "cbor_ser_vec_ptr")
        .llvm_ctx("cbor ser vec load")?
        .into_pointer_value();
    let len_fn = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_vec_len",
        i64_ty.fn_type(&[ptr_ty.into()], false),
    );
    let len = builder
        .build_call(len_fn, &[vec_ptr.into()], "cbor_ser_vec_len")
        .llvm_ctx("cbor ser vec len")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_vec_len void".into()))?
        .into_int_value();

    let begin = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_ser_begin_array",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    builder
        .build_call(begin, &[buf.into()], "cbor_ser_vec_begin")
        .llvm_ctx("cbor ser vec begin")?;

    // for (i = 0; i < len; i += 1) { encode element i }
    let idx_slot = builder
        .build_alloca(i64_ty, "cbor_ser_vec_i")
        .llvm_ctx("cbor ser vec i alloca")?;
    builder
        .build_store(idx_slot, i64_ty.const_zero())
        .llvm_ctx("cbor ser vec i init")?;
    let head_bb = ctx.append_basic_block(func, "cbor_ser_vec_head");
    let body_bb = ctx.append_basic_block(func, "cbor_ser_vec_body");
    let done_bb = ctx.append_basic_block(func, "cbor_ser_vec_done");
    builder
        .build_unconditional_branch(head_bb)
        .llvm_ctx("cbor ser vec entry br")?;

    builder.position_at_end(head_bb);
    let i_cur = builder
        .build_load(i64_ty, idx_slot, "cbor_ser_vec_i_cur")
        .llvm_ctx("cbor ser vec i load")?
        .into_int_value();
    let in_range = builder
        .build_int_compare(IntPredicate::SLT, i_cur, len, "cbor_ser_vec_cmp")
        .llvm_ctx("cbor ser vec cmp")?;
    builder
        .build_conditional_branch(in_range, body_bb, done_bb)
        .llvm_ctx("cbor ser vec head br")?;

    builder.position_at_end(body_bb);
    let elem_ptr = if owned {
        // Owned vec: borrow the element slot (`hew_vec_get_owned` returns the slot
        // pointer without transferring ownership) and encode it in place. The
        // slot's heap owners stay with the vec — the codec never frees them.
        let get_owned = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_vec_get_owned",
            ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        );
        builder
            .build_call(
                get_owned,
                &[vec_ptr.into(), i_cur.into()],
                "cbor_ser_vec_get_owned",
            )
            .llvm_ctx("cbor ser vec get_owned")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_vec_get_owned void".into()))?
            .into_pointer_value()
    } else if let Some(layout_ptr) = layout_ptr {
        let get_layout = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_vec_get_layout",
            ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), ptr_ty.into()], false),
        );
        builder
            .build_call(
                get_layout,
                &[vec_ptr.into(), i_cur.into(), layout_ptr.into()],
                "cbor_ser_vec_get_layout",
            )
            .llvm_ctx("cbor ser vec get_layout")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_vec_get_layout void".into()))?
            .into_pointer_value()
    } else {
        let get_gen = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_vec_get_generic",
            ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
        );
        builder
            .build_call(get_gen, &[vec_ptr.into(), i_cur.into()], "cbor_ser_vec_get")
            .llvm_ctx("cbor ser vec get_generic")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_vec_get_generic void".into()))?
            .into_pointer_value()
    };
    emit_ser_value_cbor(
        ctx,
        llvm_mod,
        builder,
        func,
        elem_ty,
        elem_ptr,
        buf,
        wire_layouts,
        record_layouts,
        machine_layouts,
        pipeline_records,
        enum_layouts,
        target_data,
    )?;
    let i_next = builder
        .build_int_add(i_cur, i64_ty.const_int(1, false), "cbor_ser_vec_inc")
        .llvm_ctx("cbor ser vec inc")?;
    builder
        .build_store(idx_slot, i_next)
        .llvm_ctx("cbor ser vec i store")?;
    builder
        .build_unconditional_branch(head_bb)
        .llvm_ctx("cbor ser vec body br")?;

    builder.position_at_end(done_bb);
    let end = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_ser_end_array",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    builder
        .build_call(end, &[buf.into()], "cbor_ser_vec_end")
        .llvm_ctx("cbor ser vec end")?;
    Ok(())
}

/// Decode a CBOR array into a `Vec<T>`. Constructs the backing vec matching the
/// element's ownership kind (so `hew_vec_free` releases element heap correctly),
/// then drains the array, decoding each element. Owned-compound elements fail
/// closed. On a malformed tail the partially built vec is owned by `dst` and
/// released by the enclosing type's drop helper (`hew_vec_free` is null-safe and
/// frees a string-kind vec's element strings).
#[allow(clippy::too_many_arguments)]
fn emit_de_vec_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    elem_ty: &ResolvedTy,
    dst: PointerValue<'ctx>,
    reader: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let kind = cbor_vec_elem_kind(elem_ty, pipeline_records, enum_layouts, machine_layouts);
    if kind == CborVecElemKind::Defer {
        return Err(CodegenError::FailClosed(format!(
            "wire CBOR deserialize: Vec<{elem_ty:?}> has an element outside the \
             supported wire-body floor (bytes / indirect enum / Option-with-heap / \
             nested collection / HashMap or HashSet element / no resolvable owned \
             thunk key)"
        )));
    }
    let owned = kind == CborVecElemKind::Owned;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_ty = ctx.void_type();
    let i64_ty = ctx.i64_type();
    let i32_ty = ctx.i32_type();

    // A heap-free record/enum element is reconstructed into a LAYOUT-AWARE
    // BitCopy vec (matching `Vec::new` → `VecCtor::BitCopyLayout`), so the rest
    // of the program — element indexing, scope-exit free — sees the same ABI it
    // would for a directly-constructed `Vec<record>`. Build the matching
    // descriptor once; the constructor and per-element push both consume it.
    let layout_ptr = if kind == CborVecElemKind::LayoutBitCopy {
        let elem_llvm = resolve_ty(ctx, target_data, elem_ty, record_layouts)?;
        Some(codec_bitcopy_layout_descriptor_ptr(
            ctx,
            llvm_mod,
            target_data,
            elem_llvm,
            "wire_vec",
        )?)
    } else {
        None
    };

    // A heap-owning element is reconstructed into an OWNED vec
    // (`ownership_kind = LayoutManaged`), matching `Vec::new` → `VecCtor::Owned`,
    // so element indexing and scope-exit `hew_vec_free` (which walks the per-
    // element `drop_fn`) see the same ABI a directly-constructed `Vec<owned>`
    // has. Built via the SAME single authority `Vec::new` uses, so the descriptor
    // global dedup-collapses onto the constructor's.
    let owned_layout_ptr = if owned {
        let elem_llvm = resolve_ty(ctx, target_data, elem_ty, record_layouts)?;
        let regs = cbor_owned_elem_registries(pipeline_records, enum_layouts, machine_layouts);
        Some(crate::layout::owned_elem_layout_descriptor_ptr(
            ctx,
            llvm_mod,
            target_data,
            regs.registries(),
            elem_ty,
            elem_llvm,
            "wire_vec",
        )?)
    } else {
        None
    };

    // Construct the backing vec matching the element ownership kind.
    let vec_ptr = match kind {
        CborVecElemKind::Str => {
            let new_str =
                declare_codec_prim(ctx, llvm_mod, "hew_vec_new_str", ptr_ty.fn_type(&[], false));
            builder
                .build_call(new_str, &[], "cbor_de_vec_new_str")
                .llvm_ctx("cbor de vec new_str")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_vec_new_str void".into()))?
                .into_pointer_value()
        }
        CborVecElemKind::Plain => {
            let elem_llvm = resolve_ty(ctx, target_data, elem_ty, record_layouts)?;
            let elem_size = elem_llvm
                .size_of()
                .ok_or_else(|| CodegenError::FailClosed("Vec element has no size".into()))?;
            let new_gen = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_vec_new_with_elem_size",
                ptr_ty.fn_type(&[i64_ty.into()], false),
            );
            builder
                .build_call(new_gen, &[elem_size.into()], "cbor_de_vec_new")
                .llvm_ctx("cbor de vec new_with_elem_size")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_vec_new_with_elem_size void".into()))?
                .into_pointer_value()
        }
        CborVecElemKind::LayoutBitCopy => {
            let layout_ptr = layout_ptr.ok_or_else(|| {
                CodegenError::FailClosed("layout-aware Vec decode missing descriptor".into())
            })?;
            let new_layout = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_vec_new_with_layout",
                ptr_ty.fn_type(&[ptr_ty.into()], false),
            );
            builder
                .build_call(new_layout, &[layout_ptr.into()], "cbor_de_vec_new_layout")
                .llvm_ctx("cbor de vec new_with_layout")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_vec_new_with_layout void".into()))?
                .into_pointer_value()
        }
        CborVecElemKind::Owned => {
            let owned_layout_ptr = owned_layout_ptr.ok_or_else(|| {
                CodegenError::FailClosed("owned Vec decode missing owned descriptor".into())
            })?;
            let new_owned = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_vec_new_with_elem_layout",
                ptr_ty.fn_type(&[ptr_ty.into()], false),
            );
            builder
                .build_call(
                    new_owned,
                    &[owned_layout_ptr.into()],
                    "cbor_de_vec_new_owned",
                )
                .llvm_ctx("cbor de vec new_with_elem_layout")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_vec_new_with_elem_layout void".into())
                })?
                .into_pointer_value()
        }
        CborVecElemKind::Defer => unreachable!("Defer handled above"),
    };
    builder
        .build_store(dst, vec_ptr)
        .llvm_ctx("cbor de vec store handle")?;

    let enter = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_de_enter_array",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    builder
        .build_call(enter, &[reader.into()], "cbor_de_vec_enter")
        .llvm_ctx("cbor de vec enter")?;

    // while (array_next) { decode element; push }
    let head_bb = ctx.append_basic_block(func, "cbor_de_vec_head");
    let body_bb = ctx.append_basic_block(func, "cbor_de_vec_body");
    let done_bb = ctx.append_basic_block(func, "cbor_de_vec_done");
    builder
        .build_unconditional_branch(head_bb)
        .llvm_ctx("cbor de vec entry br")?;

    builder.position_at_end(head_bb);
    let next_fn = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_de_array_next",
        i32_ty.fn_type(&[ptr_ty.into()], false),
    );
    let has = builder
        .build_call(next_fn, &[reader.into()], "cbor_de_vec_next")
        .llvm_ctx("cbor de vec next")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_array_next void".into()))?
        .into_int_value();
    let cont = builder
        .build_int_compare(
            IntPredicate::NE,
            has,
            i32_ty.const_zero(),
            "cbor_de_vec_has",
        )
        .llvm_ctx("cbor de vec has cmp")?;
    builder
        .build_conditional_branch(cont, body_bb, done_bb)
        .llvm_ctx("cbor de vec head br")?;

    builder.position_at_end(body_bb);
    match kind {
        CborVecElemKind::Str => {
            let de_string = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_cbor_de_string",
                ptr_ty.fn_type(&[ptr_ty.into()], false),
            );
            let s = builder
                .build_call(de_string, &[reader.into()], "cbor_de_vec_string")
                .llvm_ctx("cbor de vec string")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_string void".into()))?
                .into_pointer_value();
            let push_str = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_vec_push_str",
                void_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
            );
            builder
                .build_call(
                    push_str,
                    &[vec_ptr.into(), s.into()],
                    "cbor_de_vec_push_str",
                )
                .llvm_ctx("cbor de vec push_str")?;
            // push_str copies the string in; release our decode temp.
            let drop_str = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_string_drop",
                void_ty.fn_type(&[ptr_ty.into()], false),
            );
            builder
                .build_call(drop_str, &[s.into()], "cbor_de_vec_drop_str")
                .llvm_ctx("cbor de vec drop_str")?;
        }
        CborVecElemKind::Plain => {
            let elem_llvm = resolve_ty(ctx, target_data, elem_ty, record_layouts)?;
            let temp = builder
                .build_alloca(elem_llvm, "cbor_de_vec_elem")
                .llvm_ctx("cbor de vec elem alloca")?;
            // Zero the slot so a fail-closed partial element is a defined,
            // heap-free BitCopy value (a Plain element owns no heap to leak).
            builder
                .build_store(temp, elem_llvm.const_zero())
                .llvm_ctx("cbor de vec elem zero")?;
            emit_de_value_cbor(
                ctx,
                llvm_mod,
                builder,
                func,
                elem_ty,
                temp,
                reader,
                wire_layouts,
                record_layouts,
                machine_layouts,
                pipeline_records,
                enum_layouts,
                target_data,
            )?;
            let push_gen = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_vec_push_generic",
                void_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
            );
            builder
                .build_call(push_gen, &[vec_ptr.into(), temp.into()], "cbor_de_vec_push")
                .llvm_ctx("cbor de vec push_generic")?;
        }
        CborVecElemKind::LayoutBitCopy => {
            let layout_ptr = layout_ptr.ok_or_else(|| {
                CodegenError::FailClosed("layout-aware Vec decode missing descriptor".into())
            })?;
            let elem_llvm = resolve_ty(ctx, target_data, elem_ty, record_layouts)?;
            let temp = builder
                .build_alloca(elem_llvm, "cbor_de_vec_elem_layout")
                .llvm_ctx("cbor de vec layout elem alloca")?;
            // Zero the slot so a fail-closed partial element is a defined,
            // heap-free BitCopy value (a layout-BitCopy element owns no heap).
            builder
                .build_store(temp, elem_llvm.const_zero())
                .llvm_ctx("cbor de vec layout elem zero")?;
            emit_de_value_cbor(
                ctx,
                llvm_mod,
                builder,
                func,
                elem_ty,
                temp,
                reader,
                wire_layouts,
                record_layouts,
                machine_layouts,
                pipeline_records,
                enum_layouts,
                target_data,
            )?;
            let push_layout = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_vec_push_layout",
                void_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false),
            );
            builder
                .build_call(
                    push_layout,
                    &[vec_ptr.into(), temp.into(), layout_ptr.into()],
                    "cbor_de_vec_push_layout",
                )
                .llvm_ctx("cbor de vec push_layout")?;
        }
        CborVecElemKind::Owned => {
            let elem_llvm = resolve_ty(ctx, target_data, elem_ty, record_layouts)?;
            let temp = builder
                .build_alloca(elem_llvm, "cbor_de_vec_elem_owned")
                .llvm_ctx("cbor de vec owned elem alloca")?;
            // Zero the slot BEFORE decoding so a fail-closed partial element is a
            // defined value whose unwritten heap fields are null — the owned
            // `drop_fn` short-circuits on null, giving exactly-once on the failure
            // path.
            builder
                .build_store(temp, elem_llvm.const_zero())
                .llvm_ctx("cbor de vec owned elem zero")?;
            emit_de_value_cbor(
                ctx,
                llvm_mod,
                builder,
                func,
                elem_ty,
                temp,
                reader,
                wire_layouts,
                record_layouts,
                machine_layouts,
                pipeline_records,
                enum_layouts,
                target_data,
            )?;
            // MOVE the decoded element into the vec UNCONDITIONALLY — including on
            // a latched-failure iteration. `hew_vec_push_owned_move` byte-copies
            // the temp's heap owners into the vec slot (the temp goes dead, no
            // clone). On the failure path the thunk's `fail_bb` then frees the vec
            // via `hew_vec_free`, whose owned drop walks `drop_fn` per element and
            // short-circuits the zeroed-null partial fields — exactly-once on BOTH
            // success and failure, with no separate temp drop (which would
            // double-free the moved-out heap).
            let push_owned = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_vec_push_owned_move",
                void_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
            );
            builder
                .build_call(
                    push_owned,
                    &[vec_ptr.into(), temp.into()],
                    "cbor_de_vec_push_owned",
                )
                .llvm_ctx("cbor de vec push_owned_move")?;
        }
        CborVecElemKind::Defer => unreachable!("Defer handled above"),
    }
    builder
        .build_unconditional_branch(head_bb)
        .llvm_ctx("cbor de vec body br")?;

    builder.position_at_end(done_bb);
    let exit = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_de_exit_array",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    builder
        .build_call(exit, &[reader.into()], "cbor_de_vec_exit")
        .llvm_ctx("cbor de vec exit")?;
    Ok(())
}

/// Encode an `Option<T>` field under the null encoding: `None` emits CBOR
/// null; `Some(v)` emits the bare value `v`. Nested `Option<Option<_>>` is
/// ambiguous under this encoding (a `Some(None)` and an outer `None` both reduce
/// to null), so it fails closed.
#[allow(clippy::too_many_arguments)]
fn emit_ser_option_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    name: &str,
    args: &[ResolvedTy],
    value_ptr: PointerValue<'ctx>,
    buf: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_ty = ctx.void_type();
    let key = xnode_registry_key(name, args, pipeline_records, enum_layouts);
    let (Some(layout), Some(el)) = (
        machine_layouts.get(&key),
        enum_layouts.iter().find(|e| e.name == key),
    ) else {
        return Err(CodegenError::FailClosed(format!(
            "wire CBOR serialize: Option `{name}` (key `{key}`) has no enum layout"
        )));
    };
    let some_idx = el
        .variants
        .iter()
        .position(|v| !v.field_tys.is_empty())
        .ok_or_else(|| {
            CodegenError::FailClosed(format!("wire CBOR serialize: Option `{key}` has no Some"))
        })?;
    let payload_ty = &el.variants[some_idx].field_tys[0];
    if matches!(
        payload_ty,
        ResolvedTy::Named {
            builtin: Some(BuiltinType::Option),
            ..
        }
    ) {
        return Err(CodegenError::FailClosed(
            "wire CBOR serialize: Option<Option<_>> is ambiguous under the null \
             encoding — not supported"
                .into(),
        ));
    }

    let base = if el.is_indirect {
        builder
            .build_load(ptr_ty, value_ptr, "cbor_ser_opt_indirect")
            .llvm_ctx("cbor ser option indirect load")?
            .into_pointer_value()
    } else {
        value_ptr
    };
    let tag_ptr = builder
        .build_struct_gep(layout.outer_struct, base, 0, "cbor_ser_opt_tag_ptr")
        .llvm_ctx("cbor ser option tag gep")?;
    let tag = builder
        .build_load(layout.tag_int_ty, tag_ptr, "cbor_ser_opt_tag")
        .llvm_ctx("cbor ser option tag load")?
        .into_int_value();
    let some_const = layout.tag_int_ty.const_int(some_idx as u64, false);
    let is_some = builder
        .build_int_compare(IntPredicate::EQ, tag, some_const, "cbor_ser_opt_is_some")
        .llvm_ctx("cbor ser option is_some")?;

    let some_bb = ctx.append_basic_block(func, "cbor_ser_opt_some");
    let none_bb = ctx.append_basic_block(func, "cbor_ser_opt_none");
    let cont_bb = ctx.append_basic_block(func, "cbor_ser_opt_cont");
    builder
        .build_conditional_branch(is_some, some_bb, none_bb)
        .llvm_ctx("cbor ser option branch")?;

    builder.position_at_end(some_bb);
    let payload_ptr = builder
        .build_struct_gep(layout.outer_struct, base, 1, "cbor_ser_opt_payload")
        .llvm_ctx("cbor ser option payload gep")?;
    let field0_ptr = builder
        .build_struct_gep(
            layout.variant_struct_tys[some_idx],
            payload_ptr,
            0,
            "cbor_ser_opt_field0",
        )
        .llvm_ctx("cbor ser option field gep")?;
    emit_ser_value_cbor(
        ctx,
        llvm_mod,
        builder,
        func,
        payload_ty,
        field0_ptr,
        buf,
        wire_layouts,
        record_layouts,
        machine_layouts,
        pipeline_records,
        enum_layouts,
        target_data,
    )?;
    builder
        .build_unconditional_branch(cont_bb)
        .llvm_ctx("cbor ser option some br")?;

    builder.position_at_end(none_bb);
    let null_prim = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_ser_null",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    builder
        .build_call(null_prim, &[buf.into()], "cbor_ser_opt_null")
        .llvm_ctx("cbor ser option null")?;
    builder
        .build_unconditional_branch(cont_bb)
        .llvm_ctx("cbor ser option none br")?;

    builder.position_at_end(cont_bb);
    Ok(())
}

/// Decode an `Option<T>` field under the null encoding: CBOR null
/// reconstructs `None`; any other value reconstructs `Some(decode(value))`.
/// Nested `Option<Option<_>>` fails closed (ambiguous under null).
#[allow(clippy::too_many_arguments)]
fn emit_de_option_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    name: &str,
    args: &[ResolvedTy],
    dst: PointerValue<'ctx>,
    reader: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_ty = ctx.void_type();
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let key = xnode_registry_key(name, args, pipeline_records, enum_layouts);
    let (Some(layout), Some(el)) = (
        machine_layouts.get(&key),
        enum_layouts.iter().find(|e| e.name == key),
    ) else {
        return Err(CodegenError::FailClosed(format!(
            "wire CBOR deserialize: Option `{name}` (key `{key}`) has no enum layout"
        )));
    };
    let some_idx = el
        .variants
        .iter()
        .position(|v| !v.field_tys.is_empty())
        .ok_or_else(|| {
            CodegenError::FailClosed(format!("wire CBOR deserialize: Option `{key}` has no Some"))
        })?;
    let none_idx = el
        .variants
        .iter()
        .position(|v| v.field_tys.is_empty())
        .ok_or_else(|| {
            CodegenError::FailClosed(format!("wire CBOR deserialize: Option `{key}` has no None"))
        })?;
    let payload_ty = &el.variants[some_idx].field_tys[0];
    if matches!(
        payload_ty,
        ResolvedTy::Named {
            builtin: Some(BuiltinType::Option),
            ..
        }
    ) {
        return Err(CodegenError::FailClosed(
            "wire CBOR deserialize: Option<Option<_>> is ambiguous under the null \
             encoding — not supported"
                .into(),
        ));
    }

    // For an indirect Option, allocate the heap tagged-union and store the ptr.
    let base = if el.is_indirect {
        let size = layout
            .outer_struct
            .size_of()
            .ok_or_else(|| CodegenError::FailClosed("indirect Option has no size".into()))?;
        let align = i64_ty.const_int(8, false);
        let alloc_prim = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_alloc",
            ptr_ty.fn_type(&[i64_ty.into(), i64_ty.into()], false),
        );
        let heap = builder
            .build_call(
                alloc_prim,
                &[size.into(), align.into()],
                "cbor_de_opt_alloc",
            )
            .llvm_ctx("cbor de option alloc")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_alloc void".into()))?
            .into_pointer_value();
        builder
            .build_store(dst, heap)
            .llvm_ctx("cbor de option indirect store")?;
        heap
    } else {
        dst
    };

    let is_null_fn = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_de_is_null",
        i32_ty.fn_type(&[ptr_ty.into()], false),
    );
    let is_null = builder
        .build_call(is_null_fn, &[reader.into()], "cbor_de_opt_is_null")
        .llvm_ctx("cbor de option is_null")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_is_null void".into()))?
        .into_int_value();
    let is_null_b = builder
        .build_int_compare(
            IntPredicate::NE,
            is_null,
            i32_ty.const_zero(),
            "cbor_de_opt_nullc",
        )
        .llvm_ctx("cbor de option null cmp")?;

    let none_bb = ctx.append_basic_block(func, "cbor_de_opt_none");
    let some_bb = ctx.append_basic_block(func, "cbor_de_opt_some");
    let cont_bb = ctx.append_basic_block(func, "cbor_de_opt_cont");
    builder
        .build_conditional_branch(is_null_b, none_bb, some_bb)
        .llvm_ctx("cbor de option branch")?;

    builder.position_at_end(none_bb);
    let skip_fn = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_de_skip",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    builder
        .build_call(skip_fn, &[reader.into()], "cbor_de_opt_skip")
        .llvm_ctx("cbor de option skip")?;
    let none_tag = layout.tag_int_ty.const_int(none_idx as u64, false);
    let tag_ptr_none = builder
        .build_struct_gep(layout.outer_struct, base, 0, "cbor_de_opt_none_tag")
        .llvm_ctx("cbor de option none tag gep")?;
    builder
        .build_store(tag_ptr_none, none_tag)
        .llvm_ctx("cbor de option none tag store")?;
    builder
        .build_unconditional_branch(cont_bb)
        .llvm_ctx("cbor de option none br")?;

    builder.position_at_end(some_bb);
    let some_tag = layout.tag_int_ty.const_int(some_idx as u64, false);
    let tag_ptr_some = builder
        .build_struct_gep(layout.outer_struct, base, 0, "cbor_de_opt_some_tag")
        .llvm_ctx("cbor de option some tag gep")?;
    builder
        .build_store(tag_ptr_some, some_tag)
        .llvm_ctx("cbor de option some tag store")?;
    let payload_ptr = builder
        .build_struct_gep(layout.outer_struct, base, 1, "cbor_de_opt_payload")
        .llvm_ctx("cbor de option payload gep")?;
    let field0_ptr = builder
        .build_struct_gep(
            layout.variant_struct_tys[some_idx],
            payload_ptr,
            0,
            "cbor_de_opt_field0",
        )
        .llvm_ctx("cbor de option field gep")?;
    emit_de_value_cbor(
        ctx,
        llvm_mod,
        builder,
        func,
        payload_ty,
        field0_ptr,
        reader,
        wire_layouts,
        record_layouts,
        machine_layouts,
        pipeline_records,
        enum_layouts,
        target_data,
    )?;
    builder
        .build_unconditional_branch(cont_bb)
        .llvm_ctx("cbor de option some br")?;

    builder.position_at_end(cont_bb);
    Ok(())
}

/// Encode a wire enum under the "map-of-one" shape: a unit variant emits
/// its bare integer tag; a payload variant emits a single-entry map
/// `{tag: [field0, field1, ...]}`. The tag is the schema-stable wire tag
/// (`cbor_variant_tag`). An out-of-range in-memory discriminant traps
/// (exhaustiveness fallthrough), matching the bespoke codec.
#[allow(clippy::too_many_arguments)]
fn emit_ser_enum_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    key: &str,
    layout: &MachineCodegenLayout<'ctx>,
    el: &EnumLayout,
    value_ptr: PointerValue<'ctx>,
    buf: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_ty = ctx.void_type();
    let i64_ty = ctx.i64_type();

    let base = if el.is_indirect {
        builder
            .build_load(ptr_ty, value_ptr, "cbor_ser_enum_indirect")
            .llvm_ctx("cbor ser enum indirect load")?
            .into_pointer_value()
    } else {
        value_ptr
    };
    let tag_ptr = builder
        .build_struct_gep(layout.outer_struct, base, 0, "cbor_ser_enum_tag_ptr")
        .llvm_ctx("cbor ser enum tag gep")?;
    let tag = builder
        .build_load(layout.tag_int_ty, tag_ptr, "cbor_ser_enum_tag")
        .llvm_ctx("cbor ser enum tag load")?
        .into_int_value();

    let cont_bb = ctx.append_basic_block(func, "cbor_ser_enum_cont");
    let trap_bb = ctx.append_basic_block(func, "cbor_ser_enum_oob");
    let mut cases = Vec::with_capacity(layout.variant_struct_tys.len());
    let mut variant_bbs = Vec::with_capacity(layout.variant_struct_tys.len());
    for idx in 0..layout.variant_struct_tys.len() {
        let bb = ctx.append_basic_block(func, &format!("cbor_ser_enum_v{idx}"));
        cases.push((layout.tag_int_ty.const_int(idx as u64, false), bb));
        variant_bbs.push(bb);
    }
    builder
        .build_switch(tag, trap_bb, &cases)
        .llvm_ctx("cbor ser enum switch")?;

    builder.position_at_end(trap_bb);
    emit_trap_with_code_raw(
        ctx,
        llvm_mod,
        builder,
        HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH as u64,
        "cbor_ser_enum_oob",
    )?;

    let ser_u64 = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_ser_u64",
        void_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
    );
    let begin_map = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_ser_begin_map",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    let key_u64 = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_ser_key_u64",
        void_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false),
    );
    let begin_arr = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_ser_begin_array",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    let end_arr = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_ser_end_array",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    let end_map = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_ser_end_map",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );

    for (idx, variant_struct) in layout.variant_struct_tys.iter().enumerate() {
        builder.position_at_end(variant_bbs[idx]);
        let variant = &el.variants[idx];
        let wire_tag = cbor_variant_tag(wire_layouts, key, &variant.name, idx);
        let tag_const = i64_ty.const_int(wire_tag, false);
        if variant.field_tys.is_empty() {
            builder
                .build_call(
                    ser_u64,
                    &[buf.into(), tag_const.into()],
                    "cbor_ser_enum_unit",
                )
                .llvm_ctx("cbor ser enum unit tag")?;
        } else {
            builder
                .build_call(begin_map, &[buf.into()], "cbor_ser_enum_begin_map")
                .llvm_ctx("cbor ser enum begin_map")?;
            builder
                .build_call(
                    key_u64,
                    &[buf.into(), tag_const.into()],
                    "cbor_ser_enum_key",
                )
                .llvm_ctx("cbor ser enum key")?;
            builder
                .build_call(begin_arr, &[buf.into()], "cbor_ser_enum_begin_arr")
                .llvm_ctx("cbor ser enum begin_array")?;
            let payload_ptr = builder
                .build_struct_gep(
                    layout.outer_struct,
                    base,
                    1,
                    &format!("cbor_ser_enum_payload_{idx}"),
                )
                .llvm_ctx("cbor ser enum payload gep")?;
            for (fidx, fty) in variant.field_tys.iter().enumerate() {
                let field_ptr = builder
                    .build_struct_gep(
                        *variant_struct,
                        payload_ptr,
                        fidx as u32,
                        &format!("cbor_ser_enum_v{idx}_f{fidx}"),
                    )
                    .llvm_ctx("cbor ser enum field gep")?;
                emit_ser_value_cbor(
                    ctx,
                    llvm_mod,
                    builder,
                    func,
                    fty,
                    field_ptr,
                    buf,
                    wire_layouts,
                    record_layouts,
                    machine_layouts,
                    pipeline_records,
                    enum_layouts,
                    target_data,
                )?;
            }
            builder
                .build_call(end_arr, &[buf.into()], "cbor_ser_enum_end_arr")
                .llvm_ctx("cbor ser enum end_array")?;
            builder
                .build_call(end_map, &[buf.into()], "cbor_ser_enum_end_map")
                .llvm_ctx("cbor ser enum end_map")?;
        }
        builder
            .build_unconditional_branch(cont_bb)
            .llvm_ctx("cbor ser enum variant br")?;
    }
    builder.position_at_end(cont_bb);
    Ok(())
}

/// Decode a wire enum under the "map-of-one" shape. `hew_cbor_de_enum_begin` returns
/// the wire tag and stages the payload array (empty for a unit variant); a
/// switch on the wire tag selects the variant, stores the in-memory
/// discriminant, and drains the payload array into the variant's fields.
/// `hew_cbor_de_enum_end` closes the frame on every non-trap path. An unknown
/// wire tag traps (exhaustiveness fallthrough), matching the bespoke codec.
#[allow(clippy::too_many_arguments)]
fn emit_de_enum_cbor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
    func: FunctionValue<'ctx>,
    key: &str,
    layout: &MachineCodegenLayout<'ctx>,
    el: &EnumLayout,
    dst: PointerValue<'ctx>,
    reader: PointerValue<'ctx>,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_ty = ctx.void_type();
    let i64_ty = ctx.i64_type();
    let i32_ty = ctx.i32_type();

    let base = if el.is_indirect {
        let size = layout
            .outer_struct
            .size_of()
            .ok_or_else(|| CodegenError::FailClosed("indirect enum has no size".into()))?;
        let align = i64_ty.const_int(8, false);
        let alloc_prim = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_alloc",
            ptr_ty.fn_type(&[i64_ty.into(), i64_ty.into()], false),
        );
        let heap = builder
            .build_call(
                alloc_prim,
                &[size.into(), align.into()],
                "cbor_de_enum_alloc",
            )
            .llvm_ctx("cbor de enum alloc")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_alloc void".into()))?
            .into_pointer_value();
        builder
            .build_store(dst, heap)
            .llvm_ctx("cbor de enum indirect store")?;
        heap
    } else {
        dst
    };

    let enum_begin = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_de_enum_begin",
        i64_ty.fn_type(&[ptr_ty.into()], false),
    );
    let wire_tag = builder
        .build_call(enum_begin, &[reader.into()], "cbor_de_enum_begin")
        .llvm_ctx("cbor de enum begin")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_enum_begin void".into()))?
        .into_int_value();

    let cont_bb = ctx.append_basic_block(func, "cbor_de_enum_cont");
    let trap_bb = ctx.append_basic_block(func, "cbor_de_enum_oob");
    let mut cases = Vec::with_capacity(layout.variant_struct_tys.len());
    let mut variant_bbs = Vec::with_capacity(layout.variant_struct_tys.len());
    for idx in 0..layout.variant_struct_tys.len() {
        let bb = ctx.append_basic_block(func, &format!("cbor_de_enum_v{idx}"));
        let wt = cbor_variant_tag(wire_layouts, key, &el.variants[idx].name, idx);
        cases.push((i64_ty.const_int(wt, false), bb));
        variant_bbs.push(bb);
    }
    builder
        .build_switch(wire_tag, trap_bb, &cases)
        .llvm_ctx("cbor de enum switch")?;

    builder.position_at_end(trap_bb);
    // Unknown wire tag — no declared variant matches. Fail closed WITHOUT an
    // inline native trap: an inline trap here would unwind (via the actor
    // crash-recovery `siglongjmp` seam) PAST the deserialize thunk's
    // `hew_cbor_de_free(reader)` + `free(dst)` cleanup, leaking the parsed CBOR
    // tree and the partial value shell per malformed message. Instead latch the
    // reader failure and rejoin the normal control flow (the `cont_bb`
    // `enum_end` balances the payload frame `enum_begin` pushed). The thunk's
    // post-walk `failed` check then drops the partial value, frees the shell +
    // reader, and returns null — the call site traps — exactly the
    // free-before-trap discipline every other malformed shape already follows.
    //
    // Zero `base` first so the fail path's in-place drop sees a well-defined
    // variant-0 value with null payload. A direct enum's `base` is the thunk's
    // already-zeroed `dst`, but an indirect (recursive) enum's `base` is a fresh
    // `hew_alloc` block whose tag/payload are otherwise uninitialised — dropping
    // a garbage tag/payload would be undefined behaviour.
    let oob_size = layout
        .outer_struct
        .size_of()
        .ok_or_else(|| CodegenError::FailClosed("cbor de enum oob: value has no size".into()))?;
    let oob_memset_size_ty = runtime_size_ty(ctx, llvm_mod);
    let oob_memset = declare_codec_prim(
        ctx,
        llvm_mod,
        "memset",
        ptr_ty.fn_type(
            &[ptr_ty.into(), i32_ty.into(), oob_memset_size_ty.into()],
            false,
        ),
    );
    let oob_memset_size = if oob_memset_size_ty == i64_ty {
        oob_size
    } else {
        builder
            .build_int_truncate(oob_size, oob_memset_size_ty, "cbor_de_enum_oob_size_trunc")
            .llvm_ctx("cbor de enum oob size trunc")?
    };
    builder
        .build_call(
            oob_memset,
            &[
                base.into(),
                i32_ty.const_zero().into(),
                oob_memset_size.into(),
            ],
            "cbor_de_enum_oob_zero",
        )
        .llvm_ctx("cbor de enum oob zero")?;
    let de_fail = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_de_fail",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    builder
        .build_call(de_fail, &[reader.into()], "cbor_de_enum_oob_fail")
        .llvm_ctx("cbor de enum oob fail")?;
    builder
        .build_unconditional_branch(cont_bb)
        .llvm_ctx("cbor de enum oob br")?;

    let array_next = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_de_array_next",
        i32_ty.fn_type(&[ptr_ty.into()], false),
    );
    for (idx, variant_struct) in layout.variant_struct_tys.iter().enumerate() {
        builder.position_at_end(variant_bbs[idx]);
        // Store the in-memory discriminant = variant index.
        let tag_mem = layout.tag_int_ty.const_int(idx as u64, false);
        let tag_ptr = builder
            .build_struct_gep(layout.outer_struct, base, 0, "cbor_de_enum_tag_ptr")
            .llvm_ctx("cbor de enum tag gep")?;
        builder
            .build_store(tag_ptr, tag_mem)
            .llvm_ctx("cbor de enum tag store")?;
        let variant = &el.variants[idx];
        if !variant.field_tys.is_empty() {
            let payload_ptr = builder
                .build_struct_gep(
                    layout.outer_struct,
                    base,
                    1,
                    &format!("cbor_de_enum_payload_{idx}"),
                )
                .llvm_ctx("cbor de enum payload gep")?;
            for (fidx, fty) in variant.field_tys.iter().enumerate() {
                // Stage the next payload element; a missing field leaves nothing
                // staged and the field read fails closed.
                builder
                    .build_call(array_next, &[reader.into()], "cbor_de_enum_next")
                    .llvm_ctx("cbor de enum array_next")?;
                let field_ptr = builder
                    .build_struct_gep(
                        *variant_struct,
                        payload_ptr,
                        fidx as u32,
                        &format!("cbor_de_enum_v{idx}_f{fidx}"),
                    )
                    .llvm_ctx("cbor de enum field gep")?;
                emit_de_value_cbor(
                    ctx,
                    llvm_mod,
                    builder,
                    func,
                    fty,
                    field_ptr,
                    reader,
                    wire_layouts,
                    record_layouts,
                    machine_layouts,
                    pipeline_records,
                    enum_layouts,
                    target_data,
                )?;
            }
        }
        builder
            .build_unconditional_branch(cont_bb)
            .llvm_ctx("cbor de enum variant br")?;
    }
    builder.position_at_end(cont_bb);
    let enum_end = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_cbor_de_enum_end",
        void_ty.fn_type(&[ptr_ty.into()], false),
    );
    builder
        .build_call(enum_end, &[reader.into()], "cbor_de_enum_end")
        .llvm_ctx("cbor de enum end")?;
    Ok(())
}

/// Emit the full CBOR serialize + deserialize thunk bodies for `ty` (idempotent:
/// a second call for the same key no-ops). This is the sole wire body codec:
/// the `out_struct_size` write, the target-width `malloc`/`memset` zero-init,
/// and the fail-closed `emit_de_drop_owned` cleanup all route through the
/// `hew_cbor_*` runtime primitives.
#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
pub(crate) fn emit_cbor_codec_thunks<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    ty: &ResolvedTy,
    wire_layouts: &WireLayoutTable,
    record_layouts: &RecordLayoutMap<'ctx>,
    machine_layouts: &MachineLayoutMap<'ctx>,
    pipeline_records: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    target_data: &TargetData,
) -> CodegenResult<(String, String)> {
    let key = xnode_codec_key(ty);
    let ser_sym = format!("__hew_cbor_serialize_{key}");
    let de_sym = format!("__hew_cbor_deserialize_{key}");
    let ser_fn = get_or_declare_cbor_serialize_thunk(ctx, llvm_mod, &key);
    let de_fn = get_or_declare_cbor_deserialize_thunk(ctx, llvm_mod, &key);
    if ser_fn.count_basic_blocks() > 0 {
        return Ok((ser_sym, de_sym)); // already emitted
    }

    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();

    // ── serialize(value, out_len) -> bytes: build a CborSerBuf, walk, finish. ──
    {
        let builder = ctx.create_builder();
        let entry = ctx.append_basic_block(ser_fn, "entry");
        builder.position_at_end(entry);
        let value_ptr = ser_fn
            .get_nth_param(0)
            .ok_or_else(|| {
                CodegenError::FailClosed("cbor serialize thunk missing value param".into())
            })?
            .into_pointer_value();
        let out_len = ser_fn
            .get_nth_param(1)
            .ok_or_else(|| {
                CodegenError::FailClosed("cbor serialize thunk missing out_len param".into())
            })?
            .into_pointer_value();
        let buf_new = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_ser_new",
            ptr_ty.fn_type(&[], false),
        );
        let buf = builder
            .build_call(buf_new, &[], "cbor_ser_buf")
            .llvm_ctx("cbor_ser_new")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_cbor_ser_new void".into()))?
            .into_pointer_value();
        emit_ser_value_cbor(
            ctx,
            llvm_mod,
            &builder,
            ser_fn,
            ty,
            value_ptr,
            buf,
            wire_layouts,
            record_layouts,
            machine_layouts,
            pipeline_records,
            enum_layouts,
            target_data,
        )?;
        let finish = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_ser_finish",
            ptr_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
        );
        let bytes = builder
            .build_call(finish, &[buf.into(), out_len.into()], "cbor_ser_finish")
            .llvm_ctx("cbor_ser_finish")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_cbor_ser_finish void".into()))?
            .into_pointer_value();
        builder
            .build_return(Some(&bytes))
            .llvm_ctx("cbor ser return")?;
    }

    // ── deserialize: make a reader, alloc the value, walk, free reader. ──
    {
        let builder = ctx.create_builder();
        let entry = ctx.append_basic_block(de_fn, "entry");
        builder.position_at_end(entry);
        let data = de_fn
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("cbor deserialize thunk missing data".into()))?
            .into_pointer_value();
        let len = de_fn
            .get_nth_param(1)
            .ok_or_else(|| CodegenError::FailClosed("cbor deserialize thunk missing len".into()))?
            .into_int_value();
        let out_struct_size = de_fn
            .get_nth_param(2)
            .ok_or_else(|| {
                CodegenError::FailClosed("cbor deserialize thunk missing out_struct_size".into())
            })?
            .into_pointer_value();
        let de_reader_size_ty = runtime_size_ty(ctx, llvm_mod);
        let reader_new = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_de_new",
            ptr_ty.fn_type(&[ptr_ty.into(), de_reader_size_ty.into()], false),
        );
        let reader = builder
            .build_call(reader_new, &[data.into(), len.into()], "cbor_de_reader")
            .llvm_ctx("cbor_de_new")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_new void".into()))?
            .into_pointer_value();

        let llvm_ty = resolve_ty(ctx, target_data, ty, record_layouts)?;
        let size = llvm_ty.size_of().ok_or_else(|| {
            CodegenError::FailClosed("cbor deserialize: value has no static size".into())
        })?;
        builder
            .build_store(out_struct_size, size)
            .llvm_ctx("cbor de store struct size")?;
        let malloc_fn = get_or_declare_libc_malloc(ctx, llvm_mod);
        let malloc_size_ty = runtime_size_ty(ctx, llvm_mod);
        let malloc_size = if malloc_size_ty == i64_ty {
            size
        } else {
            builder
                .build_int_truncate(size, malloc_size_ty, "cbor_de_size_trunc")
                .llvm_ctx("cbor de malloc size trunc")?
        };
        let dst = builder
            .build_call(malloc_fn, &[malloc_size.into()], "cbor_de_value")
            .llvm_ctx("cbor de value malloc")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("malloc void".into()))?
            .into_pointer_value();

        // Fail closed on OOM — a null malloc must not reach memset / the walk.
        {
            let oom_bb = ctx.append_basic_block(de_fn, "cbor_de_oom");
            let alloc_ok_bb = ctx.append_basic_block(de_fn, "cbor_de_alloc_ok");
            let null_ptr = ptr_ty.const_null();
            let is_null = builder
                .build_int_compare(
                    IntPredicate::EQ,
                    builder
                        .build_ptr_to_int(dst, ctx.i64_type(), "cbor_dst_as_int")
                        .llvm_ctx("cbor dst ptr_to_int")?,
                    builder
                        .build_ptr_to_int(null_ptr, ctx.i64_type(), "cbor_null_as_int")
                        .llvm_ctx("cbor null ptr_to_int")?,
                    "cbor_dst_is_null",
                )
                .llvm_ctx("cbor de null check")?;
            builder
                .build_conditional_branch(is_null, oom_bb, alloc_ok_bb)
                .llvm_ctx("cbor de oom branch")?;
            builder.position_at_end(oom_bb);
            let reader_free_oom = declare_codec_prim(
                ctx,
                llvm_mod,
                "hew_cbor_de_free",
                ctx.void_type().fn_type(&[ptr_ty.into()], false),
            );
            builder
                .build_call(reader_free_oom, &[reader.into()], "cbor_de_reader_free_oom")
                .llvm_ctx("cbor de reader free oom")?;
            builder
                .build_return(Some(&ptr_ty.const_null()))
                .llvm_ctx("cbor de oom return")?;
            builder.position_at_end(alloc_ok_bb);
        }

        // Zero-init so partially-decoded values have well-defined slots.
        let memset_size_ty = runtime_size_ty(ctx, llvm_mod);
        let memset = declare_codec_prim(
            ctx,
            llvm_mod,
            "memset",
            ptr_ty.fn_type(
                &[ptr_ty.into(), ctx.i32_type().into(), memset_size_ty.into()],
                false,
            ),
        );
        let memset_size = if memset_size_ty == i64_ty {
            size
        } else {
            builder
                .build_int_truncate(size, memset_size_ty, "cbor_de_memset_size_trunc")
                .llvm_ctx("cbor de memset size trunc")?
        };
        builder
            .build_call(
                memset,
                &[
                    dst.into(),
                    ctx.i32_type().const_zero().into(),
                    memset_size.into(),
                ],
                "cbor_de_zero",
            )
            .llvm_ctx("cbor de zero-init")?;

        emit_de_value_cbor(
            ctx,
            llvm_mod,
            &builder,
            de_fn,
            ty,
            dst,
            reader,
            wire_layouts,
            record_layouts,
            machine_layouts,
            pipeline_records,
            enum_layouts,
            target_data,
        )?;

        // Fail closed: if the reader latched an error, drop the partial value's
        // owned fields, free the shell + the reader, and return null.
        let failed_prim = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_de_failed",
            ctx.i32_type().fn_type(&[ptr_ty.into()], false),
        );
        let failed = builder
            .build_call(failed_prim, &[reader.into()], "cbor_de_failed")
            .llvm_ctx("cbor de failed")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_cbor_de_failed void".into()))?
            .into_int_value();
        let reader_free = declare_codec_prim(
            ctx,
            llvm_mod,
            "hew_cbor_de_free",
            ctx.void_type().fn_type(&[ptr_ty.into()], false),
        );
        builder
            .build_call(reader_free, &[reader.into()], "cbor_de_reader_free")
            .llvm_ctx("cbor de reader free")?;

        let ok_bb = ctx.append_basic_block(de_fn, "cbor_de_ok");
        let fail_bb = ctx.append_basic_block(de_fn, "cbor_de_fail");
        let is_failed = builder
            .build_int_compare(
                IntPredicate::NE,
                failed,
                ctx.i32_type().const_zero(),
                "cbor_de_is_failed",
            )
            .llvm_ctx("cbor de is_failed cmp")?;
        builder
            .build_conditional_branch(is_failed, fail_bb, ok_bb)
            .llvm_ctx("cbor de fail branch")?;

        builder.position_at_end(fail_bb);
        // Drop any heap-owning fields already written before the failure; the
        // struct was zero-initialised, so unwritten fields are null and the
        // null-safe drop helpers short-circuit. Drop BEFORE free(dst) — the drop
        // helpers operate in-place — so free runs exactly once, no double-free.
        emit_de_drop_owned(
            ctx,
            llvm_mod,
            &builder,
            ty,
            dst,
            record_layouts,
            machine_layouts,
            pipeline_records,
            enum_layouts,
        )?;
        let free_fn = declare_codec_prim(
            ctx,
            llvm_mod,
            "free",
            ctx.void_type().fn_type(&[ptr_ty.into()], false),
        );
        builder
            .build_call(free_fn, &[dst.into()], "cbor_de_free_partial")
            .llvm_ctx("cbor de free partial")?;
        builder
            .build_return(Some(&ptr_ty.const_null()))
            .llvm_ctx("cbor de fail return")?;

        builder.position_at_end(ok_bb);
        builder
            .build_return(Some(&dst))
            .llvm_ctx("cbor de return")?;
    }

    Ok((ser_sym, de_sym))
}
