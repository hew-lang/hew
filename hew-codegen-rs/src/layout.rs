//! Type-layout registration and layout-descriptor emission for the LLVM backend.
//!
//! Pure relocation (R4 god-module carve) of the named-type layout machinery out
//! of llvm.rs: record/enum/machine named-struct predeclaration and body fill,
//! tagged-union layout construction, the host data-layout string, the heap-owning
//! composite-return classifiers, the layout-vec runtime declaration/type
//! helpers, and the layout-descriptor / owned-element / channel-element witness
//! pointer emitters.
//!
//! Mirrors the `crate::coro` carve: the shared `RecordLayoutMap` /
//! `MachineCodegenLayout` context types and the clone/drop in-place helpers stay
//! in `crate::llvm` (held by `FnCtx`); this module imports them and `crate::llvm`
//! calls back via `crate::layout::*`. No behaviour change — every emitted `.ll`
//! is byte-identical before and after.

use std::collections::{HashMap, HashSet};

use inkwell::context::Context;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine};
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum, IntType, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, IntPredicate};

use hew_mir::{
    EnumLayout, IrPipeline, MachineLayout, MachineVariantLayout, Place, RecordLayout,
    StateFieldCloneKind, Terminator,
};
use hew_runtime::internal::types::HEW_TRAP_INDEX_OUT_OF_BOUNDS;
use hew_types::{BuiltinType, ResolvedTy};

#[allow(unused_imports)]
use crate::llvm::*;

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
/// Allocate one opaque LLVM named struct for every record / enum / machine
/// outer type (and every `<Name>Event` companion) in the pipeline, returning
/// a `RecordLayoutMap` pre-populated with those opaque handles.
///
/// This MUST run before any body-fill pass (`fill_record_layout_bodies`,
/// `build_tagged_union_layout`) because each of those passes resolves
/// field/variant types through `resolve_ty`, which in turn looks names up
/// in this map. Without predeclaration, a record field of enum type (or an
/// enum payload of record type, etc.) would fall through `resolve_ty` to
/// `primitive_to_llvm`'s D10 fail-closed sentinel — the symptom W4.012
/// Stage 2 fixes.
///
/// **Within-class duplicates** (the same enum name appearing twice in
/// `pipeline.enum_layouts`, etc.) are tolerated: the second registration
/// is a no-op against the first opaque. This matches the pre-Stage-2
/// behaviour where `register_enum_layouts` would silently overwrite the
/// map entry on a repeated name. Repeated layouts are produced by the
/// current pipeline for some generic-enum monomorphisations (e.g.
/// `Option$$i64` registered along multiple stdlib paths); a strict
/// fail-closed there would regress existing passing tests.
///
/// **Cross-class duplicates** (a record and an enum sharing a name) surface
/// as `CodegenError::FailClosed` — defence in depth for a MIR-producer
/// invariant (layout names should be disjoint across record/enum/machine
/// spaces).
pub(crate) fn predeclare_named_layouts<'ctx>(
    ctx: &'ctx Context,
    records: &[RecordLayout],
    enums: &[EnumLayout],
    machines: &[MachineLayout],
    opaque_handle_names: &[String],
) -> CodegenResult<RecordLayoutMap<'ctx>> {
    // `class_owner` tracks which class first registered each name so a
    // later registration from a different class can fail-closed with a
    // useful diagnostic. Within-class repeats are silently idempotent.
    #[derive(Copy, Clone, PartialEq, Eq)]
    enum Class {
        Record,
        Enum,
        Machine,
    }
    impl Class {
        fn label(self) -> &'static str {
            match self {
                Class::Record => "record",
                Class::Enum => "enum",
                Class::Machine => "machine",
            }
        }
    }
    let mut map: RecordLayoutMap<'ctx> = RecordLayoutMap::new();
    map.opaque.extend(opaque_handle_names.iter().cloned());
    let mut class_owner: HashMap<String, Class> = HashMap::new();
    let insert = |map: &mut RecordLayoutMap<'ctx>,
                  owners: &mut HashMap<String, Class>,
                  class: Class,
                  name: &str|
     -> CodegenResult<()> {
        if let Some(existing) = owners.get(name) {
            if *existing == class {
                // Within-class duplicate: idempotent no-op against the
                // already-allocated opaque struct.
                return Ok(());
            }
            return Err(CodegenError::FailClosed(format!(
                "duplicate layout name `{name}` across {} and {} layout classes — \
                 MIR producer must use disjoint type names across record/enum/machine spaces",
                existing.label(),
                class.label()
            )));
        }
        let st = ctx.opaque_struct_type(name);
        map.insert(name.to_string(), st);
        owners.insert(name.to_string(), class);
        Ok(())
    };
    for layout in records {
        insert(&mut map, &mut class_owner, Class::Record, &layout.name)?;
    }
    for layout in enums {
        insert(&mut map, &mut class_owner, Class::Enum, &layout.name)?;
    }
    for layout in machines {
        insert(&mut map, &mut class_owner, Class::Machine, &layout.name)?;
        insert(
            &mut map,
            &mut class_owner,
            Class::Machine,
            &format!("{}Event", layout.name),
        )?;
    }
    Ok(map)
}

/// Pass 2 of record-layout registration: walk every record's field list,
/// lower each field type to LLVM via `resolve_ty`, and `set_body` on the
/// opaque struct allocated by `predeclare_named_layouts`.
///
/// Because the map already contains opaque entries for every other record,
/// every enum, and every machine outer/event struct in the pipeline, a
/// record field of `Named { name: "<EnumName>" }` (or any other registered
/// name) resolves to that opaque type rather than tripping the D10
/// fail-closed sentinel in `primitive_to_llvm`.
pub(crate) fn fill_record_layout_bodies<'ctx>(
    ctx: &'ctx Context,
    layouts: &[RecordLayout],
    map: &RecordLayoutMap<'ctx>,
    target_data: &TargetData,
) -> CodegenResult<()> {
    for layout in layouts {
        let st = map.get(&layout.name).copied().ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "record `{}` missing from predeclared layout map — \
                 predeclare_named_layouts must run before fill_record_layout_bodies",
                layout.name
            ))
        })?;
        let mut field_tys: Vec<BasicTypeEnum<'ctx>> = Vec::with_capacity(layout.field_tys.len());
        for fty in &layout.field_tys {
            field_tys.push(resolve_ty(ctx, target_data, fty, map)?);
        }
        // packed = false: use the target's natural alignment per
        // `RecordLayout` doc (A-6b). LESSONS: parity-or-tracked-gap.
        st.set_body(&field_tys, false);
    }
    Ok(())
}

/// Return the native host data-layout string, initialising LLVM's native target
/// exactly once per process (guarded by `OnceLock`).
///
/// `TargetData` wraps a raw pointer and is not `Sync`, so we cache the
/// layout *string* (which is `Sync`) and construct a fresh `TargetData`
/// from it on each call via `TargetData::create`. `TargetData::create` is
/// cheap — it only parses the layout string into target-description tables;
/// it does not allocate LLVM IR or touch the global PassManager.
///
/// Object emission passes target-specific `TargetData` into module
/// construction. This host helper is only for target-agnostic IR inspection
/// tests and the debug `.ll` artefact.
pub(crate) fn host_data_layout_string() -> &'static str {
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
pub(crate) fn register_machine_layouts<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    machine_layouts: &[MachineLayout],
    record_layout_map: &mut RecordLayoutMap<'ctx>,
    enum_layouts: &[EnumLayout],
    target_data: Option<&TargetData>,
) -> CodegenResult<MachineLayoutMap<'ctx>> {
    let mut map: MachineLayoutMap<'ctx> = HashMap::new();
    for layout in machine_layouts {
        // Within-class duplicate guard (mirrors `register_enum_layouts`):
        // a second `set_body` against the same predeclared opaque struct
        // is invalid LLVM. Skip the repeat if the outer struct's
        // per-variant metadata is already present.
        if map.contains_key(&layout.name) {
            continue;
        }
        // The state-side machine value: `<Name>` with state-variant payloads.
        let mut machine_cg = build_tagged_union_layout(
            ctx,
            &layout.name,
            layout.tag_width,
            &layout.variants,
            record_layout_map,
            enum_layouts,
            target_data,
        )?;
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

        // The companion event enum: `<Name>Event` with event-variant payloads.
        let event_name = format!("{}Event", layout.name);
        let event_cg = build_tagged_union_layout(
            ctx,
            &event_name,
            minimum_tag_width(layout.events.len()),
            &layout.events,
            record_layout_map,
            enum_layouts,
            target_data,
        )?;
        record_layout_map.insert(event_name.clone(), event_cg.outer_struct);
        map.insert(event_name, event_cg);
    }
    Ok(map)
}

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
pub(crate) fn register_enum_layouts<'ctx>(
    ctx: &'ctx Context,
    enum_layouts: &[EnumLayout],
    record_layout_map: &mut RecordLayoutMap<'ctx>,
    machine_layout_map: &mut MachineLayoutMap<'ctx>,
    target_data: Option<&TargetData>,
) -> CodegenResult<()> {
    // Within-class duplicate enum-layout entries (the same mangled name
    // registered along multiple stdlib paths) are tolerated by
    // `predeclare_named_layouts` as a no-op. Here we must also skip the
    // duplicate body-fill, because `build_tagged_union_layout` calls
    // `set_body` on the predeclared opaque — a second `set_body` against
    // the same struct is invalid LLVM. The first registration wins;
    // `machine_layout_map` retains the per-variant metadata it produced.
    //
    // Process in dependency order: `build_tagged_union_layout` queries the
    // ABI size of each variant's LLVM struct via `TargetData::get_abi_size`.
    // If a variant's field type is itself an enum whose body is not yet set
    // (still opaque), `get_abi_size` returns 0 and the outer payload array
    // is under-sized, silently corrupting extraction GEPs. Topological
    // ordering ensures inner enums (e.g. `Option<i64>`) are fully registered
    // before outer enums that reference them (e.g.
    // `Result<Option<i64>, TimeoutError>`).
    //
    // WHY the input order is wrong: `try_register_enum_instantiation_ty` in
    // the HIR uses a LIFO worklist that inserts `Result<Option<T>, E>` before
    // `Option<T>` because the outer type is popped and inserted first, then
    // its args are enqueued. This is a pre-existing HIR-side ordering gap;
    // sorting here is the fail-closed fix rather than a silent mis-size.
    //
    // WHEN OBSOLETE: if the HIR registration is changed to emit inner types
    // before outer types (e.g. by switching to post-order traversal), this
    // sort remains a no-op (already ordered correctly) and can be removed.
    //
    // Algorithm: Kahn's topological sort over the named-enum dependency graph.
    // Edge A → B means "A's body references B's named struct" (B must be
    // set before A). The sort is stable within each tier so unrelated layouts
    // keep their input order, which is important for the first-registration-wins
    // dedup policy.
    let names: HashSet<&str> = enum_layouts.iter().map(|l| l.name.as_str()).collect();

    // `deps(i)` = indices of enum_layouts that layout `i` depends on
    // (i.e., names that appear as variant field types and are themselves
    // enum layout names).
    let dep_of: Vec<Vec<usize>> = enum_layouts
        .iter()
        .map(|layout| {
            let mut deps: Vec<usize> = Vec::new();
            for variant in &layout.variants {
                for field_ty in &variant.field_tys {
                    collect_named_enum_deps(field_ty, &names, enum_layouts, &mut deps);
                }
            }
            deps.sort_unstable();
            deps.dedup();
            // Drop self-dependency (shouldn't exist for non-recursive enums, but
            // be defensive).
            let self_idx = enum_layouts
                .iter()
                .position(|l| l.name == layout.name)
                .unwrap_or(usize::MAX);
            deps.retain(|&d| d != self_idx);
            deps
        })
        .collect();

    // In-degree for Kahn's: in_degree[i] = number of enum deps layout i still
    // needs before it can be processed (= dep_of[i].len() initially).
    let n = enum_layouts.len();
    let in_degree: Vec<usize> = dep_of.iter().map(|d| d.len()).collect();

    // Build a reverse-adjacency: for each layout d, which layouts depend on d.
    let mut reverse: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (i, deps) in dep_of.iter().enumerate() {
        for &d in deps {
            reverse[d].push(i);
        }
    }

    // Kahn's algorithm with a VecDeque for stable (input-order) processing.
    let mut order: Vec<usize> = Vec::with_capacity(n);
    {
        let mut in_deg = in_degree.clone();
        // Use a stable queue: drain zero-in-degree nodes in input index order.
        let mut queue: std::collections::VecDeque<usize> =
            (0..n).filter(|&i| in_deg[i] == 0).collect();
        while let Some(i) = queue.pop_front() {
            order.push(i);
            for &j in &reverse[i] {
                in_deg[j] -= 1;
                if in_deg[j] == 0 {
                    queue.push_back(j);
                }
            }
        }
        if order.len() != n {
            // Cycle detected: a non-indirect (inline, value-typed) enum field
            // creates a circular inline layout. Fail closed with a diagnostic.
            //
            // Non-indirect enum values are laid out inline (finite-size tagged
            // unions). A layout cycle among non-indirect enums would produce a
            // zero-byte payload alloca — a silent miscompile that the LLVM
            // verifier does not catch. Indirect (boxed) enums are already
            // excluded from cycle edges by `collect_named_enum_deps`; only a
            // genuine inline-layout cycle reaches this branch.
            //
            // WHY: the fallback to input order was the original behaviour and
            // produced a zero-byte alloca for the cyclic variant.
            // WHEN OBSOLETE: never — an inline layout cycle is structurally
            // unrepresentable. If the type is boxed, declare it `indirect enum`
            // so the pointer-shaped layout eliminates the cycle.
            let cycle_names: Vec<&str> = enum_layouts
                .iter()
                .enumerate()
                .filter(|(i, _)| !order.contains(i))
                .map(|(_, l)| l.name.as_str())
                .collect();
            return Err(CodegenError::FailClosed(format!(
                "cyclic enum layout detected — non-indirect Hew enums are inline value types \
                 and cannot form a layout cycle; \
                 the following enum name(s) form a layout cycle: {}. \
                 Declare the enum(s) as `indirect enum` to break the cycle (indirect enums \
                 are heap-boxed and hold a pointer, which has a fixed finite size).",
                cycle_names.join(", ")
            )));
        }
    }

    for i in order {
        let layout = &enum_layouts[i];
        if machine_layout_map.contains_key(&layout.name) {
            continue;
        }
        // Convert `EnumLayout.variants` (which are `MachineVariantLayout`)
        // directly — they share the same shape (`name`, `field_tys`).
        let enum_cg = build_tagged_union_layout(
            ctx,
            &layout.name,
            layout.tag_width,
            &layout.variants,
            record_layout_map,
            enum_layouts,
            target_data,
        )?;
        // Register the outer struct so `resolve_ty` resolves
        // `ResolvedTy::Named { name: "<EnumName>" }` the same way as a
        // machine-typed or record-typed local. (The opaque was inserted
        // by `predeclare_named_layouts`; this overwrite stores the same
        // `StructType` handle that `set_body` mutated in place.)
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
pub(crate) fn build_tagged_union_layout<'ctx>(
    ctx: &'ctx Context,
    outer_name: &str,
    tag_width: u32,
    variants: &[MachineVariantLayout],
    record_layout_map: &RecordLayoutMap<'ctx>,
    enum_layouts: &[EnumLayout],
    target_data: Option<&TargetData>,
) -> CodegenResult<MachineCodegenLayout<'ctx>> {
    let fallback_target_data;
    let target_data = if let Some(target_data) = target_data {
        target_data
    } else {
        fallback_target_data = host_target_data();
        &fallback_target_data
    };
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
            .map(|fty| {
                // `indirect enum` field types must resolve to `ptr` (not the
                // named struct), because indirect-enum variables are
                // heap-allocated and every field reference is a heap pointer.
                // `resolve_ty` normally returns the named struct when the name
                // appears in the struct-layout map (struct-first ordering for
                // W4.011 collision safety: a user record can share a short name
                // with a stdlib `#[opaque]` handle, e.g. user `Value` vs
                // `json.Value`).
                //
                // Gate on `is_indirect_enum` — the same precise predicate that
                // `declare_function`, `lower_function`, and `lower_vec_index`
                // use — rather than the opaque-set proxy. The opaque set also
                // contains every `#[opaque]` stdlib handle name; checking it
                // before `resolve_ty` would invert the struct-first invariant
                // for any user aggregate whose short name collides with an
                // opaque-handle entry, causing an undersized payload (fail-open
                // heap corruption). `#[opaque]` handle fields still resolve to
                // `ptr` through `resolve_ty`'s own post-struct opaque check, so
                // no field shape regresses.
                if let ResolvedTy::Named { name, .. } = fty {
                    if is_indirect_enum(name, enum_layouts) {
                        return Ok(ctx.ptr_type(AddressSpace::default()).into());
                    }
                }
                resolve_ty(ctx, target_data, fty, record_layout_map)
            })
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
    let td = target_data;
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
        // JUSTIFIED: Display-format preserved. `custom_width_int_type` returns
        // `Result<IntType, LLVMString>`, and the original wrap uses `{e}` (not
        // `{e:?}`) to render the inkwell `LLVMString` as the LLVM diagnostic
        // text without quoting it. Migrating to `LlvmResultExt::llvm_ctx` would
        // silently switch to Debug projection (`{e:?}`) and change the
        // user-visible error text. §B4 of the design deliberately leaves this
        // one Display call un-migrated; if a second Display site ever appears,
        // introduce a sibling `.llvm_ctx_display` helper.
        .map_err(|e| CodegenError::Llvm(format!("custom_width_int_type({element_bits}): {e}")))?;

    #[cfg(debug_assertions)]
    assert_eq!(
        tag_storage_width(tag_width)?,
        legacy_tag_storage_width(variants.len())?,
        "tag-width authority drift for `{outer_name}`: carried width {tag_width} over {} variants",
        variants.len()
    );
    let tag_int_ty = tag_int_type_for_width(ctx, outer_name, tag_width)?;
    let payload_arr_ty = payload_element_int_ty.array_type(element_count_u32);

    let outer_struct = record_layout_map.get(outer_name).copied().ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "tagged-union layout `{outer_name}` missing from predeclared layout map — \
             predeclare_named_layouts must include every enum/machine outer + <Name>Event name"
        ))
    })?;
    outer_struct.set_body(&[tag_int_ty.into(), payload_arr_ty.into()], false);

    Ok(MachineCodegenLayout {
        outer_struct,
        tag_int_ty,
        variant_struct_tys,
        variant_field_tys,
        state_name_table: None,
    })
}

fn minimum_tag_width(variant_count: usize) -> u32 {
    let count = variant_count.max(1);
    (usize::BITS - (count - 1).leading_zeros()).max(1)
}

fn tag_storage_width(tag_width: u32) -> CodegenResult<u32> {
    match tag_width {
        1..=8 => Ok(8),
        9..=16 => Ok(16),
        _ => Err(CodegenError::Unsupported(
            "machine tagged-union layout supports tag widths from 1 through 16 bits",
        )),
    }
}

fn tag_int_type_for_width<'ctx>(
    ctx: &'ctx Context,
    _outer_name: &str,
    tag_width: u32,
) -> CodegenResult<IntType<'ctx>> {
    match tag_storage_width(tag_width)? {
        8 => Ok(ctx.i8_type()),
        16 => Ok(ctx.i16_type()),
        _ => unreachable!("tag_storage_width returns only supported storage buckets"),
    }
}

#[cfg(debug_assertions)]
fn legacy_tag_storage_width(variant_count: usize) -> CodegenResult<u32> {
    if variant_count <= 256 {
        Ok(8)
    } else if variant_count <= 65_536 {
        Ok(16)
    } else {
        Err(CodegenError::Unsupported(
            "machine tagged-union layout supports at most 65,536 variants",
        ))
    }
}

/// Locate a machine's tagged-union codegen layout for an MIR local known
/// to hold a machine value. Reads the local's resolved type and consults
/// the machine layout map. Fails closed if the local is not a machine
/// type or the machine name is not registered.
pub(crate) fn machine_layout_for_local<'a, 'ctx>(
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
        ResolvedTy::Named { name, args, .. } => (name, args),
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
        // Monomorphic enums/machines register under their bare declaration
        // name (`register_enum_layouts` / `register_machine_layouts` key by
        // `EnumLayout.name` = the HIR `decl.name`, which is unqualified). A
        // Place at an IMPORTER carries the module-qualified name (`fs.IoError`)
        // because the local's `ResolvedTy::Named` was resolved against the
        // importing scope. Strip the `module.` prefix when the qualified key
        // is absent but the bare name is registered, mirroring the generic-enum
        // branch's `short_name` fallback below.
        // WHY: without this, constructing/matching an imported enum-with-data
        //   (e.g. `IoError` from `std::fs`/`std::net`) fails closed at codegen
        //   even though MIR registered the layout under the bare name.
        // WHEN-OBSOLETE: if layouts are re-keyed by `(module, name)` so the
        //   importer's qualified lookup matches directly.
        if fn_ctx.machine_layouts.contains_key(name) {
            name.clone()
        } else {
            short_name(name).to_string()
        }
    } else {
        // Shorten the type-arg spine for the full-outer-name key too: the
        // registration side keys on bare args, so a raw `mangle(name, args)`
        // carrying a qualified payload would never hit and only the short_key
        // fallback would save it. Keying both candidates off the shortened
        // spine removes that raw-args trap at this layout-lookup site.
        let key = mangle_with_shortened_args(name, args);
        if !fn_ctx.machine_layouts.contains_key(&key) {
            let short_key = mangle_with_shortened_args(short_name(name), args);
            if fn_ctx.machine_layouts.contains_key(&short_key) {
                short_key
            } else if fn_ctx.machine_layouts.contains_key(short_name(name)) {
                short_name(name).to_string()
            } else {
                return Err(CodegenError::FailClosed(format!(
                    "Place::MachineTag/MachineVariant references generic enum `{name}` \
                     with type args {args:?}: mangled key `{key}` is not in \
                     IrPipeline.machine_layouts — the monomorphisation was not registered \
                     by `register_enum_layouts` (registration-mismatch)"
                )));
            }
        } else {
            key
        }
    };
    fn_ctx.machine_layouts.get(&lookup_key).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "Place::MachineTag/MachineVariant references machine `{name}` which is not \
             in IrPipeline.machine_layouts — registration mismatch between MIR producer \
             and codegen"
        ))
    })
}

/// Resolve the mangled `enum_layouts` registration key for a `ResolvedTy::Named`
/// enum composite, matching the scheme `register_enum_layouts` uses (bare name
/// for monomorphic enums, `mangle(short_name, args)` for generic
/// instantiations). This key is also the `<Enum>` segment of the synthesised
/// `__hew_enum_drop_inplace_<Enum>` helper, so the W5.020 caller-side drop and
/// the synthesis pass agree on one symbol. Fails closed for any non-enum or
/// unregistered type rather than guessing a key.
pub(crate) fn enum_layout_key_for_ty(
    fn_ctx: &FnCtx<'_, '_>,
    ty: &ResolvedTy,
) -> CodegenResult<String> {
    enum_layout_key_for_ty_from(fn_ctx.enum_layouts, ty)
}

/// `enum_layout_key_for_ty` with the enum-layout slice supplied directly, for
/// synthesis paths that resolve the key without a live `FnCtx` (e.g. the
/// state-clone/drop synthesis pass routing an indirect-enum child field through
/// the recursive free thunk). Same resolution as the `FnCtx` form.
pub(crate) fn enum_layout_key_for_ty_from(
    enum_layouts: &[EnumLayout],
    ty: &ResolvedTy,
) -> CodegenResult<String> {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return Err(CodegenError::FailClosed(format!(
            "enum in-place drop: ElabDrop::ty {ty:?} is not a named enum type"
        )));
    };
    let short = short_name(name);
    let key = if args.is_empty() {
        enum_layouts
            .iter()
            .find(|el| el.name == *name || short_name(&el.name) == short)
            .map(|el| el.name.clone())
    } else {
        let mangled = mangle_with_shortened_args(short, args);
        enum_layouts
            .iter()
            .find(|el| el.name == mangled || el.name == *name)
            .map(|el| el.name.clone())
    };
    key.ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "enum in-place drop: enum type `{name}` (args {args:?}) has no entry in \
             IrPipeline.enum_layouts — the MIR producer emitted an EnumInPlace drop for \
             an unregistered enum (registration mismatch)"
        ))
    })
}

/// True when `ty` is an inline (non-indirect) tagged-union enum composite — the
/// SHAPE the W5.020 caller-side drop covers (`Result<T, string>`,
/// `Option<string>`, a user enum with an owned-payload variant; the MIR
/// elaborator emits a matching `DropKind::EnumInPlace`). Indirect (heap-boxed)
/// enums are excluded — their payload drop runs through the boxed-storage release
/// path.
///
/// SHAPE-ONLY: the heap-ownership decision is made ONCE at the composite-return
/// gate via the record-aware `resolved_ty_contains_heap_leaf`
/// (`hew_mir::ty_owns_heap` over `CgHeapLayouts`), so an enum owning heap only
/// through a nested record field is seen — the record-blind enum-layouts walk
/// missed it, leaving the gate and the MIR drop elaborator agreeing by
/// coincidence (`dedup-semantic-boundary`). This predicate only confirms the
/// shape is a supported inline enum; the gate's outer guard already established
/// "owns heap".
pub(crate) fn is_inline_enum_composite_shape(ty: &ResolvedTy, enum_layouts: &[EnumLayout]) -> bool {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    let short = short_name(name);
    let layout = if args.is_empty() {
        enum_layouts
            .iter()
            .find(|el| el.name == *name || short_name(&el.name) == short)
    } else {
        let mangled = mangle_with_shortened_args(short, args);
        enum_layouts
            .iter()
            .find(|el| el.name == mangled || el.name == *name)
    };
    let Some(layout) = layout else {
        return false;
    };
    !layout.is_indirect
}

/// True when a function return type is a **tuple** composite — the SHAPE whose
/// per-element drop the W5.021 spine emits (`DropKind::TupleInPlace` +
/// `__hew_tuple_drop_inplace_<key>`). The caller's tuple binding (or the
/// `__tuple_N` destructure temp's element bindings) assumes the per-element drop
/// obligation, so the composite-return boundary admits this shape.
///
/// SHAPE-ONLY: the heap-ownership decision is made ONCE at the composite-return
/// gate via the record-aware `resolved_ty_contains_heap_leaf`, so a tuple whose
/// only heap is a nested record field (`(Boxed, i64)` where `Boxed { payload:
/// Vec }`) is recognised — the record-blind tuple-element walk missed it. A
/// BitCopy-only tuple never reaches the gate's inner checks (the outer guard's
/// record-aware "owns heap" is false), and the gate's `StructType` arm only fires
/// for heap-owning shapes anyway.
pub(crate) fn is_tuple_composite_shape(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Tuple(_))
}

/// Returns `true` when the named enum registered in `enum_layouts` has
/// `is_indirect = true`, meaning every variable of the type holds a
/// heap pointer rather than an inline tagged-union struct.
pub(crate) fn is_indirect_enum(name: &str, enum_layouts: &[EnumLayout]) -> bool {
    // Look up by exact name first, then by short (unqualified) name so
    // module-qualified enums (`"mod.Expr"`) match `"Expr"` entries.
    enum_layouts
        .iter()
        .find(|el| el.name == name || el.name == short_name(name))
        .is_some_and(|el| el.is_indirect)
}

/// The single layout-witness descriptor for a runtime-managed collection's
/// memory lifecycle (W5.001 F0a).
///
/// Codegen derives BOTH the clone and the drop symbol for a collection
/// state field from this ONE descriptor, so the two operations — and the
/// constructor's `*_with_layout` ABI family they must pair with — can never
/// select mismatched runtime symbols. This is the structural retirement of
/// the W4.045 UAF class: there is no longer an independent per-type symbol
/// *selection* on the clone side and the drop side that can drift apart (the
/// prior bug was a drop arm branching on the element kind while the
/// constructor and clone arms did not — `codegen-abi-authority` /
/// `lifecycle-symmetry` P0). A new collection type registers its symbol
/// pair here once; clone and drop derive mechanically.
///
/// Returns `None` for non-collection kinds (`String`/`Bytes`/`BitCopy`/
/// `IoHandle`/`UserRecord`), which continue through their dedicated
/// `clone_helper_for_kind` / `drop_helper_for_kind` / synthesised-helper
/// arms unchanged.
///
/// This is the SOLE selection authority for collection clone/drop symbols.
/// The matching arms in `clone_helper_for_kind` / `drop_helper_for_kind`
/// are retired to `unreachable!` because the only callers
/// (`emit_field_clone_step` / `emit_field_drop_step`) consult this witness
/// first and never fall through to the per-type helper arm for a collection.
pub(crate) struct CollectionLayoutWitness {
    /// Allocating clone helper: `fn(*const handle) -> *mut handle`.
    pub(crate) clone_sym: &'static str,
    /// Free helper: `fn(*mut handle)`.
    pub(crate) drop_sym: &'static str,
}

pub(crate) fn collection_layout_witness(
    kind: &StateFieldCloneKind,
) -> CodegenResult<Option<CollectionLayoutWitness>> {
    // Defence-in-depth backstop (round-4): a collection whose element/key/value
    // transitively carries an `OpaqueHandle` must NOT select the managed
    // clone/free pair — the managed clone shallow-copies the opaque pointer and
    // double-frees / UAFs on supervisor restart. The MIR classifier already fails
    // closed before such a kind can be produced (`ty_contains_unclonable_opaque`),
    // so this is unreachable in practice; it stays as a codegen-side fail-close so
    // no future producer can route an opaque-bearing container through the managed
    // collection symbols.
    if matches!(
        kind,
        StateFieldCloneKind::Vec { .. }
            | StateFieldCloneKind::HashMap { .. }
            | StateFieldCloneKind::HashSet { .. }
    ) && kind.contains_opaque_handle()
    {
        return Err(CodegenError::FailClosed(format!(
            "collection state field {kind:?} transitively carries an opaque handle \
             with no clone-dup helper; the managed clone/free pair would shallow-copy \
             the handle and double-free / use-after-free on supervisor restart. The \
             MIR classifier should have rejected this with OpaqueInContainer."
        )));
    }
    // Same backstop for the closure-pair class: a closure pair's environment
    // box has a sole owner and no retain, so the managed clone would alias it
    // (double free at the two containers' releases), and closure-pair Vecs are
    // constructed through pointer-element marshalling with a release-only Vec
    // descriptor, never the cloneable state witness. The MIR
    // value-class gate (`supports_value_class_drop_spine`) and the actor-state
    // closure guard reject these before codegen; this stays as the fail-close.
    if matches!(
        kind,
        StateFieldCloneKind::Vec { .. }
            | StateFieldCloneKind::HashMap { .. }
            | StateFieldCloneKind::HashSet { .. }
    ) && kind.contains_closure_pair()
    {
        return Err(CodegenError::FailClosed(format!(
            "collection state field {kind:?} transitively carries a closure pair; \
             the managed clone/free pair would alias its sole-owner environment \
             box. The MIR value-class gate should have rejected this kind."
        )));
    }
    Ok(match kind {
        StateFieldCloneKind::Vec { .. } => Some(CollectionLayoutWitness {
            // The canonical descriptor-driven pair handles plain, string, and
            // recursively owned element shapes alike, so state clone/drop no
            // longer branches on context or recursion depth.
            clone_sym: VEC_CLONE_OWNED_SYMBOL,
            drop_sym: VEC_FREE_OWNED_SYMBOL,
        }),
        StateFieldCloneKind::HashMap { .. } => Some(CollectionLayoutWitness {
            // Constructor lowering routes every HashMap<K,V> handle through
            // `hew_hashmap_new_with_layout`; clone/free MUST pair with the
            // matching `*_layout` family (CLAUDE.md §1; W4.045 UAF class).
            clone_sym: HASHMAP_CLONE_LAYOUT_SYMBOL,
            drop_sym: HASHMAP_FREE_LAYOUT_SYMBOL,
        }),
        StateFieldCloneKind::HashSet { .. } => Some(CollectionLayoutWitness {
            // Companion of the HashMap pairing. The constructor routes every
            // HashSet<T> handle through `hew_hashset_new_with_layout`; the
            // element kind is irrelevant — there is no `hew_hashset_new` call
            // in codegen, so clone/free always use the `*_layout` family.
            clone_sym: HASHSET_CLONE_LAYOUT_SYMBOL,
            drop_sym: HASHSET_FREE_LAYOUT_SYMBOL,
        }),
        StateFieldCloneKind::BitCopy { .. }
        | StateFieldCloneKind::String
        | StateFieldCloneKind::Bytes
        | StateFieldCloneKind::Tuple { .. }
        | StateFieldCloneKind::Array { .. }
        | StateFieldCloneKind::IoHandle { .. }
        | StateFieldCloneKind::UserRecord { .. }
        // Enum is NOT a runtime-managed collection: its clone/drop is a
        // tag-dispatched payload walk (W5.006 Slices 3/4), not a single
        // `*_layout` symbol pair. It returns `None` here so the caller
        // routes to the dedicated enum dispatch arm rather than the
        // collection witness.
        | StateFieldCloneKind::Enum { .. }
        // OpaqueHandle is a pointer-width BitCopy-class handle (e.g. json.Value,
        // cron.Expr). No layout-managed runtime collection; falls through to the
        // per-kind helpers where clone fails closed and drop is a no-op.
        | StateFieldCloneKind::OpaqueHandle { .. }
        // ClosurePair is not a runtime-managed collection: its drop is the
        // env free-thunk dispatch in `emit_field_drop_step`'s dedicated arm
        // and its clone is the rollback refusal in `emit_field_clone_step`.
        | StateFieldCloneKind::ClosurePair
        // Resource (`#[resource] #[opaque]` handle) is not a runtime-managed
        // collection: its drop is the user close-symbol call in
        // `emit_field_drop_step`'s dedicated arm and its clone is the rollback
        // refusal in `emit_field_clone_step`.
        | StateFieldCloneKind::Resource { .. } => None,
    })
}

pub(crate) fn is_layout_vec_runtime_symbol(symbol: &str) -> bool {
    use hew_types::runtime_call::{RuntimeCallAbiShape, RuntimeCallFamily};

    RuntimeCallFamily::from_c_symbol(symbol)
        .is_some_and(|family| family.abi_shape() == RuntimeCallAbiShape::VecLayout)
}

pub(crate) fn layout_vec_fn_type<'ctx>(
    ctx: &'ctx Context,
    symbol: &str,
) -> CodegenResult<inkwell::types::FunctionType<'ctx>> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    let i32_ty = ctx.i32_type();
    match symbol {
        "hew_vec_push_layout" => Ok(ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)),
        "hew_vec_get_layout" => {
            Ok(ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), ptr_ty.into()], false))
        }
        "hew_vec_set_layout" => Ok(ctx.void_type().fn_type(
            &[ptr_ty.into(), i64_ty.into(), ptr_ty.into(), ptr_ty.into()],
            false,
        )),
        "hew_vec_pop_layout" => {
            Ok(i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false))
        }
        // W3.032 Slice 3: `i32 hew_vec_contains_thunk(ptr vec, ptr val, ptr eq_fn)`.
        // The third operand is a pointer to a codegen-emitted equality thunk;
        // the runtime treats `Option<HewVecEqThunk>` as a nullable function
        // pointer (null is rejected by `abort_null_eq_fn`).
        "hew_vec_contains_thunk" => {
            Ok(i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false))
        }
        // `i32 hew_vec_remove_at_layout(ptr vec, i64 index, ptr out, ptr layout)`.
        // Index-based move-out for BitCopy layout-backed elements (the twin of
        // `pop_layout`): the removed element is copied into the codegen-supplied
        // `out` slot and the tail is shifted; the hidden `out` and `layout`
        // pointers are synthesized by codegen from the Vec element type. Returns
        // 1 on success (the runtime traps internally on an out-of-bounds index).
        "hew_vec_remove_at_layout" => Ok(i32_ty.fn_type(
            &[ptr_ty.into(), i64_ty.into(), ptr_ty.into(), ptr_ty.into()],
            false,
        )),
        // W3.003: `ptr hew_vec_clone_layout(ptr vec, ptr layout) -> ptr`.
        // BitCopy bulk-copy clone; returns a freshly allocated *mut HewVec.
        // The hidden layout pointer is synthesized by codegen from the Vec element type.
        "hew_vec_clone_layout" => Ok(ptr_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)),
        // `ptr hew_vec_slice_range_layout(ptr vec, i64 start, i64 end, ptr layout) -> ptr`.
        // Range-slice for BitCopy layout-backed elements; the hidden layout
        // pointer is synthesized by codegen from the Vec element type.
        "hew_vec_slice_range_layout" => Ok(ptr_ty.fn_type(
            &[ptr_ty.into(), i64_ty.into(), i64_ty.into(), ptr_ty.into()],
            false,
        )),
        _ => Err(CodegenError::FailClosed(format!(
            "not a layout Vec runtime symbol: {symbol}"
        ))),
    }
}

pub(crate) fn get_or_declare_layout_vec_runtime<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(fv) = llvm_mod.get_function(symbol) {
        return Ok(fv);
    }
    Ok(llvm_mod.add_function(
        symbol,
        layout_vec_fn_type(ctx, symbol)?,
        Some(Linkage::External),
    ))
}

pub(crate) fn layout_descriptor_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    elem_ty: BasicTypeEnum<'ctx>,
    label: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    // `HewTypeLayout { size: usize, align: usize, ownership_kind: u8 }`. The two
    // leading fields are target-width `usize`, not host-width `i64`: wasm32 has
    // `usize = i32`, so the size/align measurements and the descriptor struct
    // shape both flow from the active `TargetData` exactly as the
    // `hashmap_*_layout_descriptor_ptr` width authority does. A host-width
    // fallback here would be silent wrong-code on wasm32.
    let (size, align) = abi_size_align(elem_ty, Some(fn_ctx.target_data))?;
    let ctx = fn_ctx.ctx;
    let usize_ty = crate::llvm::runtime_size_ty(ctx, fn_ctx.llvm_mod);
    let i8_ty = ctx.i8_type();
    let layout_ty = ctx.struct_type(&[usize_ty.into(), usize_ty.into(), i8_ty.into()], false);
    // Dedup name mirrors the `hashmap_*_layout_descriptor_ptr` authority: a
    // module only ever targets one machine, so a native (i64) and a wasm32
    // (i32) descriptor for the same `(label, size, align)` never co-exist in
    // one module. The `(size, align)` measurements already shift with the
    // target width, so the name stays stable per target without a width suffix
    // — native IR is unchanged, wasm32 emits its own `_4_4_`/`_16_8_` globals.
    let global_name = format!("__hew_layout_{label}_{size}_{align}_plain");
    if let Some(global) = fn_ctx.llvm_mod.get_global(&global_name) {
        return Ok(global.as_pointer_value());
    }
    let init = layout_ty.const_named_struct(&[
        usize_ty.const_int(size, false).into(),
        usize_ty.const_int(u64::from(align), false).into(),
        i8_ty.const_zero().into(),
    ]);
    let global = fn_ctx.llvm_mod.add_global(layout_ty, None, &global_name);
    global.set_constant(true);
    global.set_linkage(Linkage::Private);
    global.set_initializer(&init);
    Ok(global.as_pointer_value())
}

/// Emit (or reuse) a `HewVecElemLayout`-shaped private constant for an owned
/// Vec element type, returning a pointer suitable for the owned constructor's
/// `layout` parameter.
///
/// The struct shape must match `hew_cabi::vec::HewVecElemLayout`:
/// `{ size: usize, align: usize, ownership_kind: u8, clone_fn: *const fn,
/// drop_fn: *const fn }`. `ownership_kind = LayoutManaged (2)`; the two thunk
/// fields point at the element's per-type `__hew_record/enum_{clone,drop}_
/// inplace_<key>` helpers (their bodies seeded by
/// `collect_vec_owned_element_seeds`). Dedup by `(size, align, key)` so two
/// distinct element shapes never collapse onto one descriptor.
///
/// Fails closed for any element with no resolvable thunk path — the owned ABI
/// must never reference a non-existent thunk (`boundary-fail-closed`).
#[allow(
    clippy::too_many_arguments,
    reason = "narrowed off FnCtx so the wire codec (no per-function FnCtx) can \
              reach the same owned-descriptor authority; the raw ctx/llvm_mod/\
              target_data + the three-registry borrow replace the single FnCtx arg"
)]
pub(crate) fn owned_elem_layout_descriptor_ptr<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    target_data: &TargetData,
    regs: OwnedElemRegistries<'_, 'ctx>,
    elem_resolved_ty: &ResolvedTy,
    elem_llvm_ty: BasicTypeEnum<'ctx>,
    label: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    let Some((kind, key)) = crate::thunks::owned_elem_thunk_key(regs, elem_resolved_ty) else {
        return Err(CodegenError::FailClosed(format!(
            "owned Vec element `{elem_resolved_ty:?}` has no resolvable record/enum \
             in-place thunk path at `{label}`; the checker must not route a \
             non-thunkable element through the owned ABI"
        )));
    };
    let (size, align) = abi_size_align(elem_llvm_ty, Some(target_data))?;
    let kind_tag = match kind {
        OwnedElemThunkKind::Record => "rec",
        OwnedElemThunkKind::Enum => "enum",
        OwnedElemThunkKind::Tuple => "tup",
        OwnedElemThunkKind::Collection => "coll",
    };
    let global_name = format!("__hew_vec_elem_layout_{kind_tag}_{key}_{size}_{align}");
    if let Some(g) = llvm_mod.get_global(&global_name) {
        return Ok(g.as_pointer_value());
    }
    let (clone_fn, drop_fn) = match kind {
        OwnedElemThunkKind::Record => (
            get_or_declare_record_clone_inplace(ctx, llvm_mod, &key),
            get_or_declare_record_drop_inplace(ctx, llvm_mod, &key),
        ),
        OwnedElemThunkKind::Enum => (
            get_or_declare_enum_clone_inplace(ctx, llvm_mod, &key),
            get_or_declare_enum_drop_inplace(ctx, llvm_mod, &key),
        ),
        OwnedElemThunkKind::Tuple => {
            // The tuple thunk BODY is synthesized eagerly here (it has no
            // registry-driven seeding pass, unlike record/enum): a tuple shape
            // is referenced only through this descriptor, so synthesize-on-first-
            // descriptor is the natural seeding point. Idempotent: the emitter
            // no-ops if the body already exists.
            let elems = match elem_resolved_ty {
                ResolvedTy::Tuple(elems) => elems,
                _ => {
                    return Err(CodegenError::FailClosed(format!(
                        "owned tuple descriptor at `{label}`: element {elem_resolved_ty:?} is \
                         not a tuple but resolved to the Tuple thunk kind"
                    )));
                }
            };
            crate::thunks::emit_tuple_inplace_thunk_bodies(
                ctx,
                llvm_mod,
                target_data,
                regs,
                &key,
                elems,
                elem_llvm_ty,
            )?;
            (
                get_or_declare_tuple_clone_inplace(ctx, llvm_mod, &key),
                get_or_declare_tuple_drop_inplace(ctx, llvm_mod, &key),
            )
        }
        OwnedElemThunkKind::Collection => {
            // The collection-handle wrapper thunk BODY is synthesized eagerly
            // here (like the tuple thunk — no registry-driven seeding): a
            // collection element is referenced only through this descriptor.
            // Idempotent: the emitter no-ops if the body already exists.
            let (clone_sym, drop_sym) = collection_elem_clone_drop_syms(elem_resolved_ty)
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "owned collection descriptor at `{label}`: element \
                         {elem_resolved_ty:?} has no canonical clone/free primitive \
                         (closure-pair Vec elements are a separate lane)"
                    ))
                })?;
            crate::thunks::emit_collection_handle_thunk_bodies(
                ctx, llvm_mod, &key, clone_sym, drop_sym,
            )?;
            (
                get_or_declare_collection_clone_inplace(ctx, llvm_mod, &key),
                get_or_declare_collection_drop_inplace(ctx, llvm_mod, &key),
            )
        }
    };
    let usize_ty = crate::llvm::runtime_size_ty(ctx, llvm_mod);
    let i8_ty = ctx.i8_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let layout_ty = ctx.struct_type(
        &[
            usize_ty.into(),
            usize_ty.into(),
            i8_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
        ],
        false,
    );
    // ownership_kind = LayoutManaged (HewTypeOwnershipKind::LayoutManaged = 2).
    let init = layout_ty.const_named_struct(&[
        usize_ty.const_int(size, false).into(),
        usize_ty.const_int(u64::from(align), false).into(),
        i8_ty.const_int(2, false).into(),
        clone_fn.as_global_value().as_pointer_value().into(),
        drop_fn.as_global_value().as_pointer_value().into(),
    ]);
    let g = llvm_mod.add_global(layout_ty, None, &global_name);
    g.set_constant(true);
    g.set_linkage(Linkage::Private);
    g.set_initializer(&init);
    Ok(g.as_pointer_value())
}

/// Emit the release-only descriptor for boxed closure-pair Vec elements.
///
/// Closure-pair ingress already moves a freshly allocated pair box into each
/// pointer-sized slot. The runtime drop thunk releases the environment and pair
/// box; clone remains null because closure environments have no general clone
/// protocol and actor-state closure containers are rejected upstream.
pub(crate) fn closure_pair_elem_layout_descriptor_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    const GLOBAL_NAME: &str = "__hew_vec_elem_layout_closure_pair";
    if let Some(g) = fn_ctx.llvm_mod.get_global(GLOBAL_NAME) {
        return Ok(g.as_pointer_value());
    }

    let ctx = fn_ctx.ctx;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let (size, align) = abi_size_align(ptr_ty.into(), Some(fn_ctx.target_data))?;
    let size_ty = crate::llvm::runtime_size_ty(ctx, fn_ctx.llvm_mod);
    let i8_ty = ctx.i8_type();
    let layout_ty = ctx.struct_type(
        &[
            size_ty.into(),
            size_ty.into(),
            i8_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
        ],
        false,
    );
    let drop_fn = fn_ctx
        .llvm_mod
        .get_function("hew_vec_closure_pair_drop_inplace")
        .unwrap_or_else(|| {
            fn_ctx.llvm_mod.add_function(
                "hew_vec_closure_pair_drop_inplace",
                ctx.void_type().fn_type(&[ptr_ty.into()], false),
                Some(Linkage::External),
            )
        });
    let init = layout_ty.const_named_struct(&[
        size_ty.const_int(size, false).into(),
        size_ty.const_int(u64::from(align), false).into(),
        i8_ty.const_int(2, false).into(),
        ptr_ty.const_null().into(),
        drop_fn.as_global_value().as_pointer_value().into(),
    ]);
    let global = fn_ctx.llvm_mod.add_global(layout_ty, None, GLOBAL_NAME);
    global.set_constant(true);
    global.set_linkage(Linkage::Private);
    global.set_initializer(&init);
    Ok(global.as_pointer_value())
}

/// Synthesize (or reuse) the constant `HewVecElemLayout` witness static
/// describing one channel/stream element type for the layout-witness
/// recv/send ABI (`hew_channel_*_layout` / `hew_stream_*_layout`).
///
/// Encoding contract — must mirror `hew-runtime/src/channel_common.rs`:
/// - `String` (ownership kind 1): pointer-sized element slot; the queue
///   envelope is content-encoded (the runtime materialises a fresh
///   header-aware cstring on decode).
/// - `Bytes` (kind 3): `BytesTriple`-sized slot; content-encoded envelope.
/// - scalars / BitCopy aggregates (kind 0, Plain): the raw representation,
///   witness-width wide, no thunks.
/// - heap-owning record/enum/tuple (kind 2, LayoutManaged): the W5.016 owned
///   descriptor with the per-type `__hew_*_{clone,drop}_inplace` thunks,
///   reused verbatim via [`owned_elem_layout_descriptor_ptr`].
///
/// The heap-ownership split reuses the Vec classifier
/// (`resolved_ty_element_owns_heap_for_owned_vec`) so the channel/stream
/// witness can never disagree with the Vec witness about one element type.
pub(crate) fn channel_elem_layout_witness_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    elem_ty: &ResolvedTy,
    label: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    match elem_ty {
        ResolvedTy::String => {
            let ptr_bytes = u64::from(fn_ctx.target_data.get_pointer_byte_size(None));
            #[expect(
                clippy::cast_possible_truncation,
                reason = "pointer byte size fits u32 on every supported target"
            )]
            let align = ptr_bytes as u32;
            const_elem_witness_global(fn_ctx, "__hew_elem_layout_string", ptr_bytes, align, 1)
        }
        ResolvedTy::Bytes => {
            let triple_ty = fn_ctx.ctx.struct_type(
                &[
                    fn_ctx.ctx.ptr_type(AddressSpace::default()).into(),
                    fn_ctx.ctx.i32_type().into(),
                    fn_ctx.ctx.i32_type().into(),
                ],
                false,
            );
            let (size, align) = abi_size_align(triple_ty.into(), Some(fn_ctx.target_data))?;
            const_elem_witness_global(fn_ctx, "__hew_elem_layout_bytes", size, align, 3)
        }
        _ if resolved_ty_element_owns_heap_for_owned_vec(fn_ctx, elem_ty) => {
            let elem_llvm_ty = resolve_ty(
                fn_ctx.ctx,
                fn_ctx.target_data,
                elem_ty,
                fn_ctx.record_layouts,
            )?;
            owned_elem_layout_descriptor_ptr(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                fn_ctx.target_data,
                fn_ctx.owned_elem_registries(),
                elem_ty,
                elem_llvm_ty,
                label,
            )
        }
        _ => {
            // Plain: scalars (i8..i64, u8..u64, f32/f64, bool, char) and
            // BitCopy aggregates ride the raw representation. The checker owns
            // the admission decision; an element type it should not have
            // admitted still gets a deterministic width here rather than a
            // silent misdecode (the runtime cross-checks envelope width).
            let elem_llvm_ty = resolve_ty(
                fn_ctx.ctx,
                fn_ctx.target_data,
                elem_ty,
                fn_ctx.record_layouts,
            )?;
            let (size, align) = abi_size_align(elem_llvm_ty, Some(fn_ctx.target_data))?;
            const_elem_witness_global(
                fn_ctx,
                &format!("__hew_elem_layout_plain_{size}_{align}"),
                size,
                align,
                0,
            )
        }
    }
}

/// Map a primitive `ResolvedTy` to the runtime-defined key layout extern
/// (`hew_layout_key_<prim>` in `hew-runtime/src/layout_intrinsics.rs`). Returns
/// `None` for non-primitive shapes (records, tuples, etc.), which fall through
/// to the codegen-synthesised per-record layout path.
///
/// Stage C3 (DI-017): the resolver-authority cutover admits Hash+Eq primitives
/// (i32/i64/u32/u64/bool/char/string/bytes) as HashMap keys / HashSet
/// elements. Synthesising per-primitive hash/eq thunks in codegen is
/// unnecessary (and impossible for `string` whose LLVM repr is a pointer to a
/// C string) — instead, route to the runtime-shipped static descriptors that
/// embed the canonical hash/eq/drop function pointers.
pub(crate) fn primitive_key_layout_extern_name(rty: &ResolvedTy) -> Option<&'static str> {
    Some(match rty {
        ResolvedTy::I32 => "hew_layout_key_i32",
        ResolvedTy::I64 => "hew_layout_key_i64",
        ResolvedTy::U32 => "hew_layout_key_u32",
        ResolvedTy::U64 => "hew_layout_key_u64",
        ResolvedTy::Bool => "hew_layout_key_bool",
        ResolvedTy::Char => "hew_layout_key_char",
        ResolvedTy::String => "hew_layout_key_string",
        ResolvedTy::Bytes => "hew_layout_key_bytes",
        // F32/F64 ship `hash_fn = None` / `eq_fn = None` (DI-003 fail-closed-
        // by-absence). They should never reach codegen because the resolver
        // rejects them with `BoundsNotSatisfied(Hash, _)`. Returning `None`
        // here falls through to the codegen-synthesis path, which itself
        // hard-errors on floats — defence in depth.
        _ => return None,
    })
}

// ===========================================================================
// Collection direct-call lowering: Vec / HashMap / HashSet / bytes / string
// (god-module carve)
// ===========================================================================
// Pure relocation from llvm.rs: the owned-element and layout-descriptor Vec /
// HashMap runtime declaration + direct-call lowering, the option-returning
// collection accessors, and the collection/bytes constructor lowering.
// Byte-identical IR before and after.

/// True for the W5.016 owned-element Vec runtime symbols.
pub(crate) fn is_owned_vec_runtime_symbol(symbol: &str) -> bool {
    use hew_types::runtime_call::{RuntimeCallAbiShape, RuntimeCallFamily};

    RuntimeCallFamily::from_c_symbol(symbol)
        .is_some_and(|family| family.abi_shape() == RuntimeCallAbiShape::VecOwned)
}

/// LLVM signature for an owned-element Vec runtime symbol. The descriptor is
/// stamped into the handle at construction, so the per-op ABI carries no
/// descriptor pointer (mirrors `hew_vec_*_owned` in `hew-cabi::vec`).
fn owned_vec_fn_type<'ctx>(
    ctx: &'ctx Context,
    symbol: &str,
) -> CodegenResult<inkwell::types::FunctionType<'ctx>> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    let i32_ty = ctx.i32_type();
    match symbol {
        // void hew_vec_push_owned(ptr vec, ptr data)
        // void hew_vec_push_owned_move(ptr vec, ptr data)
        "hew_vec_push_owned" | "hew_vec_push_owned_move" => Ok(ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), ptr_ty.into()], false)),
        // ptr hew_vec_get_owned(ptr vec, i64 index)
        "hew_vec_get_owned" => Ok(ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false)),
        // void hew_vec_set_owned(ptr vec, i64 index, ptr data)
        // void hew_vec_set_owned_move(ptr vec, i64 index, ptr data)
        "hew_vec_set_owned" | "hew_vec_set_owned_move" => Ok(ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into(), ptr_ty.into()], false)),
        // i32 hew_vec_pop_owned(ptr vec, ptr out)
        "hew_vec_pop_owned" => Ok(i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)),
        // i32 hew_vec_remove_at_owned(ptr vec, i64 index, ptr out)
        "hew_vec_remove_at_owned" => {
            Ok(i32_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), ptr_ty.into()], false))
        }
        // ptr hew_vec_clone_owned(ptr vec)
        "hew_vec_clone_owned" => Ok(ptr_ty.fn_type(&[ptr_ty.into()], false)),
        // i32 hew_vec_contains_owned(ptr vec, ptr data, ptr eq_fn)
        "hew_vec_contains_owned" => {
            Ok(i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false))
        }
        // ptr hew_vec_slice_range_owned(ptr vec, i64 start, i64 end)
        "hew_vec_slice_range_owned" => {
            Ok(ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false))
        }
        _ => Err(CodegenError::FailClosed(format!(
            "not an owned Vec runtime symbol: {symbol}"
        ))),
    }
}

pub(crate) fn get_or_declare_owned_vec_runtime<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(fv) = llvm_mod.get_function(symbol) {
        return Ok(fv);
    }
    Ok(llvm_mod.add_function(
        symbol,
        owned_vec_fn_type(ctx, symbol)?,
        Some(Linkage::External),
    ))
}

/// Lower an owned-element Vec direct call (`hew_vec_{push,get,set,pop,clone}_owned`).
///
/// Parallel to `lower_layout_vec_direct_call`, but the runtime reads the
/// element descriptor from the handle (stamped at construction), so no
/// descriptor pointer is passed per op. push/set deep-clone the element in,
/// clone deep-copies the whole Vec, get borrows a pointer into the live buffer,
/// and pop moves the element out.
pub(crate) fn lower_owned_vec_direct_call(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    let expected_arity = match callee {
        "hew_vec_push_owned"
        | "hew_vec_push_owned_move"
        | "hew_vec_get_owned"
        | "hew_vec_contains_owned" => 2,
        "hew_vec_set_owned" | "hew_vec_set_owned_move" => 3,
        "hew_vec_slice_range_owned" => 3,
        "hew_vec_remove_at_owned" => 2,
        "hew_vec_pop_owned" | "hew_vec_clone_owned" => 1,
        _ => {
            return Err(CodegenError::FailClosed(format!(
                "lower_owned_vec_direct_call called with non-owned Vec symbol `{callee}`"
            )));
        }
    };
    if args.len() != expected_arity {
        return Err(CodegenError::FailClosed(format!(
            "{callee}: expected {expected_arity} source-level args, got {}",
            args.len()
        )));
    }

    let vec_ptr = load_duplex_handle(fn_ctx, args[0], &format!("{callee} arg0"))?;
    let fv = get_or_declare_owned_vec_runtime(fn_ctx.ctx, fn_ctx.llvm_mod, callee)?;
    match callee {
        "hew_vec_push_owned" | "hew_vec_push_owned_move" => {
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "{callee} returns unit; producer must not supply dest={d:?}"
                )));
            }
            let (data_ptr, _elem_ty) = place_pointer(fn_ctx, args[1])?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), data_ptr.into()],
                    &format!("{callee}_call"),
                )
                .llvm_ctx_with(|| format!("{callee} call"))?;
        }
        "hew_vec_get_owned" => {
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_get_owned returns an element; producer must supply a dest".into(),
                )
            })?;
            let index = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_get_owned_arg1",
            )?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), index.into()],
                    "hew_vec_get_owned_call",
                )
                .llvm_ctx("hew_vec_get_owned call")?;
            let raw_ptr = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_vec_get_owned returned void".into()))?
                .into_pointer_value();
            // get BORROWS: load the element value from the buffer slot into the
            // dest local. The runtime element stays owned by the Vec — codegen
            // does NOT schedule the dest for an independent drop (the borrowed
            // owner is the Vec slot). MIR drop elaboration treats the get dest
            // as a borrow (F5 discipline).
            let loaded = fn_ctx
                .builder
                .build_load(dest_ty, raw_ptr, "hew_vec_get_owned_load")
                .llvm_ctx("hew_vec_get_owned load")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, loaded)
                .llvm_ctx("hew_vec_get_owned store")?;
        }
        // COPY-IN set (`hew_vec_set_owned`, deep-clone) and MOVE-in set
        // (`hew_vec_set_owned_move`, byte-transfer, no clone) share an identical
        // LLVM call shape — same `(vec, i64 index, ptr data)` signature and the
        // same argument marshalling. Only the runtime callee differs (chosen at
        // MIR lowering by whether the element operand is a fresh materialised
        // owner); the runtime function name in `fv` carries the move-vs-copy
        // discrimination, so codegen emits the identical instruction sequence.
        "hew_vec_set_owned" | "hew_vec_set_owned_move" => {
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "{callee} returns unit; producer must not supply dest={d:?}"
                )));
            }
            let index = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_set_owned_arg1",
            )?;
            let (data_ptr, _elem_ty) = place_pointer(fn_ctx, args[2])?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), index.into(), data_ptr.into()],
                    "hew_vec_set_owned_call",
                )
                .llvm_ctx("hew_vec_set_owned call")?;
        }
        "hew_vec_pop_owned" => {
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_pop_owned moves an element out; producer must supply a dest".into(),
                )
            })?;
            // pop moves the element into the dest local (no clone, no drop) —
            // possession transfers to the caller. The runtime writes the raw
            // element bytes into `out`.
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), dest_ptr.into()],
                    "hew_vec_pop_owned_call",
                )
                .llvm_ctx("hew_vec_pop_owned call")?;
        }
        "hew_vec_remove_at_owned" => {
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_remove_at_owned moves an element out; producer must supply a dest"
                        .into(),
                )
            })?;
            let index = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_remove_at_owned_arg1",
            )?;
            // remove moves the element at `index` into the dest local (no clone,
            // no drop) — possession transfers to the caller. The runtime writes
            // the raw element bytes into `out`, shifts the tail, and traps on an
            // out-of-bounds index (so codegen needs no empty/OOB branch).
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), index.into(), dest_ptr.into()],
                    "hew_vec_remove_at_owned_call",
                )
                .llvm_ctx("hew_vec_remove_at_owned call")?;
        }
        "hew_vec_clone_owned" => {
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_clone_owned returns a Vec; producer must supply a dest".into(),
                )
            })?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[vec_ptr.into()], "hew_vec_clone_owned_call")
                .llvm_ctx("hew_vec_clone_owned call")?;
            let new_vec_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_vec_clone_owned returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, new_vec_ptr)
                .llvm_ctx("hew_vec_clone_owned store")?;
        }
        "hew_vec_contains_owned" => {
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_contains_owned returns bool; producer must supply a dest".into(),
                )
            })?;
            let (val_ptr, elem_ty) = place_pointer(fn_ctx, args[1])?;
            let elem_resolved_ty = place_resolved_ty(fn_ctx, args[1])?;
            let thunk_fn =
                crate::thunks::get_or_emit_eq_thunk(fn_ctx, elem_ty, Some(elem_resolved_ty))?;
            let thunk_ptr = thunk_fn.as_global_value().as_pointer_value();
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), val_ptr.into(), thunk_ptr.into()],
                    "hew_vec_contains_owned_call",
                )
                .llvm_ctx("hew_vec_contains_owned call")?;
            let raw_i32 = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_vec_contains_owned returned void".into())
                })?
                .into_int_value();
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            if !matches!(dest_ty, BasicTypeEnum::IntType(_)) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_vec_contains_owned dest must be an integer bool slot, got {dest_ty:?}"
                )));
            }
            let nz = fn_ctx
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    raw_i32,
                    fn_ctx.ctx.i32_type().const_zero(),
                    "contains_owned_nz",
                )
                .llvm_ctx("contains_owned compare")?;
            let widened = zext_bool_i1_to_dest(fn_ctx, nz, dest_ty, "contains_owned")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .llvm_ctx("contains_owned store")?;
        }
        "hew_vec_slice_range_owned" => {
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_slice_range_owned returns a Vec; producer must supply a dest".into(),
                )
            })?;
            let start = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_slice_range_owned_arg1",
            )?;
            let end = load_int_arg(
                fn_ctx,
                args[2],
                fn_ctx.ctx.i64_type(),
                "hew_vec_slice_range_owned_arg2",
            )?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), start.into(), end.into()],
                    "hew_vec_slice_range_owned_call",
                )
                .llvm_ctx("hew_vec_slice_range_owned call")?;
            let new_vec_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_vec_slice_range_owned returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, new_vec_ptr)
                .llvm_ctx("hew_vec_slice_range_owned store")?;
        }
        _ => unreachable!("arity gate above restricts callee to the owned Vec family"),
    }

    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx_with(|| format!("{callee} br"))?;
    Ok(())
}

// =========================================================================
// W3.003-C C-3a: HashMap/HashSet layout-key hash-thunk + descriptor synthesis
// =========================================================================
//
// This module synthesises the per-record-type identity machinery that the
// layout-backed HashMap/HashSet runtime ABI consumes:
//
//   - `__hew_hash_thunk_<size>_<align>_<struct_key>` — `i64 (ptr)` FNV-1a 64
//     hash over typed field loads in **declaration order**.  No whole-struct
//     memhash, no padding bytes, no float fields (checker-gated upstream).
//   - `@__hew_map_key_layout_<size>_<align>_<struct_key>_plain` — a
//     `HewMapKeyLayout`-shaped private constant whose field layout matches
//     `hew_cabi::map::HewMapKeyLayout` byte-for-byte.
//   - `@__hew_map_value_layout_<size>_<align>_plain` — a `HewMapValueLayout`
//     private constant for POD values.
//   - `@__hew_map_value_layout_<kind>_<key>_<size>_<align>_owned` — a
//     `HewMapValueLayout` private constant for heap-owning values, carrying the
//     same per-type clone/drop in-place thunks used by the owned Vec substrate.
//
// **Equality thunk reuse.**  The hash thunk does NOT carry an equality
// twin.  The `eq_fn` field of the key-layout global re-uses the existing
// `__hew_eq_thunk_*` from `get_or_emit_eq_thunk` so the equality machinery
// proven by `Vec::contains` (W3.032) is shared.  Dedup is the function-name
// table on the LLVM module, so a single record type yields exactly one
// equality thunk regardless of whether `Vec::contains` or HashMap reaches it
// first.
//
// **Authority boundary** — Codegen does NOT re-derive layout eligibility
// from `ResolvedTy`.  Reaching the synthesis seam means the checker emitted
// the appropriate lowering fact (C-2c `HashMapLoweringFact` /
// `HashSetLoweringFact` in `Pending` state) and routed a layout callee here.
// The defensive `CodegenError::FailClosed` arms below for float/pointer
// fields are fault isolation only — they detect a checker-gate bypass and
// refuse to silently emit a meaningless hash, never an independent
// eligibility decision.  LESSONS: `codegen-abi-authority` (P0).
//
// **WASM parity** — the descriptor fields are target-width (`usize` via target
// data) and address-taken hash/eq thunks are left as ordinary LLVM function
// pointers. LLVM lowers those pointers to wasm table elements, and the runtime
// calls the descriptor hooks with `call_indirect` once linked for WASI.
//
// **Dedup key** — Hash thunk and key-layout global share the same
// `eq_thunk_struct_key` so two records with identical `(size, align)` but
// distinct field shape do not collide.  Value-layout dedup is `(size, align)`
// alone because no thunks live in the value descriptor — two scalar values
// (e.g. `i64` and `u64`) with identical ABI shape can legitimately share a
// global.

/// Mix a single `i64` field value into the FNV-1a accumulator at `acc_slot`.
///
/// FNV-1a 64-bit step: `acc = (acc XOR field) * 0x100000001b3`.
pub(crate) fn mix_into_hash_acc<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    acc_slot: PointerValue<'ctx>,
    field_u64: IntValue<'ctx>,
) -> CodegenResult<()> {
    let i64_ty = fn_ctx.ctx.i64_type();
    let prime = i64_ty.const_int(0x100000001b3_u64, false);
    let acc = fn_ctx
        .builder
        .build_load(i64_ty, acc_slot, "hash_acc_load")
        .llvm_ctx("hash_thunk acc load")?
        .into_int_value();
    let xored = fn_ctx
        .builder
        .build_xor(acc, field_u64, "hash_xor")
        .llvm_ctx("hash_thunk xor")?;
    let mul = fn_ctx
        .builder
        .build_int_mul(xored, prime, "hash_mul")
        .llvm_ctx("hash_thunk mul")?;
    fn_ctx
        .builder
        .build_store(acc_slot, mul)
        .llvm_ctx("hash_thunk acc store")?;
    Ok(())
}

/// Counterpart to `primitive_key_layout_extern_name` for the value side. Maps
/// primitive `ResolvedTy` to `hew_layout_val_<prim>`. Returns `None` for
/// non-primitive shapes which fall through to the codegen-emitted Plain
/// layout.
fn primitive_value_layout_extern_name(rty: &ResolvedTy) -> Option<&'static str> {
    Some(match rty {
        ResolvedTy::I32 => "hew_layout_val_i32",
        ResolvedTy::I64 => "hew_layout_val_i64",
        ResolvedTy::U32 => "hew_layout_val_u32",
        ResolvedTy::U64 => "hew_layout_val_u64",
        ResolvedTy::F32 => "hew_layout_val_f32",
        ResolvedTy::F64 => "hew_layout_val_f64",
        ResolvedTy::Bool => "hew_layout_val_bool",
        ResolvedTy::Char => "hew_layout_val_char",
        ResolvedTy::String => "hew_layout_val_string",
        ResolvedTy::Bytes => "hew_layout_val_bytes",
        ResolvedTy::Unit => "hew_layout_val_unit",
        _ => return None,
    })
}

/// Declare-or-fetch an extern global by name. The returned pointer is
/// suitable for passing to the runtime as a `*const HewMapKeyLayout` /
/// `*const HewMapValueLayout`. The global is shape-typed as `i8` because the
/// actual layout struct shape lives in the runtime — the kernel reads it via
/// `*const HewMapKeyLayout` pointer cast, so the LLVM-side type does not need
/// to match the Rust-side struct.
fn declare_extern_layout_global<'ctx>(fn_ctx: &FnCtx<'_, 'ctx>, name: &str) -> PointerValue<'ctx> {
    if let Some(g) = fn_ctx.llvm_mod.get_global(name) {
        return g.as_pointer_value();
    }
    // Use an opaque (i8) global type — the runtime owns the real shape and
    // the kernel pointer-casts on receipt. No initializer because the symbol
    // is resolved at link time against the runtime's `#[no_mangle]` static.
    let i8_ty = fn_ctx.ctx.i8_type();
    let g = fn_ctx.llvm_mod.add_global(i8_ty, None, name);
    g.set_linkage(Linkage::External);
    g.set_constant(true);
    g.as_pointer_value()
}

/// Emit (or reuse) a `HewMapKeyLayout`-shaped private constant for `elem_ty`,
/// returning a pointer suitable for the runtime `key_layout` parameter.
///
/// The struct shape must match `hew_cabi::map::HewMapKeyLayout`:
/// `{ size: usize, align: usize, ownership_kind: u8, hash_fn: fn ptr, eq_fn: fn ptr }`.
/// LLVM inserts natural alignment padding between `ownership_kind` (i8) and
/// the function pointers automatically because the struct is non-packed.
///
/// The `ownership_kind` byte is hard-wired to `Plain` (0) — managed/non-Copy
/// keys are rejected by the checker's `hash_eligibility` predicate (C-2a),
/// so reaching this seam implies Plain ownership.
fn hashmap_key_layout_descriptor_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    elem_ty: BasicTypeEnum<'ctx>,
    resolved_ty: Option<&ResolvedTy>,
) -> CodegenResult<PointerValue<'ctx>> {
    // Stage C3 (DI-017): for primitive K, route to the runtime-defined
    // `hew_layout_key_<prim>` extern static. The codegen-synthesised
    // hash/eq thunks below work for struct-shaped K (source records and
    // compiler-owned identity aggregates, where field walks are well-defined);
    // primitives like `string` (LLVM repr = `i8*`) need the runtime's canonical
    // hash function. See `primitive_key_layout_extern_name`.
    if let Some(rty) = resolved_ty {
        if let Some(name) = crate::layout::primitive_key_layout_extern_name(rty) {
            return Ok(declare_extern_layout_global(fn_ctx, name));
        }
    }
    let (size, align) = abi_size_align(elem_ty, Some(fn_ctx.target_data))?;
    let key = crate::thunks::eq_thunk_struct_key(elem_ty);

    // A record key whose every field is hash-eligible-or-`string` is admitted by
    // the checker (`ty_is_hash_eligible`) with a heap leaf. Such a key is owned
    // by the map: insert MOVEs the key blob into the slot (the caller's key is
    // consumed), and remove/free drop the slot key exactly once via a per-record
    // drop thunk (`__hew_record_drop_inplace_<R>`, whose body is seeded into the
    // clone/drop synthesis through `collect_vec_owned_element_seeds`'s HashMap
    // key arm). Map-clone and `.keys()` over a layout-managed key remain
    // fail-closed in the runtime (out of scope here). `ownership_kind` is
    // `LayoutManaged` (2) so the runtime's drop discipline fires; a Copy record
    // (no heap leaf) keeps Plain (0) and a null drop_fn.
    let key_drop_fn = if resolved_ty
        .is_some_and(|rty| resolved_ty_contains_heap_leaf(fn_ctx, rty, &mut HashSet::new()))
    {
        let rty = resolved_ty.expect("heap-leaf check requires a resolved type");
        match crate::thunks::owned_elem_thunk_key(fn_ctx.owned_elem_registries(), rty) {
            Some((OwnedElemThunkKind::Record, record_key)) => Some(
                get_or_declare_record_drop_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &record_key),
            ),
            _ => {
                return Err(CodegenError::FailClosed(format!(
                    "managed HashMap key `{rty:?}` admitted by the checker but has no \
                     per-record drop thunk path; only string-bearing record keys are \
                     supported as layout-managed keys"
                )));
            }
        }
    } else {
        None
    };

    let ownership_suffix = if key_drop_fn.is_some() {
        "managed"
    } else {
        "plain"
    };
    let global_name = format!("__hew_map_key_layout_{size}_{align}_{key}_{ownership_suffix}");
    if let Some(g) = fn_ctx.llvm_mod.get_global(&global_name) {
        return Ok(g.as_pointer_value());
    }
    let ctx = fn_ctx.ctx;
    let usize_ty = ctx.ptr_sized_int_type(fn_ctx.target_data, None);
    let i8_ty = ctx.i8_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    // Shape matches `hew_cabi::map::HewMapKeyLayout` (6 visible fields; the
    // first two are target-width `usize`, not always native/host `i64`). The
    // implicit padding between `ownership_kind: i8` and `hash_fn: ptr` is
    // inserted by LLVM because the struct is non-packed). W4.001 Stage C0a
    // adds the `drop_fn` field (`Option<HewMapValueDropThunk>`); for Plain
    // ownership we emit a null pointer (None via niche optimisation).
    let layout_ty = ctx.struct_type(
        &[
            usize_ty.into(),
            usize_ty.into(),
            i8_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
        ],
        false,
    );

    // Emit/dedup the per-record thunks BEFORE constructing the initializer
    // so the function pointers exist when the global references them.
    let hash_fn = crate::thunks::get_or_emit_hash_thunk(fn_ctx, elem_ty, resolved_ty)?;
    let eq_fn = crate::thunks::get_or_emit_eq_thunk(fn_ctx, elem_ty, resolved_ty)?;

    // ownership_kind: Plain (0) for Copy keys, LayoutManaged (2) for keys with
    // an owned (string) leaf. The drop_fn slot carries the per-record drop
    // thunk for managed keys; null for Plain. The runtime's `validate_key_layout`
    // requires drop_fn = Some for LayoutManaged ownership.
    let (ownership_kind, drop_fn_value) = match key_drop_fn {
        Some(drop_fn) => (2u64, drop_fn.as_global_value().as_pointer_value().into()),
        None => (0u64, ptr_ty.const_null().into()),
    };
    let init = layout_ty.const_named_struct(&[
        usize_ty.const_int(size, false).into(),
        usize_ty.const_int(u64::from(align), false).into(),
        i8_ty.const_int(ownership_kind, false).into(),
        hash_fn.as_global_value().as_pointer_value().into(),
        eq_fn.as_global_value().as_pointer_value().into(),
        drop_fn_value,
    ]);
    let g = fn_ctx.llvm_mod.add_global(layout_ty, None, &global_name);
    g.set_constant(true);
    g.set_linkage(Linkage::Private);
    g.set_initializer(&init);
    Ok(g.as_pointer_value())
}

/// Emit (or reuse) a `HewMapValueLayout`-shaped private constant for `val_ty`,
/// returning a pointer suitable for the runtime `val_layout` parameter.
///
/// The struct shape must match `hew_cabi::map::HewMapValueLayout`:
/// `{ size: usize, align: usize, ownership_kind: u8, drop_fn: *const fn,
/// clone_fn: *const fn }`. For Plain ownership both function-pointer fields
/// are null (Option::None). Dedup by `(size, align)` alone — two Plain scalar
/// values with identical ABI shape may share a global since their thunk
/// fields are both null. Heap-owning values use the owned descriptor variant
/// below, keyed by the same per-type thunk authority as owned Vec elements.
///
/// HashSet zero-size value layout is **not emitted** by this helper: per
/// C-1c, the runtime's `hew_hashset_new_with_layout` injects a hard-coded
/// zero-size `HewMapValueLayout` internally.  Callers of `lower_hashmap_*`
/// for HashMap continue to emit a real value-layout global.
fn hashmap_value_layout_descriptor_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    val_ty: BasicTypeEnum<'ctx>,
    resolved_ty: Option<&ResolvedTy>,
) -> CodegenResult<PointerValue<'ctx>> {
    // Stage C3 (DI-017): primitive V routes to the runtime-defined
    // `hew_layout_val_<prim>` extern static. This matters in particular for
    // `string` / `bytes` values whose drop semantics live in the runtime
    // (`hew_layout_string_drop`, `hew_layout_bytes_drop`); the Plain
    // codegen-emitted layout below would set `drop_fn = None`, leaking the
    // contents.
    if let Some(rty) = resolved_ty {
        if let Some(name) = primitive_value_layout_extern_name(rty) {
            return Ok(declare_extern_layout_global(fn_ctx, name));
        }
        if resolved_ty_contains_heap_leaf(fn_ctx, rty, &mut HashSet::new()) {
            return hashmap_owned_value_layout_descriptor_ptr(fn_ctx, rty, val_ty);
        }
    }
    let (size, align) = abi_size_align(val_ty, Some(fn_ctx.target_data))?;
    let global_name = format!("__hew_map_value_layout_{size}_{align}_plain");
    if let Some(g) = fn_ctx.llvm_mod.get_global(&global_name) {
        return Ok(g.as_pointer_value());
    }
    let ctx = fn_ctx.ctx;
    let usize_ty = ctx.ptr_sized_int_type(fn_ctx.target_data, None);
    let i8_ty = ctx.i8_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    // W4.001 Stage C0a: `HewMapValueLayout` is now
    // `{ size, align, ownership_kind, drop_fn, clone_fn }`. For Plain
    // ownership both new function-pointer fields are null (Option::None).
    let layout_ty = ctx.struct_type(
        &[
            usize_ty.into(),
            usize_ty.into(),
            i8_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
        ],
        false,
    );
    let init = layout_ty.const_named_struct(&[
        usize_ty.const_int(size, false).into(),
        usize_ty.const_int(u64::from(align), false).into(),
        i8_ty.const_zero().into(),
        ptr_ty.const_null().into(),
        ptr_ty.const_null().into(),
    ]);
    let g = fn_ctx.llvm_mod.add_global(layout_ty, None, &global_name);
    g.set_constant(true);
    g.set_linkage(Linkage::Private);
    g.set_initializer(&init);
    Ok(g.as_pointer_value())
}

fn hashmap_owned_value_layout_descriptor_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    val_resolved_ty: &ResolvedTy,
    val_llvm_ty: BasicTypeEnum<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    let Some((kind, key)) =
        crate::thunks::owned_elem_thunk_key(fn_ctx.owned_elem_registries(), val_resolved_ty)
    else {
        return Err(CodegenError::FailClosed(format!(
            "owned HashMap value `{val_resolved_ty:?}` has no resolvable clone/drop \
             thunk path; checker admission must reject non-POD values without a \
             map value clone_fn"
        )));
    };
    let (size, align) = abi_size_align(val_llvm_ty, Some(fn_ctx.target_data))?;
    let kind_tag = match kind {
        OwnedElemThunkKind::Record => "rec",
        OwnedElemThunkKind::Enum => "enum",
        OwnedElemThunkKind::Tuple => "tup",
        OwnedElemThunkKind::Collection => "coll",
    };
    let global_name = format!("__hew_map_value_layout_{kind_tag}_{key}_{size}_{align}_owned");
    if let Some(g) = fn_ctx.llvm_mod.get_global(&global_name) {
        return Ok(g.as_pointer_value());
    }

    let (clone_fn, drop_fn) = match kind {
        OwnedElemThunkKind::Record => (
            get_or_declare_record_clone_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
            get_or_declare_record_drop_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
        ),
        OwnedElemThunkKind::Enum => (
            get_or_declare_enum_clone_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
            get_or_declare_enum_drop_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
        ),
        OwnedElemThunkKind::Tuple => {
            let elems = match val_resolved_ty {
                ResolvedTy::Tuple(elems) => elems,
                _ => {
                    return Err(CodegenError::FailClosed(format!(
                        "owned HashMap tuple value `{val_resolved_ty:?}` resolved to Tuple \
                         thunk kind but is not a tuple"
                    )));
                }
            };
            crate::thunks::emit_tuple_inplace_thunk_bodies(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                fn_ctx.target_data,
                fn_ctx.owned_elem_registries(),
                &key,
                elems,
                val_llvm_ty,
            )?;
            (
                get_or_declare_tuple_clone_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
                get_or_declare_tuple_drop_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
            )
        }
        OwnedElemThunkKind::Collection => {
            let (clone_sym, drop_sym) = collection_elem_clone_drop_syms(val_resolved_ty)
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "owned HashMap collection value `{val_resolved_ty:?}` has no canonical \
                         clone/free primitive"
                    ))
                })?;
            crate::thunks::emit_collection_handle_thunk_bodies(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &key,
                clone_sym,
                drop_sym,
            )?;
            (
                get_or_declare_collection_clone_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
                get_or_declare_collection_drop_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
            )
        }
    };

    let ctx = fn_ctx.ctx;
    let usize_ty = ctx.ptr_sized_int_type(fn_ctx.target_data, None);
    let i8_ty = ctx.i8_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let layout_ty = ctx.struct_type(
        &[
            usize_ty.into(),
            usize_ty.into(),
            i8_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
        ],
        false,
    );
    // HewMapValueLayout field order is { size, align, ownership_kind, drop_fn,
    // clone_fn }. Do not reuse HewVecElemLayout's clone/drop order.
    let init = layout_ty.const_named_struct(&[
        usize_ty.const_int(size, false).into(),
        usize_ty.const_int(u64::from(align), false).into(),
        i8_ty.const_int(2, false).into(),
        drop_fn.as_global_value().as_pointer_value().into(),
        clone_fn.as_global_value().as_pointer_value().into(),
    ]);
    let g = fn_ctx.llvm_mod.add_global(layout_ty, None, &global_name);
    g.set_constant(true);
    g.set_linkage(Linkage::Private);
    g.set_initializer(&init);
    Ok(g.as_pointer_value())
}

/// Walk all raw-MIR `Terminator::Call` sites and transition any `Pending`
/// layout lowering facts to `Finalized` when a matching operation-call
/// site is found.
///
/// Must be invoked before `verify_hashmap_lowering_facts_consistent` so
/// that operation-call sites authored by the checker correctly advance
/// the fact lifecycle before the consistency gate fires.
///
/// The walk is idempotent: already-`Finalized` facts are left unchanged
/// (double-finalize is safe — the consistency check only cares about the
/// final state, not the number of transitions).
///
/// Key-name resolution: for a `HashMap<K, V>` handle in `args[0]`, the
/// arg0 local's `ResolvedTy` is `Named { name: "HashMap", args: [K, ..] }`.
/// The key record name is the `name` field of `K` when `K` is also a
/// `Named` variant.  Same logic for `HashSet<K>` with the elem type.
///
/// LESSONS: `codegen-abi-authority` (P0).
fn finalize_layout_facts_against_pipeline(
    pipeline: &IrPipeline,
    hashmap_facts: &mut [hew_types::HashMapLoweringFact],
    hashset_facts: &mut [hew_types::HashSetLoweringFact],
) {
    use hew_types::{HashMapAbi, HashMapLoweringFactState, HashSetAbi};

    use hew_types::runtime_call::RuntimeCallFamily as F;

    for mir_fn in &pipeline.raw_mir {
        for block in &mir_fn.blocks {
            let Terminator::Call {
                builtin: Some(family),
                args,
                dest,
                ..
            } = &block.terminator
            else {
                continue;
            };

            // Two classes of call sites finalize a `Pending` fact:
            //   (a) The op-call families from slice-i/iii (insert / get /
            //       remove / contains / len / is_empty / keys / values) —
            //       handle is `args[0]`.
            //   (b) The constructor families from slice-ii — handle is the
            //       return value (`dest`); arg list is empty.
            // Probe callees and the `*FreeLayout` families are NOT
            // included here; their fact-lifecycle wiring is either
            // already done (probes) or drops are dispatched through the
            // `drop_helper_for_kind` path, not through this walker.
            // Classification keys on the carried typed family — a layout
            // op minted without its family never finalizes a fact, and the
            // stuck `Pending` fact fails closed at
            // `verify_hashmap_lowering_facts_consistent`.
            let (key_or_elem_name, is_hashset) = match family {
                F::HashMapInsertLayout
                | F::HashMapContainsKeyLayout
                | F::HashMapRemoveLayout
                | F::HashMapLenLayout
                | F::HashMapKeysLayout
                | F::HashMapValuesLayout
                | F::HashMapGetLayout
                | F::HashSetInsertLayout
                | F::HashSetContainsLayout
                | F::HashSetRemoveLayout
                | F::HashSetLenLayout
                | F::HashSetIsEmptyLayout => {
                    if args.is_empty() {
                        continue;
                    }
                    let arg0_ty = match args[0] {
                        Place::Local(id) => mir_fn.locals.get(id as usize),
                        _ => None,
                    };
                    let Some(resolved_ty) = arg0_ty else {
                        continue;
                    };
                    let is_set = matches!(
                        family,
                        F::HashSetInsertLayout
                            | F::HashSetContainsLayout
                            | F::HashSetRemoveLayout
                            | F::HashSetLenLayout
                            | F::HashSetIsEmptyLayout
                    );
                    let expected_outer = if is_set { "HashSet" } else { "HashMap" };
                    let Some(name) = extract_layout_record_name(resolved_ty, expected_outer) else {
                        continue;
                    };
                    (name, is_set)
                }
                F::HashMapNewWithLayout
                | F::HashSetNewWithLayout
                | F::HashMapNew
                | F::HashSetNew => {
                    let Some(dest_place) = dest else {
                        continue;
                    };
                    let dest_ty = match dest_place {
                        Place::Local(id) => mir_fn.locals.get(*id as usize),
                        _ => None,
                    };
                    let Some(resolved_ty) = dest_ty else {
                        continue;
                    };
                    // Faithful transcription of the historical string compare
                    // (`callee == "hew_hashset_new_with_layout"`): the
                    // `HashSet::new` surface form was NOT classified as a set
                    // constructor (its dest then fails the `HashMap` outer
                    // check below and the site never finalizes a fact).
                    // Preserved byte-identically; reclassifying it is a
                    // behaviour change owned by the layout-fact lane.
                    let is_set = matches!(family, F::HashSetNewWithLayout);
                    let expected_outer = if is_set { "HashSet" } else { "HashMap" };
                    let Some(name) = extract_layout_record_name(resolved_ty, expected_outer) else {
                        continue;
                    };
                    (name, is_set)
                }
                _ => continue,
            };

            if is_hashset {
                for fact in hashset_facts.iter_mut() {
                    if let HashSetAbi::Layout { elem_record_name } = &fact.abi {
                        if elem_record_name == &key_or_elem_name
                            && fact.state == HashMapLoweringFactState::Pending
                        {
                            fact.state = HashMapLoweringFactState::Finalized;
                        }
                    }
                }
            } else {
                for fact in hashmap_facts.iter_mut() {
                    if let HashMapAbi::LayoutKey {
                        key_record_name, ..
                    } = &fact.abi
                    {
                        if key_record_name == &key_or_elem_name
                            && fact.state == HashMapLoweringFactState::Pending
                        {
                            fact.state = HashMapLoweringFactState::Finalized;
                        }
                    }
                }
            }
        }
    }
}

/// Extract the first type-argument's record name from a `Named` type
/// whose outer name matches `expected_outer`. Returns `None` if the
/// shape doesn't match.
///
/// Used by `finalize_layout_facts_against_pipeline` to resolve the key
/// record name for `HashMap<K, V>` (returns `K`'s name) or the element
/// record name for `HashSet<T>` (returns `T`'s name). Shared between the
/// op-call and constructor branches of the walker so both arms agree on
/// how to read the type-argument's identity.
fn extract_layout_record_name(ty: &ResolvedTy, expected_outer: &str) -> Option<String> {
    match ty {
        ResolvedTy::Named {
            name,
            args: ty_args,
            ..
        } if name == expected_outer && !ty_args.is_empty() => match &ty_args[0] {
            ResolvedTy::Named { name: k, .. } => Some(k.clone()),
            _ => None,
        },
        _ => None,
    }
}

/// Verify the checker-authored layout-fact lifecycle is consistent.
///
/// Called from `emit_module` and `validate_codegen_front` (the two
/// codegen entry points) so any orphaned `Pending` fact — i.e. a
/// `LayoutKey` site the checker admitted but operation lowering never
/// reached — fails closed with a structured diagnostic before any IR
/// is emitted.
///
/// This is the codegen-side enforcement of the W3.003-C council Rev 5
/// invariant: facts authored at the checker boundary are the single
/// source of truth for layout-backed runtime calls, and codegen MUST
/// consume them by transitioning each to `Finalized`.  Reaching this
/// helper with a `Pending` fact left means the checker admitted a
/// site the codegen path could not handle — a contract violation that
/// would otherwise miscompile into a runtime memory-safety hole.
///
/// LESSONS: `codegen-abi-authority` (P0), `boundary-fail-closed` (P0).
///
/// The HashSet path is folded into the same check by promoting each
/// `HashSetLoweringFact` to a `HashMapLoweringFact`-shaped record: the
/// only attribute checked at this layer is the `Pending`/`Finalized`
/// state plus the layout's size/align well-formedness, both of which
/// the existing `assert_lowering_facts_consistent` already validates.
pub(crate) fn verify_hashmap_lowering_facts_consistent(pipeline: &IrPipeline) -> CodegenResult<()> {
    use hew_types::{
        assert_lowering_facts_consistent, HashMapAbi, HashMapLoweringFact, HashMapValueType,
        HashSetAbi, LoweringFactConsistencyError,
    };

    // Clone the pipeline's facts so the finalize walk can mutate them
    // without holding a mutable borrow on the (otherwise immutable)
    // pipeline.  The finalize walk transitions any Pending fact that
    // has a matching operation-call site to Finalized before the
    // consistency gate runs.  LESSONS: codegen-abi-authority (P0).
    let mut hashmap_facts = pipeline.hashmap_lowering_facts.clone();
    let mut hashset_facts = pipeline.hashset_lowering_facts.clone();
    finalize_layout_facts_against_pipeline(pipeline, &mut hashmap_facts, &mut hashset_facts);

    let mut facts: Vec<HashMapLoweringFact> = hashmap_facts;
    // Promote each HashSet fact (post-finalize) to a HashMap-shaped fact whose
    // abi is `LayoutKey { key_record_name = elem_record_name, val = Char }` —
    // an arbitrary non-Layout value variant — so the existing
    // `assert_lowering_facts_consistent` walks the elem layout's
    // size/align bytes through the same `key_*` invariant path.  The
    // value side is not checked for non-Layout values.
    for set_fact in &hashset_facts {
        let elem_record_name = match &set_fact.abi {
            HashSetAbi::Layout { elem_record_name } => elem_record_name.clone(),
            // Non-Layout HashSet ABIs (scalar Int64 / String) have no
            // layout descriptor to consume; codegen never reaches the
            // operation-lowering transition for them, so they are not
            // subject to the Pending → Finalized invariant.
            _ => continue,
        };
        facts.push(HashMapLoweringFact {
            abi: HashMapAbi::LayoutKey {
                key_record_name: elem_record_name,
                val: HashMapValueType::Char,
            },
            state: set_fact.state,
            key_size: set_fact.elem_size,
            key_align: set_fact.elem_align,
            val_size: None,
            val_align: None,
        });
    }

    match assert_lowering_facts_consistent(&facts) {
        Ok(()) => Ok(()),
        Err(err) => Err(CodegenError::FailClosed(match err {
            LoweringFactConsistencyError::PendingLayoutKeyFact { record_name } => format!(
                "checker emitted a layout-backed HashMap/HashSet site for record \
                 `{record_name}` that codegen never finalized; this is a \
                 codegen/checker contract violation (LESSONS: codegen-abi-authority \
                 P0).  Operation-lowering MUST transition Pending → Finalized at \
                 each call site before pipeline finalization."
            ),
            LoweringFactConsistencyError::FinalizedLayoutKeyMissingSize { record_name } => format!(
                "Finalized layout-key fact for `{record_name}` has zero or missing \
                 key size — codegen wrote back an inconsistent layout descriptor"
            ),
            LoweringFactConsistencyError::FinalizedLayoutKeyBadAlign { record_name, align } => {
                format!(
                    "Finalized layout-key fact for `{record_name}` has non-power-of-two \
                     key alignment {align}"
                )
            }
            LoweringFactConsistencyError::FinalizedLayoutValueMissingSize { record_name } => {
                format!(
                    "Finalized layout-value fact for key `{record_name}` has zero or \
                     missing value size"
                )
            }
            LoweringFactConsistencyError::FinalizedLayoutValueBadAlign { record_name, align } => {
                format!(
                    "Finalized layout-value fact for key `{record_name}` has \
                     non-power-of-two value alignment {align}"
                )
            }
        })),
    }
}

/// Recognise the two C-3a synthesis probe callees.  Distinct from the
/// runtime-symbol predicates because they are a descriptor-synthesis seam, not
/// a runtime ABI call.
pub(crate) fn is_hashmap_layout_probe_symbol(symbol: &str) -> bool {
    matches!(
        symbol,
        "__hew_codegen_emit_hashmap_layout_probe"
            | "__hew_codegen_emit_hashset_layout_probe"
            | "__hew_codegen_emit_vec_layout_probe"
    )
}

/// Recognise the non-probe, non-constructor, non-get, non-free layout
/// operation families handled by `lower_hashmap_layout_direct_call`.
///
/// Does NOT include the probe callees (those are caught by
/// `is_hashmap_layout_probe_symbol`), the constructors (`*_new_with_layout`
/// — caught by `is_hashmap_constructor_symbol`, slice-ii), `.get()`
/// (`hew_hashmap_get_layout` — slice-iii), or the free helpers
/// (`*_free_layout` — actor-state drop-plan reroute handled in
/// `drop_helper_for_kind`, slice-ii).
pub(crate) fn is_hashmap_layout_runtime_symbol(symbol: &str) -> bool {
    use hew_types::runtime_call::{RuntimeCallAbiShape, RuntimeCallFamily};

    RuntimeCallFamily::from_c_symbol(symbol)
        .is_some_and(|family| family.abi_shape() == RuntimeCallAbiShape::HashCollectionLayoutOp)
}

pub(crate) fn is_hashmap_layout_get_symbol(symbol: &str) -> bool {
    use hew_types::runtime_call::{RuntimeCallAbiShape, RuntimeCallFamily};

    RuntimeCallFamily::from_c_symbol(symbol)
        .is_some_and(|family| family.abi_shape() == RuntimeCallAbiShape::HashMapLayoutGet)
}

/// Recognise the layout `HashMap`/`HashSet` constructor identities.
///
/// Kept distinct from `is_hashmap_layout_runtime_symbol` because the constructor
/// shape is fundamentally different from the operation arms: it carries no
/// source-level args (codegen synthesises the layout descriptor pointers
/// from the dest's `HashMap<K,V>` / `HashSet<T>` type), and the handle is
/// the *return value* rather than `args[0]`. The clarity is worth the
/// extra predicate per slice-ii commit-body justification.
pub(crate) fn is_hashmap_constructor_symbol(symbol: &str) -> bool {
    use hew_types::runtime_call::{RuntimeCallAbiShape, RuntimeCallFamily};

    RuntimeCallFamily::from_c_symbol(symbol)
        .is_some_and(|family| family.abi_shape() == RuntimeCallAbiShape::HashCollectionConstructor)
}

/// True for the `bytes::new` associated-function constructor. Catalogued as
/// `BuiltinLinkage::CalleeNameDispatchOnly` (`stdlib_catalog.rs`), so it reaches
/// codegen as the literal callee name and must be intercepted here rather than
/// resolved as a user/runtime symbol.
pub(crate) fn is_bytes_constructor_symbol(symbol: &str) -> bool {
    use hew_types::runtime_call::{RuntimeCallAbiShape, RuntimeCallFamily};

    RuntimeCallFamily::from_c_symbol(symbol)
        .is_some_and(|family| family.abi_shape() == RuntimeCallAbiShape::BytesConstructor)
}

fn hashmap_constructor_runtime_symbol(symbol: &str) -> CodegenResult<&'static str> {
    match symbol {
        "hew_hashmap_new_with_layout" | "HashMap::new" => Ok("hew_hashmap_new_with_layout"),
        "hew_hashset_new_with_layout" | "HashSet::new" => Ok("hew_hashset_new_with_layout"),
        _ => Err(CodegenError::FailClosed(format!(
            "not a layout HashMap/HashSet constructor symbol: {symbol}"
        ))),
    }
}

/// Per-symbol LLVM function type for the two layout HashMap/HashSet
/// constructor runtime entry points.
///
/// Runtime ABI authority:
/// - `hew-runtime/src/hashmap.rs:1295` —
///   `hew_hashmap_new_with_layout(*const HewMapKeyLayout, *const HewMapValueLayout) -> *mut HewLayoutHashMap`
/// - `hew-runtime/src/hashset.rs:611` —
///   `hew_hashset_new_with_layout(*const HewMapKeyLayout) -> *mut HewLayoutHashSet`
fn hashmap_constructor_fn_type<'ctx>(
    ctx: &'ctx Context,
    symbol: &str,
) -> CodegenResult<inkwell::types::FunctionType<'ctx>> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    match symbol {
        "hew_hashmap_new_with_layout" => Ok(ptr_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)),
        "hew_hashset_new_with_layout" => Ok(ptr_ty.fn_type(&[ptr_ty.into()], false)),
        _ => Err(CodegenError::FailClosed(format!(
            "not a layout HashMap/HashSet constructor symbol: {symbol}"
        ))),
    }
}

fn get_or_declare_hashmap_constructor<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(fv) = llvm_mod.get_function(symbol) {
        return Ok(fv);
    }
    Ok(llvm_mod.add_function(
        symbol,
        hashmap_constructor_fn_type(ctx, symbol)?,
        Some(Linkage::External),
    ))
}

/// Per-symbol LLVM function type for the 8 layout HashMap/HashSet operation
/// runtime entry points.
///
/// Parallel to `layout_vec_fn_type` (`llvm.rs:8707`).
///
/// Runtime ABI authority: `hew-runtime/src/hashmap.rs:1295-1589` (HashMap)
/// and `hew-runtime/src/hashset.rs:611-742` (HashSet).
///
/// All bool-returning arms declare `i1` return; `lower_hashmap_layout_direct_call`
/// widens to the dest local's integer width via `zext_bool_i1_to_dest`,
/// matching the `bool_vec_fn_type` convention.
fn layout_hashmap_fn_type<'ctx>(
    ctx: &'ctx Context,
    symbol: &str,
) -> CodegenResult<inkwell::types::FunctionType<'ctx>> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    let i1_ty = ctx.bool_type();
    match symbol {
        // `bool hew_hashmap_insert_layout(map, key_ptr, val_ptr)`
        "hew_hashmap_insert_layout" => {
            Ok(i1_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false))
        }
        // `bool hew_hashmap_contains_key_layout(map, key_ptr)`
        "hew_hashmap_contains_key_layout" => {
            Ok(i1_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false))
        }
        // `*const c_void hew_hashmap_get_layout(map, key_ptr)`
        "hew_hashmap_get_layout" => Ok(ptr_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)),
        // `bool hew_hashmap_get_clone_layout(map, key_ptr, out_ptr)`
        "hew_hashmap_get_clone_layout" => {
            Ok(i1_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false))
        }
        // `bool hew_hashmap_remove_layout(map, key_ptr)`
        "hew_hashmap_remove_layout" => Ok(i1_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)),
        // `bool hew_hashmap_remove_take_layout(map, key_ptr, out_ptr)` — the
        // move-out remove backing `HashMap::remove(k) -> Option<V>` (drop-K,
        // move-V into `out`); same shape as `get_clone_layout`.
        "hew_hashmap_remove_take_layout" => {
            Ok(i1_ty.fn_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false))
        }
        // `i64 hew_hashmap_len_layout(map)`
        "hew_hashmap_len_layout" => Ok(i64_ty.fn_type(&[ptr_ty.into()], false)),
        // `*mut HewVec hew_hashmap_keys_layout(map)`
        "hew_hashmap_keys_layout" => Ok(ptr_ty.fn_type(&[ptr_ty.into()], false)),
        // `*mut HewVec hew_hashmap_values_layout(map)`
        "hew_hashmap_values_layout" => Ok(ptr_ty.fn_type(&[ptr_ty.into()], false)),
        // `*mut HewLayoutHashMap hew_hashmap_clone_layout(map)`
        "hew_hashmap_clone_layout" => Ok(ptr_ty.fn_type(&[ptr_ty.into()], false)),
        // `void hew_hashmap_clear_layout(map)`
        "hew_hashmap_clear_layout" => Ok(ctx.void_type().fn_type(&[ptr_ty.into()], false)),
        // `bool hew_hashset_insert_layout(set, elem_ptr)`
        "hew_hashset_insert_layout" => Ok(i1_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)),
        // `bool hew_hashset_contains_layout(set, elem_ptr)`
        "hew_hashset_contains_layout" => Ok(i1_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)),
        // `bool hew_hashset_remove_layout(set, elem_ptr)`
        "hew_hashset_remove_layout" => Ok(i1_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)),
        // `i64 hew_hashset_len_layout(set)`
        "hew_hashset_len_layout" => Ok(i64_ty.fn_type(&[ptr_ty.into()], false)),
        // `bool hew_hashset_is_empty_layout(set)`
        "hew_hashset_is_empty_layout" => Ok(i1_ty.fn_type(&[ptr_ty.into()], false)),
        // `*mut HewVec hew_hashset_to_vec_layout(set)`
        "hew_hashset_to_vec_layout" => Ok(ptr_ty.fn_type(&[ptr_ty.into()], false)),
        // `*mut HewLayoutHashSet hew_hashset_clone_layout(set)`
        "hew_hashset_clone_layout" => Ok(ptr_ty.fn_type(&[ptr_ty.into()], false)),
        // `void hew_hashset_clear_layout(set)`
        "hew_hashset_clear_layout" => Ok(ctx.void_type().fn_type(&[ptr_ty.into()], false)),
        _ => Err(CodegenError::FailClosed(format!(
            "not a layout HashMap/HashSet runtime symbol: {symbol}"
        ))),
    }
}

fn get_or_declare_layout_hashmap_runtime<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(fv) = llvm_mod.get_function(symbol) {
        return Ok(fv);
    }
    Ok(llvm_mod.add_function(
        symbol,
        layout_hashmap_fn_type(ctx, symbol)?,
        Some(Linkage::External),
    ))
}

/// Lower a synthesis probe callee.  Pure side-effect: emits the per-key hash
/// thunk, reuses the per-key equality thunk, and emits the key/value layout
/// globals.  No runtime call is issued — operation-call lowering is deferred.
///
/// Arity contract:
/// - `__hew_codegen_emit_hashmap_layout_probe(key_local, val_local)` → no dest.
/// - `__hew_codegen_emit_hashset_layout_probe(elem_local)` → no dest.
///
/// The probe arg types drive synthesis: codegen reads each Place's resolved
/// LLVM type (via `place_pointer`) and feeds it into the descriptor builders.
/// This keeps the seam test-driveable without inventing a new MIR carrier for
/// "type to synthesise"; a real local of the right type is the unforgeable
/// witness.
pub(crate) fn lower_hashmap_layout_probe(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if let Some(d) = dest {
        return Err(CodegenError::FailClosed(format!(
            "{callee}: synthesis probe must not carry a dest, got {d:?}"
        )));
    }
    match callee {
        "__hew_codegen_emit_hashmap_layout_probe" => {
            if args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "{callee}: expected 2 args (key, value), got {}",
                    args.len()
                )));
            }
            let (_, key_ty) = place_pointer(fn_ctx, args[0])?;
            let (_, val_ty) = place_pointer(fn_ctx, args[1])?;
            let key_resolved = place_resolved_ty(fn_ctx, args[0])?;
            // LIFECYCLE-NOTE (deferred): the codegen-abi-authority
            // lesson (LESSONS Rev 5) wants this probe site to drain the
            // matching `HashMapLoweringFact` out of `IrPipeline` and
            // transition it Pending → Finalized, failing closed when
            // the fact is already Finalized (double consume) or when
            // no matching fact exists (orphan probe).
            //
            // This slice landed the `IrPipeline.hashmap_lowering_facts`
            // / `hashset_lowering_facts` fields, the
            // `attach_lowering_facts` driver glue, and an
            // `assert_lowering_facts_consistent` check at `emit_module`
            // / `validate_codegen_front`.  The per-site Pending →
            // Finalized transition is deferred to the follow-up slice
            // that also lands the missing MIR-side dispatch lowering
            // that would emit `Terminator::Call` to the 13
            // `hew_hashmap_*_layout` / `hew_hashset_*_layout` symbols.
            // Until then the dedup-by-name guard in
            // `get_or_emit_hash_thunk` /
            // `hashmap_*_layout_descriptor_ptr` and the finalize-time
            // consistency check preserve the observable safety
            // property.
            //
            // Order matters: key layout pulls in hash+eq thunks; value
            // layout is independent.  Emit both so the test surface sees
            // both globals.
            let _ = hashmap_key_layout_descriptor_ptr(fn_ctx, key_ty, Some(key_resolved))?;
            let val_resolved = place_resolved_ty(fn_ctx, args[1])?;
            let _ = hashmap_value_layout_descriptor_ptr(fn_ctx, val_ty, Some(val_resolved))?;
        }
        "__hew_codegen_emit_hashset_layout_probe" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "{callee}: expected 1 arg (elem), got {}",
                    args.len()
                )));
            }
            let (_, elem_ty) = place_pointer(fn_ctx, args[0])?;
            let elem_resolved = place_resolved_ty(fn_ctx, args[0])?;
            // LIFECYCLE-NOTE (deferred): see the HashMap arm above —
            // the matching `HashSetLoweringFact` Pending → Finalized
            // transition is deferred for the same reasons, and the
            // finalize-time `assert_lowering_facts_consistent` check
            // landed by this slice covers the orphan-Pending case.
            //
            // HashSet path: only the key/element layout is emitted by
            // codegen.  The zero-size value layout is fabricated inside
            // `hew_hashset_new_with_layout` itself (C-1c).
            let _ = hashmap_key_layout_descriptor_ptr(fn_ctx, elem_ty, Some(elem_resolved))?;
        }
        "__hew_codegen_emit_vec_layout_probe" => {
            if args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "{callee}: expected 1 arg (elem), got {}",
                    args.len()
                )));
            }
            // Vec layout path: a single `HewTypeLayout` descriptor whose two
            // leading `usize` fields are target-width. The probe drives the
            // same `layout_descriptor_ptr` synthesis the Vec ops use, so the
            // emitted `@__hew_layout_probe_*` global is the unforgeable witness
            // that the descriptor width follows the active target.
            let (_, elem_ty) = place_pointer(fn_ctx, args[0])?;
            let _ = crate::layout::layout_descriptor_ptr(fn_ctx, elem_ty, "probe")?;
        }
        _ => {
            return Err(CodegenError::FailClosed(format!(
                "lower_hashmap_layout_probe called with non-probe symbol `{callee}`"
            )));
        }
    }

    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("probe next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx_with(|| format!("{callee} br"))?;
    Ok(())
}

fn bool_vec_fn_type<'ctx>(
    ctx: &'ctx Context,
    symbol: &str,
) -> CodegenResult<inkwell::types::FunctionType<'ctx>> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    let i1_ty = ctx.bool_type();
    match symbol {
        "hew_vec_push_bool" => Ok(ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i1_ty.into()], false)),
        "hew_vec_get_bool" => Ok(i1_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false)),
        "hew_vec_set_bool" => Ok(ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into(), i1_ty.into()], false)),
        "hew_vec_pop_bool" => Ok(i1_ty.fn_type(&[ptr_ty.into()], false)),
        // `remove_at_bool(vec, index) -> i1` — index-based move-out twin of get.
        "hew_vec_remove_at_bool" => Ok(i1_ty.fn_type(&[ptr_ty.into(), i64_ty.into()], false)),
        _ => Err(CodegenError::FailClosed(format!(
            "not a bool Vec runtime symbol: {symbol}"
        ))),
    }
}

pub(crate) fn get_or_declare_bool_vec_runtime<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(fv) = llvm_mod.get_function(symbol) {
        return Ok(fv);
    }
    Ok(llvm_mod.add_function(
        symbol,
        bool_vec_fn_type(ctx, symbol)?,
        Some(Linkage::External),
    ))
}

fn truncate_bool_arg_to_i1<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    place: Place,
    label: &str,
) -> CodegenResult<IntValue<'ctx>> {
    let (ptr, ty) = place_pointer(fn_ctx, place)?;
    let BasicTypeEnum::IntType(int_ty) = ty else {
        return Err(CodegenError::FailClosed(format!(
            "{label}: bool Vec argument must be an integer local, got {ty:?}"
        )));
    };
    let loaded = fn_ctx
        .builder
        .build_load(ty, ptr, &format!("{label}_load"))
        .llvm_ctx_with(|| format!("{label} load"))?
        .into_int_value();
    if int_ty.get_bit_width() == 1 {
        Ok(loaded)
    } else {
        fn_ctx
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                loaded,
                int_ty.const_zero(),
                &format!("{label}_trunc_i1"),
            )
            .llvm_ctx_with(|| format!("{label} trunc bool to i1"))
    }
}

pub(crate) fn zext_bool_i1_to_dest<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    value: IntValue<'ctx>,
    dest_ty: BasicTypeEnum<'ctx>,
    label: &str,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let BasicTypeEnum::IntType(dest_int) = dest_ty else {
        return Err(CodegenError::FailClosed(format!(
            "{label}: bool Vec result destination must be an integer local, got {dest_ty:?}"
        )));
    };
    if dest_int.get_bit_width() == value.get_type().get_bit_width() {
        Ok(value.into())
    } else {
        Ok(fn_ctx
            .builder
            .build_int_z_extend(value, dest_int, &format!("{label}_zext_i8"))
            .llvm_ctx_with(|| format!("{label} zext bool result"))?
            .into())
    }
}

fn load_signed_int_arg_to_declared_width<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    place: Place,
    expected: IntType<'ctx>,
    label: &str,
) -> CodegenResult<IntValue<'ctx>> {
    let (ptr, ty) = place_pointer(fn_ctx, place)?;
    let BasicTypeEnum::IntType(int_ty) = ty else {
        return Err(CodegenError::FailClosed(format!(
            "{label}: Vec index argument must be an integer local, got {ty:?}"
        )));
    };
    let loaded = fn_ctx
        .builder
        .build_load(ty, ptr, &format!("{label}_load"))
        .llvm_ctx_with(|| format!("{label} load"))?
        .into_int_value();
    let actual_width = int_ty.get_bit_width();
    let expected_width = expected.get_bit_width();
    if actual_width == expected_width {
        Ok(loaded)
    } else if actual_width < expected_width {
        fn_ctx
            .builder
            .build_int_s_extend(loaded, expected, &format!("{label}_sext"))
            .llvm_ctx_with(|| format!("{label} sign-extend to ABI width"))
    } else {
        Err(CodegenError::FailClosed(format!(
            "{label}: Vec index argument width i{actual_width} is wider than the \
             declared runtime ABI width i{expected_width}"
        )))
    }
}

pub(crate) fn lower_vec_i32_get_set<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    fn_symbols: &FnSymbolMap<'ctx>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    let expected_arity = match callee {
        "hew_vec_get_i32" => 2,
        "hew_vec_set_i32" => 3,
        _ => {
            return Err(CodegenError::FailClosed(format!(
                "lower_vec_i32_get_set called with non-i32 Vec get/set symbol `{callee}`"
            )));
        }
    };
    if args.len() != expected_arity {
        return Err(CodegenError::FailClosed(format!(
            "{callee}: expected {expected_arity} args, got {}",
            args.len()
        )));
    }

    let (fv, return_ty, returns_unit) = (*fn_symbols.get(callee).ok_or_else(|| {
        CodegenError::FailClosed(format!("Vec<i32> call `{callee}` has no declared symbol"))
    })?)
    .real(callee, "lower_vec_i32_get_set")?;
    let params = fv.get_type().get_param_types();
    if params.len() != expected_arity {
        return Err(CodegenError::FailClosed(format!(
            "{callee}: declared runtime ABI expects {} params, but direct call lowering has {expected_arity} args",
            params.len()
        )));
    }
    let BasicMetadataTypeEnum::IntType(index_ty) = params[1] else {
        return Err(CodegenError::FailClosed(format!(
            "{callee}: declared Vec index ABI param must be integer, got {:?}",
            params[1]
        )));
    };

    let vec_ptr = load_duplex_handle(fn_ctx, args[0], &format!("{callee}_arg0"))?;
    let index = load_signed_int_arg_to_declared_width(
        fn_ctx,
        args[1],
        index_ty,
        &format!("{callee}_arg1"),
    )?;

    let mut arg_vals: Vec<BasicMetadataValueEnum> = Vec::with_capacity(expected_arity);
    arg_vals.push(vec_ptr.into());
    arg_vals.push(index.into());
    if callee == "hew_vec_set_i32" {
        if let Some(d) = dest {
            return Err(CodegenError::FailClosed(format!(
                "hew_vec_set_i32 returns unit; producer must not supply dest={d:?}"
            )));
        }
        if !returns_unit {
            return Err(CodegenError::FailClosed(
                "hew_vec_set_i32 declaration must return unit".into(),
            ));
        }
        let (value_ptr, value_ty) = place_pointer(fn_ctx, args[2])?;
        let value = fn_ctx
            .builder
            .build_load(value_ty, value_ptr, "hew_vec_set_i32_arg2_load")
            .llvm_ctx("hew_vec_set_i32 arg2 load")?;
        arg_vals.push(metadata_value_from_basic(value));
    }
    let call_site = fn_ctx
        .builder
        .build_call(fv, &arg_vals, &format!("{callee}_call"))
        .llvm_ctx_with(|| format!("{callee} call"))?;

    if callee == "hew_vec_get_i32" {
        if returns_unit {
            return Err(CodegenError::FailClosed(
                "hew_vec_get_i32 declaration must return a value".into(),
            ));
        }
        let dest_place = dest.ok_or_else(|| {
            CodegenError::FailClosed(
                "hew_vec_get_i32 returns a value; producer must supply a dest".into(),
            )
        })?;
        let ret_val = call_site
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_vec_get_i32 returned void".into()))?;
        let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
        if dest_ty != return_ty {
            return Err(CodegenError::FailClosed(format!(
                "hew_vec_get_i32: dest type {dest_ty:?} does not match return type {return_ty:?}"
            )));
        }
        fn_ctx
            .builder
            .build_store(dest_ptr, ret_val)
            .llvm_ctx("hew_vec_get_i32 store")?;
    }

    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx_with(|| format!("{callee} br"))?;
    Ok(())
}

pub(crate) fn lower_bool_vec_direct_call(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    let expected_arity = match callee {
        "hew_vec_push_bool" | "hew_vec_get_bool" | "hew_vec_remove_at_bool" => 2,
        "hew_vec_set_bool" => 3,
        "hew_vec_pop_bool" => 1,
        _ => {
            return Err(CodegenError::FailClosed(format!(
                "lower_bool_vec_direct_call called with non-bool Vec symbol `{callee}`"
            )));
        }
    };
    if args.len() != expected_arity {
        return Err(CodegenError::FailClosed(format!(
            "{callee}: expected {expected_arity} args, got {}",
            args.len()
        )));
    }

    let vec_ptr = load_duplex_handle(fn_ctx, args[0], &format!("{callee} arg0"))?;
    let fv = get_or_declare_bool_vec_runtime(fn_ctx.ctx, fn_ctx.llvm_mod, callee)?;
    let call_site = match callee {
        "hew_vec_push_bool" => {
            let val = truncate_bool_arg_to_i1(fn_ctx, args[1], "hew_vec_push_bool_arg1")?;
            fn_ctx
                .builder
                .build_call(fv, &[vec_ptr.into(), val.into()], "hew_vec_push_bool_call")
                .llvm_ctx("hew_vec_push_bool call")?
        }
        "hew_vec_get_bool" => {
            let index = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_get_bool_arg1",
            )?;
            fn_ctx
                .builder
                .build_call(fv, &[vec_ptr.into(), index.into()], "hew_vec_get_bool_call")
                .llvm_ctx("hew_vec_get_bool call")?
        }
        "hew_vec_set_bool" => {
            let index = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_set_bool_arg1",
            )?;
            let val = truncate_bool_arg_to_i1(fn_ctx, args[2], "hew_vec_set_bool_arg2")?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), index.into(), val.into()],
                    "hew_vec_set_bool_call",
                )
                .llvm_ctx("hew_vec_set_bool call")?
        }
        "hew_vec_pop_bool" => fn_ctx
            .builder
            .build_call(fv, &[vec_ptr.into()], "hew_vec_pop_bool_call")
            .llvm_ctx("hew_vec_pop_bool call")?,
        "hew_vec_remove_at_bool" => {
            let index = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_remove_at_bool_arg1",
            )?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), index.into()],
                    "hew_vec_remove_at_bool_call",
                )
                .llvm_ctx("hew_vec_remove_at_bool call")?
        }
        _ => unreachable!("matched above"),
    };

    match (callee, dest) {
        ("hew_vec_push_bool" | "hew_vec_set_bool", Some(d)) => {
            return Err(CodegenError::FailClosed(format!(
                "{callee} returns unit; producer must not supply dest={d:?}"
            )));
        }
        ("hew_vec_push_bool" | "hew_vec_set_bool", None) => {}
        ("hew_vec_get_bool" | "hew_vec_pop_bool" | "hew_vec_remove_at_bool", Some(dest_place)) => {
            let ret_val = call_site
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed(format!("{callee} returned void")))?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let store_val =
                zext_bool_i1_to_dest(fn_ctx, ret_val.into_int_value(), dest_ty, callee)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, store_val)
                .llvm_ctx_with(|| format!("{callee} store"))?;
        }
        ("hew_vec_get_bool" | "hew_vec_pop_bool" | "hew_vec_remove_at_bool", None) => {
            return Err(CodegenError::FailClosed(format!(
                "{callee} returns bool; producer must supply a dest"
            )));
        }
        _ => unreachable!("matched above"),
    }

    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx_with(|| format!("{callee} br"))?;
    Ok(())
}

pub(crate) fn layout_vec_element_needs_descriptor<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    elem_ty: &ResolvedTy,
) -> CodegenResult<Option<BasicTypeEnum<'ctx>>> {
    match elem_ty {
        ResolvedTy::Tuple(_) => {
            if resolved_ty_is_plain_bitcopy(fn_ctx, elem_ty, &mut HashSet::new())? {
                Ok(Some(resolve_ty(
                    fn_ctx.ctx,
                    fn_ctx.target_data,
                    elem_ty,
                    fn_ctx.record_layouts,
                )?))
            } else {
                Ok(None)
            }
        }
        ResolvedTy::Named { name, args, .. } => {
            // An indirect enum is heap-allocated: every element slot holds an
            // 8-byte pointer, so it rides the pointer ABI (`hew_vec_*_ptr`) —
            // the same routing the checker (`vec_authority` pointer ABI) and
            // the MIR getter (`lower_vec_index`'s `!is_indirect`
            // gate) already use. It must NOT take a layout descriptor: its
            // tagged-union struct is registered in `record_layouts` (every enum
            // is), so without this gate the `contains_key` check below would
            // build a layout-aware (BitCopy) Vec, and a `hew_vec_push_ptr` then
            // tripped the runtime layout-aware abort ("Vec layout-aware
            // operation is not implemented"). Fall through to `Ok(None)` so the
            // constructor selects the plain pointer-element Vec
            // (`dedup-semantic-boundary`).
            if crate::layout::is_indirect_enum(name, fn_ctx.enum_layouts) {
                return Ok(None);
            }
            let lookup_key = if args.is_empty() {
                name.clone()
            } else {
                mangle_with_shortened_args(short_name(name), args)
            };
            if fn_ctx.record_layouts.contains_key(lookup_key.as_str())
                || fn_ctx.record_layouts.contains_key(short_name(name))
            {
                return Ok(Some(resolve_ty(
                    fn_ctx.ctx,
                    fn_ctx.target_data,
                    elem_ty,
                    fn_ctx.record_layouts,
                )?));
            }
            Ok(None)
        }
        _ => Ok(None),
    }
}

fn resolved_ty_is_plain_bitcopy(
    fn_ctx: &FnCtx<'_, '_>,
    ty: &ResolvedTy,
    visited_records: &mut HashSet<String>,
) -> CodegenResult<bool> {
    match ty {
        ResolvedTy::I8
        | ResolvedTy::U8
        | ResolvedTy::Bool
        | ResolvedTy::I16
        | ResolvedTy::U16
        | ResolvedTy::I32
        | ResolvedTy::U32
        | ResolvedTy::F32
        | ResolvedTy::Char
        | ResolvedTy::I64
        | ResolvedTy::U64
        | ResolvedTy::F64
        | ResolvedTy::Duration
        | ResolvedTy::Isize
        | ResolvedTy::Usize
        | ResolvedTy::Unit
        | ResolvedTy::Never => Ok(true),
        ResolvedTy::Tuple(elems) => {
            for elem in elems {
                if !resolved_ty_is_plain_bitcopy(fn_ctx, elem, visited_records)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        ResolvedTy::Named { name, args, .. } => {
            if name.as_str() == "LocalPid" {
                return Ok(true);
            }
            let lookup_key = if args.is_empty() {
                name.clone()
            } else {
                mangle_with_shortened_args(short_name(name), args)
            };
            let short_key = short_name(name);
            let (record_key, fields) =
                if let Some(fields) = fn_ctx.record_field_resolved_tys.get(lookup_key.as_str()) {
                    (lookup_key.as_str(), fields)
                } else if let Some(fields) = fn_ctx.record_field_resolved_tys.get(short_key) {
                    (short_key, fields)
                } else {
                    return Ok(false);
                };
            if !visited_records.insert(record_key.to_string()) {
                return Err(CodegenError::FailClosed(format!(
                    "BitCopy tuple Vec layout probe found recursive record `{record_key}`"
                )));
            };
            for field_ty in fields {
                if !resolved_ty_is_plain_bitcopy(fn_ctx, field_ty, visited_records)? {
                    visited_records.remove(record_key);
                    return Ok(false);
                }
            }
            visited_records.remove(record_key);
            Ok(true)
        }
        ResolvedTy::String
        | ResolvedTy::Bytes
        | ResolvedTy::CancellationToken
        | ResolvedTy::Pointer { .. }
        | ResolvedTy::Borrow { .. }
        | ResolvedTy::Function { .. }
        | ResolvedTy::Closure { .. }
        | ResolvedTy::TraitObject { .. }
        | ResolvedTy::Array(..)
        | ResolvedTy::Slice(_)
        | ResolvedTy::Task(_)
        | ResolvedTy::TypeParam { .. } => Ok(false),
    }
}

pub(crate) fn lower_vec_constructor_call(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if callee != "Vec::new" {
        return Err(CodegenError::FailClosed(format!(
            "lower_vec_constructor_call called with non-Vec constructor `{callee}`"
        )));
    }
    if !args.is_empty() {
        return Err(CodegenError::FailClosed(format!(
            "Vec::new expects 0 args, got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(
            "Vec::new returns a Vec handle; producer must supply a dest".into(),
        )
    })?;
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    let ResolvedTy::Named {
        name,
        args: vec_args,
        ..
    } = dest_ty
    else {
        return Err(CodegenError::FailClosed(format!(
            "Vec::new dest must be Vec<T>, got {dest_ty:?}"
        )));
    };
    if name != "Vec" || vec_args.len() != 1 {
        return Err(CodegenError::FailClosed(format!(
            "Vec::new dest must be Vec<T>, got Named {{ name: {name:?}, args: {vec_args:?} }}"
        )));
    }
    let elem_ty = &vec_args[0];
    // Decide the constructor ABI for the element type. Three descriptor-bearing
    // shapes: Plain (scalar/string/ptr — no descriptor), BitCopy-layout
    // (`hew_vec_new_with_layout`, thunk-less `HewTypeLayout`), and Owned
    // (`hew_vec_new_with_elem_layout`, thunk-bearing `HewVecElemLayout`). The
    // owned branch is checked first because a non-BitCopy record/enum with a
    // resolvable thunk path is the W5.016 owned case; only when the element is
    // NOT owned does it fall through to the BitCopy-layout / plain decision.
    enum VecCtor<'c> {
        /// Argless constructor (scalar / string / ptr).
        Plain(&'static str),
        /// `hew_vec_new_with_layout` with a thunk-less `HewTypeLayout`.
        BitCopyLayout(BasicTypeEnum<'c>),
        /// `hew_vec_new_with_elem_layout` with a thunk-bearing `HewVecElemLayout`.
        Owned(BasicTypeEnum<'c>),
        /// Release-only descriptor for boxed closure-pair pointer elements.
        ClosurePair,
    }
    let ctor = match elem_ty {
        ResolvedTy::Bool => VecCtor::Plain("hew_vec_new_bool"),
        ResolvedTy::I8 => VecCtor::Plain("hew_vec_new_i8"),
        ResolvedTy::U8 => VecCtor::Plain("hew_vec_new_u8"),
        ResolvedTy::I16 => VecCtor::Plain("hew_vec_new_i16"),
        ResolvedTy::U16 => VecCtor::Plain("hew_vec_new_u16"),
        ResolvedTy::Char | ResolvedTy::I32 | ResolvedTy::U32 => VecCtor::Plain("hew_vec_new"),
        // `duration` is a signed 8-byte newtype — i64-class element, same
        // constructor as i64 (`instant` arrives canonicalised to I64). This must
        // match the i64 routing on every accessor (push/get/set/pop/slice) so
        // the Vec is built and operated under one ABI tier
        // (`vec-element-width-symmetric-abi`).
        ResolvedTy::I64
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize
        | ResolvedTy::Duration => VecCtor::Plain("hew_vec_new_i64"),
        ResolvedTy::F32 => VecCtor::Plain("hew_vec_new_f32"),
        ResolvedTy::F64 => VecCtor::Plain("hew_vec_new_f64"),
        ResolvedTy::String => VecCtor::Plain("hew_vec_new_str"),
        // Closure-pair elements use pointer operations for their boxed pair
        // handles, but carry a release-only descriptor so the canonical Vec
        // free recursively drops every environment and box.
        ResolvedTy::Function { .. } | ResolvedTy::Closure { .. } => VecCtor::ClosurePair,
        // Indirect-enum elements are heap-allocated pointer handles: each slot
        // holds an 8-byte pointer, the same convention the checker
        // (`vec_authority` pointer ABI) and the MIR getter
        // (`lower_vec_index`'s `!is_indirect` gate) route through. Selected
        // explicitly because `resolve_ty` returns the registered tagged-union
        // STRUCT for an indirect enum (struct-layout-first), so the generic
        // pointer-detection in the `_` arm below would not recognise it and
        // would fail closed. Without this arm the indirect enum fell through to
        // the layout-descriptor path and `hew_vec_push_ptr` tripped the runtime
        // layout-aware abort (`dedup-semantic-boundary`).
        ResolvedTy::Named { name, .. }
            if crate::layout::is_indirect_enum(name, fn_ctx.enum_layouts) =>
        {
            VecCtor::Plain("hew_vec_new_ptr")
        }
        _ => {
            // Owned (heap-owning record/enum/tuple) takes precedence over the
            // BitCopy-layout descriptor. The owned-vs-BitCopy verdict MUST be the
            // SAME single authority the push/get/set/pop and scope-exit-free sides
            // consult (`resolved_ty_element_owns_heap_for_owned_vec`, which mirrors
            // the MIR `named_elem_owns_heap` allow-list) — otherwise the Vec is
            // constructed under one ABI and operated under the other, tripping the
            // runtime layout-aware abort (`dedup-semantic-boundary`). In
            // particular a payload-free or all-scalar-payload enum owns no heap, so
            // it stays on the BitCopy `_layout` path on BOTH sides.
            let owned = resolved_ty_element_owns_heap_for_owned_vec(fn_ctx, elem_ty);
            if owned {
                let elem_llvm_ty = resolve_ty(
                    fn_ctx.ctx,
                    fn_ctx.target_data,
                    elem_ty,
                    fn_ctx.record_layouts,
                )?;
                VecCtor::Owned(elem_llvm_ty)
            } else if let Some(layout_ty) = layout_vec_element_needs_descriptor(fn_ctx, elem_ty)? {
                VecCtor::BitCopyLayout(layout_ty)
            } else {
                let elem_llvm_ty = resolve_ty(
                    fn_ctx.ctx,
                    fn_ctx.target_data,
                    elem_ty,
                    fn_ctx.record_layouts,
                )?;
                if matches!(elem_llvm_ty, BasicTypeEnum::PointerType(_)) {
                    VecCtor::Plain("hew_vec_new_ptr")
                } else {
                    return Err(CodegenError::FailClosed(format!(
                        "Vec::new has no constructor lowering for element type {elem_ty:?}"
                    )));
                }
            }
        }
    };

    let runtime_symbol = match ctor {
        VecCtor::Plain(sym) => sym,
        VecCtor::BitCopyLayout(_) => "hew_vec_new_with_layout",
        VecCtor::Owned(_) | VecCtor::ClosurePair => "hew_vec_new_with_elem_layout",
    };
    let fv = get_or_declare_vec_constructor(fn_ctx.ctx, fn_ctx.llvm_mod, runtime_symbol)?;
    let call = match ctor {
        VecCtor::BitCopyLayout(elem_llvm_ty) => {
            let layout_ptr = crate::layout::layout_descriptor_ptr(fn_ctx, elem_llvm_ty, "new")?;
            fn_ctx
                .builder
                .build_call(fv, &[layout_ptr.into()], "hew_vec_new_with_layout_call")
                .llvm_ctx("hew_vec_new_with_layout call")?
        }
        VecCtor::Owned(elem_llvm_ty) => {
            let layout_ptr = crate::layout::owned_elem_layout_descriptor_ptr(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                fn_ctx.target_data,
                fn_ctx.owned_elem_registries(),
                elem_ty,
                elem_llvm_ty,
                "new",
            )?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[layout_ptr.into()],
                    "hew_vec_new_with_elem_layout_call",
                )
                .llvm_ctx("hew_vec_new_with_elem_layout call")?
        }
        VecCtor::ClosurePair => {
            let layout_ptr = crate::layout::closure_pair_elem_layout_descriptor_ptr(fn_ctx)?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[layout_ptr.into()],
                    "hew_vec_new_with_elem_layout_call",
                )
                .llvm_ctx("hew_vec_new_with_elem_layout closure-pair call")?
        }
        VecCtor::Plain(_) => fn_ctx
            .builder
            .build_call(fv, &[], &format!("{runtime_symbol}_call"))
            .llvm_ctx_with(|| format!("{runtime_symbol} call"))?,
    };
    let handle = call
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed(format!("{runtime_symbol} returned void")))?;
    let (dest_ptr, dest_slot_ty) = place_pointer(fn_ctx, *dest_place)?;
    let BasicTypeEnum::PointerType(_) = dest_slot_ty else {
        return Err(CodegenError::FailClosed(format!(
            "Vec::new dest slot must be pointer-shaped, got {dest_slot_ty:?}"
        )));
    };
    fn_ctx
        .builder
        .build_store(dest_ptr, handle)
        .llvm_ctx("Vec::new store")?;

    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("Vec::new br")?;
    Ok(())
}

pub(crate) fn lower_layout_vec_direct_call(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let i32_ty = fn_ctx.ctx.i32_type();
    let expected_arity = match callee {
        "hew_vec_push_layout" | "hew_vec_get_layout" => 2,
        "hew_vec_set_layout" => 3,
        "hew_vec_pop_layout" => 1,
        // W3.032 Slice 3d: `hew_vec_contains_thunk(vec, val) -> bool`.
        // Source-level args: receiver Vec handle + needle value place.
        // The hidden `eq_fn` operand is synthesized by codegen.
        "hew_vec_contains_thunk" => 2,
        // W3.003: `hew_vec_remove_at_layout(vec, index)`.
        // Source-level args: receiver Vec handle + index (i64).
        // The hidden `layout` operand is synthesized by codegen from the
        // Vec element type.
        "hew_vec_remove_at_layout" => 2,
        // W3.003: `hew_vec_clone_layout(vec) -> *mut HewVec`.
        // Source-level args: receiver Vec handle only.
        // The hidden `layout` operand is synthesized by codegen from the
        // Vec element type.
        "hew_vec_clone_layout" => 1,
        // Range-slice for BitCopy layout-backed elements.
        // Source-level args: receiver Vec handle + start + end.
        // The hidden `layout` operand is synthesized by codegen from the
        // Vec element type.
        "hew_vec_slice_range_layout" => 3,
        _ => {
            return Err(CodegenError::FailClosed(format!(
                "lower_layout_vec_direct_call called with non-layout Vec symbol `{callee}`"
            )));
        }
    };
    if args.len() != expected_arity {
        return Err(CodegenError::FailClosed(format!(
            "{callee}: expected {expected_arity} source-level args, got {}",
            args.len()
        )));
    }

    let vec_ptr = load_duplex_handle(fn_ctx, args[0], &format!("{callee} arg0"))?;
    let fv = crate::layout::get_or_declare_layout_vec_runtime(fn_ctx.ctx, fn_ctx.llvm_mod, callee)?;
    match callee {
        "hew_vec_push_layout" => {
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_vec_push_layout returns unit; producer must not supply dest={d:?}"
                )));
            }
            let (data_ptr, elem_ty) = place_pointer(fn_ctx, args[1])?;
            let layout_ptr = crate::layout::layout_descriptor_ptr(fn_ctx, elem_ty, "push")?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), data_ptr.into(), layout_ptr.into()],
                    "hew_vec_push_layout_call",
                )
                .llvm_ctx("hew_vec_push_layout call")?;
        }
        "hew_vec_get_layout" => {
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_get_layout returns an element; producer must supply a dest".into(),
                )
            })?;
            let index = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_get_layout_arg1",
            )?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let layout_ptr = crate::layout::layout_descriptor_ptr(fn_ctx, dest_ty, "get")?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), index.into(), layout_ptr.into()],
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
        }
        "hew_vec_set_layout" => {
            if let Some(d) = dest {
                return Err(CodegenError::FailClosed(format!(
                    "hew_vec_set_layout returns unit; producer must not supply dest={d:?}"
                )));
            }
            let index = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_set_layout_arg1",
            )?;
            let (data_ptr, elem_ty) = place_pointer(fn_ctx, args[2])?;
            let layout_ptr = crate::layout::layout_descriptor_ptr(fn_ctx, elem_ty, "set")?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[
                        vec_ptr.into(),
                        index.into(),
                        data_ptr.into(),
                        layout_ptr.into(),
                    ],
                    "hew_vec_set_layout_call",
                )
                .llvm_ctx("hew_vec_set_layout call")?;
        }
        "hew_vec_pop_layout" => {
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_pop_layout returns an element; producer must supply a dest".into(),
                )
            })?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let layout_ptr = crate::layout::layout_descriptor_ptr(fn_ctx, dest_ty, "pop")?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), dest_ptr.into(), layout_ptr.into()],
                    "hew_vec_pop_layout_call",
                )
                .llvm_ctx("hew_vec_pop_layout call")?;
            let ok = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("hew_vec_pop_layout returned void".into()))?
                .into_int_value();
            let nonzero = fn_ctx
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    ok,
                    i32_ty.const_zero(),
                    "hew_vec_pop_layout_nonempty",
                )
                .llvm_ctx("hew_vec_pop_layout compare")?;
            let current = fn_ctx
                .builder
                .get_insert_block()
                .ok_or_else(|| CodegenError::FailClosed("layout pop has no insert block".into()))?;
            let parent = current.get_parent().ok_or_else(|| {
                CodegenError::FailClosed("layout pop insert block has no parent function".into())
            })?;
            let trap_bb = fn_ctx
                .ctx
                .append_basic_block(parent, "hew_vec_pop_layout_empty");
            let next_bb = *fn_ctx
                .blocks
                .get(&next)
                .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
            fn_ctx
                .builder
                .build_conditional_branch(nonzero, next_bb, trap_bb)
                .llvm_ctx("hew_vec_pop_layout branch")?;
            fn_ctx.builder.position_at_end(trap_bb);
            emit_trap_with_code(
                fn_ctx,
                HEW_TRAP_INDEX_OUT_OF_BOUNDS as u64,
                "hew_vec_pop_layout_empty_trap",
            )?;
            return Ok(());
        }
        "hew_vec_contains_thunk" => {
            // W3.032 Slice 3d: lower the checker-authorized
            // `Vec<Record/Tuple>::contains` rewrite.  Source-level args are
            // `[vec_handle, needle_place]`; the third runtime operand is a
            // pointer to a codegen-emitted equality thunk.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_contains_thunk returns bool; producer must supply a dest".into(),
                )
            })?;
            let (val_ptr, elem_ty) = place_pointer(fn_ctx, args[1])?;
            let val_resolved_ty = place_resolved_ty(fn_ctx, args[1])?;
            let thunk_fn =
                crate::thunks::get_or_emit_eq_thunk(fn_ctx, elem_ty, Some(val_resolved_ty))?;
            let thunk_ptr = thunk_fn.as_global_value().as_pointer_value();
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), val_ptr.into(), thunk_ptr.into()],
                    "hew_vec_contains_thunk_call",
                )
                .llvm_ctx("hew_vec_contains_thunk call")?;
            let raw_i32 = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_vec_contains_thunk returned void".into())
                })?
                .into_int_value();
            // Result is `i32` (0/1) from the runtime — convert to the dest's
            // stored bool shape.  Hew bool locals are stored as i8 (see
            // `primitive_to_llvm`); compare-NE-zero + zext-to-dest preserves
            // the "non-zero ⇒ equal" contract without assuming a specific
            // integer width at the dest.
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            if !matches!(dest_ty, BasicTypeEnum::IntType(_)) {
                return Err(CodegenError::FailClosed(format!(
                    "hew_vec_contains_thunk dest must be an integer bool slot, got {dest_ty:?}"
                )));
            }
            let nz = fn_ctx
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    raw_i32,
                    i32_ty.const_zero(),
                    "contains_thunk_nz",
                )
                .llvm_ctx("contains_thunk compare")?;
            // Use the width-aware helper so this remains valid if the bool
            // storage representation changes (e.g. i1 ↔ i8).
            let widened = zext_bool_i1_to_dest(fn_ctx, nz, dest_ty, "contains_thunk")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .llvm_ctx("contains_thunk store")?;
        }
        "hew_vec_remove_at_layout" => {
            // `Vec<T>::remove(index) -> T` for BitCopy layout-backed elements —
            // the index-based move-out twin of `pop_layout`. Source-level:
            // `vec.remove(i)` → the removed element in `dest`. Runtime ABI:
            // `i32 hew_vec_remove_at_layout(ptr vec, i64 index, ptr out, ptr layout)`.
            // The hidden `out` (the dest slot) and `layout` ptr are synthesized
            // here; the runtime moves the element bytes into `out`, shifts the
            // tail (no clone, no drop — possession transfers), and traps
            // internally on an out-of-bounds index (so codegen needs no branch).
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_remove_at_layout moves an element out; producer must supply a dest"
                        .into(),
                )
            })?;
            let index = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_remove_at_layout_arg1",
            )?;
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let layout_ptr = crate::layout::layout_descriptor_ptr(fn_ctx, dest_ty, "remove")?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[
                        vec_ptr.into(),
                        index.into(),
                        dest_ptr.into(),
                        layout_ptr.into(),
                    ],
                    "hew_vec_remove_at_layout_call",
                )
                .llvm_ctx("hew_vec_remove_at_layout call")?;
        }
        "hew_vec_clone_layout" => {
            // W3.003: BitCopy bulk-copy clone for layout-backed Vec elements.
            // Source-level: `let clone = v.clone()` → dest holds the new vec handle.
            // Runtime ABI: `*mut HewVec hew_vec_clone_layout(const HewVec* v, const HewTypeLayout* layout)`.
            // The hidden `layout` ptr is synthesized here from the Vec element type.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_clone_layout returns a Vec; producer must supply a dest".into(),
                )
            })?;
            // Derive the LLVM element type from the Vec<T> resolved type.
            let vec_resolved_ty = place_resolved_ty(fn_ctx, args[0])?.clone();
            let elem_hew_ty = match &vec_resolved_ty {
                ResolvedTy::Named { args: vec_args, .. }
                    if vec_resolved_ty.is_builtin(BuiltinType::Vec) && vec_args.len() == 1 =>
                {
                    vec_args[0].clone()
                }
                other => {
                    return Err(CodegenError::FailClosed(format!(
                        "hew_vec_clone_layout: arg0 must be Vec<T>, got {other:?}"
                    )));
                }
            };
            let layout_elem_ty = layout_vec_element_needs_descriptor(fn_ctx, &elem_hew_ty)?
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "hew_vec_clone_layout: element type {elem_hew_ty:?} \
                         is not a layout-descriptor-backed record/tuple"
                    ))
                })?;
            let layout_ptr = crate::layout::layout_descriptor_ptr(fn_ctx, layout_elem_ty, "clone")?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), layout_ptr.into()],
                    "hew_vec_clone_layout_call",
                )
                .llvm_ctx("hew_vec_clone_layout call")?;
            let new_vec_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_vec_clone_layout returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, new_vec_ptr)
                .llvm_ctx("hew_vec_clone_layout store")?;
        }
        "hew_vec_slice_range_layout" => {
            // BitCopy range-slice for layout-backed Vec elements.
            // Runtime ABI: `*mut HewVec hew_vec_slice_range_layout(
            //   const HewVec* v, i64 start, i64 end, const HewTypeLayout* layout)`.
            // The returned vec preserves the same thunk-less layout descriptor.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_vec_slice_range_layout returns a Vec; producer must supply a dest".into(),
                )
            })?;
            let start = load_int_arg(
                fn_ctx,
                args[1],
                fn_ctx.ctx.i64_type(),
                "hew_vec_slice_range_layout_arg1",
            )?;
            let end = load_int_arg(
                fn_ctx,
                args[2],
                fn_ctx.ctx.i64_type(),
                "hew_vec_slice_range_layout_arg2",
            )?;
            let vec_resolved_ty = place_resolved_ty(fn_ctx, args[0])?.clone();
            let elem_hew_ty = match &vec_resolved_ty {
                ResolvedTy::Named { args: vec_args, .. }
                    if vec_resolved_ty.is_builtin(BuiltinType::Vec) && vec_args.len() == 1 =>
                {
                    vec_args[0].clone()
                }
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
            let layout_ptr = crate::layout::layout_descriptor_ptr(fn_ctx, layout_elem_ty, "slice")?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[vec_ptr.into(), start.into(), end.into(), layout_ptr.into()],
                    "hew_vec_slice_range_layout_call",
                )
                .llvm_ctx("hew_vec_slice_range_layout call")?;
            let new_vec_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_vec_slice_range_layout returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, new_vec_ptr)
                .llvm_ctx("hew_vec_slice_range_layout store")?;
        }
        _ => unreachable!("matched above"),
    }

    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx_with(|| format!("{callee} br"))?;
    let _ = ptr_ty;
    Ok(())
}

/// Lower a `Terminator::Call` to one of the layout HashMap/HashSet
/// operation runtime entry points.
///
/// Parallel to `lower_layout_vec_direct_call` (`llvm.rs:10158`).
///
/// Source-level arity (verified before dispatch):
/// - `hew_hashmap_insert_layout`:      3 args (handle, key, val)
/// - `hew_hashmap_contains_key_layout`: 2 args (handle, key)
/// - `hew_hashmap_remove_layout`:       2 args (handle, key)
/// - `hew_hashmap_len_layout`:          1 arg  (handle)
/// - `hew_hashmap_keys_layout`:         1 arg  (handle) → *mut HewVec
/// - `hew_hashmap_values_layout`:       1 arg  (handle) → *mut HewVec
/// - `hew_hashset_insert_layout`:       2 args (handle, elem)
/// - `hew_hashset_contains_layout`:     2 args (handle, elem)
/// - `hew_hashset_remove_layout`:       2 args (handle, elem)
/// - `hew_hashset_len_layout`:          1 arg  (handle)
/// - `hew_hashset_is_empty_layout`:     1 arg  (handle)
///
/// No hidden descriptor operands are synthesised at the call site: the
/// key/value layout was baked into the HashMap/HashSet handle at
/// construction time (`hew_hashmap_new_with_layout` / `…set…`).
///
/// Bool-returning arms store the `i1` result via `zext_bool_i1_to_dest`,
/// matching the `lower_bool_vec_direct_call` convention.
///
/// WASM gate: this function is only reached after the wasm fail-closed
/// gate in the dispatch switch (`uses_wasm_excluded_symbol`); WASM builds
/// never arrive here.
///
/// Release the caller's duplicate key on the OVERWRITE path of a
/// `HashMap.insert` / `HashSet.insert` (#2033).
///
/// `hew_hashmap_insert_layout` / `hew_hashset_insert_layout` return `true` for a
/// vacant insert (the runtime MOVES the caller's key into the slot — consumed)
/// and `false` for an overwrite (the runtime reuses the stored key in place; the
/// caller's freshly-built duplicate key is NOT consumed). The MIR move-checker
/// statically consumes the key binding at every insert site
/// (`consume_moved_builtin_method_arg`), which is correct on the vacant path but
/// orphans the caller's key on the overwrite path: its scope-exit drop is
/// suppressed and nothing else frees it, so it leaks one allocation per
/// colliding insert.
///
/// This is the Stage-C conditional-drop materialiser the runtime ownership
/// comments anticipate: branch on the returned `i1` and, on the overwrite path
/// (`returned == false`), release the caller's key with its type-appropriate
/// drop:
///   - `string` key -> `hew_string_drop` on the loaded string pointer (the key
///     blob is the 8-byte string handle; this is the same release the vacant
///     path's static consume relies on). Null- and static-literal-safe.
///   - layout-managed record key (a record with a heap leaf) ->
///     `__hew_record_drop_inplace_<R>` on the key blob in place (the record's
///     per-field drop spine, the same thunk wired into the key layout's
///     `drop_fn`).
///   - any other key (BitCopy scalar / Copy record) -> no heap to release; emit
///     nothing.
///
/// Frees EXACTLY ONCE on the overwrite path only: the static consume already
/// suppressed the scope-exit drop (vacant: map owns it; overwrite: this release
/// owns it), so the key is never double-freed and never leaked. The `key_place`
/// type drives the disposition via `place_resolved_ty`; congruence with the key
/// layout's `drop_fn` (see `hashmap_key_layout_descriptor_ptr`) keeps the two
/// authorities from drifting. LESSONS: `cleanup-all-exits`,
/// `boundary-fail-closed`, `lifecycle-symmetry`.
fn emit_insert_overwrite_key_release(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    raw_bool: inkwell::values::IntValue<'_>,
    key_place: Place,
) -> CodegenResult<()> {
    let key_ty = place_resolved_ty(fn_ctx, key_place)?;
    // Decide the release ritual for the caller's duplicate key. `None` = a key
    // with no owned heap leaf (BitCopy scalar / Copy record); nothing to drop.
    enum KeyRelease<'ctx> {
        /// Load the string handle from the key slot and `hew_string_drop` it.
        StringPtr,
        /// `__hew_record_drop_inplace_<R>` over the key blob in place.
        RecordInPlace(FunctionValue<'ctx>),
    }
    let release = match key_ty {
        ResolvedTy::String => Some(KeyRelease::StringPtr),
        rty if resolved_ty_contains_heap_leaf(fn_ctx, rty, &mut HashSet::new()) => {
            match crate::thunks::owned_elem_thunk_key(fn_ctx.owned_elem_registries(), rty) {
                Some((OwnedElemThunkKind::Record, record_key)) => Some(KeyRelease::RecordInPlace(
                    get_or_declare_record_drop_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &record_key),
                )),
                _ => {
                    return Err(CodegenError::FailClosed(format!(
                        "{callee}: caller-key overwrite release for `{rty:?}` has no per-record \
                         drop thunk path; only string and string-bearing record keys are \
                         layout-managed (see hashmap_key_layout_descriptor_ptr)"
                    )));
                }
            }
        }
        // BitCopy scalar / Copy record key: no owned heap, nothing to release.
        _ => None,
    };
    let Some(release) = release else {
        return Ok(());
    };

    // The drop fires only when the slot already existed (`returned == false`).
    // Branch: `is_overwrite = (returned == 0)` -> release block -> continuation.
    let parent_fn = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "{callee}: no insertion block/parent for overwrite-key release branch"
            ))
        })?;
    let release_bb = fn_ctx
        .ctx
        .append_basic_block(parent_fn, "insert_overwrite_key_release");
    let cont_bb = fn_ctx
        .ctx
        .append_basic_block(parent_fn, "insert_overwrite_key_cont");
    let zero = raw_bool.get_type().const_zero();
    let is_overwrite = fn_ctx
        .builder
        .build_int_compare(inkwell::IntPredicate::EQ, raw_bool, zero, "insert_existed")
        .llvm_ctx_with(|| format!("{callee} overwrite-flag compare"))?;
    fn_ctx
        .builder
        .build_conditional_branch(is_overwrite, release_bb, cont_bb)
        .llvm_ctx_with(|| format!("{callee} overwrite-key branch"))?;

    fn_ctx.builder.position_at_end(release_bb);
    match release {
        KeyRelease::StringPtr => {
            // The key blob is the string handle; load it and drop. `key_place`
            // resolves to the slot holding the `ptr`. `load_duplex_handle`
            // validates the slot type is a pointer and yields the loaded handle.
            let key_handle =
                load_duplex_handle(fn_ctx, key_place, &format!("{callee} overwrite key"))?;
            let helper = get_or_declare_drop_helper(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &DropHelper {
                    name: "hew_string_drop",
                },
            );
            fn_ctx
                .builder
                .build_call(helper, &[key_handle.into()], "overwrite_key_string_drop")
                .llvm_ctx_with(|| format!("{callee} overwrite key hew_string_drop"))?;
        }
        KeyRelease::RecordInPlace(thunk) => {
            // The key blob is the record struct; `__hew_record_drop_inplace_<R>`
            // takes a pointer to it and drops its owned fields in place. The key
            // arg the insert call passed is exactly that blob pointer.
            let (key_ptr, _key_slot_ty) = place_pointer(fn_ctx, key_place)?;
            fn_ctx
                .builder
                .build_call(thunk, &[key_ptr.into()], "overwrite_key_record_drop")
                .llvm_ctx_with(|| format!("{callee} overwrite key record drop"))?;
        }
    }
    fn_ctx
        .builder
        .build_unconditional_branch(cont_bb)
        .llvm_ctx_with(|| format!("{callee} overwrite-key release join"))?;

    fn_ctx.builder.position_at_end(cont_bb);
    Ok(())
}

/// LESSONS: `codegen-abi-authority` (P0), `no-silent-no-op-stubs`.
pub(crate) fn lower_hashmap_layout_direct_call(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    let expected_arity: usize = match callee {
        "hew_hashmap_insert_layout" => 3,
        "hew_hashmap_contains_key_layout" | "hew_hashmap_remove_layout" => 2,
        "hew_hashmap_len_layout"
        | "hew_hashmap_keys_layout"
        | "hew_hashmap_values_layout"
        | "hew_hashmap_clone_layout"
        | "hew_hashmap_clear_layout" => 1,
        "hew_hashset_insert_layout"
        | "hew_hashset_contains_layout"
        | "hew_hashset_remove_layout" => 2,
        "hew_hashset_len_layout" | "hew_hashset_is_empty_layout" => 1,
        "hew_hashset_to_vec_layout" => 1,
        "hew_hashset_clone_layout" => 1,
        "hew_hashset_clear_layout" => 1,
        _ => {
            return Err(CodegenError::FailClosed(format!(
                "lower_hashmap_layout_direct_call called with non-layout symbol `{callee}`"
            )));
        }
    };
    if args.len() != expected_arity {
        return Err(CodegenError::FailClosed(format!(
            "{callee}: expected {expected_arity} source-level args, got {}",
            args.len()
        )));
    }

    // arg0 is always the map/set handle: a `*mut HewLayoutHashMap` or
    // `*mut HewLayoutHashSet` stored in a ptr-typed local alloca.
    // `load_duplex_handle` validates the slot type is a pointer, loads the
    // raw pointer value, and returns it — identical to the Vec path.
    let map_ptr = load_duplex_handle(fn_ctx, args[0], &format!("{callee} arg0"))?;
    let fv = get_or_declare_layout_hashmap_runtime(fn_ctx.ctx, fn_ctx.llvm_mod, callee)?;

    match callee {
        "hew_hashmap_insert_layout" => {
            // `bool hew_hashmap_insert_layout(map, key_ptr, val_ptr)`.
            // Source: map.insert(key, val) → bool (true = key was new).
            let (key_ptr, _key_ty) = place_pointer(fn_ctx, args[1])?;
            let (val_ptr, _val_ty) = place_pointer(fn_ctx, args[2])?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[map_ptr.into(), key_ptr.into(), val_ptr.into()],
                    "hew_hashmap_insert_layout_call",
                )
                .llvm_ctx("hew_hashmap_insert_layout call")?;
            // The `i1` return is always needed for the #2033 overwrite-key
            // release, even when the source-level `bool` is discarded.
            let raw_bool = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_hashmap_insert_layout returned void".into())
                })?
                .into_int_value();
            if let Some(dest_place) = dest {
                let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
                let widened =
                    zext_bool_i1_to_dest(fn_ctx, raw_bool, dest_ty, "hashmap_insert_bool")?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, widened)
                    .llvm_ctx("hew_hashmap_insert_layout store")?;
            }
            // #2033 — on the overwrite path the runtime keeps the stored key and
            // the caller's duplicate is orphaned; release it (the vacant path's
            // static consume is correct and emits nothing here).
            emit_insert_overwrite_key_release(fn_ctx, callee, raw_bool, args[1])?;
        }
        "hew_hashmap_contains_key_layout" => {
            // `bool hew_hashmap_contains_key_layout(map, key_ptr)`.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashmap_contains_key_layout returns bool; call must supply a dest".into(),
                )
            })?;
            let (key_ptr, _key_ty) = place_pointer(fn_ctx, args[1])?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[map_ptr.into(), key_ptr.into()],
                    "hew_hashmap_contains_key_layout_call",
                )
                .llvm_ctx("hew_hashmap_contains_key_layout call")?;
            let raw_bool = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_hashmap_contains_key_layout returned void".into())
                })?
                .into_int_value();
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let widened =
                zext_bool_i1_to_dest(fn_ctx, raw_bool, dest_ty, "hashmap_contains_key_bool")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .llvm_ctx("hew_hashmap_contains_key_layout store")?;
        }
        "hew_hashmap_remove_layout" => {
            // `bool hew_hashmap_remove_layout(map, key_ptr)`.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashmap_remove_layout returns bool; call must supply a dest".into(),
                )
            })?;
            let (key_ptr, _key_ty) = place_pointer(fn_ctx, args[1])?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[map_ptr.into(), key_ptr.into()],
                    "hew_hashmap_remove_layout_call",
                )
                .llvm_ctx("hew_hashmap_remove_layout call")?;
            let raw_bool = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_hashmap_remove_layout returned void".into())
                })?
                .into_int_value();
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let widened = zext_bool_i1_to_dest(fn_ctx, raw_bool, dest_ty, "hashmap_remove_bool")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .llvm_ctx("hew_hashmap_remove_layout store")?;
        }
        "hew_hashmap_len_layout" => {
            // `i64 hew_hashmap_len_layout(map)`.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashmap_len_layout returns i64; call must supply a dest".into(),
                )
            })?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashmap_len_layout_call")
                .llvm_ctx("hew_hashmap_len_layout call")?;
            let len_val = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_hashmap_len_layout returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, len_val)
                .llvm_ctx("hew_hashmap_len_layout store")?;
        }
        "hew_hashmap_keys_layout" => {
            // `*mut HewVec hew_hashmap_keys_layout(map)` — returns an eager
            // Vec<K> snapshot; caller is the sole owner.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashmap_keys_layout returns *mut HewVec; call must supply a dest".into(),
                )
            })?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashmap_keys_layout_call")
                .llvm_ctx("hew_hashmap_keys_layout call")?;
            let vec_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_hashmap_keys_layout returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, vec_ptr)
                .llvm_ctx("hew_hashmap_keys_layout store")?;
        }
        "hew_hashmap_values_layout" => {
            // `*mut HewVec hew_hashmap_values_layout(map)` — returns an eager
            // Vec<V> snapshot; caller is the sole owner.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashmap_values_layout returns *mut HewVec; call must supply a dest".into(),
                )
            })?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashmap_values_layout_call")
                .llvm_ctx("hew_hashmap_values_layout call")?;
            let vec_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_hashmap_values_layout returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, vec_ptr)
                .llvm_ctx("hew_hashmap_values_layout store")?;
        }
        "hew_hashmap_clone_layout" => {
            // `*mut HewLayoutHashMap hew_hashmap_clone_layout(map)` — returns a
            // deep-cloned map; caller is the sole owner. The runtime duplicates
            // every owned key/value blob via the descriptor clone discipline and
            // fails closed on a missing clone thunk, so the result is an
            // independent map that the dest's type-driven scope-exit drop frees
            // exactly once (the `HASHMAP_FREE_LAYOUT_SYMBOL` half of the pair).
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashmap_clone_layout returns *mut map; call must supply a dest".into(),
                )
            })?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashmap_clone_layout_call")
                .llvm_ctx("hew_hashmap_clone_layout call")?;
            let cloned_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_hashmap_clone_layout returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, cloned_ptr)
                .llvm_ctx("hew_hashmap_clone_layout store")?;
        }
        "hew_hashmap_clear_layout" => {
            // `void hew_hashmap_clear_layout(map)` — drops owned K/V on every
            // occupied slot and resets len to 0; returns Unit, no dest.
            fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashmap_clear_layout_call")
                .llvm_ctx("hew_hashmap_clear_layout call")?;
        }
        "hew_hashset_insert_layout" => {
            // `bool hew_hashset_insert_layout(set, elem_ptr)`.
            let (elem_ptr, _elem_ty) = place_pointer(fn_ctx, args[1])?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[map_ptr.into(), elem_ptr.into()],
                    "hew_hashset_insert_layout_call",
                )
                .llvm_ctx("hew_hashset_insert_layout call")?;
            // The `i1` return is always needed for the #2033 overwrite-key
            // release, even when the source-level `bool` is discarded.
            let raw_bool = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_hashset_insert_layout returned void".into())
                })?
                .into_int_value();
            if let Some(dest_place) = dest {
                let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
                let widened =
                    zext_bool_i1_to_dest(fn_ctx, raw_bool, dest_ty, "hashset_insert_bool")?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, widened)
                    .llvm_ctx("hew_hashset_insert_layout store")?;
            }
            // #2033 — a HashSet element is its own key; on the overwrite path
            // (`returned == false`) the runtime keeps the stored element and the
            // caller's duplicate is orphaned. Release it (vacant path consumes).
            emit_insert_overwrite_key_release(fn_ctx, callee, raw_bool, args[1])?;
        }
        "hew_hashset_contains_layout" => {
            // `bool hew_hashset_contains_layout(set, elem_ptr)`.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashset_contains_layout returns bool; call must supply a dest".into(),
                )
            })?;
            let (elem_ptr, _elem_ty) = place_pointer(fn_ctx, args[1])?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[map_ptr.into(), elem_ptr.into()],
                    "hew_hashset_contains_layout_call",
                )
                .llvm_ctx("hew_hashset_contains_layout call")?;
            let raw_bool = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_hashset_contains_layout returned void".into())
                })?
                .into_int_value();
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let widened = zext_bool_i1_to_dest(fn_ctx, raw_bool, dest_ty, "hashset_contains_bool")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .llvm_ctx("hew_hashset_contains_layout store")?;
        }
        "hew_hashset_remove_layout" => {
            // `bool hew_hashset_remove_layout(set, elem_ptr)`.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashset_remove_layout returns bool; call must supply a dest".into(),
                )
            })?;
            let (elem_ptr, _elem_ty) = place_pointer(fn_ctx, args[1])?;
            let call = fn_ctx
                .builder
                .build_call(
                    fv,
                    &[map_ptr.into(), elem_ptr.into()],
                    "hew_hashset_remove_layout_call",
                )
                .llvm_ctx("hew_hashset_remove_layout call")?;
            let raw_bool = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_hashset_remove_layout returned void".into())
                })?
                .into_int_value();
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let widened = zext_bool_i1_to_dest(fn_ctx, raw_bool, dest_ty, "hashset_remove_bool")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .llvm_ctx("hew_hashset_remove_layout store")?;
        }
        "hew_hashset_len_layout" => {
            // `i64 hew_hashset_len_layout(set)`.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashset_len_layout returns i64; call must supply a dest".into(),
                )
            })?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashset_len_layout_call")
                .llvm_ctx("hew_hashset_len_layout call")?;
            let len_val = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_hashset_len_layout returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, len_val)
                .llvm_ctx("hew_hashset_len_layout store")?;
        }
        "hew_hashset_is_empty_layout" => {
            // `bool hew_hashset_is_empty_layout(set)`.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashset_is_empty_layout returns bool; call must supply a dest".into(),
                )
            })?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashset_is_empty_layout_call")
                .llvm_ctx("hew_hashset_is_empty_layout call")?;
            let raw_bool = call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_hashset_is_empty_layout returned void".into())
                })?
                .into_int_value();
            let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            let widened = zext_bool_i1_to_dest(fn_ctx, raw_bool, dest_ty, "hashset_is_empty_bool")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, widened)
                .llvm_ctx("hew_hashset_is_empty_layout store")?;
        }
        "hew_hashset_clone_layout" => {
            // `*mut HewLayoutHashSet hew_hashset_clone_layout(set)` — returns a
            // deep-cloned set (delegates to the map clone for the inner storage);
            // caller is the sole owner. Elements are constrained to Plain/string
            // by the same Hash+Eq admission as map keys, so the runtime's
            // layout-managed abort is unreachable; the dest's type-driven
            // scope-exit drop frees it once via `HASHSET_FREE_LAYOUT_SYMBOL`.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashset_clone_layout returns *mut set; call must supply a dest".into(),
                )
            })?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashset_clone_layout_call")
                .llvm_ctx("hew_hashset_clone_layout call")?;
            let cloned_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_hashset_clone_layout returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, cloned_ptr)
                .llvm_ctx("hew_hashset_clone_layout store")?;
        }
        "hew_hashset_to_vec_layout" => {
            // `*mut HewVec hew_hashset_to_vec_layout(set)` — returns an eager
            // Vec<T> snapshot of the set's elements (delegates to the inner
            // map's keys projection, so each element is cloned with its layout's
            // clone discipline); caller is the sole owner. This is the `for x in
            // s` source: the Vec then drives a VecIter cursor.
            let dest_place = dest.ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_hashset_to_vec_layout returns *mut HewVec; call must supply a dest".into(),
                )
            })?;
            let call = fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashset_to_vec_layout_call")
                .llvm_ctx("hew_hashset_to_vec_layout call")?;
            let vec_ptr = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::FailClosed("hew_hashset_to_vec_layout returned void".into())
            })?;
            let (dest_ptr, _dest_ty) = place_pointer(fn_ctx, *dest_place)?;
            fn_ctx
                .builder
                .build_store(dest_ptr, vec_ptr)
                .llvm_ctx("hew_hashset_to_vec_layout store")?;
        }
        "hew_hashset_clear_layout" => {
            // `void hew_hashset_clear_layout(set)` — delegates to the inner
            // map's clear; returns Unit, no dest.
            fn_ctx
                .builder
                .build_call(fv, &[map_ptr.into()], "hew_hashset_clear_layout_call")
                .llvm_ctx("hew_hashset_clear_layout call")?;
        }
        _ => unreachable!("matched above"),
    }

    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx_with(|| format!("{callee} br"))?;
    Ok(())
}

pub(crate) fn lower_hashmap_get_layout_call(
    fn_ctx: &FnCtx<'_, '_>,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if args.len() != 2 {
        return Err(CodegenError::FailClosed(format!(
            "hew_hashmap_get_layout expects 2 source-level args (handle, key), got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(
            "hew_hashmap_get_layout returns Option<V>; call must supply a dest".into(),
        )
    })?;

    let map_ty = place_resolved_ty(fn_ctx, args[0])?.clone();
    let val_resolved = match &map_ty {
        ResolvedTy::Named { args: ty_args, .. }
            if map_ty.is_builtin(BuiltinType::HashMap) && ty_args.len() == 2 =>
        {
            ty_args[1].clone()
        }
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_hashmap_get_layout arg0 must be HashMap<K,V>, got {other:?}"
            )));
        }
    };
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    match &dest_ty {
        ResolvedTy::Named { args: ty_args, .. }
            if dest_ty.is_builtin(BuiltinType::Option)
                && ty_args.len() == 1
                && ty_args[0] == val_resolved => {}
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_hashmap_get_layout dest must be Option<{val_resolved:?}>, got {other:?}"
            )));
        }
    }

    let val_llvm_ty = resolve_ty(
        fn_ctx.ctx,
        fn_ctx.target_data,
        &val_resolved,
        fn_ctx.record_layouts,
    )?;
    let map_ptr = load_duplex_handle(fn_ctx, args[0], "hew_hashmap_get_layout arg0")?;
    let (key_ptr, _key_ty) = place_pointer(fn_ctx, args[1])?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_hashmap_get_layout has no parent fn".into())
        })?;
    let none_bb = fn_ctx.ctx.append_basic_block(parent, "hashmap_get_none");
    let some_bb = fn_ctx.ctx.append_basic_block(parent, "hashmap_get_some");
    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    // `HashMap::get` returns an owned `Option<V>`. Compute the Some payload
    // address up front, then let the runtime clone the stored slot value into
    // it through the descriptor's semantic clone_fn. This replaces the old
    // borrowed-slot memcpy, which aliased owned V into the Option payload.
    let dest_local = composite_dest_local(*dest_place, "hew_hashmap_get_layout")?;
    let (payload_ptr, payload_ty) = place_pointer(
        fn_ctx,
        Place::MachineVariant {
            local: dest_local,
            variant_idx: 0,
            field_idx: 0,
        },
    )?;
    if payload_ty != val_llvm_ty {
        return Err(CodegenError::FailClosed(format!(
            "hew_hashmap_get_layout Option::Some payload type {payload_ty:?} \
             does not match HashMap value type {val_llvm_ty:?}"
        )));
    }
    let fv = get_or_declare_layout_hashmap_runtime(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        "hew_hashmap_get_clone_layout",
    )?;
    let is_some = fn_ctx
        .builder
        .build_call(
            fv,
            &[map_ptr.into(), key_ptr.into(), payload_ptr.into()],
            "hew_hashmap_get_clone_layout_call",
        )
        .llvm_ctx("hew_hashmap_get_clone_layout call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_hashmap_get_clone_layout returned void".into())
        })?
        .into_int_value();
    fn_ctx
        .builder
        .build_conditional_branch(is_some, some_bb, none_bb)
        .llvm_ctx("hew_hashmap_get_clone_layout condbr")?;

    // None = variant 1 for Hew's builtin `Option<T>` layout.
    fn_ctx.builder.position_at_end(none_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 1, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_hashmap_get_layout none br")?;

    // Some = variant 0. The runtime already cloned into the payload slot.
    fn_ctx.builder.position_at_end(some_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 0, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_hashmap_get_layout some br")?;
    Ok(())
}

/// Lower `HashMap::remove(k) -> Option<V>` (A233). The removing twin of
/// [`lower_hashmap_get_layout_call`]: structurally identical Option
/// construction, but it calls `hew_hashmap_remove_take_layout`, which MOVES the
/// stored value into the `Some` payload (no clone — the map keeps no copy) and
/// drops the key. On a miss the runtime returns `false` without writing the
/// payload, and we build `None`; on a hit the moved-out value is the caller's
/// sole owner (drop-safe: no leaked key, no double-freed value).
pub(crate) fn lower_hashmap_remove_take_call(
    fn_ctx: &FnCtx<'_, '_>,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if args.len() != 2 {
        return Err(CodegenError::FailClosed(format!(
            "hew_hashmap_remove_take_layout expects 2 source-level args (handle, key), got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(
            "hew_hashmap_remove_take_layout returns Option<V>; call must supply a dest".into(),
        )
    })?;

    let map_ty = place_resolved_ty(fn_ctx, args[0])?.clone();
    let val_resolved = match &map_ty {
        ResolvedTy::Named { args: ty_args, .. }
            if map_ty.is_builtin(BuiltinType::HashMap) && ty_args.len() == 2 =>
        {
            ty_args[1].clone()
        }
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_hashmap_remove_take_layout arg0 must be HashMap<K,V>, got {other:?}"
            )));
        }
    };
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    match &dest_ty {
        ResolvedTy::Named { args: ty_args, .. }
            if dest_ty.is_builtin(BuiltinType::Option)
                && ty_args.len() == 1
                && ty_args[0] == val_resolved => {}
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_hashmap_remove_take_layout dest must be Option<{val_resolved:?}>, \
                 got {other:?}"
            )));
        }
    }

    let val_llvm_ty = resolve_ty(
        fn_ctx.ctx,
        fn_ctx.target_data,
        &val_resolved,
        fn_ctx.record_layouts,
    )?;
    let map_ptr = load_duplex_handle(fn_ctx, args[0], "hew_hashmap_remove_take_layout arg0")?;
    let (key_ptr, _key_ty) = place_pointer(fn_ctx, args[1])?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_hashmap_remove_take_layout has no parent fn".into())
        })?;
    let none_bb = fn_ctx.ctx.append_basic_block(parent, "hashmap_remove_none");
    let some_bb = fn_ctx.ctx.append_basic_block(parent, "hashmap_remove_some");
    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    // The removed `V` is MOVED into the Some payload slot: compute its address,
    // then let the runtime byte-copy the stored value into it (drop-K, move-V).
    let dest_local = composite_dest_local(*dest_place, "hew_hashmap_remove_take_layout")?;
    let (payload_ptr, payload_ty) = place_pointer(
        fn_ctx,
        Place::MachineVariant {
            local: dest_local,
            variant_idx: 0,
            field_idx: 0,
        },
    )?;
    if payload_ty != val_llvm_ty {
        return Err(CodegenError::FailClosed(format!(
            "hew_hashmap_remove_take_layout Option::Some payload type {payload_ty:?} \
             does not match HashMap value type {val_llvm_ty:?}"
        )));
    }
    let fv = get_or_declare_layout_hashmap_runtime(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        "hew_hashmap_remove_take_layout",
    )?;
    let is_some = fn_ctx
        .builder
        .build_call(
            fv,
            &[map_ptr.into(), key_ptr.into(), payload_ptr.into()],
            "hew_hashmap_remove_take_layout_call",
        )
        .llvm_ctx("hew_hashmap_remove_take_layout call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_hashmap_remove_take_layout returned void".into())
        })?
        .into_int_value();
    fn_ctx
        .builder
        .build_conditional_branch(is_some, some_bb, none_bb)
        .llvm_ctx("hew_hashmap_remove_take_layout condbr")?;

    // None = variant 1 for Hew's builtin `Option<T>` layout.
    fn_ctx.builder.position_at_end(none_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 1, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_hashmap_remove_take_layout none br")?;

    // Some = variant 0. The runtime already MOVED the value into the payload.
    fn_ctx.builder.position_at_end(some_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 0, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_hashmap_remove_take_layout some br")?;
    Ok(())
}

/// Lower the trapping `m[k]` (`HashMap` `Index::at`) read. The map twin of
/// `lower_vec_get_clone_call`, and structurally `lower_hashmap_get_layout_call`
/// with the `None` branch replaced by an `IndexOutOfBounds` trap and the dest
/// narrowed from `Option<V>` to a BARE `V`.
///
/// Calls the fresh-owner clone choke `hew_hashmap_get_clone_layout(map, key,
/// out) -> bool`: on a hit the runtime semantic-clones the stored value into
/// `out` (the dest slot) through the value descriptor's `clone_fn` — a
/// retained/independent owner, never a borrow into the live table
/// (`by-value-heap-params-are-borrows` P0 drop-safety; the headline GAP-2
/// invariant). The map keeps its own copy. On a miss the runtime
/// returns `false` WITHOUT writing `out`, and we trap (`IndexOutOfBounds`)
/// instead of materialising a value, so the dest is never observed and never
/// dropped on the miss path (MIR schedules the dest's scope-exit drop only on
/// the through path, which the trap pre-empts). `m.get(k) -> Option<V>` is the
/// non-aborting sibling, lowered by `lower_hashmap_get_layout_call`.
pub(crate) fn lower_hashmap_index_trap_call(
    fn_ctx: &FnCtx<'_, '_>,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if args.len() != 2 {
        return Err(CodegenError::FailClosed(format!(
            "hew_hashmap_get_clone_layout (m[k] trap) expects 2 source-level \
             args (handle, key), got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(
            "m[k] (HashMap Index::at) yields a bare V; call must supply a dest".into(),
        )
    })?;

    let map_ty = place_resolved_ty(fn_ctx, args[0])?.clone();
    let val_resolved = match &map_ty {
        ResolvedTy::Named { args: ty_args, .. }
            if map_ty.is_builtin(BuiltinType::HashMap) && ty_args.len() == 2 =>
        {
            ty_args[1].clone()
        }
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_hashmap_get_clone_layout (m[k] trap) arg0 must be HashMap<K,V>, got {other:?}"
            )));
        }
    };
    // The dest is the BARE value `V` (not `Option<V>`): the trapping accessor
    // never materialises an `Option`.
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    if dest_ty != val_resolved {
        return Err(CodegenError::FailClosed(format!(
            "m[k] (HashMap Index::at) dest must be the bare value type \
             {val_resolved:?}, got {dest_ty:?}"
        )));
    }

    let val_llvm_ty = resolve_ty(
        fn_ctx.ctx,
        fn_ctx.target_data,
        &val_resolved,
        fn_ctx.record_layouts,
    )?;
    let map_ptr = load_duplex_handle(
        fn_ctx,
        args[0],
        "hew_hashmap_get_clone_layout (m[k] trap) arg0",
    )?;
    let (key_ptr, _key_ty) = place_pointer(fn_ctx, args[1])?;

    // The matched value is cloned directly into the dest slot (the bare `V`), so
    // the runtime out-pointer is the dest place's own address — not a
    // `Some`-payload subplace (the `Option`-building sibling's distinction).
    let (out_ptr, out_ty) = place_pointer(fn_ctx, *dest_place)?;
    if out_ty != val_llvm_ty {
        return Err(CodegenError::FailClosed(format!(
            "m[k] (HashMap Index::at) dest slot type {out_ty:?} does not match \
             HashMap value type {val_llvm_ty:?}"
        )));
    }

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::FailClosed(
                "hew_hashmap_get_clone_layout (m[k] trap) has no parent fn".into(),
            )
        })?;
    let trap_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "hashmap_index_absent_trap");
    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;

    let fv = get_or_declare_layout_hashmap_runtime(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        "hew_hashmap_get_clone_layout",
    )?;
    let found = fn_ctx
        .builder
        .build_call(
            fv,
            &[map_ptr.into(), key_ptr.into(), out_ptr.into()],
            "hashmap_index_trap_clone_call",
        )
        .llvm_ctx("hew_hashmap_get_clone_layout (m[k] trap) call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed(
                "hew_hashmap_get_clone_layout (m[k] trap) returned void".into(),
            )
        })?
        .into_int_value();
    // Hit → fall through to `next` with the cloned `V` already in the dest slot.
    // Miss → trap (`IndexOutOfBounds`), mirroring `v[i]` OOB.
    fn_ctx
        .builder
        .build_conditional_branch(found, next_bb, trap_bb)
        .llvm_ctx("hew_hashmap_get_clone_layout (m[k] trap) condbr")?;

    fn_ctx.builder.position_at_end(trap_bb);
    emit_trap_with_code(
        fn_ctx,
        HEW_TRAP_INDEX_OUT_OF_BOUNDS as u64,
        "hashmap_index_trap",
    )?;
    Ok(())
}

/// Lower the fresh-owner Vec getter used by trait-routed `Vec::get -> Option<T>`
/// and trapping owned-value `v[i] -> T`. Calls the runtime choke point
/// `hew_vec_get_clone(vec, index, out) -> bool`: the runtime bounds-checks and,
/// on a hit, writes a freshly retained/cloned owner into the `Some` payload
/// slot, so the `Option` payload is never a borrow into the live buffer
/// (`by-value-heap-params-are-borrows` P0 — the headline drop-safety invariant
/// of this change). On OOB the runtime returns `false` and we build `None`.
pub(crate) fn lower_vec_get_clone_call(
    fn_ctx: &FnCtx<'_, '_>,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if args.len() != 2 {
        return Err(CodegenError::FailClosed(format!(
            "hew_vec_get_clone expects 2 source-level args (handle, index), got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(
            "hew_vec_get_clone returns a value; call must supply a dest".into(),
        )
    })?;

    let vec_ty = place_resolved_ty(fn_ctx, args[0])?.clone();
    let elem_resolved = match &vec_ty {
        ResolvedTy::Named { args: ty_args, .. }
            if vec_ty.is_builtin(BuiltinType::Vec) && ty_args.len() == 1 =>
        {
            ty_args[0].clone()
        }
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_vec_get_clone arg0 must be Vec<T>, got {other:?}"
            )));
        }
    };
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    let dest_is_option = match &dest_ty {
        ResolvedTy::Named { args: ty_args, .. }
            if dest_ty.is_builtin(BuiltinType::Option)
                && ty_args.len() == 1
                && ty_args[0] == elem_resolved =>
        {
            true
        }
        ty if ty == &elem_resolved => false,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_vec_get_clone dest must be Option<{elem_resolved:?}> or bare \
                 {elem_resolved:?}, got {other:?}"
            )));
        }
    };

    let elem_llvm_ty = resolve_ty(
        fn_ctx.ctx,
        fn_ctx.target_data,
        &elem_resolved,
        fn_ctx.record_layouts,
    )?;
    let vec_ptr = load_duplex_handle(fn_ctx, args[0], "hew_vec_get_clone arg0")?;
    let i64_ty = fn_ctx.ctx.i64_type();
    let index = load_int_arg(fn_ctx, args[1], i64_ty, "hew_vec_get_clone index")?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::FailClosed("hew_vec_get_clone has no parent fn".into()))?;
    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;

    // `Vec::get` writes into the `Some` payload. Trapping scalar index over an
    // owned element writes the same fresh owner directly into its bare `T` dest.
    let (out_ptr, out_ty) = if dest_is_option {
        let dest_local = composite_dest_local(*dest_place, "hew_vec_get_clone")?;
        place_pointer(
            fn_ctx,
            Place::MachineVariant {
                local: dest_local,
                variant_idx: 0,
                field_idx: 0,
            },
        )?
    } else {
        place_pointer(fn_ctx, *dest_place)?
    };
    if out_ty != elem_llvm_ty {
        return Err(CodegenError::FailClosed(format!(
            "hew_vec_get_clone output slot type {out_ty:?} does not match Vec \
             element type {elem_llvm_ty:?}"
        )));
    }
    let fv = get_or_declare_vec_get_clone_runtime(fn_ctx.ctx, fn_ctx.llvm_mod);
    let is_some = fn_ctx
        .builder
        .build_call(
            fv,
            &[vec_ptr.into(), index.into(), out_ptr.into()],
            "hew_vec_get_clone_call",
        )
        .llvm_ctx("hew_vec_get_clone call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_vec_get_clone returned void".into()))?
        .into_int_value();

    if !dest_is_option {
        let trap_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "vec_index_clone_absent_trap");
        fn_ctx
            .builder
            .build_conditional_branch(is_some, next_bb, trap_bb)
            .llvm_ctx("hew_vec_get_clone bare condbr")?;
        fn_ctx.builder.position_at_end(trap_bb);
        emit_trap_with_code(
            fn_ctx,
            HEW_TRAP_INDEX_OUT_OF_BOUNDS as u64,
            "vec_index_clone_absent",
        )?;
        return Ok(());
    }

    let none_bb = fn_ctx.ctx.append_basic_block(parent, "vec_get_none");
    let some_bb = fn_ctx.ctx.append_basic_block(parent, "vec_get_some");
    fn_ctx
        .builder
        .build_conditional_branch(is_some, some_bb, none_bb)
        .llvm_ctx("hew_vec_get_clone condbr")?;

    // None = variant 1 for Hew's builtin `Option<T>` layout.
    fn_ctx.builder.position_at_end(none_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 1, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_vec_get_clone none br")?;

    // Some = variant 0. The runtime already cloned into the payload slot.
    fn_ctx.builder.position_at_end(some_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 0, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_vec_get_clone some br")?;
    Ok(())
}

/// Lower the `bytes.get(index) -> Option<u8>` choke (`Index::get`), de-aliased
/// from the trapping `b[i]` (`hew_bytes_index`). Mirrors `lower_vec_get_clone_call`
/// but owns the bounds check inline (bytes carries no `*_get_clone` runtime
/// export — plan §4.2: "do the check in MIR/codegen + an in-bounds typed load"):
///
/// ```text
///   entry:  unpack BytesTriple {ptr, offset, len}; index_in_bounds := index <u len
///           condbr index_in_bounds -> some_bb, none_bb
///   some_bb: byte := hew_bytes_index(ptr, offset, len, index)   -- in-bounds, no trap
///            store byte into Option::Some payload; tag := 0; br next
///   none_bb: tag := 1 (None); br next
/// ```
///
/// The receiver is borrowed (`is_collection_receiver_borrow_callee`), so `buf`
/// keeps its scope-exit drop. `u8` is a scalar (Copy): the `Some` payload is a
/// by-value load — no owned clone, no drop obligation on the payload.
pub(crate) fn lower_bytes_get_option_call(
    fn_ctx: &FnCtx<'_, '_>,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if args.len() != 2 {
        return Err(CodegenError::FailClosed(format!(
            "hew_bytes_get expects 2 source-level args (bytes, index), got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed("hew_bytes_get returns Option<u8>; call must supply a dest".into())
    })?;
    if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::Bytes) {
        return Err(CodegenError::FailClosed(format!(
            "hew_bytes_get arg0 must be a `bytes` receiver; got {:?}",
            place_resolved_ty(fn_ctx, args[0])?
        )));
    }
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    match &dest_ty {
        ResolvedTy::Named {
            name,
            args: ty_args,
            ..
        } if name == "Option" && ty_args.len() == 1 && ty_args[0] == ResolvedTy::U8 => {}
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_bytes_get dest must be Option<u8>, got {other:?}"
            )));
        }
    }

    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();

    // Unpack the stack-resident BytesTriple {ptr, i32 offset, i32 len} by value,
    // field-by-field — identical to the `hew_bytes_index` ABI marshalling. A
    // `bytes` value is NOT a `*mut HewVec`, so it must not route through the Vec
    // handle load.
    let (triple_ptr, triple_ty) = place_pointer(fn_ctx, args[0])?;
    let BasicTypeEnum::StructType(triple_struct_ty) = triple_ty else {
        return Err(CodegenError::FailClosed(format!(
            "hew_bytes_get: bytes receiver alloca has non-struct type {triple_ty:?}; \
             expected the `{{ptr, i32, i32}}` BytesTriple layout"
        )));
    };
    let data_ptr_gep = fn_ctx
        .builder
        .build_struct_gep(triple_struct_ty, triple_ptr, 0, "bytes_get_ptr_gep")
        .llvm_ctx("hew_bytes_get ptr GEP")?;
    let data_ptr = fn_ctx
        .builder
        .build_load(ptr_ty, data_ptr_gep, "bytes_get_ptr")
        .llvm_ctx("hew_bytes_get ptr load")?
        .into_pointer_value();
    let offset_gep = fn_ctx
        .builder
        .build_struct_gep(triple_struct_ty, triple_ptr, 1, "bytes_get_offset_gep")
        .llvm_ctx("hew_bytes_get offset GEP")?;
    let offset_val = fn_ctx
        .builder
        .build_load(i32_ty, offset_gep, "bytes_get_offset")
        .llvm_ctx("hew_bytes_get offset load")?
        .into_int_value();
    let len_gep = fn_ctx
        .builder
        .build_struct_gep(triple_struct_ty, triple_ptr, 2, "bytes_get_len_gep")
        .llvm_ctx("hew_bytes_get len GEP")?;
    let len_val = fn_ctx
        .builder
        .build_load(i32_ty, len_gep, "bytes_get_len")
        .llvm_ctx("hew_bytes_get len load")?
        .into_int_value();
    let index = load_int_arg(fn_ctx, args[1], i64_ty, "hew_bytes_get index")?;

    // index_in_bounds := (index <u zext(len)). The unsigned compare folds the
    // negative-index case into "out of bounds" (a negative i64 reinterpreted as
    // u64 exceeds any real length) so `.get(-1)` yields `None`, never a trap.
    let len_i64 = fn_ctx
        .builder
        .build_int_z_extend_or_bit_cast(len_val, i64_ty, "bytes_get_len_i64")
        .llvm_ctx("hew_bytes_get len zext")?;
    let in_bounds = fn_ctx
        .builder
        .build_int_compare(IntPredicate::ULT, index, len_i64, "bytes_get_in_bounds")
        .llvm_ctx("hew_bytes_get bounds cmp")?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::FailClosed("hew_bytes_get has no parent fn".into()))?;
    let some_bb = fn_ctx.ctx.append_basic_block(parent, "bytes_get_some");
    let none_bb = fn_ctx.ctx.append_basic_block(parent, "bytes_get_none");
    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_conditional_branch(in_bounds, some_bb, none_bb)
        .llvm_ctx("hew_bytes_get condbr")?;

    // Some = variant 0. The in-bounds typed load reuses the canonical
    // `hew_bytes_index` getter (single source of truth for byte addressing); it
    // is reached only on the in-bounds edge, so its OOB trap is unreachable.
    fn_ctx.builder.position_at_end(some_bb);
    let byte_val = fn_ctx.call_runtime_int(
        "hew_bytes_index",
        &[
            data_ptr.into(),
            offset_val.into(),
            len_val.into(),
            index.into(),
        ],
        "bytes_get_byte",
        "hew_bytes_get element load",
    )?;
    let dest_local = composite_dest_local(*dest_place, "hew_bytes_get")?;
    let (payload_ptr, payload_ty) = place_pointer(
        fn_ctx,
        Place::MachineVariant {
            local: dest_local,
            variant_idx: 0,
            field_idx: 0,
        },
    )?;
    let BasicTypeEnum::IntType(payload_int_ty) = payload_ty else {
        return Err(CodegenError::FailClosed(format!(
            "hew_bytes_get Option::Some payload must be integer-shaped (u8); got {payload_ty:?}"
        )));
    };
    let store_val = fn_ctx
        .builder
        .build_int_z_extend_or_bit_cast(byte_val, payload_int_ty, "bytes_get_byte_cast")
        .llvm_ctx("hew_bytes_get payload cast")?;
    fn_ctx
        .builder
        .build_store(payload_ptr, store_val)
        .llvm_ctx("hew_bytes_get payload store")?;
    emit_enum_variant_literal(fn_ctx, *dest_place, 0, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_bytes_get some br")?;

    // None = variant 1 for Hew's builtin `Option<T>` layout.
    fn_ctx.builder.position_at_end(none_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 1, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_bytes_get none br")?;
    Ok(())
}

/// Lower the codegen-intercepted `hew_string_get(s, index) -> Option<char>`
/// accessor: the non-trapping, drop-safe counterpart to the trapping `s[i]`
/// (`hew_string_index`). Builds the bounds-check CFG and the `Some`/`None`
/// materialisation inline, mirroring `lower_bytes_get_option_call`:
///
/// ```text
///   entry:  s_ptr := load string handle; count := hew_string_char_count(s_ptr)
///           index_in_bounds := index <u zext(count)
///           condbr index_in_bounds -> some_bb, none_bb
///   some_bb: cp := hew_string_index(s_ptr, index)   -- in-bounds, no trap
///            store cp into Option::Some payload; tag := 0; br next
///   none_bb: tag := 1 (None); br next
/// ```
///
/// The receiver is borrowed (`is_collection_receiver_borrow_callee`), so `s`
/// keeps its scope-exit drop. `char` is a scalar (Copy): the `Some` payload is
/// a by-value codepoint load — no owned clone, no drop obligation on the
/// payload. Unlike `bytes` (a stack `BytesTriple`), a `string` value is a
/// single heap `*const c_char` handle, so the receiver marshals through
/// `load_duplex_handle` exactly like the trapping `hew_string_index` arm.
pub(crate) fn lower_string_get_option_call(
    fn_ctx: &FnCtx<'_, '_>,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if args.len() != 2 {
        return Err(CodegenError::FailClosed(format!(
            "hew_string_get expects 2 source-level args (string, index), got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(
            "hew_string_get returns Option<char>; call must supply a dest".into(),
        )
    })?;
    if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::String) {
        return Err(CodegenError::FailClosed(format!(
            "hew_string_get arg0 must be a `string` receiver; got {:?}",
            place_resolved_ty(fn_ctx, args[0])?
        )));
    }
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    match &dest_ty {
        ResolvedTy::Named {
            name,
            args: ty_args,
            ..
        } if name == "Option" && ty_args.len() == 1 && ty_args[0] == ResolvedTy::Char => {}
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_string_get dest must be Option<char>, got {other:?}"
            )));
        }
    }

    let i64_ty = fn_ctx.ctx.i64_type();

    // A `string` value is a single heap `*const c_char` handle — marshal it
    // exactly like the trapping `hew_string_index` ABI arm.
    let s_ptr = load_duplex_handle(fn_ctx, args[0], "hew_string_get arg0")?;
    let index = load_int_arg(fn_ctx, args[1], i64_ty, "hew_string_get index")?;

    // count := hew_string_char_count(s_ptr) -> i32 (codepoint count, NOT byte
    // length). index_in_bounds := (index <u zext(count)). The unsigned compare
    // folds the negative-index case into "out of bounds" so `.get(-1)` yields
    // `None`, never a trap.
    let count = fn_ctx.call_runtime_int(
        "hew_string_char_count",
        &[s_ptr.into()],
        "string_get_count",
        "hew_string_get char count",
    )?;
    let count_i64 = fn_ctx
        .builder
        .build_int_z_extend_or_bit_cast(count, i64_ty, "string_get_count_i64")
        .llvm_ctx("hew_string_get count zext")?;
    let in_bounds = fn_ctx
        .builder
        .build_int_compare(IntPredicate::ULT, index, count_i64, "string_get_in_bounds")
        .llvm_ctx("hew_string_get bounds cmp")?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::FailClosed("hew_string_get has no parent fn".into()))?;
    let some_bb = fn_ctx.ctx.append_basic_block(parent, "string_get_some");
    let none_bb = fn_ctx.ctx.append_basic_block(parent, "string_get_none");
    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_conditional_branch(in_bounds, some_bb, none_bb)
        .llvm_ctx("hew_string_get condbr")?;

    // Some = variant 0. The in-bounds codepoint load reuses the canonical
    // `hew_string_index` getter (single source of truth for codepoint
    // addressing); it is reached only on the in-bounds edge, so its OOB trap is
    // unreachable.
    fn_ctx.builder.position_at_end(some_bb);
    let codepoint = fn_ctx.call_runtime_int(
        "hew_string_index",
        &[s_ptr.into(), index.into()],
        "string_get_codepoint",
        "hew_string_get codepoint load",
    )?;
    let dest_local = composite_dest_local(*dest_place, "hew_string_get")?;
    let (payload_ptr, payload_ty) = place_pointer(
        fn_ctx,
        Place::MachineVariant {
            local: dest_local,
            variant_idx: 0,
            field_idx: 0,
        },
    )?;
    let BasicTypeEnum::IntType(payload_int_ty) = payload_ty else {
        return Err(CodegenError::FailClosed(format!(
            "hew_string_get Option::Some payload must be integer-shaped (char); got {payload_ty:?}"
        )));
    };
    let store_val = fn_ctx
        .builder
        .build_int_z_extend_or_bit_cast(codepoint, payload_int_ty, "string_get_codepoint_cast")
        .llvm_ctx("hew_string_get payload cast")?;
    fn_ctx
        .builder
        .build_store(payload_ptr, store_val)
        .llvm_ctx("hew_string_get payload store")?;
    emit_enum_variant_literal(fn_ctx, *dest_place, 0, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_string_get some br")?;

    // None = variant 1 for Hew's builtin `Option<T>` layout.
    fn_ctx.builder.position_at_end(none_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 1, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("hew_string_get none br")?;
    Ok(())
}

/// Materialise `Option<LocalPid<T>>` from a `__HewChildLookupResult`.
///
/// MIR emits the pool runtime lookup first, then routes its aggregate result
/// through this layout-aware call so Option construction follows the same
/// registered enum layout as source-level `Some` / `None`.
pub(crate) fn lower_supervisor_pool_get_option_call(
    fn_ctx: &FnCtx<'_, '_>,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if args.len() != 1 {
        return Err(CodegenError::FailClosed(format!(
            "hew_supervisor_pool_get_option expects 1 child-lookup result, got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(
            "hew_supervisor_pool_get_option returns Option<LocalPid<T>>; call needs a dest".into(),
        )
    })?;
    match place_resolved_ty(fn_ctx, *dest_place)? {
        ResolvedTy::Named {
            builtin: Some(BuiltinType::Option),
            args,
            ..
        } if matches!(
            args.as_slice(),
            [ResolvedTy::Named {
                builtin: Some(BuiltinType::LocalPid),
                ..
            }]
        ) => {}
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_supervisor_pool_get_option dest must be Option<LocalPid<T>>, got {other:?}"
            )));
        }
    }
    match place_resolved_ty(fn_ctx, args[0])? {
        ResolvedTy::Named { name, args, .. }
            if name == "__HewChildLookupResult" && args.is_empty() => {}
        other => {
            return Err(CodegenError::FailClosed(format!(
                "hew_supervisor_pool_get_option arg must be __HewChildLookupResult, got {other:?}"
            )));
        }
    }

    let (lookup_ptr, lookup_ty) = place_pointer(fn_ctx, args[0])?;
    let BasicTypeEnum::StructType(lookup_struct) = lookup_ty else {
        return Err(CodegenError::FailClosed(format!(
            "hew_supervisor_pool_get_option lookup slot must be a struct, got {lookup_ty:?}"
        )));
    };
    let word0_ptr = fn_ctx
        .builder
        .build_struct_gep(lookup_struct, lookup_ptr, 0, "pool_get_word0_ptr")
        .llvm_ctx("pool get word0 GEP")?;
    let handle_ptr = fn_ctx
        .builder
        .build_struct_gep(lookup_struct, lookup_ptr, 1, "pool_get_handle_ptr")
        .llvm_ctx("pool get handle GEP")?;
    let i64_ty = fn_ctx.ctx.i64_type();
    let word0 = fn_ctx
        .builder
        .build_load(i64_ty, word0_ptr, "pool_get_word0")
        .llvm_ctx("pool get word0 load")?
        .into_int_value();
    let tag = fn_ctx
        .builder
        .build_int_truncate(word0, fn_ctx.ctx.i8_type(), "pool_get_tag")
        .llvm_ctx("pool get tag truncate")?;
    let is_live = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            tag,
            fn_ctx.ctx.i8_type().const_zero(),
            "pool_get_is_live",
        )
        .llvm_ctx("pool get tag compare")?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::FailClosed("pool get Option call has no parent fn".into()))?;
    let some_bb = fn_ctx.ctx.append_basic_block(parent, "pool_get_some");
    let none_bb = fn_ctx.ctx.append_basic_block(parent, "pool_get_none");
    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_conditional_branch(is_live, some_bb, none_bb)
        .llvm_ctx("pool get Option condbr")?;

    fn_ctx.builder.position_at_end(some_bb);
    let handle = fn_ctx
        .builder
        .build_load(i64_ty, handle_ptr, "pool_get_handle")
        .llvm_ctx("pool get handle load")?
        .into_int_value();
    let dest_local = composite_dest_local(*dest_place, "hew_supervisor_pool_get_option")?;
    let (payload_ptr, payload_ty) = place_pointer(
        fn_ctx,
        Place::MachineVariant {
            local: dest_local,
            variant_idx: 0,
            field_idx: 0,
        },
    )?;
    let BasicTypeEnum::PointerType(payload_ty) = payload_ty else {
        return Err(CodegenError::FailClosed(format!(
            "Option<LocalPid<T>>::Some payload must be pointer-shaped, got {payload_ty:?}"
        )));
    };
    let handle = fn_ctx
        .builder
        .build_int_to_ptr(handle, payload_ty, "pool_get_handle_ptr_value")
        .llvm_ctx("pool get handle inttoptr")?;
    fn_ctx
        .builder
        .build_store(payload_ptr, handle)
        .llvm_ctx("pool get Some payload store")?;
    emit_enum_variant_literal(fn_ctx, *dest_place, 0, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("pool get Some branch")?;

    fn_ctx.builder.position_at_end(none_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 1, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("pool get None branch")?;
    Ok(())
}

/// Lower the sentinel-wrapping string inspectors (D46 sentinel -> Option
/// sweep): `string.find(needle) -> Option<i64>`, `string.char_at(i) ->
/// Option<char>`, `string.codepoint_at_utf8(i) -> Option<i64>`.
///
/// Each calls its UNCHANGED runtime entry (which keeps the `-1` miss/OOB
/// sentinel at the C ABI) and materialises the Option from the sign of the
/// i32 result: `>= 0` -> `Some(value)` (variant 0, payload stored into the
/// `Option::Some` slot), `-1` -> `None` (variant 1, payload untouched). The
/// `-1` sentinel therefore never escapes into Hew-visible values.
///
/// Runtime ABIs (verified against `hew-runtime/src/string.rs`):
/// - `hew_string_find(ptr, ptr) -> i32` (byte index of first occurrence)
/// - `hew_string_char_at(ptr, i32) -> i32` (byte at byte-offset)
/// - `hew_string_char_at_utf8(ptr, i32) -> i32` (codepoint at codepoint-offset)
pub(crate) fn lower_string_sentinel_option_call(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if args.len() != 2 {
        return Err(CodegenError::FailClosed(format!(
            "{callee} expects 2 source-level args (receiver, needle/index), got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "{callee} returns an Option; call must supply a dest"
        ))
    })?;
    if !matches!(place_resolved_ty(fn_ctx, args[0])?, ResolvedTy::String) {
        return Err(CodegenError::FailClosed(format!(
            "{callee} arg0 must be a `string` receiver; got {:?}",
            place_resolved_ty(fn_ctx, args[0])?
        )));
    }
    // The checker-projected payload type per symbol: `char` for the byte
    // accessor, `i64` for the index/codepoint inspectors.
    let expected_payload = match callee {
        "hew_string_char_at" => ResolvedTy::Char,
        "hew_string_find" | "hew_string_char_at_utf8" => ResolvedTy::I64,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "lower_string_sentinel_option_call called with unknown symbol `{other}`"
            )));
        }
    };
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    match &dest_ty {
        ResolvedTy::Named {
            name,
            args: ty_args,
            ..
        } if name == "Option" && ty_args.len() == 1 && ty_args[0] == expected_payload => {}
        other => {
            return Err(CodegenError::FailClosed(format!(
                "{callee} dest must be Option<{expected_payload:?}>, got {other:?}"
            )));
        }
    }

    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();

    // A `string` value is a single heap `*const c_char` handle.
    let s_ptr = load_duplex_handle(fn_ctx, args[0], &format!("{callee} arg0"))?;

    // Declare the runtime entry with its REAL C ABI and marshal arg1
    // accordingly: `find` takes a second string handle; the two indexed
    // inspectors take an i32 index (truncated from the Hew-facing i64 — the
    // same width bridge the retired FFI-shim path applied).
    let (fn_ty, call_args): (
        inkwell::types::FunctionType<'_>,
        Vec<inkwell::values::BasicMetadataValueEnum<'_>>,
    ) = if callee == "hew_string_find" {
        let needle_ptr = load_duplex_handle(fn_ctx, args[1], &format!("{callee} arg1"))?;
        (
            i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
            vec![s_ptr.into(), needle_ptr.into()],
        )
    } else {
        let index = load_int_arg(fn_ctx, args[1], i64_ty, &format!("{callee}_arg1"))?;
        let index_i32 = fn_ctx
            .builder
            .build_int_truncate_or_bit_cast(index, i32_ty, "string_sentinel_idx_i32")
            .llvm_ctx_with(|| format!("{callee} index trunc"))?;
        (
            i32_ty.fn_type(&[ptr_ty.into(), i32_ty.into()], false),
            vec![s_ptr.into(), index_i32.into()],
        )
    };
    let fv = match fn_ctx.llvm_mod.get_function(callee) {
        Some(f) => f,
        None => fn_ctx
            .llvm_mod
            .add_function(callee, fn_ty, Some(Linkage::External)),
    };
    let raw = fn_ctx
        .builder
        .build_call(fv, &call_args, &format!("{callee}_call"))
        .llvm_ctx_with(|| format!("{callee} call"))?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed(format!("{callee} returned void")))?
        .into_int_value();

    // `>= 0` is a hit; the runtime's only negative return is the `-1` sentinel.
    let is_some = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::SGE,
            raw,
            i32_ty.const_zero(),
            "string_sentinel_is_some",
        )
        .llvm_ctx_with(|| format!("{callee} sentinel cmp"))?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::FailClosed(format!("{callee} has no parent fn")))?;
    let some_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "string_sentinel_some");
    let none_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "string_sentinel_none");
    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_conditional_branch(is_some, some_bb, none_bb)
        .llvm_ctx_with(|| format!("{callee} condbr"))?;

    // Some = variant 0: widen the non-negative i32 into the payload slot
    // (sign- and zero-extension agree on non-negative values).
    fn_ctx.builder.position_at_end(some_bb);
    let dest_local = composite_dest_local(*dest_place, callee)?;
    let (payload_ptr, payload_ty) = place_pointer(
        fn_ctx,
        Place::MachineVariant {
            local: dest_local,
            variant_idx: 0,
            field_idx: 0,
        },
    )?;
    let BasicTypeEnum::IntType(payload_int_ty) = payload_ty else {
        return Err(CodegenError::FailClosed(format!(
            "{callee} Option::Some payload must be integer-shaped; got {payload_ty:?}"
        )));
    };
    let store_val = fn_ctx
        .builder
        .build_int_z_extend_or_bit_cast(raw, payload_int_ty, "string_sentinel_payload")
        .llvm_ctx_with(|| format!("{callee} payload cast"))?;
    fn_ctx
        .builder
        .build_store(payload_ptr, store_val)
        .llvm_ctx_with(|| format!("{callee} payload store"))?;
    emit_enum_variant_literal(fn_ctx, *dest_place, 0, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx_with(|| format!("{callee} some br"))?;

    // None = variant 1 for Hew's builtin `Option<T>` layout.
    fn_ctx.builder.position_at_end(none_bb);
    emit_enum_variant_literal(fn_ctx, *dest_place, 1, &[])?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx_with(|| format!("{callee} none br"))?;
    Ok(())
}

/// Declare (or fetch) the `hew_vec_get_clone(vec, index, out) -> bool` runtime
/// choke point: `(ptr, i64, ptr) -> i1`.
fn get_or_declare_vec_get_clone_runtime<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
) -> FunctionValue<'ctx> {
    if let Some(fv) = llvm_mod.get_function("hew_vec_get_clone") {
        return fv;
    }
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i1_ty = ctx.bool_type();
    let i64_ty = ctx.i64_type();
    llvm_mod.add_function(
        "hew_vec_get_clone",
        i1_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), ptr_ty.into()], false),
        Some(Linkage::External),
    )
}
/// call to a runtime allocator invocation.
///
/// Pattern parallel to `lower_vec_constructor_call` (`llvm.rs:10265`).
/// Unlike Vec — which fans out across 6 monomorphised constructor
/// symbols based on the element type — HashMap/HashSet always route to
/// the layout-keyed constructor (`hew_hashmap_new_with_layout` /
/// `hew_hashset_new_with_layout`) because every key/elem type that
/// passes the checker's `hash_eligibility` predicate must already carry
/// a layout descriptor (`HewMapKeyLayout` with hash + eq thunks). The
/// constructor descriptor pointers are synthesised here from the dest
/// local's `HashMap<K,V>` / `HashSet<T>` resolved type via the
/// existing `hashmap_key_layout_descriptor_ptr` /
/// `hashmap_value_layout_descriptor_ptr` helpers (`llvm.rs:9506` /
/// `:9566`), so the source-level call carries zero args.
///
/// Drop-safety (CLAUDE.md §1): the returned handle is paired with the
/// `_free_layout` close path via `drop_helper_for_kind` (`llvm.rs:4682`)
/// for the actor-state-field execution context (covering both
/// sync-return at scope exit and actor-shutdown via the synthesised
/// `state_drop_fn`). Async-cancel: the drop elaborator inserts the
/// close on cancel paths via the same `drop_plans` table consumed by
/// codegen, so once the elaborator routes `HashMap`/`HashSet` locals
/// through `drop_plans` (slice-iii lane wiring), the cancel path is
/// covered. See commit body for the residual DROP-TODO.
///
/// Fail-closed (CLAUDE.md §2): every step returns an explicit
/// `CodegenError::FailClosed` or `CodegenError::Llvm` on shape mismatch;
/// no `.ok()?` / `.unwrap_or_default()` in this path.
pub(crate) fn lower_hashmap_constructor_call(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if !is_hashmap_constructor_symbol(callee) {
        return Err(CodegenError::FailClosed(format!(
            "lower_hashmap_constructor_call called with non-constructor symbol `{callee}`"
        )));
    }
    if !args.is_empty() {
        return Err(CodegenError::FailClosed(format!(
            "{callee} expects 0 source-level args (layout descriptor pointers are \
             synthesised by codegen from the dest's HashMap<K,V>/HashSet<T> type), \
             got {} arg(s)",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "{callee} returns a HashMap/HashSet handle; producer must supply a dest"
        ))
    })?;
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    let ResolvedTy::Named {
        name,
        args: ty_args,
        ..
    } = dest_ty
    else {
        return Err(CodegenError::FailClosed(format!(
            "{callee} dest must be a Named HashMap<K,V>/HashSet<T>, got {dest_ty:?}"
        )));
    };

    let runtime_symbol = hashmap_constructor_runtime_symbol(callee)?;
    let fv = get_or_declare_hashmap_constructor(fn_ctx.ctx, fn_ctx.llvm_mod, runtime_symbol)?;

    let call = match runtime_symbol {
        "hew_hashmap_new_with_layout" => {
            if name != "HashMap" || ty_args.len() != 2 {
                return Err(CodegenError::FailClosed(format!(
                    "hew_hashmap_new_with_layout dest must be HashMap<K,V>, \
                     got Named {{ name: {name:?}, args: {ty_args:?} }}"
                )));
            }
            let key_resolved = &ty_args[0];
            let val_resolved = &ty_args[1];
            let key_llvm = resolve_ty(
                fn_ctx.ctx,
                fn_ctx.target_data,
                key_resolved,
                fn_ctx.record_layouts,
            )?;
            let val_llvm = resolve_ty(
                fn_ctx.ctx,
                fn_ctx.target_data,
                val_resolved,
                fn_ctx.record_layouts,
            )?;
            let key_layout_ptr =
                hashmap_key_layout_descriptor_ptr(fn_ctx, key_llvm, Some(key_resolved))?;
            let val_layout_ptr =
                hashmap_value_layout_descriptor_ptr(fn_ctx, val_llvm, Some(val_resolved))?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[key_layout_ptr.into(), val_layout_ptr.into()],
                    "hew_hashmap_new_with_layout_call",
                )
                .llvm_ctx("hew_hashmap_new_with_layout call")?
        }
        "hew_hashset_new_with_layout" => {
            if name != "HashSet" || ty_args.len() != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "hew_hashset_new_with_layout dest must be HashSet<T>, \
                     got Named {{ name: {name:?}, args: {ty_args:?} }}"
                )));
            }
            let elem_resolved = &ty_args[0];
            let elem_llvm = resolve_ty(
                fn_ctx.ctx,
                fn_ctx.target_data,
                elem_resolved,
                fn_ctx.record_layouts,
            )?;
            // Per C-1c, the runtime injects the ZST value layout
            // internally — only the element (key) layout pointer is
            // passed across the ABI.
            let elem_layout_ptr =
                hashmap_key_layout_descriptor_ptr(fn_ctx, elem_llvm, Some(elem_resolved))?;
            fn_ctx
                .builder
                .build_call(
                    fv,
                    &[elem_layout_ptr.into()],
                    "hew_hashset_new_with_layout_call",
                )
                .llvm_ctx("hew_hashset_new_with_layout call")?
        }
        _ => unreachable!("guarded by is_hashmap_constructor_symbol"),
    };

    let handle = call
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed(format!("{callee} returned void")))?;
    let (dest_ptr, dest_slot_ty) = place_pointer(fn_ctx, *dest_place)?;
    let BasicTypeEnum::PointerType(_) = dest_slot_ty else {
        return Err(CodegenError::FailClosed(format!(
            "{callee} dest slot must be pointer-shaped, got {dest_slot_ty:?}"
        )));
    };
    fn_ctx
        .builder
        .build_store(dest_ptr, handle)
        .llvm_ctx_with(|| format!("{callee} store"))?;

    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx_with(|| format!("{callee} br"))?;
    Ok(())
}

/// Lower `bytes::new()` — the empty `bytes` constructor.
///
/// A `bytes` value is a stack `BytesTriple { ptr, offset: u32, len: u32 }`; the
/// empty value is the zero struct `{ null, 0, 0 }`. There is no heap allocation
/// and no runtime call: `hew_bytes_push` allocates lazily on the first push when
/// `triple.ptr` is null (`hew-runtime/src/bytes.rs:307`), so storing the zero
/// `BytesTriple` constant into the dest slot is the complete, correct lowering.
/// Mirrors the `Vec::new` / `HashMap::new` intercept shape (store result, branch
/// to next block).
pub(crate) fn lower_bytes_constructor_call(
    fn_ctx: &FnCtx<'_, '_>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    if !is_bytes_constructor_symbol(callee) {
        return Err(CodegenError::FailClosed(format!(
            "lower_bytes_constructor_call called with non-bytes constructor `{callee}`"
        )));
    }
    if !args.is_empty() {
        return Err(CodegenError::FailClosed(format!(
            "bytes::new expects 0 args, got {}",
            args.len()
        )));
    }
    let dest_place = dest.ok_or_else(|| {
        CodegenError::FailClosed(
            "bytes::new produces a bytes value; producer must supply a dest".into(),
        )
    })?;
    let dest_ty = place_resolved_ty(fn_ctx, *dest_place)?.clone();
    if !matches!(dest_ty, ResolvedTy::Bytes) {
        return Err(CodegenError::FailClosed(format!(
            "bytes::new dest must be `bytes`, got {dest_ty:?}"
        )));
    }
    let (dest_ptr, dest_slot_ty) = place_pointer(fn_ctx, *dest_place)?;
    let BasicTypeEnum::StructType(struct_ty) = dest_slot_ty else {
        return Err(CodegenError::FailClosed(format!(
            "bytes::new dest slot must be a BytesTriple struct, got {dest_slot_ty:?}"
        )));
    };
    // The empty BytesTriple is the all-zero struct: { null ptr, len 0, offset 0 }.
    fn_ctx
        .builder
        .build_store(dest_ptr, struct_ty.const_zero())
        .llvm_ctx("bytes::new store zero BytesTriple")?;

    let next_bb = *fn_ctx
        .blocks
        .get(&next)
        .ok_or_else(|| CodegenError::FailClosed(format!("Call next bb{next} missing")))?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("bytes::new br")?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn carried_tag_width_matches_legacy_storage_buckets() {
        for variant_count in [0, 1, 2, 3, 255, 256, 257, 65_535, 65_536] {
            let tag_width = minimum_tag_width(variant_count);
            assert_eq!(
                tag_storage_width(tag_width).expect("supported carried tag width"),
                legacy_tag_storage_width(variant_count).expect("supported legacy variant count"),
                "storage width moved for {variant_count} variants"
            );
        }
    }

    /// `owned_elem_layout_descriptor_ptr` reconstructs `HewVecElemLayout`'s
    /// byte layout as an independent LLVM `StructType` because codegen does
    /// not link `hew-cabi`. This test pins every field offset and the total
    /// size of that mirror against the real `#[repr(C)]` struct so a future
    /// field reorder on either side fails here instead of silently
    /// desyncing until a generated Vec's clone/drop thunk reads the wrong
    /// slot at runtime.
    ///
    /// The comparison struct below is built the same way
    /// `owned_elem_layout_descriptor_ptr`'s `layout_ty` is (same field-type
    /// list, same order) but independently — this test never calls that
    /// function or inspects its output, so the comparison is not
    /// tautological against codegen's own construction.
    #[test]
    fn hew_vec_elem_layout_offset_parity() {
        use hew_cabi::vec::HewVecElemLayout;

        let ctx = Context::create();
        let td = host_target_data();
        let usize_ty = ctx.ptr_sized_int_type(&td, None);
        let i8_ty = ctx.i8_type();
        let ptr_ty = ctx.ptr_type(AddressSpace::default());
        let llvm_struct = ctx.struct_type(
            &[
                usize_ty.into(),
                usize_ty.into(),
                i8_ty.into(),
                ptr_ty.into(),
                ptr_ty.into(),
            ],
            false,
        );

        // (Rust offset_of, LLVM element index) for every field, in
        // declaration order.
        let fields: [(usize, u32, &str); 5] = [
            (std::mem::offset_of!(HewVecElemLayout, size), 0, "size"),
            (std::mem::offset_of!(HewVecElemLayout, align), 1, "align"),
            (
                std::mem::offset_of!(HewVecElemLayout, ownership_kind),
                2,
                "ownership_kind",
            ),
            (
                std::mem::offset_of!(HewVecElemLayout, clone_fn),
                3,
                "clone_fn",
            ),
            (
                std::mem::offset_of!(HewVecElemLayout, drop_fn),
                4,
                "drop_fn",
            ),
        ];

        for (rust_offset, llvm_idx, name) in fields {
            let llvm_offset = td
                .offset_of_element(&llvm_struct, llvm_idx)
                .unwrap_or_else(|| panic!("LLVM struct has no element {llvm_idx} for `{name}`"));
            assert_eq!(
                llvm_offset as usize, rust_offset,
                "HewVecElemLayout field `{name}` offset mismatch: LLVM {llvm_offset} vs Rust \
                 {rust_offset} — codegen's owned_elem_layout_descriptor_ptr mirror drifted \
                 from hew_cabi::vec::HewVecElemLayout"
            );
        }

        assert_eq!(
            td.get_abi_size(&llvm_struct) as usize,
            std::mem::size_of::<HewVecElemLayout>(),
            "HewVecElemLayout total size mismatch between the codegen mirror and the \
             hew_cabi::vec::HewVecElemLayout struct"
        );
    }

    /// `layout_descriptor_ptr` reconstructs `HewTypeLayout`'s byte layout as
    /// an independent LLVM `StructType` because codegen does not link
    /// `hew-cabi`. This test pins every field offset and the total size of
    /// that mirror against the real `#[repr(C)]` struct so a future field
    /// reorder on either side fails here instead of silently desyncing.
    ///
    /// The comparison struct below is built the same way
    /// `layout_descriptor_ptr`'s `layout_ty` is (same field-type list, same
    /// order) but independently — this test never calls that function or
    /// inspects its output, so the comparison is not tautological against
    /// codegen's own construction.
    #[test]
    fn hew_type_layout_offset_parity() {
        use hew_cabi::vec::HewTypeLayout;

        let ctx = Context::create();
        let td = host_target_data();
        let usize_ty = ctx.ptr_sized_int_type(&td, None);
        let i8_ty = ctx.i8_type();
        let llvm_struct = ctx.struct_type(&[usize_ty.into(), usize_ty.into(), i8_ty.into()], false);

        let fields: [(usize, u32, &str); 3] = [
            (std::mem::offset_of!(HewTypeLayout, size), 0, "size"),
            (std::mem::offset_of!(HewTypeLayout, align), 1, "align"),
            (
                std::mem::offset_of!(HewTypeLayout, ownership_kind),
                2,
                "ownership_kind",
            ),
        ];

        for (rust_offset, llvm_idx, name) in fields {
            let llvm_offset = td
                .offset_of_element(&llvm_struct, llvm_idx)
                .unwrap_or_else(|| panic!("LLVM struct has no element {llvm_idx} for `{name}`"));
            assert_eq!(
                llvm_offset as usize, rust_offset,
                "HewTypeLayout field `{name}` offset mismatch: LLVM {llvm_offset} vs Rust \
                 {rust_offset} — codegen's layout_descriptor_ptr mirror drifted from \
                 hew_cabi::vec::HewTypeLayout"
            );
        }

        assert_eq!(
            td.get_abi_size(&llvm_struct) as usize,
            std::mem::size_of::<HewTypeLayout>(),
            "HewTypeLayout total size mismatch between the codegen mirror and the \
             hew_cabi::vec::HewTypeLayout struct"
        );
    }
}
