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
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::AddressSpace;

use hew_mir::{EnumLayout, MachineLayout, MachineVariantLayout, RecordLayout, StateFieldCloneKind};
use hew_types::ResolvedTy;

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
            field_tys.push(resolve_ty(ctx, fty, map)?);
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

        // The companion event enum: `<Name>Event` with event-variant
        // payloads. Tag bit width is derived from the event count
        let event_name = format!("{}Event", layout.name);
        let event_cg = build_tagged_union_layout(
            ctx,
            &event_name,
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
    variants: &[MachineVariantLayout],
    record_layout_map: &RecordLayoutMap<'ctx>,
    enum_layouts: &[EnumLayout],
    target_data: Option<&TargetData>,
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
                resolve_ty(ctx, fty, record_layout_map)
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
    let host_td;
    let td = if let Some(td) = target_data {
        td
    } else {
        host_td = host_target_data();
        &host_td
    };
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

    let tag_int_ty = tag_int_type_for_variant_count(ctx, outer_name, variants.len())?;
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
    let ResolvedTy::Named { name, args, .. } = ty else {
        return Err(CodegenError::FailClosed(format!(
            "enum in-place drop: ElabDrop::ty {ty:?} is not a named enum type"
        )));
    };
    let short = short_name(name);
    let key = if args.is_empty() {
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
    // constructed/released through the dedicated pointer-element +
    // `hew_vec_free_closure_pairs` path, never the managed witness. The MIR
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
            // Constructor lowering stamps every layout-backed Vec<T> handle with
            // its `HewTypeLayout` descriptor; the managed pair reads it from the
            // handle and pairs allocate/free so they cannot drift (W4.045 UAF
            // class). Layout-absent Vecs (legacy typed constructors) clone/free
            // by `elem_kind`. LayoutManaged elements fail closed in the runtime.
            clone_sym: VEC_CLONE_MANAGED_SYMBOL,
            drop_sym: VEC_FREE_MANAGED_SYMBOL,
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
        | StateFieldCloneKind::ClosurePair => None,
    })
}

pub(crate) fn is_layout_vec_runtime_symbol(symbol: &str) -> bool {
    matches!(
        symbol,
        "hew_vec_push_layout"
            | "hew_vec_get_layout"
            | "hew_vec_set_layout"
            | "hew_vec_pop_layout"
            | "hew_vec_contains_thunk"
            | "hew_vec_remove_at_layout"
            | "hew_vec_clone_layout"
            | "hew_vec_slice_range_layout"
    )
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
        // W3.003: `void hew_vec_remove_at_layout(ptr vec, i64 index, ptr layout)`.
        // Index-based remove for BitCopy layout-backed elements; the hidden
        // layout pointer is synthesized by codegen from the Vec element type.
        "hew_vec_remove_at_layout" => Ok(ctx
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into(), ptr_ty.into()], false)),
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
    let usize_ty = ctx.ptr_sized_int_type(fn_ctx.target_data, None);
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
pub(crate) fn owned_elem_layout_descriptor_ptr<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    elem_resolved_ty: &ResolvedTy,
    elem_llvm_ty: BasicTypeEnum<'ctx>,
    label: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    let Some((kind, key)) = crate::thunks::owned_elem_thunk_key(fn_ctx, elem_resolved_ty) else {
        return Err(CodegenError::FailClosed(format!(
            "owned Vec element `{elem_resolved_ty:?}` has no resolvable record/enum \
             in-place thunk path at `{label}`; the checker must not route a \
             non-thunkable element through the owned ABI"
        )));
    };
    let (size, align) = abi_size_align(elem_llvm_ty, Some(fn_ctx.target_data))?;
    let kind_tag = match kind {
        OwnedElemThunkKind::Record => "rec",
        OwnedElemThunkKind::Enum => "enum",
        OwnedElemThunkKind::Tuple => "tup",
        OwnedElemThunkKind::Collection => "coll",
    };
    let global_name = format!("__hew_vec_elem_layout_{kind_tag}_{key}_{size}_{align}");
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
            crate::thunks::emit_tuple_inplace_thunk_bodies(fn_ctx, &key, elems, elem_llvm_ty)?;
            (
                get_or_declare_tuple_clone_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
                get_or_declare_tuple_drop_inplace(fn_ctx.ctx, fn_ctx.llvm_mod, &key),
            )
        }
        OwnedElemThunkKind::Collection => {
            // The collection-handle wrapper thunk BODY is synthesized eagerly
            // here (like the tuple thunk — no registry-driven seeding): a
            // collection element is referenced only through this descriptor.
            // Idempotent: the emitter no-ops if the body already exists.
            let (clone_sym, drop_sym) = collection_elem_clone_drop_syms(fn_ctx, elem_resolved_ty)
                .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "owned collection descriptor at `{label}`: element \
                         {elem_resolved_ty:?} has no canonical clone/free primitive \
                         (closure-pair Vec elements are a separate lane)"
                ))
            })?;
            crate::thunks::emit_collection_handle_thunk_bodies(fn_ctx, &key, clone_sym, drop_sym)?;
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
    // ownership_kind = LayoutManaged (HewTypeOwnershipKind::LayoutManaged = 2).
    let init = layout_ty.const_named_struct(&[
        usize_ty.const_int(size, false).into(),
        usize_ty.const_int(u64::from(align), false).into(),
        i8_ty.const_int(2, false).into(),
        clone_fn.as_global_value().as_pointer_value().into(),
        drop_fn.as_global_value().as_pointer_value().into(),
    ]);
    let g = fn_ctx.llvm_mod.add_global(layout_ty, None, &global_name);
    g.set_constant(true);
    g.set_linkage(Linkage::Private);
    g.set_initializer(&init);
    Ok(g.as_pointer_value())
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
            let elem_llvm_ty = resolve_ty(fn_ctx.ctx, elem_ty, fn_ctx.record_layouts)?;
            owned_elem_layout_descriptor_ptr(fn_ctx, elem_ty, elem_llvm_ty, label)
        }
        _ => {
            // Plain: scalars (i8..i64, u8..u64, f32/f64, bool, char) and
            // BitCopy aggregates ride the raw representation. The checker owns
            // the admission decision; an element type it should not have
            // admitted still gets a deterministic width here rather than a
            // silent misdecode (the runtime cross-checks envelope width).
            let elem_llvm_ty = resolve_ty(fn_ctx.ctx, elem_ty, fn_ctx.record_layouts)?;
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
