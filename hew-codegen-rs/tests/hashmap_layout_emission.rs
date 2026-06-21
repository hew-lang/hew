//! Layout-backed HashMap/HashSet codegen synthesis tests.
//!
//! These tests exercise the codegen synthesis machinery for layout-keyed
//! HashMap/HashSet:
//!   - Per-record FNV-1a 64-bit hash thunk (`__hew_hash_thunk_*`).
//!   - Reuse of the `__hew_eq_thunk_*` machinery shared with `Vec::contains`.
//!   - `@__hew_map_key_layout_*` and `@__hew_map_value_layout_*` private
//!     globals shaped to match `hew_cabi::map::HewMapKeyLayout` /
//!     `HewMapValueLayout`.
//!   - wasm32 object emission for the descriptor/thunk path.
//!
//! The synthesis seam is reached via two probe callees:
//! `__hew_codegen_emit_hashmap_layout_probe(key_local, val_local)` and
//! `__hew_codegen_emit_hashset_layout_probe(elem_local)`.  These are
//! deliberately distinct from the runtime ABI callees that the operation-
//! lowering slice will lower (`hew_hashmap_new_with_layout`, etc.) so this
//! synthesis slice does not commit to an operation-lowering pathway yet.

use hew_codegen_rs::{emit_module, emit_module_objects, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, Instr, IrPipeline, Place, RawMirFunction, RecordLayout, Terminator,
};
use hew_types::ResolvedTy;

fn named(name: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: name.to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    }
}

fn vec_of(elem: ResolvedTy) -> ResolvedTy {
    ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![elem],
        builtin: None,
        is_opaque: false,
    }
}

/// Build a minimal `IrPipeline` whose `main` function lowers `entry_block`,
/// then branches to a trivial return block.  `locals` lists the local-slot
/// types in MIR order.
fn base_pipeline(
    entry_block: BasicBlock,
    locals: Vec<ResolvedTy>,
    record_layouts: Vec<RecordLayout>,
) -> IrPipeline {
    let return_block = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Return,
    };
    let blocks = vec![entry_block, return_block];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals,
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(1),
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ],
            drop_plans: vec![(ExitPath::Return { block: 1 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts,
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

fn emit_ll(pipeline: IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-hashmap-layout-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options)
        .expect("hashmap layout synthesis pipeline must emit successfully");
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(&ll_path).expect("read emitted .ll")
}

/// Build a probe pipeline that synthesises a HashMap key+value layout pair
/// for `key_ty` / `val_ty`.  Returns the `.ll` text.
///
/// The pipeline's main fn:
///   - local 0: a `key_ty`-typed slot
///   - local 1: a `val_ty`-typed slot
///   - entry block: probe call with locals (0, 1) → branch to return block.
fn emit_hashmap_probe_ll(
    key_ty: ResolvedTy,
    val_ty: ResolvedTy,
    record_layouts: Vec<RecordLayout>,
    module_name: &str,
) -> String {
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashmap_layout_probe".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "__hew_codegen_emit_hashmap_layout_probe",
            ),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: None,
            next: 1,
        },
    };
    emit_ll(
        base_pipeline(block, vec![key_ty, val_ty], record_layouts),
        module_name,
    )
}

fn emit_hashset_probe_ll(
    elem_ty: ResolvedTy,
    record_layouts: Vec<RecordLayout>,
    module_name: &str,
) -> String {
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashset_layout_probe".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "__hew_codegen_emit_hashset_layout_probe",
            ),
            args: vec![Place::Local(0)],
            dest: None,
            next: 1,
        },
    };
    emit_ll(
        base_pipeline(block, vec![elem_ty], record_layouts),
        module_name,
    )
}

fn point_layout() -> RecordLayout {
    // Two i64 fields => size 16, align 8 on every supported host.
    RecordLayout {
        name: "Point".to_string(),
        field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
        field_names: vec![],
    }
}

// ===========================================================================
// Key layout global synthesis
// ===========================================================================

#[test]
fn hashmap_layout_key_global_synthesizes_descriptor() {
    let ll = emit_hashmap_probe_ll(
        named("Point"),
        ResolvedTy::I64,
        vec![point_layout()],
        "key_global",
    );
    // size=16, align=8 for two-i64 Point.  The trailing struct-key segment
    // is the sanitised LLVM textual form so we match by prefix only.
    assert!(
        ll.contains("@__hew_map_key_layout_16_8_"),
        "missing @__hew_map_key_layout_16_8_* global in:\n{ll}"
    );
}

#[test]
fn hashmap_layout_global_includes_hash_eq_fn_pointers() {
    let ll = emit_hashmap_probe_ll(
        named("Point"),
        ResolvedTy::I64,
        vec![point_layout()],
        "key_global_thunks",
    );
    // Each thunk family emitted once for Point.
    assert!(
        ll.contains("define internal i64 @__hew_hash_thunk_16_8_"),
        "missing @__hew_hash_thunk_16_8_* definition in:\n{ll}"
    );
    assert!(
        ll.contains("define internal i32 @__hew_eq_thunk_16_8_"),
        "missing @__hew_eq_thunk_16_8_* definition in:\n{ll}"
    );
    // The key-layout global's initializer must mention both thunk symbols.
    let key_layout_line = ll
        .lines()
        .find(|line| line.contains("@__hew_map_key_layout_16_8_") && line.contains("= private"))
        .unwrap_or_else(|| panic!("no key-layout global initializer line in:\n{ll}"));
    assert!(
        key_layout_line.contains("@__hew_hash_thunk_16_8_"),
        "key-layout global must reference hash thunk:\n{key_layout_line}"
    );
    assert!(
        key_layout_line.contains("@__hew_eq_thunk_16_8_"),
        "key-layout global must reference eq thunk:\n{key_layout_line}"
    );
}

#[test]
fn hashmap_layout_primitive_value_uses_runtime_descriptor() {
    let ll = emit_hashmap_probe_ll(
        named("Point"),
        ResolvedTy::I64,
        vec![point_layout()],
        "value_global",
    );
    assert!(
        ll.contains("@hew_layout_val_i64 = external constant i8"),
        "primitive i64 value must use the runtime descriptor extern:\n{ll}"
    );
    assert!(
        !ll.contains("@__hew_map_value_layout_8_8_plain"),
        "primitive i64 value must not synthesize a Plain value descriptor:\n{ll}"
    );
}

#[test]
fn hashmap_layout_record_value_global_synthesizes_descriptor() {
    // Record Point value => size 16, align 8.
    let ll = emit_hashmap_probe_ll(
        named("Point"),
        named("Point"),
        vec![point_layout()],
        "record_value_global",
    );
    assert!(
        ll.contains("@__hew_map_value_layout_16_8_plain"),
        "missing @__hew_map_value_layout_16_8_plain global in:\n{ll}"
    );
}

// ===========================================================================
// Hash thunk shape
// ===========================================================================

#[test]
fn hash_thunk_uses_fnv1a_seed_and_prime() {
    let ll = emit_hashmap_probe_ll(
        named("Point"),
        ResolvedTy::I64,
        vec![point_layout()],
        "fnv_constants",
    );
    // FNV-1a 64 offset basis and prime literally appear in the emitted IR.
    // LLVM prints i64 constants in signed decimal (so the offset basis
    // `0xcbf29ce484222325`, whose high bit is set, prints as a negative
    // number).  Match both signed-decimal and the hex form defensively so
    // a future LLVM tweak doesn't silently break the assertion.
    let offset_basis_signed = format!("{}", 0xcbf29ce484222325_u64 as i64);
    let offset_basis_unsigned = format!("{}", 0xcbf29ce484222325_u64);
    let fnv_prime_dec = format!("{}", 0x100000001b3_u64);
    assert!(
        ll.contains(&offset_basis_signed)
            || ll.contains(&offset_basis_unsigned)
            || ll.contains("0xCBF29CE484222325"),
        "hash thunk must initialise acc with FNV-1a offset basis \
         (0xcbf29ce484222325 / signed {offset_basis_signed}):\n{ll}"
    );
    assert!(
        ll.contains(&fnv_prime_dec) || ll.contains("0x100000001B3"),
        "hash thunk must mix with FNV-1a prime (0x100000001b3):\n{ll}"
    );
    // The mix step is xor + mul: both must appear inside a hash thunk.
    let hash_thunk_region = ll
        .split("define internal i64 @__hew_hash_thunk_16_8_")
        .nth(1)
        .unwrap_or_else(|| panic!("no hash thunk definition in:\n{ll}"));
    let body = hash_thunk_region
        .split("\ndefine ")
        .next()
        .unwrap_or(hash_thunk_region);
    assert!(
        body.contains("xor i64"),
        "hash thunk body must contain `xor i64` for FNV-1a mix:\n{body}"
    );
    assert!(
        body.contains("mul i64"),
        "hash thunk body must contain `mul i64` for FNV-1a mix:\n{body}"
    );
}

#[test]
fn hash_thunk_walks_fields_in_declaration_order() {
    // Deliberately non-alphabetical field names {y, z, x} so any
    // alphabetical-sort regression would re-order GEPs to {x, y, z}.
    // Hew authority for field order is RecordLayout.field_tys positional
    // index, which maps 1:1 to the LLVM struct field index used by
    // build_struct_gep.
    let layout = RecordLayout {
        name: "Pt3".to_string(),
        field_tys: vec![ResolvedTy::I32, ResolvedTy::I64, ResolvedTy::I32],
        field_names: vec![],
    };
    let ll = emit_hashmap_probe_ll(named("Pt3"), ResolvedTy::I64, vec![layout], "field_order");
    let body = ll
        .split("define internal i64 @__hew_hash_thunk_")
        .nth(1)
        .unwrap_or_else(|| panic!("no hash thunk definition in:\n{ll}"));
    let body = body.split("\ndefine ").next().unwrap_or(body);
    // GEPs use the named struct (%Pt3) so the textual form is
    // `getelementptr inbounds %Pt3, ptr %0, i32 0, i32 N`.
    // Find positions of `i32 0, i32 0`, `i32 0, i32 1`, `i32 0, i32 2`.
    let p0 = body
        .find("i32 0, i32 0")
        .unwrap_or_else(|| panic!("missing GEP to field 0 in:\n{body}"));
    let p1 = body
        .find("i32 0, i32 1")
        .unwrap_or_else(|| panic!("missing GEP to field 1 in:\n{body}"));
    let p2 = body
        .find("i32 0, i32 2")
        .unwrap_or_else(|| panic!("missing GEP to field 2 in:\n{body}"));
    assert!(
        p0 < p1 && p1 < p2,
        "hash thunk must walk fields in declaration order \
         (0, 1, 2); got offsets {p0}, {p1}, {p2}:\n{body}"
    );
}

#[test]
fn hash_thunk_loads_typed_field_widths() {
    // {b: bool, c: char}.  Hew bool stores as i8 (boolean byte) and char
    // stores as i32 (Unicode scalar).  The hash thunk must load each at its
    // declared width and zero-extend, never load i64 across them.
    let layout = RecordLayout {
        name: "Mixed".to_string(),
        field_tys: vec![ResolvedTy::Bool, ResolvedTy::Char],
        field_names: vec![],
    };
    let ll = emit_hashmap_probe_ll(
        named("Mixed"),
        ResolvedTy::I64,
        vec![layout],
        "typed_widths",
    );
    let body = ll
        .split("define internal i64 @__hew_hash_thunk_")
        .nth(1)
        .unwrap_or_else(|| panic!("no hash thunk definition in:\n{ll}"));
    let body = body.split("\ndefine ").next().unwrap_or(body);
    assert!(
        body.contains("load i8"),
        "hash thunk for bool field must load i8:\n{body}"
    );
    assert!(
        body.contains("load i32"),
        "hash thunk for char field must load i32:\n{body}"
    );
    // Bool defence-in-depth: the i8 load for the `bool` field must be
    // masked with `and i8 ..., 1` before the FNV-1a mix so a stray
    // non-canonical bool byte (an upstream invariant violation) cannot
    // silently desynchronise hashing from equality.  The `char` field
    // is an i32 and must NOT be masked.
    let bool_mask_line = body
        .lines()
        .find(|line| line.contains("and i8") && line.contains(" 1"))
        .unwrap_or_else(|| panic!("hash thunk must mask bool field with `and i8 ..., 1`:\n{body}"));
    assert!(
        bool_mask_line.contains("hash_bool_mask"),
        "bool-mask SSA name `hash_bool_mask` should appear on the masking \
         instruction (defence-in-depth signpost):\n{bool_mask_line}"
    );
    // Both fields are <64 bits so each must zext to i64 before mixing.
    assert!(
        body.contains("zext i8") && body.contains("zext i32"),
        "hash thunk must zext narrow field loads to i64:\n{body}"
    );
}

#[test]
fn hash_thunk_does_not_hash_padding_bytes() {
    // {x: i32, y: i64} — size 16, align 8, with 4 padding bytes between
    // x@0 and y@8.  A correct field-typed hash must load i32 from offset 0
    // and i64 from offset 8; a broken implementation would load i64 from
    // offset 0 (sweeping padding into the hash) or do whole-struct memhash.
    let layout = RecordLayout {
        name: "Pad".to_string(),
        field_tys: vec![ResolvedTy::I32, ResolvedTy::I64],
        field_names: vec![],
    };
    let ll = emit_hashmap_probe_ll(named("Pad"), ResolvedTy::I64, vec![layout], "padding");
    let body = ll
        .split("define internal i64 @__hew_hash_thunk_")
        .nth(1)
        .unwrap_or_else(|| panic!("no hash thunk definition in:\n{ll}"));
    let body = body.split("\ndefine ").next().unwrap_or(body);
    assert!(
        body.contains("load i32"),
        "padding-sensitive struct must load i32 for the i32 field:\n{body}"
    );
    assert!(
        body.contains("load i64"),
        "padding-sensitive struct must load i64 for the i64 field:\n{body}"
    );
    // No memhash / memcmp-style helpers.
    assert!(
        !body.contains("call i64 @memcpy") && !body.contains("call i32 @memcmp"),
        "hash thunk must not use memcpy/memcmp over the whole struct:\n{body}"
    );
    // Defence-in-depth: float must never appear (checker gate).
    assert!(
        !body.contains("fadd") && !body.contains("fmul"),
        "hash thunk must not contain float ops:\n{body}"
    );
}

// ===========================================================================
// Deduplication
// ===========================================================================

#[test]
fn hash_thunk_dedup_one_per_record_per_module() {
    // Two probe call sites for the same key type must yield exactly one
    // `__hew_hash_thunk_*` and exactly one `__hew_eq_thunk_*` per record.
    let entry = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashmap_layout_probe".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "__hew_codegen_emit_hashmap_layout_probe",
            ),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: None,
            next: 1,
        },
    };
    let mid = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashmap_layout_probe".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "__hew_codegen_emit_hashmap_layout_probe",
            ),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: None,
            next: 2,
        },
    };
    let ret = BasicBlock {
        id: 2,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Return,
    };
    let blocks = vec![entry, mid, ret];
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![named("Point"), ResolvedTy::I64],
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(1),
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(2),
                },
                ElabBlock {
                    id: 2,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ],
            drop_plans: vec![(ExitPath::Return { block: 2 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![point_layout()],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    };
    let ll = emit_ll(pipeline, "hash_dedup");

    let hash_defs = ll
        .lines()
        .filter(|line| line.contains("define internal i64 @__hew_hash_thunk_16_8_"))
        .count();
    assert_eq!(
        hash_defs, 1,
        "expected exactly one __hew_hash_thunk_16_8_* definition, got {hash_defs}:\n{ll}"
    );
    let eq_defs = ll
        .lines()
        .filter(|line| line.contains("define internal i32 @__hew_eq_thunk_16_8_"))
        .count();
    assert_eq!(
        eq_defs, 1,
        "expected exactly one __hew_eq_thunk_16_8_* definition, got {eq_defs}:\n{ll}"
    );
    // And exactly one key-layout global since (record, size, align) match.
    let key_globals = ll
        .lines()
        .filter(|line| line.contains("@__hew_map_key_layout_16_8_") && line.contains("= private"))
        .count();
    assert_eq!(
        key_globals, 1,
        "expected exactly one @__hew_map_key_layout_16_8_*_plain global, got {key_globals}:\n{ll}"
    );
}

#[test]
fn hash_thunk_dedup_no_double_emit_with_vec_contains_eq_thunk() {
    // Both `hew_vec_contains_thunk` (W3.032 path) and the hashmap layout
    // probe synthesise an equality thunk for the same record type.  They
    // must share the dedup table — exactly one `__hew_eq_thunk_*` per
    // (size, align, struct-shape) regardless of which path runs first.
    let vec_ty = vec_of(named("Point"));
    let entry = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "hew_vec_contains_thunk".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "hew_vec_contains_thunk",
            ),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: Some(Place::Local(3)),
            next: 1,
        },
    };
    let mid = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashmap_layout_probe".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "__hew_codegen_emit_hashmap_layout_probe",
            ),
            args: vec![Place::Local(1), Place::Local(2)],
            dest: None,
            next: 2,
        },
    };
    let ret = BasicBlock {
        id: 2,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Return,
    };
    let blocks = vec![entry, mid, ret];
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![vec_ty, named("Point"), ResolvedTy::I64, ResolvedTy::Bool],
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(1),
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(2),
                },
                ElabBlock {
                    id: 2,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ],
            drop_plans: vec![(ExitPath::Return { block: 2 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![point_layout()],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    };
    let ll = emit_ll(pipeline, "shared_eq_dedup");
    let eq_defs = ll
        .lines()
        .filter(|line| line.contains("define internal i32 @__hew_eq_thunk_16_8_"))
        .count();
    assert_eq!(
        eq_defs, 1,
        "Vec::contains and HashMap layout probe must share the eq-thunk \
         dedup table; got {eq_defs} __hew_eq_thunk_16_8_* defs:\n{ll}"
    );
}

// ===========================================================================
// HashSet probe
// ===========================================================================

#[test]
fn hashset_layout_probe_emits_key_global_without_value_global() {
    let ll = emit_hashset_probe_ll(named("Point"), vec![point_layout()], "set_probe");
    assert!(
        ll.contains("@__hew_map_key_layout_16_8_"),
        "HashSet probe must emit key-layout global:\n{ll}"
    );
    // HashSet's zero-size value layout is fabricated inside the runtime
    // constructor (C-1c) — codegen must NOT emit a value-layout global on
    // the HashSet path.  No value global of any shape should appear.
    assert!(
        !ll.contains("@__hew_map_value_layout_"),
        "HashSet probe must NOT emit a value-layout global:\n{ll}"
    );
}

// ===========================================================================
// WASM descriptor/thunk object emission
// ===========================================================================

#[test]
fn hashmap_layout_emit_for_wasm_target_emits_descriptor_object() {
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashmap_layout_probe".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "__hew_codegen_emit_hashmap_layout_probe",
            ),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: None,
            next: 1,
        },
    };
    let pipeline = base_pipeline(
        block,
        vec![named("Point"), ResolvedTy::I64],
        vec![point_layout()],
    );
    let tmp = tempfile::Builder::new()
        .prefix("hew-hashmap-layout-wasm-")
        .tempdir()
        .expect("create out_dir");
    let options = EmitOptions {
        module_name: "wasm_hashmap_descriptor",
        out_dir: tmp.path(),
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module_objects(&pipeline, &options)
        .expect("layout HashMap descriptor path must emit a wasm object");
    let wasm_obj = artefacts
        .wasm_obj_path
        .expect("wasm object path must be recorded");
    assert!(
        wasm_obj.exists(),
        "wasm object must be written for the layout HashMap descriptor path"
    );
    assert!(
        artefacts.wasm_path.is_none(),
        "emit_module_objects must not freestanding-link the wasm object"
    );
    let ll = std::fs::read_to_string(
        artefacts
            .ll_path
            .expect("textual IR path must be recorded for wasm descriptor emission"),
    )
    .expect("read emitted .ll");
    assert!(
        ll.contains("@__hew_map_key_layout_16_8_")
            && ll.contains("@__hew_hash_thunk_16_8_")
            && ll.contains("@__hew_eq_thunk_16_8_"),
        "wasm descriptor emission must retain key-layout global and hash/eq thunks:\n{ll}"
    );
}

#[test]
fn hashset_layout_emit_for_wasm_target_emits_descriptor_object() {
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashset_layout_probe".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "__hew_codegen_emit_hashset_layout_probe",
            ),
            args: vec![Place::Local(0)],
            dest: None,
            next: 1,
        },
    };
    let pipeline = base_pipeline(block, vec![named("Point")], vec![point_layout()]);
    let tmp = tempfile::Builder::new()
        .prefix("hew-hashset-layout-wasm-")
        .tempdir()
        .expect("create out_dir");
    let options = EmitOptions {
        module_name: "wasm_hashset_descriptor",
        out_dir: tmp.path(),
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module_objects(&pipeline, &options)
        .expect("layout HashSet descriptor path must emit a wasm object");
    let wasm_obj = artefacts
        .wasm_obj_path
        .expect("wasm object path must be recorded");
    assert!(
        wasm_obj.exists(),
        "wasm object must be written for the layout HashSet descriptor path"
    );
    let ll = std::fs::read_to_string(
        artefacts
            .ll_path
            .expect("textual IR path must be recorded for wasm descriptor emission"),
    )
    .expect("read emitted .ll");
    assert!(
        ll.contains("@__hew_map_key_layout_16_8_") && !ll.contains("@__hew_map_value_layout_"),
        "wasm HashSet descriptor emission must retain key-layout global only:\n{ll}"
    );
}

// ===========================================================================
// Probe arity / shape fail-closed
// ===========================================================================

/// Helper: build a one-block, one-terminator pipeline whose entry block ends
/// in the given `Terminator::Call` and whose next block is a trivial return.
fn pipeline_with_probe_call(
    callee: &str,
    args: Vec<Place>,
    dest: Option<Place>,
    locals: Vec<ResolvedTy>,
    record_layouts: Vec<RecordLayout>,
) -> IrPipeline {
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: callee.to_string(),
            // Mirror the producer lift: the walker classifies layout-op
            // sites by their carried typed family.
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(callee),
            args,
            dest,
            next: 1,
        },
    };
    base_pipeline(block, locals, record_layouts)
}

#[test]
fn lower_hashmap_layout_probe_with_wrong_arity_fails_closed() {
    // The HashMap probe declares arity 2 (key, value).  A 3-arg call must
    // be rejected with a structured FailClosed mentioning the callee.
    let pipeline = pipeline_with_probe_call(
        "__hew_codegen_emit_hashmap_layout_probe",
        vec![Place::Local(0), Place::Local(1), Place::Local(2)],
        None,
        vec![named("Point"), ResolvedTy::I64, ResolvedTy::I64],
        vec![point_layout()],
    );
    let tmp = std::env::temp_dir().join("hew-hashmap-layout-arity");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "probe_arity",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    match emit_module(&pipeline, &options) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("__hew_codegen_emit_hashmap_layout_probe")
                    && msg.contains("expected 2 args"),
                "FailClosed message must name probe + arity contract; got: {msg}"
            );
        }
        other => panic!("expected FailClosed for arity mismatch; got: {other:?}"),
    }
}

#[test]
fn lower_hashset_layout_probe_with_wrong_arity_fails_closed() {
    // The HashSet probe declares arity 1 (elem).  A 0-arg call must fail
    // closed.  We exercise 0 args here (rather than ≥2) because the HashSet
    // probe's positive shape is unary, so any non-1 arity is a contract
    // violation; 0 is the smallest provably-wrong case.
    let pipeline = pipeline_with_probe_call(
        "__hew_codegen_emit_hashset_layout_probe",
        vec![],
        None,
        vec![named("Point")],
        vec![point_layout()],
    );
    let tmp = std::env::temp_dir().join("hew-hashset-layout-arity");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "set_probe_arity",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    match emit_module(&pipeline, &options) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("__hew_codegen_emit_hashset_layout_probe")
                    && msg.contains("expected 1 arg"),
                "FailClosed message must name probe + arity contract; got: {msg}"
            );
        }
        other => panic!("expected FailClosed for arity mismatch; got: {other:?}"),
    }
}

#[test]
fn lower_hashmap_layout_probe_with_dest_fails_closed() {
    // Synthesis probes are pure side-effect — they must not carry a dest.
    let pipeline = pipeline_with_probe_call(
        "__hew_codegen_emit_hashmap_layout_probe",
        vec![Place::Local(0), Place::Local(1)],
        Some(Place::Local(2)),
        vec![named("Point"), ResolvedTy::I64, ResolvedTy::I64],
        vec![point_layout()],
    );
    let tmp = std::env::temp_dir().join("hew-hashmap-layout-dest");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "probe_dest",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    match emit_module(&pipeline, &options) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("synthesis probe must not carry a dest"),
                "FailClosed message must name dest contract; got: {msg}"
            );
        }
        other => panic!("expected FailClosed for dest violation; got: {other:?}"),
    }
}

// ===========================================================================
// Cross-shape dedup isolation
// ===========================================================================

#[test]
fn hash_thunk_dedup_isolates_distinct_records_with_same_size_align() {
    // Two records with identical (size, align) but DIFFERENT shapes (and
    // different Hew type names) must emit two distinct hash thunks.  The
    // dedup key embeds the LLVM struct identity via `eq_thunk_struct_key`
    // (sanitised `print_to_string`), which encodes the named struct's
    // identity (`%Point` vs `%Pair`) — so even when (size, align) collide
    // the two thunks remain separate.
    //
    // `Point { i64, i64 }` and `Pair { i64, i64 }` are the canonical
    // collision pair: both are 16 bytes, 8-aligned, two i64 fields, but
    // the LLVM types `%Point` and `%Pair` are distinct named structs.
    let pair_layout = RecordLayout {
        name: "Pair".to_string(),
        field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
        field_names: vec![],
    };
    let entry = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashmap_layout_probe".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "__hew_codegen_emit_hashmap_layout_probe",
            ),
            args: vec![Place::Local(0), Place::Local(2)],
            dest: None,
            next: 1,
        },
    };
    let mid = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashmap_layout_probe".to_string(),
            builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                "__hew_codegen_emit_hashmap_layout_probe",
            ),
            args: vec![Place::Local(1), Place::Local(2)],
            dest: None,
            next: 2,
        },
    };
    let ret = BasicBlock {
        id: 2,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Return,
    };
    let blocks = vec![entry, mid, ret];
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![named("Point"), named("Pair"), ResolvedTy::I64],
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(1),
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(2),
                },
                ElabBlock {
                    id: 2,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ],
            drop_plans: vec![(ExitPath::Return { block: 2 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![point_layout(), pair_layout],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    };
    let ll = emit_ll(pipeline, "cross_shape_dedup");
    let hash_defs = ll
        .lines()
        .filter(|line| line.contains("define internal i64 @__hew_hash_thunk_16_8_"))
        .count();
    assert_eq!(
        hash_defs, 2,
        "Point and Pair share (size=16, align=8) but are distinct LLVM \
         named structs; expected two __hew_hash_thunk_16_8_* definitions, \
         got {hash_defs}:\n{ll}"
    );
    let eq_defs = ll
        .lines()
        .filter(|line| line.contains("define internal i32 @__hew_eq_thunk_16_8_"))
        .count();
    assert_eq!(
        eq_defs, 2,
        "Point and Pair must also have distinct eq thunks (parity with \
         hash dedup); got {eq_defs}:\n{ll}"
    );
    let key_globals = ll
        .lines()
        .filter(|line| line.contains("@__hew_map_key_layout_16_8_") && line.contains("= private"))
        .count();
    assert_eq!(
        key_globals, 2,
        "expected two __hew_map_key_layout_16_8_*_plain globals (one per \
         record), got {key_globals}:\n{ll}"
    );
}

// Silence unused-import warning when only some helpers are referenced after
// future test additions.
#[allow(dead_code)]
const _UNUSED_INSTR: Option<Instr> = None;
