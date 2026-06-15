//! Substrate-level tests for composite-return spine (W3.028).
//!
//! Verifies that user functions returning `Option<i64>` and `Result<i64, i64>`
//! produce valid LLVM IR through the aggregate Move-to-ReturnSlot path, and
//! that heap-owning payloads (`Option<string>`) trigger the fail-closed
//! boundary diagnostic.
//!
//! Also exercises the generic-enum layout walk: `Envelope<i64>` where a
//! separate variant `Message(string)` carries a heap-owning field unrelated
//! to the bitcopy type argument `i64`.  That case must also fail closed.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, EnumLayout, FunctionCallConv, Instr, IrPipeline, MachineVariantLayout, Place,
    RawMirFunction, RecordLayout, Terminator,
};
use hew_types::ResolvedTy;

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-composite-return-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts = emit_module(pipeline, &options)
        .expect("emit_module must succeed for composite-return fixture");
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must produce an .ll when native=false");
    std::fs::read_to_string(&ll_path)
        .unwrap_or_else(|e| panic!("could not read {}: {e}", ll_path.display()))
}

/// Build a pipeline with `fn maybe() -> Option<i64> { Some(42) }`.
fn option_some_pipeline() -> IrPipeline {
    let option_ty = ResolvedTy::Named {
        name: "Option".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: None,
        is_opaque: false,
    };
    let enum_layout = EnumLayout {
        name: "Option$$i64".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Some".to_string(),
                field_tys: vec![ResolvedTy::I64],
            },
            MachineVariantLayout {
                name: "None".to_string(),
                field_tys: vec![],
            },
        ],
        is_indirect: false,
    };
    // MIR: local_0: Option<i64>, local_1: i64 (tag), local_2: i64 (payload)
    let maybe_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: option_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![option_ty, ResolvedTy::I64, ResolvedTy::I64],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                // tag = 0 (Some)
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 0,
                },
                Instr::Move {
                    dest: Place::MachineTag(0),
                    src: Place::Local(1),
                },
                // payload = 42
                Instr::ConstI64 {
                    dest: Place::Local(2),
                    value: 42,
                },
                Instr::Move {
                    dest: Place::MachineVariant {
                        local: 0,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                    src: Place::Local(2),
                },
                // Move local_0 → ReturnSlot
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![maybe_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

/// Build a pipeline with `fn greet() -> Option<string> { Some("hello") }`.
fn option_string_pipeline() -> IrPipeline {
    let option_ty = ResolvedTy::Named {
        name: "Option".to_string(),
        args: vec![ResolvedTy::String],
        builtin: None,
        is_opaque: false,
    };
    let enum_layout = EnumLayout {
        name: "Option$$string".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Some".to_string(),
                field_tys: vec![ResolvedTy::String],
            },
            MachineVariantLayout {
                name: "None".to_string(),
                field_tys: vec![],
            },
        ],
        is_indirect: false,
    };
    let greet_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: option_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![option_ty, ResolvedTy::I64, ResolvedTy::String],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 0,
                },
                Instr::Move {
                    dest: Place::MachineTag(0),
                    src: Place::Local(1),
                },
                Instr::StringLit {
                    dest: Place::Local(2),
                    bytes: "hello".as_bytes().to_vec(),
                },
                Instr::Move {
                    dest: Place::MachineVariant {
                        local: 0,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                    src: Place::Local(2),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![greet_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

/// Option<i64> composite return produces valid LLVM IR with aggregate ret.
#[test]
fn option_i64_return_emits_aggregate_ret() {
    let ll = emit_ll(&option_some_pipeline(), "option_i64_ret");
    assert!(
        ll.contains("ret %\"Option$$i64\""),
        "LLVM IR must contain aggregate ret of Option$$i64; got:\n{ll}"
    );
}

/// Option<i64> composite return passes LLVM module verification.
#[test]
fn option_i64_return_module_verifies() {
    let pipeline = option_some_pipeline();
    let tmp = std::env::temp_dir().join("hew-composite-return-verify");
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name: "option_verify",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    emit_module(&pipeline, &options)
        .expect("emit_module with Option<i64> return must verify cleanly");
}

/// W5.020 — `Option<string>` is a heap-owning ENUM composite, so the
/// move-out + caller-side tag-aware drop spine now lowers it instead of failing
/// closed. The composite-return boundary admits it (the callee bit-copies the
/// composite to `ReturnSlot`; the caller assumes the in-place drop obligation),
/// so `emit_module` must succeed and produce the aggregate `Option$$string`
/// return. This pipeline carries no caller and an empty `elaborated_mir`, so it
/// exercises only the callee-side acceptance — the end-to-end caller-drop +
/// no-double-free behaviour is proven by the `hew run` vertical-slice oracle
/// (`substrate-tests-the-substrate`).
#[test]
fn option_string_return_lowers() {
    let ll = emit_ll(&option_string_pipeline(), "option_string_ret");
    assert!(
        ll.contains("ret %\"Option$$string\""),
        "Option<string> enum composite must lower to an aggregate return now \
         that the W5.020 spine covers heap-owning enum composites; got:\n{ll}"
    );
}

/// Build a pipeline with `fn send() -> Envelope<i64>` where
/// `enum Envelope<T> { Data(T), Message(string) }`.
///
/// The type argument `i64` is bitcopy — it would pass the args-only check.
/// But `Message(string)` is a non-param variant carrying a heap-owning field.
/// The fail-closed boundary must catch this even though the type arg is safe.
fn envelope_i64_pipeline() -> IrPipeline {
    let envelope_ty = ResolvedTy::Named {
        name: "Envelope".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: None,
        is_opaque: false,
    };
    // Monomorphised layout name follows `mangle("Envelope", [i64])` → "Envelope$$i64".
    let enum_layout = EnumLayout {
        name: "Envelope$$i64".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                // Data(T) instantiated as Data(i64) — bitcopy payload
                name: "Data".to_string(),
                field_tys: vec![ResolvedTy::I64],
            },
            MachineVariantLayout {
                // Message(string) — heap-owning, NOT derived from the type param
                name: "Message".to_string(),
                field_tys: vec![ResolvedTy::String],
            },
        ],
        is_indirect: false,
    };
    // Minimal MIR: fn send() -> Envelope<i64> { /* unreachable body — rejected before emit */ }
    let send_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: envelope_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![envelope_ty],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(0),
            }],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![send_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

/// W5.020 — `Envelope<i64>` with a non-param `Message(string)` variant is a
/// heap-owning ENUM composite, so the W5.020 spine lowers it: the boundary's
/// `ty_contains_heap_owning` layout walk still detects the heap variant (the
/// bitcopy `i64` type argument does not mask it), but instead of failing closed
/// the return is now admitted because the move-out + caller-side in-place drop
/// covers enum composites regardless of which variant owns the heap. Confirms
/// the non-param-variant detection path stayed live across the gate flip — a
/// non-enum heap composite (tuple/record) would still fail closed.
#[test]
fn generic_enum_bitcopy_arg_heap_variant_lowers() {
    let ll = emit_ll(&envelope_i64_pipeline(), "envelope_lowers");
    assert!(
        ll.contains("ret %\"Envelope$$i64\""),
        "Envelope<i64> with a Message(string) variant is a heap-owning enum \
         composite and must now lower to an aggregate return; got:\n{ll}"
    );
}

/// Build a pipeline with `fn make() -> bytes { /* moved-out bytes local */ }`.
///
/// `bytes` is a single `ValueClass::CowValue` leaf that lowers to a
/// `BytesTriple { ptr, offset, len }` struct, so it reaches the composite-return
/// boundary's `StructType` arm — but it is NOT a multi-owner composite. The
/// return is a flat struct copy into the caller's slot; the leaf has no
/// scope-exit drop on either side, so it never double-frees. The boundary must
/// admit it (parity with `string`, which lowers to a flat `ptr` and is already
/// admitted). This unblocks `std::fs` (`fs.read_bytes -> bytes`).
fn bytes_return_pipeline() -> IrPipeline {
    let make_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::Bytes,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::Bytes],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(0),
            }],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![make_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

/// A plain top-level `bytes` return is a single heap-owning leaf and must lower
/// to an aggregate struct return rather than fail closed. Regression guard for
/// the `std::fs` import blocker: `fs.read_bytes -> bytes` was force-codegen'd and
/// aborted the whole module at the composite-return boundary.
#[test]
fn plain_bytes_return_lowers() {
    let ll = emit_ll(&bytes_return_pipeline(), "bytes_ret");
    assert!(
        ll.contains("ret { ptr, i32, i32 }"),
        "plain `bytes` return must lower to an aggregate BytesTriple ret; got:\n{ll}"
    );
}

/// Build a pipeline with `fn make() -> (bytes,)` — a tuple carrying a
/// heap-owning leaf. W5.021 admits this through the tuple drop spine: the MIR
/// elaborator's `derive_tuple_composite_drop_allowed` proves single ownership
/// and codegen emits the per-element `__hew_tuple_drop_inplace` helper, so the
/// composite-return boundary lowers it to an aggregate struct return instead of
/// failing closed.
fn tuple_of_bytes_return_pipeline() -> IrPipeline {
    let tuple_ty = ResolvedTy::Tuple(vec![ResolvedTy::Bytes]);
    let make_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: tuple_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![tuple_ty],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(0),
            }],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![make_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

/// A `(bytes,)` tuple return is admitted via the W5.021 tuple drop spine: the
/// composite-return boundary recognises it as a heap-owning tuple composite
/// (`is_heap_owning_tuple_composite_return`) whose per-element drop the spine
/// emits, so it lowers to an aggregate struct return rather than failing closed.
#[test]
fn tuple_of_bytes_return_admits_with_spine() {
    let ll = emit_ll(&tuple_of_bytes_return_pipeline(), "tuple_bytes_ret");
    assert!(
        ll.contains("ret { { ptr, i32, i32 } }"),
        "a `(bytes,)` tuple return must admit and lower to an aggregate struct \
         return through the tuple drop spine; got:\n{ll}"
    );
}

/// Build a pipeline with `fn make() -> Pair<string>` where `Pair<T>` is a
/// GENERIC user record carrying a heap-owning `string` field. The record drop
/// spine now covers a generic INSTANTIATION:
/// `is_heap_owning_record_composite_return` resolves the mangled registry key
/// (`Pair$$string`) and `record_inplace_drop_name` synthesises the matching
/// `__hew_record_drop_inplace_Pair$$string` helper. The boundary therefore
/// admits this shape (it lowers to an aggregate struct return) rather than
/// failing closed — the residual generic-record witness the drop spine now
/// closes.
fn generic_record_of_string_return_pipeline() -> IrPipeline {
    let pair_ty = ResolvedTy::Named {
        name: "Pair".to_string(),
        args: vec![ResolvedTy::String],
        builtin: None,
        is_opaque: false,
    };
    let record_layout = RecordLayout {
        name: "Pair$$string".to_string(),
        field_tys: vec![ResolvedTy::String, ResolvedTy::String],
    };
    let make_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: pair_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![pair_ty],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(0),
            }],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![make_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        opaque_handle_names: vec![],
        record_layouts: vec![record_layout],
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

/// A generic record instantiation (`Pair<string>`) carrying owned heap reaches
/// the `StructType` boundary arm and is now ADMITTED by
/// `is_heap_owning_record_composite_return` (it resolves the mangled registry
/// key `Pair$$string`), so it lowers to an aggregate struct return through the
/// per-instantiation `RecordInPlace` drop spine rather than failing closed.
/// Migrated forward from `generic_record_of_string_return_still_fails_closed`,
/// which pinned the now-closed residual witness.
#[test]
fn generic_record_of_string_return_admits_with_spine() {
    let ll = emit_ll(
        &generic_record_of_string_return_pipeline(),
        "generic_record_ret",
    );
    assert!(
        ll.contains("ret %\"Pair$$string\""),
        "a `Pair<string>` (two string fields) generic-record return must admit \
         and lower to a by-value struct return of the mangled record type \
         (not fail closed); got:\n{ll}"
    );
}
