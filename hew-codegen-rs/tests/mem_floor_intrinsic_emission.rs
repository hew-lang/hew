//! Codegen-side coverage for the memory-intrinsic floor (W5.005 / F1b,
//! Decision 4 Option A). A `#[intrinsic("mem.*")]` floor function carries a
//! typed catalog id on `RawMirFunction::intrinsic_id`. Codegen discards the
//! bodyless placeholder MIR blocks and synthesizes the real trampoline body
//! from that id via the central `FloorIntrinsic` authority.
//!
//! These tests pin two contracts that must survive every revision:
//!
//!  1. **D343 (the historic fail-OPEN bug):** a tagged `mem.alloc` function
//!     must emit a real `call ... @hew_alloc` body, NOT a silent empty body
//!     that returns an uninitialised pointer.
//!  2. **Fail-closed:** an `intrinsic_id` codegen does not recognise is a hard
//!     `CodegenError::FailClosed`, never a silent empty-body no-op — the
//!     producer surface must never outrun the codegen surface (LESSONS
//!     `parity-or-tracked-gap`, `boundary-fail-closed`).

use std::path::Path;

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::BlockKind;
use hew_mir::{
    BasicBlock, CheckedMirFunction, ElabBlock, ElaboratedMirFunction, FunctionCallConv, IrPipeline,
    RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

fn mut_u8_ptr() -> ResolvedTy {
    ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::U8),
    }
}

/// Build a single-function `IrPipeline` for a memory-floor intrinsic. The
/// `blocks` are a bodyless placeholder (a lone `Return`) exactly as the HIR
/// floor stub lowers — codegen must IGNORE them and synthesize the body from
/// `intrinsic_id`.
fn floor_pipeline(
    name: &str,
    intrinsic_id: &str,
    params: Vec<ResolvedTy>,
    return_ty: ResolvedTy,
) -> IrPipeline {
    let placeholder = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Return,
    }];
    let locals = params.clone();
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: name.to_string(),
            return_ty: return_ty.clone(),
            call_conv: FunctionCallConv::Default,
            params,
            locals,
            blocks: placeholder.clone(),
            decisions: vec![],
            intrinsic_id: Some(intrinsic_id.to_string()),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: name.to_string(),
            return_ty: return_ty.clone(),
            blocks: placeholder,
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: name.to_string(),
            return_ty,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
    }
}

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-mem-floor-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let artefacts =
        emit_module(pipeline, &options).expect("floor-intrinsic pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// D343 regression: a tagged `mem.alloc` function must emit a real body that
/// calls the runtime allocator `hew_alloc` and returns its result — NOT a
/// silent empty body that hands back an uninitialised pointer.
#[test]
fn mem_alloc_floor_emits_hew_alloc_call() {
    let pipeline = floor_pipeline(
        "mem$alloc",
        "mem.alloc",
        vec![ResolvedTy::U64, ResolvedTy::U64],
        mut_u8_ptr(),
    );
    let ll = emit_ll(&pipeline, "mem_alloc");

    assert!(
        ll.contains("call ptr @hew_alloc(") || ll.contains("call ptr @hew_alloc "),
        "mem.alloc must emit a real `call ptr @hew_alloc(...)` body (D343 \
         fail-OPEN regression);\n--- IR ---\n{ll}"
    );
    assert!(
        ll.contains("@hew_alloc"),
        "the runtime allocator extern must be declared;\n--- IR ---\n{ll}"
    );
}

/// `mem.realloc` must call the 4-arg `hew_realloc` runtime symbol.
#[test]
fn mem_realloc_floor_emits_hew_realloc_call() {
    let pipeline = floor_pipeline(
        "mem$realloc",
        "mem.realloc",
        vec![
            mut_u8_ptr(),
            ResolvedTy::U64,
            ResolvedTy::U64,
            ResolvedTy::U64,
        ],
        mut_u8_ptr(),
    );
    let ll = emit_ll(&pipeline, "mem_realloc");
    assert!(
        ll.contains("call ptr @hew_realloc("),
        "mem.realloc must emit a real `call ptr @hew_realloc(...)` body;\n--- IR ---\n{ll}"
    );
}

/// `mem.dealloc` must call `hew_dealloc` and return void.
#[test]
fn mem_dealloc_floor_emits_hew_dealloc_call() {
    let pipeline = floor_pipeline(
        "mem$dealloc",
        "mem.dealloc",
        vec![mut_u8_ptr(), ResolvedTy::U64, ResolvedTy::U64],
        ResolvedTy::Unit,
    );
    let ll = emit_ll(&pipeline, "mem_dealloc");
    assert!(
        ll.contains("call void @hew_dealloc("),
        "mem.dealloc must emit a real `call void @hew_dealloc(...)` body;\n--- IR ---\n{ll}"
    );
}

/// `mem.ptr_offset` must emit a byte-level (i8) in-bounds GEP — A612 byte
/// monomorphism: the offset is a raw byte count, no element scaling here.
#[test]
fn mem_ptr_offset_floor_emits_i8_gep() {
    let pipeline = floor_pipeline(
        "mem$ptr_offset",
        "mem.ptr_offset",
        vec![mut_u8_ptr(), ResolvedTy::U64],
        mut_u8_ptr(),
    );
    let ll = emit_ll(&pipeline, "mem_ptr_offset");
    assert!(
        ll.contains("getelementptr inbounds i8") || ll.contains("getelementptr inbounds (i8"),
        "mem.ptr_offset must emit a byte-level i8 in-bounds GEP (A612);\n--- IR ---\n{ll}"
    );
}

/// `mem.ptr_copy` must emit a memcpy intrinsic.
#[test]
fn mem_ptr_copy_floor_emits_memcpy() {
    let pipeline = floor_pipeline(
        "mem$ptr_copy",
        "mem.ptr_copy",
        vec![mut_u8_ptr(), mut_u8_ptr(), ResolvedTy::U64],
        ResolvedTy::Unit,
    );
    let ll = emit_ll(&pipeline, "mem_ptr_copy");
    assert!(
        ll.contains("@llvm.memcpy"),
        "mem.ptr_copy must lower to an `@llvm.memcpy` intrinsic;\n--- IR ---\n{ll}"
    );
}

/// Fail-closed: an `intrinsic_id` codegen does not recognise must surface
/// `CodegenError::FailClosed` naming the id — never a silent empty-body
/// no-op (the producer surface must not outrun the codegen surface).
#[test]
fn unknown_floor_intrinsic_id_fails_closed() {
    let pipeline = floor_pipeline(
        "mem$bogus",
        "mem.bogus",
        vec![ResolvedTy::U64],
        mut_u8_ptr(),
    );
    let tmp = std::env::temp_dir().join("hew-mem-floor-unknown");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "mem_unknown",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    match emit_module(&pipeline, &options) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("mem.bogus"),
                "FailClosed message must name the unknown intrinsic id so the \
                 rejection points at the unwired seam; got: {msg}"
            );
        }
        Err(other) => panic!(
            "expected CodegenError::FailClosed for unknown floor intrinsic id; \
             got {other:?}"
        ),
        Ok(_) => panic!(
            "expected codegen to fail closed on an unknown floor intrinsic id; \
             got Ok(_). A tagged function with no recognised lowering must NEVER \
             emit a silent empty body (D343 fail-OPEN regression)"
        ),
    }
}
