//! Positive-emission proof for foreign C-string adoption.
//!
//! A user `extern "C" -> string` classified `malloc_string_return = true` (a raw
//! malloc-owned foreign C string, per its root/user-package provenance) must, at
//! a tracked `Terminator::Call` dest, lower the adoption edge that copies the
//! foreign string into Hew's header-aware refcounted domain and frees the
//! foreign allocation. This test pins the STRUCTURE of that emission: the
//! null-guarded copy via `hew_string_slice_codepoints` and the paired libc
//! `free` of the raw pointer.
//!
//! Complements `scalar_discard_guard.rs`, which pins the fail-closed direction
//! (a discarded owned-string return must never silently leak).
//!
//! LESSONS applied: `boundary-fail-closed` (P0), `lifecycle-symmetry` (P0).

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, ExternDecl, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

/// Build a pipeline whose `probe` fn calls extern `probe_make_owned_str`
/// (`malloc_string_return = true`) into a tracked `string` dest local, then
/// returns unit. The tracked dest is what routes the call through the adoption
/// edge in `Terminator::Call`.
fn pipeline_adopt_extern(malloc_string_return: bool) -> IrPipeline {
    let fn_name = "probe_make_owned_str";
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: fn_name.to_string(),
                builtin: None,
                args: vec![],
                dest: Some(Place::Local(0)), // tracked string dest — adoption edge
                next: 1,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];

    let raw = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "probe".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::String], // Local(0): adopted string
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
    };
    let checked = CheckedMirFunction {
        name: "probe".to_string(),
        return_ty: ResolvedTy::Unit,
        blocks: blocks.clone(),
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
    };
    let elab = ElaboratedMirFunction {
        name: "probe".to_string(),
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
    };

    IrPipeline {
        thir: vec![],
        raw_mir: vec![raw],
        checked_mir: vec![checked],
        elaborated_mir: vec![elab],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: vec![],
        extern_decls: vec![ExternDecl {
            name: fn_name.to_string(),
            abi: "C".to_string(),
            param_tys: vec![],
            return_ty: ResolvedTy::String,
            provenance: hew_hir::ExternProvenance::Root,
            malloc_string_return,
        }],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: vec![],
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-adopt-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let out_dir: &'static std::path::Path = Box::leak(Box::new(tmp)).as_path();
    let module_name_static: &'static str = Box::leak(module_name.to_string().into_boxed_str());
    let options = EmitOptions {
        module_name: module_name_static,
        out_dir,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(pipeline, &options).expect("adoption pipeline must emit");
    let ll_path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// `malloc_string_return = true` at a tracked string dest emits the adoption
/// sequence: a null-guarded copy into Hew's string domain via
/// `hew_string_slice_codepoints`, then a `free` of the raw foreign pointer.
#[test]
fn malloc_string_return_true_emits_adoption_and_free() {
    let pipeline = pipeline_adopt_extern(true);
    let ll = emit_ll(&pipeline, "adopt_true");
    assert!(
        ll.contains("hew_string_slice_codepoints"),
        "adoption must copy the foreign string into Hew's domain via \
         hew_string_slice_codepoints; IR:\n{ll}"
    );
    assert!(
        ll.contains("hew_string_char_count"),
        "adoption must measure the foreign string via hew_string_char_count; IR:\n{ll}"
    );
    assert!(
        ll.contains("extern_string_adopt"),
        "adoption must branch through the null-guarded adopt block; IR:\n{ll}"
    );
    assert!(
        ll.contains("@free"),
        "adoption must free the raw foreign allocation after copying; IR:\n{ll}"
    );
}

/// The SAME pipeline with `malloc_string_return = false` (a header-aware Hew
/// string) must NOT emit the adoption sequence — the returned pointer is stored
/// directly and never freed. This pins that the adoption edge is gated strictly
/// on the classification, so a std/header-aware producer is never double-freed.
#[test]
fn malloc_string_return_false_stores_without_adoption() {
    let pipeline = pipeline_adopt_extern(false);
    let ll = emit_ll(&pipeline, "adopt_false");
    assert!(
        !ll.contains("hew_string_slice_codepoints"),
        "a header-aware string return must NOT be copied via \
         hew_string_slice_codepoints; IR:\n{ll}"
    );
    assert!(
        !ll.contains("extern_string_adopt"),
        "a header-aware string return must NOT branch through the adopt block; IR:\n{ll}"
    );
}
