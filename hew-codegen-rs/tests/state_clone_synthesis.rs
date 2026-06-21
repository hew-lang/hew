//! W2.002 Stage 3 — per-actor `__hew_state_clone_<Actor>` /
//! `__hew_state_drop_<Actor>` body-synthesis coverage.
//!
//! Each test builds a hand-crafted `IrPipeline` whose `ActorLayout`
//! carries the `state_clone_fn_symbol` / `state_drop_fn_symbol` /
//! `state_field_clone_kinds` triple as Stage 1 would have produced, then
//! emits the module and inspects the resulting LLVM IR to verify:
//!
//! - The synthesised function symbols exist with the expected linkage.
//! - The body shape matches plan §6 Stage 3 (null guard → wholesale memcpy
//!   → per-field clone with rollback chain → success / null return).
//! - Per-field clone helpers (`hew_string_clone`, `hew_vec_clone_managed`,
//!   `hew_bytes_clone_ref`, `hew_hashmap_clone_layout`, `hew_hashset_clone_layout`,
//!   per-record `__hew_record_clone_inplace_*`) are declared and called.
//! - Per-field drop helpers (`hew_string_drop`, `hew_vec_free_managed`,
//!   `hew_bytes_drop`, HashMap/HashSet free helpers,
//!   `__hew_record_drop_inplace_*`) are declared and called. Collection
//!   clone/drop symbols are derived from the single `collection_layout_witness`
//!   (W5.001/W5.002): Vec routes to the `*_managed` pair, HashMap/HashSet to
//!   the `*_layout` family — matching the constructor ABI
//!   (`codegen-abi-authority` P0, `lifecycle-symmetry` P0).
//! - Reverse-order LIFO drop discipline in the drop fn (CLAUDE.md custom #1).
//! - Per-field-alloc-fail rollback chain has the correct cardinality
//!   (one rollback BB per non-trivial field index).
//! - Connection-bearing actor's clone fn returns null up front (plan §4.5 B
//!   defence-in-depth — Stage 2 codegen-time gate is the primary surface).
//!
//! LESSONS: boundary-fail-closed (P0) — every synthesised body is a
//! fail-closed seam against the runtime restart-with-state contract.

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    ActorLayout, BasicBlock, FunctionCallConv, IoHandleKind, IrPipeline, RawMirFunction,
    RecordLayout, StateFieldCloneKind, Terminator,
};
use hew_types::ResolvedTy;

// ─── Pipeline builders ──────────────────────────────────────────────────

fn trivial_main() -> RawMirFunction {
    RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64],
        local_names: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                hew_mir::Instr::ConstI64 {
                    dest: hew_mir::Place::Local(0),
                    value: 0,
                },
                hew_mir::Instr::Move {
                    dest: hew_mir::Place::ReturnSlot,
                    src: hew_mir::Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }
}

/// Construct a minimal `ActorLayout` carrying the Stage 1 clone/drop
/// classification triple.
fn classified_actor(
    name: &str,
    state_field_names: Vec<&str>,
    state_field_tys: Vec<ResolvedTy>,
    kinds: Vec<StateFieldCloneKind>,
) -> ActorLayout {
    assert_eq!(state_field_tys.len(), kinds.len());
    ActorLayout {
        name: name.to_string(),
        defining_module: None,
        state_field_names: state_field_names.into_iter().map(String::from).collect(),
        state_field_tys,
        state_field_defaults: vec![],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbols: vec![],
        on_crash_symbol: None,
        max_heap_bytes: None,
        cycle_capable: false,
        handlers: vec![],
        state_clone_fn_symbol: Some(format!("__hew_state_clone_{name}")),
        state_drop_fn_symbol: Some(format!("__hew_state_drop_{name}")),
        state_field_clone_kinds: Some(kinds),
    }
}

fn pipeline_with(actors: Vec<ActorLayout>, records: Vec<RecordLayout>) -> IrPipeline {
    IrPipeline {
        thir: vec![],
        raw_mir: vec![trivial_main()],
        checked_mir: vec![],
        elaborated_mir: vec![],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: records,
        actor_layouts: actors,
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

fn try_emit_to_string(pipeline: &IrPipeline, slug: &str) -> Result<String, CodegenError> {
    let tmp = std::env::temp_dir().join(format!("hew-state-clone-synthesis-{slug}"));
    // Recreate to avoid stale-file confusion between runs.
    let _ = std::fs::remove_dir_all(&tmp);
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    emit_module(pipeline, &options)?;
    let ll = tmp.join("probe.ll");
    std::fs::read_to_string(&ll).map_err(CodegenError::Io)
}

fn emit_to_string(pipeline: &IrPipeline, slug: &str) -> String {
    try_emit_to_string(pipeline, slug).expect("emit_module should succeed")
}

// ─── Tests ──────────────────────────────────────────────────────────────

/// Counter actor: single `i64` field — entirely BitCopy. Clone body is
/// just wholesale memcpy + return; drop body is a no-op (fields-only
/// contract — the runtime frees the wrapper).
#[test]
fn state_clone_counter_bitcopy_only_emits_wholesale_memcpy() {
    let counter = classified_actor(
        "Counter",
        vec!["n"],
        vec![ResolvedTy::I64],
        vec![StateFieldCloneKind::BitCopy { size_bytes: 8 }],
    );
    let record = RecordLayout {
        name: "Counter".into(),
        field_tys: vec![ResolvedTy::I64],
    };
    let ir = emit_to_string(
        &pipeline_with(vec![counter], vec![record]),
        "counter-bitcopy",
    );

    assert!(
        ir.contains("define ptr @__hew_state_clone_Counter("),
        "expected synthesized clone fn for Counter; IR:\n{ir}"
    );
    assert!(
        ir.contains("define void @__hew_state_drop_Counter("),
        "expected synthesized drop fn for Counter; IR:\n{ir}"
    );
    assert!(
        ir.contains("@malloc("),
        "expected malloc call for wrapper allocation; IR:\n{ir}"
    );
    assert!(
        ir.contains("llvm.memcpy"),
        "expected wholesale memcpy in clone body; IR:\n{ir}"
    );
    // Fields-only drop contract: the wrapper allocation is freed by the
    // runtime consumers (actor free / supervisor spec drop) AFTER the
    // callback returns. A `free` inside the drop body double-frees on
    // `supervisor_stop` of arg-initialized stateful children.
    let drop_start = ir
        .find("define void @__hew_state_drop_Counter(")
        .expect("drop fn must exist");
    let drop_end = ir[drop_start..]
        .find("\n}")
        .map(|p| drop_start + p)
        .unwrap_or(ir.len());
    let drop_body = &ir[drop_start..drop_end];
    assert!(
        !drop_body.contains("@free("),
        "drop body must NOT free the wrapper (runtime owns it); drop body:\n{drop_body}"
    );
    // No per-field clone helpers should appear for a BitCopy-only actor.
    // We don't assert absence of `@hew_string_clone` because other parts
    // of the codegen pipeline (string literals in `main`, stdlib-catalog
    // predeclares) may introduce the symbol; the load-bearing check is
    // that the Counter clone body itself uses no per-field helpers,
    // which we verify by inspecting just the body's brace-delimited range.
    let counter_clone_start = ir
        .find("define ptr @__hew_state_clone_Counter(")
        .expect("clone fn must exist");
    let counter_clone_end = ir[counter_clone_start..]
        .find("\n}")
        .map(|p| counter_clone_start + p)
        .unwrap_or(ir.len());
    let body = &ir[counter_clone_start..counter_clone_end];
    assert!(
        !body.contains("@hew_string_clone"),
        "BitCopy-only Counter clone body must call no per-field helpers; body:\n{body}"
    );
    assert!(
        !body.contains("@hew_vec_clone"),
        "BitCopy-only Counter clone body must call no per-field helpers; body:\n{body}"
    );
}

/// ChatRoom-shape actor: `string` + `Vec<i32>` — exercises String clone
/// + Vec clone with rollback. Drop body must drop in reverse order
///   (Vec first, then string).
#[test]
fn state_clone_chatroom_string_and_vec_clone_with_rollback() {
    let chat = classified_actor(
        "ChatRoom",
        vec!["topic", "members"],
        vec![
            ResolvedTy::String,
            ResolvedTy::Named {
                name: "Vec".into(),
                args: vec![ResolvedTy::I32],
                builtin: None,
                is_opaque: false,
            },
        ],
        vec![
            StateFieldCloneKind::String,
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 4 }),
            },
        ],
    );
    let record = RecordLayout {
        name: "ChatRoom".into(),
        field_tys: vec![
            ResolvedTy::String,
            ResolvedTy::Named {
                name: "Vec".into(),
                args: vec![ResolvedTy::I32],
                builtin: None,
                is_opaque: false,
            },
        ],
    };
    let ir = emit_to_string(
        &pipeline_with(vec![chat], vec![record]),
        "chatroom-string-vec",
    );

    assert!(ir.contains("define ptr @__hew_state_clone_ChatRoom("));
    assert!(ir.contains("define void @__hew_state_drop_ChatRoom("));
    // Per-field clone helpers — both must be declared and called. The Vec
    // field now routes through the witness-managed pair (W5.002 F0b), not the
    // legacy `hew_vec_clone`/`hew_vec_free`.
    assert!(
        ir.contains("@hew_string_clone"),
        "expected hew_string_clone declaration; IR:\n{ir}"
    );
    assert!(
        ir.contains("@hew_vec_clone_managed"),
        "expected hew_vec_clone_managed declaration; IR:\n{ir}"
    );
    // Per-field drop helpers.
    assert!(ir.contains("@hew_string_drop"));
    assert!(ir.contains("@hew_vec_free_managed"));
    // Regression guard: the migrated Vec field must NOT *call* the legacy
    // non-layout pair. (`hew_vec_clone`/`hew_vec_free` may still be *declared*
    // via the stdlib runtime-ABI predeclaration; only call-site routing
    // matters here. The trailing `(` excludes the `_managed` symbols.)
    assert!(
        !ir.contains("call ptr @hew_vec_clone("),
        "Vec state field must use the layout-managed clone, not legacy \
         hew_vec_clone; IR:\n{ir}"
    );
    assert!(
        !ir.contains("call void @hew_vec_free("),
        "Vec state field must use the layout-managed free, not legacy \
         hew_vec_free; IR:\n{ir}"
    );
    // Rollback BB labels: 2 non-trivial fields → 2 rollback BBs.
    assert!(
        ir.contains("rollback_before_step_0"),
        "expected rollback BB for first field; IR:\n{ir}"
    );
    assert!(
        ir.contains("rollback_before_step_1"),
        "expected rollback BB for second field; IR:\n{ir}"
    );
    // Reverse-order drop: in the drop body, field 1 (Vec) must be
    // dropped before field 0 (String). We detect this by ordering of
    // drop helper calls in the drop function only.
    let drop_fn_start = ir
        .find("define void @__hew_state_drop_ChatRoom(")
        .expect("drop fn must exist");
    let drop_fn_end = ir[drop_fn_start..]
        .find("\n}")
        .map(|p| drop_fn_start + p)
        .unwrap_or(ir.len());
    let drop_body = &ir[drop_fn_start..drop_fn_end];
    let vec_pos = drop_body
        .find("call void @hew_vec_free_managed")
        .expect("vec drop call must appear in drop body");
    let string_pos = drop_body
        .find("call void @hew_string_drop")
        .expect("string drop call must appear in drop body");
    assert!(
        vec_pos < string_pos,
        "drop must call hew_vec_free_managed BEFORE hew_string_drop (reverse-order LIFO); \
         vec_pos={vec_pos}, string_pos={string_pos}\nbody:\n{drop_body}"
    );
}

/// Router-shape: three parallel `Vec`s. Stresses the per-field rollback
/// chain cardinality — three non-trivial fields means three rollback
/// BBs, with the third dropping fields 0 and 1 in reverse on failure.
#[test]
fn state_clone_router_three_vecs_full_rollback_chain() {
    let router = classified_actor(
        "Router",
        vec!["clients", "subs", "pending"],
        vec![
            ResolvedTy::Named {
                name: "Vec".into(),
                args: vec![ResolvedTy::I64],
                builtin: None,
                is_opaque: false,
            },
            ResolvedTy::Named {
                name: "Vec".into(),
                args: vec![ResolvedTy::I64],
                builtin: None,
                is_opaque: false,
            },
            ResolvedTy::Named {
                name: "Vec".into(),
                args: vec![ResolvedTy::I64],
                builtin: None,
                is_opaque: false,
            },
        ],
        vec![
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 8 }),
            },
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 8 }),
            },
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 8 }),
            },
        ],
    );
    let record = RecordLayout {
        name: "Router".into(),
        field_tys: router.state_field_tys.clone(),
    };
    let ir = emit_to_string(&pipeline_with(vec![router], vec![record]), "router-3vec");

    // Three non-trivial fields → three rollback BBs labelled 0..2.
    for k in 0..3 {
        assert!(
            ir.contains(&format!("rollback_before_step_{k}")),
            "expected rollback_before_step_{k}; IR:\n{ir}"
        );
    }
    // Three per-field clone BBs.
    for k in 0..3 {
        assert!(
            ir.contains(&format!("field_step_{k}_clone")),
            "expected field_step_{k}_clone; IR:\n{ir}"
        );
    }
}

/// Workspace synthetic fixture: actor whose state contains a UserRecord
/// field carrying its own owned-heap subfields. Exercises recursive
/// per-record `__hew_record_clone_inplace_<R>` synthesis with visited-set
/// termination (plan §4.6).
#[test]
fn state_clone_workspace_nested_user_record_synthesizes_record_helper() {
    let entry = RecordLayout {
        name: "Entry".into(),
        field_tys: vec![
            ResolvedTy::String, // id
            ResolvedTy::Named {
                name: "Vec".into(),
                args: vec![ResolvedTy::I32],
                builtin: None,
                is_opaque: false,
            }, // payload
        ],
    };
    let workspace_record = RecordLayout {
        name: "Workspace".into(),
        field_tys: vec![
            ResolvedTy::Named {
                name: "Entry".into(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            }, // entry
            ResolvedTy::String, // name
        ],
    };
    let workspace_actor = classified_actor(
        "Workspace",
        vec!["entry", "name"],
        vec![
            ResolvedTy::Named {
                name: "Entry".into(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            },
            ResolvedTy::String,
        ],
        vec![
            StateFieldCloneKind::UserRecord {
                name: "Entry".into(),
            },
            StateFieldCloneKind::String,
        ],
    );
    let ir = emit_to_string(
        &pipeline_with(vec![workspace_actor], vec![workspace_record, entry]),
        "workspace-nested-record",
    );

    // Per-record helpers synthesised for the nested Entry.
    assert!(
        ir.contains("define internal i32 @__hew_record_clone_inplace_Entry("),
        "expected synthesised in-place clone helper for nested Entry record; IR:\n{ir}"
    );
    assert!(
        ir.contains("define internal void @__hew_record_drop_inplace_Entry("),
        "expected synthesised in-place drop helper for nested Entry record; IR:\n{ir}"
    );
    // Actor body must call the in-place record helper for field 0.
    assert!(
        ir.contains("call i32 @__hew_record_clone_inplace_Entry("),
        "actor clone must invoke nested record's in-place clone helper; IR:\n{ir}"
    );
    // Entry helper itself must call hew_string_clone + hew_vec_clone_managed
    // for its two non-trivial fields (id: String, payload: Vec<i32>). The Vec
    // field routes through the witness-managed pair (W5.002 F0b).
    let entry_clone_start = ir
        .find("define internal i32 @__hew_record_clone_inplace_Entry(")
        .expect("Entry clone fn must exist");
    let entry_clone_end = ir[entry_clone_start..]
        .find("\n}")
        .map(|p| entry_clone_start + p)
        .unwrap_or(ir.len());
    let entry_body = &ir[entry_clone_start..entry_clone_end];
    assert!(entry_body.contains("@hew_string_clone"));
    assert!(entry_body.contains("@hew_vec_clone_managed"));
}

/// Connection-bearing actor: clone fn must short-circuit to `ret ptr
/// null` up front (plan §4.5 B defence-in-depth; Stage 2 codegen-time
/// gate at supervisor-restart is the primary fail-closed surface).
///
/// Note: `ResolvedTy::Named { name: "Connection", .. }` is not yet
/// supported by `resolve_ty` (D10 gate). For Stage 3 body-synthesis
/// coverage we substitute `LocalPid` as a ptr-typed storage stand-in;
/// the synthesis code only inspects the `StateFieldCloneKind` for
/// helper selection, not the storage `ResolvedTy`. Once D10 lifts
/// `Connection`, the substitution can be removed.
#[test]
fn state_clone_connection_actor_returns_null_up_front() {
    let storage_ty = ResolvedTy::Named {
        name: "LocalPid".into(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    let conn_actor = classified_actor(
        "NetReader",
        vec!["conn"],
        vec![storage_ty.clone()],
        vec![StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Connection,
        }],
    );
    let record = RecordLayout {
        name: "NetReader".into(),
        field_tys: vec![storage_ty],
    };
    let ir = emit_to_string(
        &pipeline_with(vec![conn_actor], vec![record]),
        "netreader-connection",
    );

    let clone_start = ir
        .find("define ptr @__hew_state_clone_NetReader(")
        .expect("clone fn must exist");
    let clone_end = ir[clone_start..]
        .find("\n}")
        .map(|p| clone_start + p)
        .unwrap_or(ir.len());
    let body = &ir[clone_start..clone_end];
    // Body must NOT contain malloc / memcpy / per-field helpers. It must
    // return null directly from entry.
    assert!(
        !body.contains("@malloc"),
        "Connection actor clone must NOT allocate; body:\n{body}"
    );
    assert!(
        !body.contains("llvm.memcpy"),
        "Connection actor clone must NOT memcpy; body:\n{body}"
    );
    assert!(
        body.contains("ret ptr null"),
        "Connection actor clone must return null up front; body:\n{body}"
    );
    // Drop body for Connection actor: Connection field is a no-op at drop,
    // and under the fields-only contract the wrapper is freed by the runtime
    // consumers — so the whole drop body is a guarded no-op.
    let drop_start = ir
        .find("define void @__hew_state_drop_NetReader(")
        .expect("drop fn must exist");
    let drop_end = ir[drop_start..]
        .find("\n}")
        .map(|p| drop_start + p)
        .unwrap_or(ir.len());
    let drop_body = &ir[drop_start..drop_end];
    assert!(
        !drop_body.contains("@free"),
        "Connection actor drop must NOT free the wrapper (runtime owns it); body:\n{drop_body}"
    );
}

/// Per-field-alloc-fail cardinality check: an actor with N=4 non-trivial
/// fields produces exactly 4 rollback BBs, one per field index. Each
/// rollback BB at index k drops fields 0..k-1 in reverse before freeing
/// the wrapper. We verify the cardinality (4 BBs) and inspect the
/// rollback at index 3 specifically to confirm it contains three drop
/// helper calls in reverse order.
#[test]
fn state_clone_alloc_fail_per_field_rollback_cardinality_and_order() {
    // state { a: i32 (bitcopy), b: string, c: Vec<i32>, d: string, e: string }
    // Non-trivial fields at indices 1, 2, 3, 4 → 4 rollback BBs.
    let actor = classified_actor(
        "QuadOwned",
        vec!["a", "b", "c", "d", "e"],
        vec![
            ResolvedTy::I32,
            ResolvedTy::String,
            ResolvedTy::Named {
                name: "Vec".into(),
                args: vec![ResolvedTy::I32],
                builtin: None,
                is_opaque: false,
            },
            ResolvedTy::String,
            ResolvedTy::String,
        ],
        vec![
            StateFieldCloneKind::BitCopy { size_bytes: 4 },
            StateFieldCloneKind::String,
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 4 }),
            },
            StateFieldCloneKind::String,
            StateFieldCloneKind::String,
        ],
    );
    let record = RecordLayout {
        name: "QuadOwned".into(),
        field_tys: actor.state_field_tys.clone(),
    };
    let ir = emit_to_string(&pipeline_with(vec![actor], vec![record]), "quad-owned");

    // Cardinality: rollback_before_step_0 through _3 must exist; _4 must NOT.
    for k in 0..4 {
        assert!(
            ir.contains(&format!("rollback_before_step_{k}:")),
            "expected rollback_before_step_{k}: label; IR:\n{ir}"
        );
    }
    assert!(
        !ir.contains("rollback_before_step_4:"),
        "should not have a 5th rollback BB; IR:\n{ir}"
    );

    // Inspect rollback_before_step_3 (failure at the 4th non-trivial
    // field = the 5th declared field, index 4 in the struct). Must drop
    // already-cloned fields in REVERSE: f3 (string), then f2 (vec),
    // then f1 (string), then free wrapper.
    let rb_start = ir
        .find("rollback_before_step_3:")
        .expect("rollback_before_step_3 must exist");
    // The next BB label terminates this rollback's text. Find the
    // earliest of {next "rollback_before_step_", "field_step_",
    // "success:", "ret_null:"}.
    let rb_end = [
        "\nrollback_before_step_",
        "\nfield_step_",
        "\nsuccess:",
        "\nret_null:",
    ]
    .iter()
    .filter_map(|tag| {
        ir[rb_start + tag.len()..]
            .find(tag)
            .map(|p| rb_start + tag.len() + p)
    })
    .min()
    .unwrap_or(ir.len());
    let rb_body = &ir[rb_start..rb_end];
    // Should contain three drop calls + one free call.
    let drop_call_count =
        rb_body.matches("call void @hew_").count() + rb_body.matches("call void @free").count();
    assert!(
        drop_call_count >= 4,
        "rollback_before_step_3 should make 3 field-drop + 1 free calls (>=4 total); \
         got {drop_call_count} in:\n{rb_body}"
    );
    // Reverse order: the FIRST drop helper call in this rollback body
    // is for the last successfully-cloned field — which is field 3
    // (string). The Vec drop must precede the string drop AT INDEX 1
    // because field 2 is the Vec.
    let first_string_pos = rb_body
        .find("call void @hew_string_drop")
        .expect("string_drop must appear");
    let vec_pos = rb_body
        .find("call void @hew_vec_free")
        .expect("vec_free must appear");
    let free_pos = rb_body
        .find("call void @free(")
        .expect("wrapper free must appear");
    // First call is a string_drop (field 3); vec_free is between two
    // string drops; wrapper free is last.
    assert!(
        first_string_pos < vec_pos,
        "first string drop (f3) must precede vec free (f2); IR:\n{rb_body}"
    );
    assert!(
        vec_pos < free_pos,
        "vec free (f2) must precede wrapper free; IR:\n{rb_body}"
    );
}

/// Bytes refcount-bump is infallible — the clone path for a Bytes-typed
/// field calls `hew_bytes_clone_ref` with void return and falls through
/// without a null check on the call result.
///
/// The storage slot must be the real `ResolvedTy::Bytes` lowering:
/// `BytesTriple { ptr, i32, i32 }`. Pointer-shaped stand-ins are a producer
/// bug and must fail closed before the drop path can GEP field 0.
#[test]
fn state_clone_bytes_refcount_bump_has_no_failure_path() {
    let actor = classified_actor(
        "BytesHolder",
        vec!["payload"],
        vec![ResolvedTy::Bytes],
        vec![StateFieldCloneKind::Bytes],
    );
    let record = RecordLayout {
        name: "BytesHolder".into(),
        field_tys: vec![ResolvedTy::Bytes],
    };
    let ir = emit_to_string(&pipeline_with(vec![actor], vec![record]), "bytes-refcount");

    // hew_bytes_clone_ref is declared with void return — refcount bump.
    assert!(ir.contains("@hew_bytes_clone_ref"));
    assert!(ir.contains("@hew_bytes_drop"));
    // The clone body should still produce a rollback BB for substrate
    // symmetry (cardinality matches the non-trivial field count) — the
    // BB exists and is reachable only via the wrapper-malloc-null edge,
    // not from the bump itself. Inspecting that the bump call has NO
    // null-check is cumbersome at the IR-string level; the substrate
    // verification is that the helper has been declared with void return.
    // We assert the more important invariant: the body does call the
    // bump helper unconditionally (no null guard around its single use).
    let clone_start = ir
        .find("define ptr @__hew_state_clone_BytesHolder(")
        .expect("clone fn must exist");
    let clone_end = ir[clone_start..]
        .find("\n}")
        .map(|p| clone_start + p)
        .unwrap_or(ir.len());
    let body = &ir[clone_start..clone_end];
    assert!(
        body.contains("call void @hew_bytes_clone_ref"),
        "Bytes refcount-bump call must appear with void return type; body:\n{body}"
    );
}

/// A `StateFieldCloneKind::Bytes` classification on a pointer-shaped storage
/// slot is invalid. Bytes drop emits an explicit inner GEP through
/// `BytesTriple { ptr, i32, i32 }`; accepting a raw pointer field would make the
/// drop path free whichever word happens to sit at offset 0.
#[test]
fn state_clone_bytes_pointer_shaped_field_fails_closed() {
    let actor = classified_actor(
        "PointerBackedBytes",
        vec!["payload"],
        vec![ResolvedTy::String],
        vec![StateFieldCloneKind::Bytes],
    );
    let record = RecordLayout {
        name: "PointerBackedBytes".into(),
        field_tys: vec![ResolvedTy::String],
    };

    let err = try_emit_to_string(
        &pipeline_with(vec![actor], vec![record]),
        "bytes-pointer-field-fail-closed",
    )
    .expect_err("pointer-shaped storage must not satisfy StateFieldCloneKind::Bytes");

    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("parent struct field 0"),
                "diagnostic must identify the bytes field index; got: {msg}"
            );
            assert!(
                msg.contains("PointerType") || msg.contains("not a struct"),
                "diagnostic must identify the pointer-shaped field; got: {msg}"
            );
            assert!(
                msg.contains("BytesTriple"),
                "diagnostic must cite the required BytesTriple ABI; got: {msg}"
            );
        }
        other => {
            panic!("expected CodegenError::FailClosed for pointer-shaped bytes slot, got {other:?}")
        }
    }
}

/// HashMap and HashSet — string-element collections route to the layout ABI.
///
/// `HashMap<string, i64>` and `HashSet<string>` classify as
/// `StateFieldCloneKind::HashMap { key: String, val: BitCopy }` /
/// `HashSet { elem: String }`. The constructor lowering routes EVERY
/// `HashMap<K,V>` / `HashSet<T>` handle through
/// `hew_hashmap_new_with_layout` / `hew_hashset_new_with_layout`
/// (returning `HewLayoutHashMap*` / `HewLayoutHashSet*`) regardless of the
/// element kind — there is no `hew_hashmap_new` / `hew_hashset_new` call in
/// codegen. The clone/drop helpers selected by `clone_helper_for_kind` /
/// `drop_helper_for_kind` MUST therefore be the layout-keyed names.
///
/// Emitting the legacy `hew_hashset_free` / `hew_hashset_clone` (or
/// `hew_hashmap_free_impl` / `hew_hashmap_clone_impl`) against a layout handle
/// reinterprets the layout entries as 48-byte `HewMapEntry` records and
/// corrupts the heap (W4.045 / e5ba9c78 P0 UAF; `lifecycle-symmetry` /
/// `codegen-abi-authority`).
#[test]
fn state_clone_hashmap_and_hashset_string_elem_route_to_layout_helpers() {
    let storage_ty = ResolvedTy::Named {
        name: "LocalPid".into(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    let actor = classified_actor(
        "MapAndSetStringElem",
        vec!["by_name", "tags"],
        vec![storage_ty.clone(), storage_ty.clone()],
        vec![
            StateFieldCloneKind::HashMap {
                key: Box::new(StateFieldCloneKind::String),
                val: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 8 }),
            },
            StateFieldCloneKind::HashSet {
                elem: Box::new(StateFieldCloneKind::String),
            },
        ],
    );
    let record = RecordLayout {
        name: "MapAndSetStringElem".into(),
        field_tys: actor.state_field_tys.clone(),
    };
    let ir = emit_to_string(
        &pipeline_with(vec![actor], vec![record]),
        "map-and-set-string-elem",
    );

    // Clone side: layout-keyed clone helpers for both collections.
    assert!(
        ir.contains("@hew_hashmap_clone_layout"),
        "string-key HashMap clone must route to hew_hashmap_clone_layout; IR:\n{ir}"
    );
    assert!(
        ir.contains("@hew_hashset_clone_layout"),
        "string-elem HashSet clone must route to hew_hashset_clone_layout; IR:\n{ir}"
    );
    // Drop side: layout-keyed free helpers for both collections.
    assert!(
        ir.contains("@hew_hashmap_free_layout"),
        "string-key HashMap drop must route to hew_hashmap_free_layout; IR:\n{ir}"
    );
    assert!(
        ir.contains("@hew_hashset_free_layout"),
        "string-elem HashSet drop must route to hew_hashset_free_layout; IR:\n{ir}"
    );
    // The legacy substrate symbols must NOT appear. Use precise matches: the
    // legacy names are substrings of the layout names (`@hew_hashset_free` ⊂
    // `@hew_hashset_free_layout`), so anchor on the call/decl terminators.
    assert!(
        !ir.contains("@hew_hashmap_free_impl"),
        "string-key HashMap drop must NOT regress to legacy _impl free; IR:\n{ir}"
    );
    assert!(
        !ir.contains("@hew_hashmap_clone_impl"),
        "string-key HashMap clone must NOT regress to legacy _impl clone; IR:\n{ir}"
    );
    assert!(
        !ir.contains("@hew_hashset_free\n") && !ir.contains("@hew_hashset_free("),
        "string-elem HashSet drop must NOT regress to legacy hew_hashset_free; IR:\n{ir}"
    );
    assert!(
        !ir.contains("@hew_hashset_clone\n") && !ir.contains("@hew_hashset_clone("),
        "string-elem HashSet clone must NOT regress to legacy hew_hashset_clone; IR:\n{ir}"
    );
}

/// HashMap and HashSet — layout-keyed ABI pairing.
///
/// `HashMap<K, V>` / `HashSet<K>` where `K` is a user record classify
/// as `StateFieldCloneKind::HashMap { key: UserRecord, val: ... }` /
/// `HashSet { elem: UserRecord }`. The constructor lowered by
/// `lower_hashmap_constructor_call` is `hew_hashmap_new_with_layout` /
/// `hew_hashset_new_with_layout`, returning `HewLayoutHashMap*` /
/// `HewLayoutHashSet*`, so the drop helper MUST be the matching
/// `_free_layout` name (`lifecycle-symmetry` P0).
#[test]
fn state_clone_hashmap_and_hashset_layout_route_to_layout_free() {
    let storage_ty = ResolvedTy::Named {
        name: "LocalPid".into(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    // Record `K` is the user-record key/elem; its in-place clone/drop
    // is synthesised by the runtime helper, not invoked from the actor
    // body, so a minimal field-list is enough.
    let key_record = RecordLayout {
        name: "K".into(),
        field_tys: vec![ResolvedTy::I64],
    };
    let actor = classified_actor(
        "MapAndSetLayout",
        vec!["by_record", "record_set"],
        vec![storage_ty.clone(), storage_ty.clone()],
        vec![
            StateFieldCloneKind::HashMap {
                key: Box::new(StateFieldCloneKind::UserRecord { name: "K".into() }),
                val: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 8 }),
            },
            StateFieldCloneKind::HashSet {
                elem: Box::new(StateFieldCloneKind::UserRecord { name: "K".into() }),
            },
        ],
    );
    let actor_record = RecordLayout {
        name: "MapAndSetLayout".into(),
        field_tys: actor.state_field_tys.clone(),
    };
    let ir = emit_to_string(
        &pipeline_with(vec![actor], vec![actor_record, key_record]),
        "map-and-set-layout",
    );

    // Clone side: layout-keyed clone helpers (the clone ABI split landed
    // with the layout-free migration — W4.045 / e5ba9c78).
    assert!(
        ir.contains("@hew_hashmap_clone_layout"),
        "layout-keyed HashMap clone must route to hew_hashmap_clone_layout; IR:\n{ir}"
    );
    assert!(
        ir.contains("@hew_hashset_clone_layout"),
        "layout-keyed HashSet clone must route to hew_hashset_clone_layout; IR:\n{ir}"
    );
    // Drop side: layout-keyed maps/sets MUST route to the layout free
    // helpers and MUST NOT regress to the legacy `_impl` / non-layout
    // names (which would treat a HewLayoutHashMap* as a HewHashMap*).
    assert!(
        ir.contains("@hew_hashmap_free_layout"),
        "layout-keyed HashMap drop must route to hew_hashmap_free_layout; IR:\n{ir}"
    );
    assert!(
        ir.contains("@hew_hashset_free_layout"),
        "layout-keyed HashSet drop must route to hew_hashset_free_layout; IR:\n{ir}"
    );
    assert!(
        !ir.contains("@hew_hashmap_free_impl"),
        "layout-keyed HashMap drop must NOT regress to legacy _impl free; IR:\n{ir}"
    );
    assert!(
        !ir.contains("@hew_hashmap_clone_impl"),
        "layout-keyed HashMap clone must NOT regress to legacy _impl clone; IR:\n{ir}"
    );
    assert!(
        !ir.contains("@hew_hashset_clone\n") && !ir.contains("@hew_hashset_clone("),
        "layout-keyed HashSet clone must NOT regress to legacy hew_hashset_clone; IR:\n{ir}"
    );
}

/// Empty (zero-state) actors still get clone/drop bodies — Stage 2 wires
/// the setters unconditionally per plan §4.2 emission policy. Body is
/// trivial: clone malloc(0) + return; drop free + return.
#[test]
fn state_clone_zero_state_actor_emits_trivial_bodies() {
    let actor = ActorLayout {
        name: "Empty".into(),
        defining_module: None,
        state_field_names: vec![],
        state_field_tys: vec![],
        state_field_defaults: vec![],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbols: vec![],
        on_crash_symbol: None,
        max_heap_bytes: None,
        cycle_capable: false,
        handlers: vec![],
        state_clone_fn_symbol: Some("__hew_state_clone_Empty".into()),
        state_drop_fn_symbol: Some("__hew_state_drop_Empty".into()),
        state_field_clone_kinds: Some(vec![]),
    };
    // Empty state means no RecordLayout entry (lower_hir_module skips
    // pushing one for zero-field actors). The synthesis must tolerate
    // record_layouts.get(actor.name) == None.
    let ir = emit_to_string(&pipeline_with(vec![actor], vec![]), "zero-state");

    assert!(ir.contains("define ptr @__hew_state_clone_Empty("));
    assert!(ir.contains("define void @__hew_state_drop_Empty("));
    assert!(ir.contains("ret ptr"));
    assert!(ir.contains("ret void"));
}

/// W5.006 Slice 3 — actor with an enum state field carrying a heap payload
/// (`enum Maybe { Just(string); Nothing }`). The synthesised
/// `__hew_enum_clone_inplace_Maybe` / `__hew_enum_drop_inplace_Maybe`
/// helpers must tag-dispatch (LLVM `switch`), deep-clone / drop only the
/// active variant's owned string payload (`hew_string_clone` /
/// `hew_string_drop`), and trap fail-closed (208) on an out-of-range tag.
/// The actor clone/drop bodies must route the enum field through those
/// helpers (mirroring the UserRecord arm).
#[test]
fn state_clone_actor_enum_field_tag_dispatches_payload_clone_and_drop() {
    use hew_mir::{EnumLayout, MachineVariantLayout};

    let maybe_ty = ResolvedTy::Named {
        name: "Maybe".into(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    let mailbox = classified_actor(
        "Mailbox",
        vec!["msg"],
        vec![maybe_ty.clone()],
        vec![StateFieldCloneKind::Enum {
            name: "Maybe".into(),
        }],
    );
    let record = RecordLayout {
        name: "Mailbox".into(),
        field_tys: vec![maybe_ty],
    };
    let mut pipeline = pipeline_with(vec![mailbox], vec![record]);
    pipeline.enum_layouts = vec![EnumLayout {
        name: "Maybe".into(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Just".into(),
                field_tys: vec![ResolvedTy::String],
            },
            MachineVariantLayout {
                name: "Nothing".into(),
                field_tys: vec![],
            },
        ],
        is_indirect: false,
    }];

    let ir = emit_to_string(&pipeline, "actor-enum-field");

    // Per-enum helpers are synthesised.
    assert!(
        ir.contains("define internal i32 @__hew_enum_clone_inplace_Maybe("),
        "expected enum clone helper; IR:\n{ir}"
    );
    assert!(
        ir.contains("define internal void @__hew_enum_drop_inplace_Maybe("),
        "expected enum drop helper; IR:\n{ir}"
    );
    // Actor bodies route the enum field through the helpers.
    assert!(
        ir.contains("call i32 @__hew_enum_clone_inplace_Maybe("),
        "actor clone must call the enum clone helper; IR:\n{ir}"
    );
    assert!(
        ir.contains("call void @__hew_enum_drop_inplace_Maybe("),
        "actor drop must call the enum drop helper; IR:\n{ir}"
    );

    // The clone helper body tag-dispatches and deep-clones the string
    // payload; the drop helper drops it. Inspect each helper body range.
    let clone_start = ir
        .find("define internal i32 @__hew_enum_clone_inplace_Maybe(")
        .expect("clone helper exists");
    let clone_end = ir[clone_start..]
        .find("\n}")
        .map(|p| clone_start + p)
        .expect("clone helper terminates");
    let clone_body = &ir[clone_start..clone_end];
    assert!(
        clone_body.contains("switch"),
        "enum clone helper must tag-dispatch via switch; body:\n{clone_body}"
    );
    assert!(
        clone_body.contains("@hew_string_clone"),
        "enum clone helper must deep-clone the string payload; body:\n{clone_body}"
    );
    assert!(
        clone_body.contains("call void @hew_trap_with_code(i32 208)"),
        "enum clone helper must trap (208) on out-of-range tag; body:\n{clone_body}"
    );

    let drop_start = ir
        .find("define internal void @__hew_enum_drop_inplace_Maybe(")
        .expect("drop helper exists");
    let drop_end = ir[drop_start..]
        .find("\n}")
        .map(|p| drop_start + p)
        .expect("drop helper terminates");
    let drop_body = &ir[drop_start..drop_end];
    assert!(
        drop_body.contains("switch"),
        "enum drop helper must tag-dispatch via switch; body:\n{drop_body}"
    );
    assert!(
        drop_body.contains("@hew_string_drop"),
        "enum drop helper must drop the string payload; body:\n{drop_body}"
    );
    assert!(
        drop_body.contains("call void @hew_trap_with_code(i32 208)"),
        "enum drop helper must trap (208) on out-of-range tag; body:\n{drop_body}"
    );
}
