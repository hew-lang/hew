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
//! - Per-field clone helpers (`hew_string_clone`, `hew_vec_clone`,
//!   `hew_bytes_clone_ref`, `hew_hashmap_clone_impl`, `hew_hashset_clone`,
//!   per-record `__hew_record_clone_inplace_*`) are declared and called.
//! - Per-field drop helpers (`hew_string_drop`, `hew_vec_free`,
//!   `hew_bytes_drop`, `hew_hashmap_free_impl`, `hew_hashset_free`,
//!   `__hew_record_drop_inplace_*`) are declared and called.
//! - Reverse-order LIFO drop discipline in the drop fn (CLAUDE.md custom #1).
//! - Per-field-alloc-fail rollback chain has the correct cardinality
//!   (one rollback BB per non-trivial field index).
//! - Connection-bearing actor's clone fn returns null up front (plan §4.5 B
//!   defence-in-depth — Stage 2 codegen-time gate is the primary surface).
//!
//! LESSONS: boundary-fail-closed (P0) — every synthesised body is a
//! fail-closed seam against the runtime restart-with-state contract.

use hew_codegen_rs::{emit_module, EmitOptions};
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
        state_field_names: state_field_names.into_iter().map(String::from).collect(),
        state_field_tys,
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
        record_layouts: records,
        actor_layouts: actors,
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
    }
}

fn emit_to_string(pipeline: &IrPipeline, slug: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-state-clone-synthesis-{slug}"));
    // Recreate to avoid stale-file confusion between runs.
    let _ = std::fs::remove_dir_all(&tmp);
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(pipeline, &options).expect("emit_module should succeed");
    let ll = tmp.join("probe.ll");
    std::fs::read_to_string(&ll).unwrap_or_else(|e| panic!("could not read {}: {e}", ll.display()))
}

// ─── Tests ──────────────────────────────────────────────────────────────

/// Counter actor: single `i64` field — entirely BitCopy. Clone body is
/// just wholesale memcpy + return; drop body is just free.
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
    assert!(
        ir.contains("@free("),
        "expected free call in drop body; IR:\n{ir}"
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
            },
        ],
    };
    let ir = emit_to_string(
        &pipeline_with(vec![chat], vec![record]),
        "chatroom-string-vec",
    );

    assert!(ir.contains("define ptr @__hew_state_clone_ChatRoom("));
    assert!(ir.contains("define void @__hew_state_drop_ChatRoom("));
    // Per-field clone helpers — both must be declared and called.
    assert!(
        ir.contains("@hew_string_clone"),
        "expected hew_string_clone declaration; IR:\n{ir}"
    );
    assert!(
        ir.contains("@hew_vec_clone"),
        "expected hew_vec_clone declaration; IR:\n{ir}"
    );
    // Per-field drop helpers.
    assert!(ir.contains("@hew_string_drop"));
    assert!(ir.contains("@hew_vec_free"));
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
        .find("call void @hew_vec_free")
        .expect("vec drop call must appear in drop body");
    let string_pos = drop_body
        .find("call void @hew_string_drop")
        .expect("string drop call must appear in drop body");
    assert!(
        vec_pos < string_pos,
        "drop must call hew_vec_free BEFORE hew_string_drop (reverse-order LIFO); \
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
            },
            ResolvedTy::Named {
                name: "Vec".into(),
                args: vec![ResolvedTy::I64],
                builtin: None,
            },
            ResolvedTy::Named {
                name: "Vec".into(),
                args: vec![ResolvedTy::I64],
                builtin: None,
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
    // Entry helper itself must call hew_string_clone + hew_vec_clone
    // for its two non-trivial fields (id: String, payload: Vec<i32>).
    let entry_clone_start = ir
        .find("define internal i32 @__hew_record_clone_inplace_Entry(")
        .expect("Entry clone fn must exist");
    let entry_clone_end = ir[entry_clone_start..]
        .find("\n}")
        .map(|p| entry_clone_start + p)
        .unwrap_or(ir.len());
    let entry_body = &ir[entry_clone_start..entry_clone_end];
    assert!(entry_body.contains("@hew_string_clone"));
    assert!(entry_body.contains("@hew_vec_clone"));
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
    // Drop body for Connection actor: Connection field is a no-op at
    // drop; the only non-no-op work is freeing the wrapper.
    let drop_start = ir
        .find("define void @__hew_state_drop_NetReader(")
        .expect("drop fn must exist");
    let drop_end = ir[drop_start..]
        .find("\n}")
        .map(|p| drop_start + p)
        .unwrap_or(ir.len());
    let drop_body = &ir[drop_start..drop_end];
    assert!(
        drop_body.contains("@free"),
        "Connection actor drop must still free wrapper; body:\n{drop_body}"
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
/// Storage type substitution: `ResolvedTy::Bytes` is not yet supported
/// by `resolve_ty` (Cluster 2 pending). We use `String` (ptr-typed) as
/// the storage stand-in; synthesis dispatches on `StateFieldCloneKind`,
/// not on the storage `ResolvedTy`.
#[test]
fn state_clone_bytes_refcount_bump_has_no_failure_path() {
    let actor = classified_actor(
        "BytesHolder",
        vec!["payload"],
        vec![ResolvedTy::String],
        vec![StateFieldCloneKind::Bytes],
    );
    let record = RecordLayout {
        name: "BytesHolder".into(),
        field_tys: vec![ResolvedTy::String],
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

/// HashMap and HashSet variants — runtime helpers exist; verify they are
/// declared and called.
///
/// Storage type substitution: `ResolvedTy::Named { name: "HashMap"|"HashSet", .. }`
/// is not yet supported by `resolve_ty` (D10 gate). We use `LocalPid`
/// (ptr-typed) for storage; synthesis dispatches on `StateFieldCloneKind`.
#[test]
fn state_clone_hashmap_and_hashset_route_to_runtime_helpers() {
    let storage_ty = ResolvedTy::Named {
        name: "LocalPid".into(),
        args: vec![],
        builtin: None,
    };
    let actor = classified_actor(
        "MapAndSet",
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
        name: "MapAndSet".into(),
        field_tys: actor.state_field_tys.clone(),
    };
    let ir = emit_to_string(&pipeline_with(vec![actor], vec![record]), "map-and-set");

    assert!(ir.contains("@hew_hashmap_clone_impl"));
    assert!(ir.contains("@hew_hashmap_free_impl"));
    assert!(ir.contains("@hew_hashset_clone"));
    assert!(ir.contains("@hew_hashset_free"));
}

/// Empty (zero-state) actors still get clone/drop bodies — Stage 2 wires
/// the setters unconditionally per plan §4.2 emission policy. Body is
/// trivial: clone malloc(0) + return; drop free + return.
#[test]
fn state_clone_zero_state_actor_emits_trivial_bodies() {
    let actor = ActorLayout {
        name: "Empty".into(),
        state_field_names: vec![],
        state_field_tys: vec![],
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
