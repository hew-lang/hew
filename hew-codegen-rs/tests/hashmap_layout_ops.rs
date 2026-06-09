//! W3.003-C C-3b — HashMap/HashSet layout operation lowering boundary
//! tests.
//!
//! This slice extends the C-3a synthesis seam with:
//!   1. The Pending → Finalized lowering-fact lifecycle plumbing on
//!      `IrPipeline`.  Driver glue routes `TypeCheckOutput.*_layout_facts`
//!      onto the pipeline via `IrPipeline::attach_lowering_facts`; codegen
//!      enforces a `Pending` fact has been consumed before module
//!      finalization via `assert_lowering_facts_consistent`.
//!   2. Expansion of the wasm fail-closed substrate-symbol map to cover
//!      every layout-backed `HashMap`/`HashSet` runtime ABI symbol (7 for
//!      HashMap, 6 for HashSet).  Reaching any of these under a wasm32
//!      emission produces a structured `WasmUnsupportedSubstrate` diagnostic
//!      naming the offending symbol before any link attempt.
//!   3. W3.041b-i: 12-arm op-call dispatch + Pending→Finalized fact walk.
//!      `is_hashmap_layout_runtime_symbol` + `lower_hashmap_layout_direct_call`
//!      mirror the Vec pattern.  `finalize_layout_facts_against_pipeline`
//!      drives the Pending→Finalized transition before the consistency gate.

use hew_codegen_rs::{emit_module, validate_codegen_front, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, IrPipeline, Place, RawMirFunction, RecordLayout, Terminator,
};
use hew_types::{
    hashmap_layout_key_fact, hashset_layout_fact, HashMapLoweringFactState, HashMapValueType,
    ResolvedTy,
};

fn point_layout() -> RecordLayout {
    RecordLayout {
        name: "Point".to_string(),
        field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
    }
}

/// Build a minimal pipeline with a single block (entry → return).
/// Caller supplies the entry terminator so per-test callee shapes are
/// independent.
fn pipeline_with_entry_terminator(
    entry_terminator: Terminator,
    locals: Vec<ResolvedTy>,
    record_layouts: Vec<RecordLayout>,
) -> IrPipeline {
    let return_block = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Return,
    };
    let entry_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: entry_terminator,
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
            blocks: blocks.clone(),
            decisions: vec![],
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
        opaque_handle_names: vec![],
        record_layouts,
        actor_layouts: vec![],
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

/// Try to emit `pipeline` as a wasm module; return the resulting error
/// (the wasm fail-closed substrate check fires before any link / IR
/// emission, so this never produces a real `.wasm` artefact).
fn emit_wasm(pipeline: &IrPipeline, module_name: &str) -> CodegenError {
    let tmp = std::env::temp_dir().join(format!("hew-hashmap-ops-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: true,
    };
    emit_module(pipeline, &options).expect_err("wasm emission must fail closed on layout symbols")
}

// ============================================================================
// Wasm fail-closed for every layout-backed runtime ABI symbol (Rev 7)
// ============================================================================

/// One symbol per row → exhaustive list of the 13 operation callees the
/// council Rev 7 wasm fail-closed gate must recognise.
fn all_layout_op_symbols() -> &'static [&'static str] {
    &[
        "hew_hashmap_new_with_layout",
        "hew_hashmap_insert_layout",
        "hew_hashmap_get_layout",
        "hew_hashmap_contains_key_layout",
        "hew_hashmap_remove_layout",
        "hew_hashmap_len_layout",
        "hew_hashmap_free_layout",
        "hew_hashset_new_with_layout",
        "hew_hashset_insert_layout",
        "hew_hashset_contains_layout",
        "hew_hashset_remove_layout",
        "hew_hashset_len_layout",
        "hew_hashset_free_layout",
    ]
}

#[test]
fn wasm_fail_closed_for_every_layout_op_symbol() {
    for symbol in all_layout_op_symbols() {
        let pipeline = pipeline_with_entry_terminator(
            Terminator::Call {
                callee: (*symbol).to_string(),
                args: vec![],
                dest: None,
                next: 1,
            },
            vec![],
            vec![point_layout()],
        );
        let err = emit_wasm(&pipeline, &format!("wasm_fc_{symbol}"));
        match err {
            CodegenError::WasmUnsupportedSubstrate { symbol: reported } => {
                assert_eq!(
                    reported, *symbol,
                    "wasm fail-closed must name the offending substrate symbol exactly"
                );
            }
            other => panic!(
                "wasm emission for `{symbol}` must produce WasmUnsupportedSubstrate, got {other:?}"
            ),
        }
    }
}

#[test]
fn wasm_fail_closed_diagnostic_labels_hashmap_construct() {
    let pipeline = pipeline_with_entry_terminator(
        Terminator::Call {
            callee: "hew_hashmap_insert_layout".to_string(),
            args: vec![],
            dest: None,
            next: 1,
        },
        vec![],
        vec![point_layout()],
    );
    let err = emit_wasm(&pipeline, "wasm_hashmap_label");
    let text = err.to_string();
    assert!(
        text.contains("layout-backed `HashMap`"),
        "wasm diagnostic for hashmap layout symbol should label the construct: {text}"
    );
}

#[test]
fn wasm_fail_closed_diagnostic_labels_hashset_construct() {
    let pipeline = pipeline_with_entry_terminator(
        Terminator::Call {
            callee: "hew_hashset_contains_layout".to_string(),
            args: vec![],
            dest: None,
            next: 1,
        },
        vec![],
        vec![point_layout()],
    );
    let err = emit_wasm(&pipeline, "wasm_hashset_label");
    let text = err.to_string();
    assert!(
        text.contains("layout-backed `HashSet`"),
        "wasm diagnostic for hashset layout symbol should label the construct: {text}"
    );
}

// ============================================================================
// Pending → Finalized lifecycle consistency check (Rev 5)
// ============================================================================

#[test]
fn consistency_check_rejects_pending_fact_at_pipeline_finalize() {
    // Construct a pipeline whose `hashmap_lowering_facts` carries a
    // `Pending` LayoutKey fact (i.e. the checker admitted a layout
    // HashMap site, codegen never consumed it).  Reaching `emit_module`
    // must produce a structured `FailClosed` diagnostic before any IR
    // emission.
    let mut pipeline =
        pipeline_with_entry_terminator(Terminator::Return, vec![], vec![point_layout()]);
    pipeline
        .hashmap_lowering_facts
        .push(hashmap_layout_key_fact(
            "Point".to_string(),
            16,
            8,
            HashMapValueType::I64,
        ));
    let tmp = std::env::temp_dir().join("hew-hashmap-ops-pending-fact");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "pending_fact",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    match emit_module(&pipeline, &options) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("Point"),
                "fail-closed message should name the orphan fact record: {msg}"
            );
            assert!(
                msg.contains("codegen-abi-authority"),
                "fail-closed message should cite the LESSONS row: {msg}"
            );
        }
        other => {
            panic!("Pending LayoutKey fact at pipeline finalize must FailClosed; got {other:?}")
        }
    }
}

#[test]
fn consistency_check_rejects_pending_hashset_fact_at_pipeline_finalize() {
    let mut pipeline =
        pipeline_with_entry_terminator(Terminator::Return, vec![], vec![point_layout()]);
    pipeline
        .hashset_lowering_facts
        .push(hashset_layout_fact("Point".to_string(), 16, 8));
    let tmp = std::env::temp_dir().join("hew-hashmap-ops-pending-set-fact");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "pending_set_fact",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    match emit_module(&pipeline, &options) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("Point"),
                "fail-closed message should name the orphan HashSet record: {msg}"
            );
        }
        other => panic!("Pending HashSet fact at pipeline finalize must FailClosed; got {other:?}"),
    }
}

#[test]
fn consistency_check_passes_when_facts_are_finalized() {
    let mut pipeline =
        pipeline_with_entry_terminator(Terminator::Return, vec![], vec![point_layout()]);
    let mut fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
    fact.state = HashMapLoweringFactState::Finalized;
    pipeline.hashmap_lowering_facts.push(fact);

    // validate_codegen_front exercises the same consistency-check path
    // without writing artefacts.  A Finalized fact with non-zero
    // size/align must pass.
    validate_codegen_front(&pipeline)
        .expect("Finalized layout fact with valid size/align must pass codegen-front check");
}

#[test]
fn consistency_check_passes_when_hashset_facts_are_finalized() {
    let mut pipeline =
        pipeline_with_entry_terminator(Terminator::Return, vec![], vec![point_layout()]);
    let mut fact = hashset_layout_fact("Point".to_string(), 16, 8);
    fact.state = HashMapLoweringFactState::Finalized;
    pipeline.hashset_lowering_facts.push(fact);

    // Mirrors the HashMap variant: a Finalized HashSet element-layout
    // fact with non-zero size/align must pass the lifecycle
    // consistency check (HashSet facts are promoted into synthetic
    // HashMap-shaped facts internally).
    validate_codegen_front(&pipeline).expect(
        "Finalized HashSet layout fact with valid size/align must pass codegen-front check",
    );
}

#[test]
fn consistency_check_passes_with_empty_fact_vectors() {
    // The default-empty case (no HashMap or HashSet sites in the
    // program) must not trip the consistency check.  Every other
    // codegen test exercises this path implicitly; assert it
    // explicitly here so future widening of the check has a canary.
    let pipeline = pipeline_with_entry_terminator(Terminator::Return, vec![], vec![point_layout()]);
    validate_codegen_front(&pipeline)
        .expect("empty fact vectors must not trip the lifecycle consistency check");
}

// ============================================================================
// attach_lowering_facts (driver-glue seam)
// ============================================================================

#[test]
fn attach_lowering_facts_clones_typecheck_output() {
    use hew_types::check::SpanKey;
    use hew_types::TypeCheckOutput;

    let mut tco = TypeCheckOutput::default();
    let fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
    tco.hashmap_layout_facts
        .insert(SpanKey { start: 0, end: 0 }, fact);
    let set_fact = hashset_layout_fact("Point".to_string(), 16, 8);
    tco.hashset_layout_facts
        .insert(SpanKey { start: 1, end: 1 }, set_fact);

    let mut pipeline =
        pipeline_with_entry_terminator(Terminator::Return, vec![], vec![point_layout()]);
    pipeline.attach_lowering_facts(&tco);
    assert_eq!(pipeline.hashmap_lowering_facts.len(), 1);
    assert_eq!(pipeline.hashset_lowering_facts.len(), 1);
    assert_eq!(
        pipeline.hashmap_lowering_facts[0].state,
        HashMapLoweringFactState::Pending
    );
    assert_eq!(
        pipeline.hashset_lowering_facts[0].state,
        HashMapLoweringFactState::Pending
    );
}

// ============================================================================
// W3.041b-i — operation-call lowering + Pending → Finalized fact walk
// ============================================================================

/// Helper types and builder for the operation-lowering tests.
fn point_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "Point".to_string(),
        args: vec![],
        builtin: None,
    }
}

fn hashmap_ty(key: ResolvedTy, val: ResolvedTy) -> ResolvedTy {
    ResolvedTy::Named {
        name: "HashMap".to_string(),
        args: vec![key, val],
        builtin: None,
    }
}

fn hashset_ty(elem: ResolvedTy) -> ResolvedTy {
    ResolvedTy::Named {
        name: "HashSet".to_string(),
        args: vec![elem],
        builtin: None,
    }
}

fn emit_ll(pipeline: IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-hashmap-ops-ll-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(&pipeline, &options)
        .unwrap_or_else(|e| panic!("hashmap op pipeline for {module_name} must emit: {e:?}"));
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(&ll_path).expect("read emitted .ll")
}

/// Pipeline with a Pending LayoutKey fact AND a matching call terminator.
/// The finalize walk must transition the fact so the consistency check passes.
fn op_pipeline(
    callee: &str,
    args: Vec<Place>,
    dest: Option<Place>,
    locals: Vec<ResolvedTy>,
    fact: hew_types::HashMapLoweringFact,
) -> IrPipeline {
    let mut pipeline = pipeline_with_entry_terminator(
        Terminator::Call {
            callee: callee.to_string(),
            args,
            dest,
            next: 1,
        },
        locals,
        vec![point_layout()],
    );
    pipeline.hashmap_lowering_facts.push(fact);
    pipeline
}

fn set_op_pipeline(
    callee: &str,
    args: Vec<Place>,
    dest: Option<Place>,
    locals: Vec<ResolvedTy>,
    fact: hew_types::HashSetLoweringFact,
) -> IrPipeline {
    let mut pipeline = pipeline_with_entry_terminator(
        Terminator::Call {
            callee: callee.to_string(),
            args,
            dest,
            next: 1,
        },
        locals,
        vec![point_layout()],
    );
    pipeline.hashset_lowering_facts.push(fact);
    pipeline
}

/// `hew_hashmap_insert_layout(map, key_ptr, val_ptr) -> bool`
///
/// Verifies:
/// (a) module compiles,
/// (b) IR contains `call i1 @hew_hashmap_insert_layout(ptr, ptr, ptr)`,
/// (c) Pending fact advances (pipeline would fail if still Pending).
#[test]
fn hashmap_layout_insert_emits_call_and_finalizes_fact() {
    // locals: [0] HashMap<Point,i64> handle, [1] Point key, [2] i64 val, [3] bool dest
    let locals = vec![
        hashmap_ty(point_ty(), ResolvedTy::I64),
        point_ty(),
        ResolvedTy::I64,
        ResolvedTy::Bool,
    ];
    let fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
    let pipeline = op_pipeline(
        "hew_hashmap_insert_layout",
        vec![Place::Local(0), Place::Local(1), Place::Local(2)],
        Some(Place::Local(3)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashmap_insert");
    assert!(
        ll.contains("declare i1 @hew_hashmap_insert_layout(ptr, ptr, ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call i1 @hew_hashmap_insert_layout"),
        "missing call site in:\n{ll}"
    );
}

/// `hew_hashmap_contains_key_layout(map, key_ptr) -> bool`
#[test]
fn hashmap_layout_contains_key_emits_call_and_finalizes_fact() {
    let locals = vec![
        hashmap_ty(point_ty(), ResolvedTy::I64),
        point_ty(),
        ResolvedTy::Bool,
    ];
    let fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
    let pipeline = op_pipeline(
        "hew_hashmap_contains_key_layout",
        vec![Place::Local(0), Place::Local(1)],
        Some(Place::Local(2)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashmap_contains_key");
    assert!(
        ll.contains("declare i1 @hew_hashmap_contains_key_layout(ptr, ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call i1 @hew_hashmap_contains_key_layout"),
        "missing call site in:\n{ll}"
    );
}

/// `hew_hashmap_remove_layout(map, key_ptr) -> bool`
#[test]
fn hashmap_layout_remove_emits_call_and_finalizes_fact() {
    let locals = vec![
        hashmap_ty(point_ty(), ResolvedTy::I64),
        point_ty(),
        ResolvedTy::Bool,
    ];
    let fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
    let pipeline = op_pipeline(
        "hew_hashmap_remove_layout",
        vec![Place::Local(0), Place::Local(1)],
        Some(Place::Local(2)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashmap_remove");
    assert!(
        ll.contains("declare i1 @hew_hashmap_remove_layout(ptr, ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call i1 @hew_hashmap_remove_layout"),
        "missing call site in:\n{ll}"
    );
}

/// `hew_hashmap_len_layout(map) -> i64`
#[test]
fn hashmap_layout_len_emits_call_and_finalizes_fact() {
    let locals = vec![hashmap_ty(point_ty(), ResolvedTy::I64), ResolvedTy::I64];
    let fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
    let pipeline = op_pipeline(
        "hew_hashmap_len_layout",
        vec![Place::Local(0)],
        Some(Place::Local(1)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashmap_len");
    assert!(
        ll.contains("declare i64 @hew_hashmap_len_layout(ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call i64 @hew_hashmap_len_layout"),
        "missing call site in:\n{ll}"
    );
}

/// `hew_hashset_insert_layout(set, elem_ptr) -> bool`
#[test]
fn hashset_layout_insert_emits_call_and_finalizes_fact() {
    let locals = vec![hashset_ty(point_ty()), point_ty(), ResolvedTy::Bool];
    let fact = hashset_layout_fact("Point".to_string(), 16, 8);
    let pipeline = set_op_pipeline(
        "hew_hashset_insert_layout",
        vec![Place::Local(0), Place::Local(1)],
        Some(Place::Local(2)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashset_insert");
    assert!(
        ll.contains("declare i1 @hew_hashset_insert_layout(ptr, ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call i1 @hew_hashset_insert_layout"),
        "missing call site in:\n{ll}"
    );
}

/// `hew_hashset_contains_layout(set, elem_ptr) -> bool`
#[test]
fn hashset_layout_contains_emits_call_and_finalizes_fact() {
    let locals = vec![hashset_ty(point_ty()), point_ty(), ResolvedTy::Bool];
    let fact = hashset_layout_fact("Point".to_string(), 16, 8);
    let pipeline = set_op_pipeline(
        "hew_hashset_contains_layout",
        vec![Place::Local(0), Place::Local(1)],
        Some(Place::Local(2)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashset_contains");
    assert!(
        ll.contains("declare i1 @hew_hashset_contains_layout(ptr, ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call i1 @hew_hashset_contains_layout"),
        "missing call site in:\n{ll}"
    );
}

/// Verify that a Pending fact WITHOUT a matching call site still fails closed.
/// The finalize walk only finalizes facts that have a matching operation call;
/// orphaned Pending facts must still produce a diagnostic.
#[test]
fn pending_fact_without_matching_call_still_fails_closed() {
    // Pipeline has a Pending fact but the terminator is Return (no call site).
    let mut pipeline =
        pipeline_with_entry_terminator(Terminator::Return, vec![], vec![point_layout()]);
    pipeline
        .hashmap_lowering_facts
        .push(hashmap_layout_key_fact(
            "Point".to_string(),
            16,
            8,
            HashMapValueType::I64,
        ));
    let tmp = std::env::temp_dir().join("hew-hashmap-ops-pending-no-call");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    match emit_module(
        &pipeline,
        &EmitOptions {
            module_name: "pending_no_call",
            out_dir: &tmp,
            native: false,
            wasm: false,
        },
    ) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("Point"),
                "fail-closed message must name the orphan record: {msg}"
            );
            assert!(
                msg.contains("codegen-abi-authority"),
                "fail-closed message must cite the LESSONS row: {msg}"
            );
        }
        other => panic!("Pending fact without matching call must FailClosed; got {other:?}"),
    }
}

/// Verify the finalize walk is idempotent: a fact that was already Finalized
/// by the test (as in `consistency_check_passes_when_facts_are_finalized`)
/// is not double-transitioned, and the pipeline still passes validation.
#[test]
fn finalize_walk_is_idempotent_on_already_finalized_fact() {
    // Locals include a HashMap handle so the finalize walk could theoretically
    // match, but the fact is already Finalized — should be a no-op.
    let locals = vec![
        hashmap_ty(point_ty(), ResolvedTy::I64),
        point_ty(),
        ResolvedTy::Bool,
    ];
    let mut fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
    fact.state = HashMapLoweringFactState::Finalized;
    let mut pipeline = pipeline_with_entry_terminator(
        Terminator::Call {
            callee: "hew_hashmap_contains_key_layout".to_string(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: Some(Place::Local(2)),
            next: 1,
        },
        locals,
        vec![point_layout()],
    );
    pipeline.hashmap_lowering_facts.push(fact);
    validate_codegen_front(&pipeline)
        .expect("already-Finalized fact with matching call must still pass");
}

// ============================================================================
// W3.041b-ii — HashMap::new / HashSet::new constructor lowering
// ============================================================================
//
// Constructor calls carry zero source-level args; the key/value layout
// descriptor pointers are synthesised by codegen from the dest local's
// `HashMap<K,V>` / `HashSet<T>` resolved type. The runtime ABI is:
//
//   hew_hashmap_new_with_layout(*const HewMapKeyLayout,
//                               *const HewMapValueLayout)
//     -> *mut HewLayoutHashMap
//   hew_hashset_new_with_layout(*const HewMapKeyLayout)
//     -> *mut HewLayoutHashSet
//
// The finalize walker (slice-i) was extended in slice-ii to recognise
// constructor call sites via the dest local's type, so a Pending fact
// authored at the checker boundary is finalised by either a constructor
// or any of the 8 op-call arms.

/// `hew_hashmap_new_with_layout() -> *mut HewLayoutHashMap`.
///
/// Verifies:
/// (a) module compiles,
/// (b) IR contains `call ptr @hew_hashmap_new_with_layout(ptr, ptr)`,
/// (c) the key + value layout globals are emitted,
/// (d) the Pending key fact advances via the constructor walker arm.
#[test]
fn hashmap_new_emits_constructor_call() {
    // locals: [0] HashMap<Point,i64> handle (the dest).
    let locals = vec![hashmap_ty(point_ty(), ResolvedTy::I64)];
    let fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
    let pipeline = op_pipeline(
        "hew_hashmap_new_with_layout",
        vec![],
        Some(Place::Local(0)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashmap_new");
    assert!(
        ll.contains("declare ptr @hew_hashmap_new_with_layout(ptr, ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call ptr @hew_hashmap_new_with_layout"),
        "missing call site in:\n{ll}"
    );
    // Layout descriptor globals: per `hashmap_key_layout_descriptor_ptr` /
    // `hashmap_value_layout_descriptor_ptr` the names embed
    // `<size>_<align>_<key>_plain` (key) and `<size>_<align>_plain`
    // (value). Point is `{i64, i64}` → size 16, align 8.
    assert!(
        ll.contains("@__hew_map_key_layout_16_8_"),
        "constructor must emit the HewMapKeyLayout-shaped global:\n{ll}"
    );
    assert!(
        ll.contains("@__hew_map_value_layout_8_8_plain"),
        "constructor must emit the HewMapValueLayout-shaped global \
         (i64 value → size 8, align 8):\n{ll}"
    );
}

/// `hew_hashset_new_with_layout() -> *mut HewLayoutHashSet`.
///
/// HashSet constructor takes a single elem layout pointer (the runtime
/// injects the ZST value layout internally per C-1c).
#[test]
fn hashset_new_emits_constructor_call() {
    let locals = vec![hashset_ty(point_ty())];
    let fact = hashset_layout_fact("Point".to_string(), 16, 8);
    let pipeline = set_op_pipeline(
        "hew_hashset_new_with_layout",
        vec![],
        Some(Place::Local(0)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashset_new");
    assert!(
        ll.contains("declare ptr @hew_hashset_new_with_layout(ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call ptr @hew_hashset_new_with_layout"),
        "missing call site in:\n{ll}"
    );
    assert!(
        ll.contains("@__hew_map_key_layout_16_8_"),
        "HashSet constructor must emit the elem (HewMapKeyLayout) global:\n{ll}"
    );
    // HashSet must NOT emit a value-layout global — the runtime injects
    // the ZST internally. Defence-in-depth against accidental val_layout
    // synthesis.
    assert!(
        !ll.contains("@__hew_map_value_layout_0_1_plain"),
        "HashSet constructor must not emit a ZST value-layout global \
         (runtime injects it):\n{ll}"
    );
}

/// Constructor with no Pending fact present: emission still succeeds.
/// Guards against the walker introducing a spurious requirement that
/// constructors *must* observe a Pending fact to be admissible.
#[test]
fn hashmap_new_without_pending_fact_still_emits() {
    let locals = vec![hashmap_ty(point_ty(), ResolvedTy::I64)];
    let pipeline = pipeline_with_entry_terminator(
        Terminator::Call {
            callee: "hew_hashmap_new_with_layout".to_string(),
            args: vec![],
            dest: Some(Place::Local(0)),
            next: 1,
        },
        locals,
        vec![point_layout()],
    );
    // No fact attached — constructor must still lower cleanly.
    assert!(pipeline.hashmap_lowering_facts.is_empty());
    let ll = emit_ll(pipeline, "hashmap_new_no_fact");
    assert!(
        ll.contains("call ptr @hew_hashmap_new_with_layout"),
        "constructor must emit even without a Pending fact:\n{ll}"
    );
}

// ============================================================================
// W3.041b-ii — drop-helper reroute coverage
// ============================================================================
//
// The actor-state drop helper for HashMap/HashSet field kinds was
// rerouted from the legacy string-keyed runtime free helpers
// (`hew_hashmap_free_impl` / `hew_hashset_free`) to the layout-keyed
// variants (`hew_hashmap_free_layout` / `hew_hashset_free_layout`).
// The primary IR-emission assertion lives in
// `state_clone_synthesis::state_clone_hashmap_and_hashset_route_to_runtime_helpers`
// (updated in the same commit). The tests below provide complementary
// coverage at the symbol-table level:
//
//   1. The wasm fail-closed substrate map already names both
//      `_free_layout` symbols (asserted by
//      `wasm_fail_closed_for_every_layout_op_symbol` above) so the
//      reroute target is wasm-gate-honoured.
//   2. These tests verify the codegen-side ABI declaration shape so the
//      drop dispatch and constructor agree on the handle's pointer type.

/// `hew_hashmap_free_layout` is one-arg-void; the constructor's return
/// type and the free's argument type must agree on `ptr` so the
/// sync-return drop path produces a sound IR.
#[test]
fn hashmap_drop_routes_to_runtime_drop_helper() {
    // Use a constructor call to force the module to declare the free
    // symbol's sibling (`_new_with_layout`) and confirm the layout-keyed
    // ABI is the constructor's return shape (`ptr`). The actor-state
    // drop helper at `drop_helper_for_kind` consumes the same `ptr`,
    // matching the runtime's `hew_hashmap_free_layout(*mut HewLayoutHashMap)`.
    let locals = vec![hashmap_ty(point_ty(), ResolvedTy::I64)];
    let fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
    let pipeline = op_pipeline(
        "hew_hashmap_new_with_layout",
        vec![],
        Some(Place::Local(0)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashmap_drop_routes");
    // Constructor returns ptr — the type the drop helper expects.
    assert!(
        ll.contains("declare ptr @hew_hashmap_new_with_layout(ptr, ptr)"),
        "constructor return type must be `ptr` to match \
         hew_hashmap_free_layout(ptr) reroute target:\n{ll}"
    );
}

#[test]
fn hashset_drop_routes_to_runtime_drop_helper() {
    let locals = vec![hashset_ty(point_ty())];
    let fact = hashset_layout_fact("Point".to_string(), 16, 8);
    let pipeline = set_op_pipeline(
        "hew_hashset_new_with_layout",
        vec![],
        Some(Place::Local(0)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashset_drop_routes");
    assert!(
        ll.contains("declare ptr @hew_hashset_new_with_layout(ptr)"),
        "constructor return type must be `ptr` to match \
         hew_hashset_free_layout(ptr) reroute target:\n{ll}"
    );
}

// ============================================================================
// W3.041b-ii — slice-i reviewer follow-up: HashSet remove/len op-call
// coverage parallels (closes the 6 HashSet ops × emission gap).
// ============================================================================

/// `hew_hashset_remove_layout(set, elem_ptr) -> bool` — mirrors
/// `hashset_layout_insert_emits_call_and_finalizes_fact`. Adding this
/// closes the slice-i reviewer note that 2 of the 4 HashSet op-call
/// arms (remove, len) lacked direct emission assertions even though
/// the dispatch + lowering arms covered them.
#[test]
fn hashset_remove_emits_runtime_call() {
    let locals = vec![hashset_ty(point_ty()), point_ty(), ResolvedTy::Bool];
    let fact = hashset_layout_fact("Point".to_string(), 16, 8);
    let pipeline = set_op_pipeline(
        "hew_hashset_remove_layout",
        vec![Place::Local(0), Place::Local(1)],
        Some(Place::Local(2)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashset_remove");
    assert!(
        ll.contains("declare i1 @hew_hashset_remove_layout(ptr, ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call i1 @hew_hashset_remove_layout"),
        "missing call site in:\n{ll}"
    );
}

/// `hew_hashset_len_layout(set) -> i64` — sibling closure of the
/// slice-i HashSet op-call coverage.
#[test]
fn hashset_len_emits_runtime_call() {
    let locals = vec![hashset_ty(point_ty()), ResolvedTy::I64];
    let fact = hashset_layout_fact("Point".to_string(), 16, 8);
    let pipeline = set_op_pipeline(
        "hew_hashset_len_layout",
        vec![Place::Local(0)],
        Some(Place::Local(1)),
        locals,
        fact,
    );
    let ll = emit_ll(pipeline, "hashset_len");
    assert!(
        ll.contains("declare i64 @hew_hashset_len_layout(ptr)"),
        "missing fn declaration in:\n{ll}"
    );
    assert!(
        ll.contains("call i64 @hew_hashset_len_layout"),
        "missing call site in:\n{ll}"
    );
}
