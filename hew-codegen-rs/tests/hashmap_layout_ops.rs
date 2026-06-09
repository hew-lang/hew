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
//!
//! Operation-call lowering itself (declaring + emitting calls to each of
//! the 13 runtime symbols at MIR `Terminator::Call` sites) is staged
//! behind MIR-side dispatch lowering and lands in the follow-on slice;
//! these tests assert the codegen-boundary contracts that the operation
//! lowering will rely on.

use hew_codegen_rs::{emit_module, validate_codegen_front, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, IrPipeline, RawMirFunction, RecordLayout, Terminator,
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
