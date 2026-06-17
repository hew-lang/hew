//! Targeted tests for `validate_codegen_front` and its `verify_pipeline` alias.
//!
//! Asserts the artifact-free codegen-front verifier contract:
//!
//! 1. A valid pipeline returns `Ok(())` without creating any files.
//! 2. A pipeline with an unsupported construct returns `Err(CodegenError)`
//!    without creating any files.
//!
//! The "no artifacts" guarantee is structural: the verifier accepts only an
//! `&IrPipeline` — no `out_dir`, no `EmitOptions` — so it has no path through
//! which to write files. The tests below verify the observable surface (return
//! value) and confirm the CWD is unchanged as a sanity check.

use hew_codegen_rs::{validate_codegen_front, verify_pipeline, CodegenError};
use hew_mir::{BasicBlock, DecisionFact, IrPipeline, RawMirFunction, Terminator};
use hew_mir::{Instr, Place};
use hew_types::ResolvedTy;

// ---------------------------------------------------------------------------
// Test fixtures
// ---------------------------------------------------------------------------

fn pipeline_const_42() -> IrPipeline {
    let return_ty = ResolvedTy::I64;
    let main = RawMirFunction {
        name: "main".to_string(),
        return_ty: return_ty.clone(),
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![return_ty.clone()],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(0),
                    value: 42,
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::<DecisionFact>::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
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

/// A pipeline whose function declares an array return type. Array returns are
/// outside the current spine subset; codegen rejects them with
/// `CodegenError::Unsupported`.
fn pipeline_unsupported_array_return() -> IrPipeline {
    let return_ty = ResolvedTy::Array(Box::new(ResolvedTy::I64), 2);
    let main = RawMirFunction {
        name: "main".to_string(),
        return_ty: return_ty.clone(),
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// `validate_codegen_front` succeeds for a valid spine-subset pipeline and
/// does not create any files. The CWD snapshot before/after is identical as a
/// belt-and-suspenders sanity check.
#[test]
fn validate_codegen_front_succeeds_for_valid_pipeline_and_creates_no_artifacts() {
    let pipeline = pipeline_const_42();

    // Snapshot CWD entries before the call.
    let cwd = std::env::current_dir().expect("cwd must be readable");
    let before: std::collections::BTreeSet<_> = std::fs::read_dir(&cwd)
        .expect("read cwd")
        .filter_map(|e| e.ok())
        .map(|e| e.file_name())
        .collect();

    let result = validate_codegen_front(&pipeline);

    // CWD must be unchanged.
    let after: std::collections::BTreeSet<_> = std::fs::read_dir(&cwd)
        .expect("read cwd")
        .filter_map(|e| e.ok())
        .map(|e| e.file_name())
        .collect();
    assert_eq!(
        before, after,
        "validate_codegen_front must not create files in the CWD"
    );

    // The call must succeed.
    result.expect("validate_codegen_front must return Ok(()) for a valid pipeline");
}

/// `validate_codegen_front` returns a structured `CodegenError` for a pipeline
/// with an unsupported construct (array return). No artifacts are created.
#[test]
fn validate_codegen_front_returns_codegen_error_for_unsupported_construct() {
    let pipeline = pipeline_unsupported_array_return();

    let cwd = std::env::current_dir().expect("cwd must be readable");
    let before: std::collections::BTreeSet<_> = std::fs::read_dir(&cwd)
        .expect("read cwd")
        .filter_map(|e| e.ok())
        .map(|e| e.file_name())
        .collect();

    let result = validate_codegen_front(&pipeline);

    // CWD must be unchanged even on error.
    let after: std::collections::BTreeSet<_> = std::fs::read_dir(&cwd)
        .expect("read cwd")
        .filter_map(|e| e.ok())
        .map(|e| e.file_name())
        .collect();
    assert_eq!(
        before, after,
        "validate_codegen_front must not create files in the CWD on error"
    );

    // The call must fail with a named CodegenError, not silently succeed.
    let err =
        result.expect_err("validate_codegen_front must return Err for an unsupported construct");
    assert!(
        matches!(err, CodegenError::Unsupported(_)),
        "array return must surface as CodegenError::Unsupported, got: {err:?}"
    );
}

/// `verify_pipeline` is callable concurrently: two threads each verify an
/// independent pipeline. Both must succeed (the codegen lock is per-call,
/// not per-process).
#[test]
fn verify_pipeline_is_safe_to_call_concurrently() {
    use std::thread;

    let t1 = thread::spawn(|| verify_pipeline(&pipeline_const_42()));
    let t2 = thread::spawn(|| verify_pipeline(&pipeline_const_42()));

    t1.join()
        .expect("thread 1 must not panic")
        .expect("thread 1 verify must succeed");
    t2.join()
        .expect("thread 2 must not panic")
        .expect("thread 2 verify must succeed");
}
