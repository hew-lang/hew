//! Fail-closed gate for `bytes`-typed extern declarations.
//!
//! Any extern whose signature involves `bytes` (return or param) and whose
//! symbol is not in the appropriate ABI-shaping allowlist must produce
//! `CodegenError::FailClosed` naming the symbol. The gate fires in
//! `predeclare_extern_decls`, before any LLVM IR is emitted, so the error
//! points at the registration gap rather than surfacing as a miscompile.
//!
//! Three allowlists govern bytes ABI shaping:
//! - `is_bytes_triple_return_producer`   — Rust impl returns BytesTriple by value
//! - `is_bytes_by_pointer_consumer`      — Rust impl takes *const BytesTriple
//! - `is_bytes_struct_expansion_consumer` — Rust impl takes (ptr, offset, len)
//!
//! LESSONS applied: `boundary-fail-closed` (P0).

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, ExternDecl, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

// ---------------------------------------------------------------------------
// Pipeline builder
// ---------------------------------------------------------------------------

/// Build a minimal `IrPipeline` that declares `extern_decl` and calls it
/// in a function (dest = Some so the call is not a discard). The function
/// body returns Unit so the pipeline is well-formed beyond the extern decl.
fn pipeline_with_extern(
    fn_name: &str,
    return_ty: ResolvedTy,
    param_tys: Vec<ResolvedTy>,
) -> IrPipeline {
    // Two blocks: block 0 calls the extern into Local(0) then falls to block 1;
    // block 1 returns. For `-> bytes` we store the result in a local of type Bytes.
    // For `bytes` param we pass Local(0) as the arg (a bytes local).
    let has_return_value = !matches!(return_ty, ResolvedTy::Unit);
    let args: Vec<Place> = param_tys
        .iter()
        .enumerate()
        .map(|(i, _)| Place::Local(i as u32))
        .collect();
    let dest = if has_return_value {
        Some(Place::Local(param_tys.len() as u32))
    } else {
        None
    };

    let mut locals = param_tys.clone();
    if has_return_value {
        locals.push(return_ty.clone());
    }

    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: fn_name.to_string(),
                builtin: None,
                args,
                dest,
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
        name: "probe".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: locals.clone(),
        blocks: blocks.clone(),
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
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
        diagnostics: vec![],
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
            param_tys,
            return_ty,
        }],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: vec![],
        user_clone_record_seeds: vec![],
    }
}

fn emit_options(label: &str) -> EmitOptions<'static> {
    let tmp = std::env::temp_dir().join(format!("hew-bytes-extern-fc-{label}"));
    // Leak PathBuf + str so they satisfy 'static; acceptable in test code.
    let out_dir: &'static std::path::Path = Box::leak(Box::new(tmp)).as_path();
    let module_name: &'static str = Box::leak(format!("bytes_fc_{label}").into_boxed_str());
    EmitOptions {
        module_name,
        out_dir,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    }
}

// ---------------------------------------------------------------------------
// Fail-closed: unknown `-> bytes` return
// ---------------------------------------------------------------------------

/// An extern that returns `bytes` and is not in `is_bytes_triple_return_producer`
/// must produce `CodegenError::FailClosed` naming the symbol and pointing at
/// the registry. This is the primary regression guard for Stage 2 of the
/// bytes-extern-fail-closed hardening.
#[test]
fn unknown_bytes_return_extern_fails_closed_naming_symbol() {
    let pipeline =
        pipeline_with_extern("probe_unregistered_bytes_return", ResolvedTy::Bytes, vec![]);
    let result = emit_module(&pipeline, &emit_options("bytes_return"));
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("probe_unregistered_bytes_return"),
                "FailClosed must name the offending symbol; got: {msg}"
            );
            assert!(
                msg.contains("is_bytes_triple_return_producer"),
                "FailClosed must point at the return-producer registry; got: {msg}"
            );
        }
        Err(other) => panic!(
            "expected CodegenError::FailClosed for unregistered bytes-return extern, got {other:?}"
        ),
        Ok(_) => panic!(
            "unregistered bytes-return extern must fail closed; got Ok(_) — \
             boundary-fail-closed regression"
        ),
    }
}

// ---------------------------------------------------------------------------
// Fail-closed: unknown `bytes` param
// ---------------------------------------------------------------------------

/// An extern that takes a `bytes` param and is not in any bytes-param registry
/// must produce `CodegenError::FailClosed` naming the symbol. This guards the
/// param-side of the ABI boundary symmetrically with the return side.
#[test]
fn unknown_bytes_param_extern_fails_closed_naming_symbol() {
    let pipeline = pipeline_with_extern(
        "probe_unregistered_bytes_param",
        ResolvedTy::I32,
        vec![ResolvedTy::Bytes],
    );
    let result = emit_module(&pipeline, &emit_options("bytes_param"));
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("probe_unregistered_bytes_param"),
                "FailClosed must name the offending symbol; got: {msg}"
            );
            // Gate points at either the pointer-consumer or struct-expansion list.
            assert!(
                msg.contains("is_bytes_by_pointer_consumer")
                    || msg.contains("is_bytes_struct_expansion_consumer"),
                "FailClosed must point at one of the bytes-param registries; got: {msg}"
            );
        }
        Err(other) => panic!(
            "expected CodegenError::FailClosed for unregistered bytes-param extern, got {other:?}"
        ),
        Ok(_) => panic!(
            "unregistered bytes-param extern must fail closed; got Ok(_) — \
             boundary-fail-closed regression"
        ),
    }
}

// ---------------------------------------------------------------------------
// Regression: a known bytes-return producer must still compile
// ---------------------------------------------------------------------------

/// `hew_sha256_hew` is in `is_bytes_struct_expansion_consumer` (bytes param)
/// and the bytes value it receives is handled by field-expansion ABI.
/// Verify it does NOT trigger the fail-closed gate — the gate must only reject
/// symbols that are in neither list.
///
/// We test the param side: `hew_sha256_hew(data: bytes) -> bytes`. Because
/// `hew_sha256_hew` is in `is_bytes_triple_return_producer` (return side) AND
/// `is_bytes_struct_expansion_consumer` (param side), both guards pass.
#[test]
fn known_bytes_extern_does_not_fail_closed() {
    // hew_sha256_hew takes bytes and returns bytes — both sides are registered.
    let pipeline =
        pipeline_with_extern("hew_sha256_hew", ResolvedTy::Bytes, vec![ResolvedTy::Bytes]);
    let result = emit_module(&pipeline, &emit_options("sha256_known"));
    // We do not assert Ok(_) because downstream codegen may still fail on
    // unrelated shape/lowering details; we only assert the gate did NOT fire.
    match result {
        Err(CodegenError::FailClosed(msg))
            if msg.contains("is_bytes_triple_return_producer")
                || msg.contains("is_bytes_by_pointer_consumer")
                || msg.contains("is_bytes_struct_expansion_consumer") =>
        {
            panic!(
                "hew_sha256_hew is a known registered extern; the bytes ABI \
                 gate must not fire for it. Got FailClosed: {msg}"
            );
        }
        // Any other outcome (Ok or a different error) is acceptable here.
        _ => {}
    }
}
