//! Fail-closed posture for `bytes`-typed extern declarations.
//!
//! A `-> bytes` extern's return crosses the C-ABI boundary as a
//! `#[repr(C)] BytesTriple`, classified per target by the R5 aggregate ABI
//! classifier (`abi_class::declare_aggregate_return`): RegisterPair on
//! SysV/AAPCS, sret (Indirect) on MSVC/wasm32. There is NO per-symbol
//! return-producer allowlist — every `-> bytes` extern is classified, and the
//! classifier itself fails closed on an unmodelled target rather than guessing.
//!
//! A `bytes` PARAM crosses the C-ABI boundary by ONE unconditional rule: it is
//! declared as a plain `ptr` (the caller's `{ptr, i32, i32}` BytesTriple alloca
//! address). There is no per-symbol param allowlist and no "unclassified bytes
//! param" state — a `bytes` param is always `ptr`, so the old fail-closed param
//! gate is gone. The runtime reads the triple through the pointer; a 16-byte
//! triple by value is not reliably ABI-portable in any argument position.
//!
//! LESSONS applied: `boundary-fail-closed` (P0),
//! `aggregate-abi-by-classifier-not-per-symbol` (P0),
//! `inline-collection-param-aliases-by-pointer` (P0).

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
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "probe".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: locals.clone(),
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
            param_tys,
            return_ty,
            provenance: hew_hir::ExternProvenance::Root,
            runtime_capability: None,
            malloc_string_return: false,
        }],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: vec![],
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
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
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    }
}

/// Emit `pipeline` and return the textual LLVM IR of the emitted module.
fn emit_ir(pipeline: &IrPipeline, label: &str) -> String {
    let artefacts = emit_module(pipeline, &emit_options(label))
        .unwrap_or_else(|error| panic!("emit_module must succeed for {label}: {error:?}"));
    let ll_path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

// ---------------------------------------------------------------------------
// Classified `-> bytes` return: no per-symbol gate, classifier is the backstop
// ---------------------------------------------------------------------------

/// An arbitrary `-> bytes` extern — one that was never on any allowlist —
/// declares a classified aggregate return and emits successfully on a modelled
/// host target. This is the deletion proof for the per-symbol
/// `is_bytes_triple_return_producer` registry: adding a `-> bytes` runtime
/// symbol no longer requires an allowlist edit; the classifier composes for
/// free. (LESSONS: aggregate-abi-by-classifier-not-per-symbol.)
#[test]
fn arbitrary_bytes_return_extern_is_classified_not_gated() {
    let pipeline = pipeline_with_extern("probe_new_bytes_return", ResolvedTy::Bytes, vec![]);
    emit_module(&pipeline, &emit_options("bytes_return")).unwrap_or_else(|error| {
        panic!(
            "a `-> bytes` extern must classify and emit on a modelled target \
             (no per-symbol allowlist gate); got {error:?}"
        )
    });
}

/// The fail-closed posture is preserved at the classifier: a `-> bytes` extern
/// targeting a triple whose aggregate ABI is not modelled fails closed rather
/// than guessing an ABI. This is the NEGATIVE guard that replaces the old
/// per-symbol registry rejection. (LESSONS: boundary-fail-closed.)
#[test]
fn bytes_return_extern_fails_closed_on_unmodelled_target() {
    let pipeline = pipeline_with_extern("probe_new_bytes_return", ResolvedTy::Bytes, vec![]);
    let mut opts = emit_options("bytes_return_unmodelled");
    // 32-bit x86 (i686) has no modelled aggregate ABI in `classify_aggregate`:
    // `is_sysv_or_aapcs` admits only `x86_64-`/`amd64-`/`aarch64-`/`arm64-`, so a
    // `-> bytes` return here reaches the fail-closed arm. i686 is deliberately
    // chosen over an exotic triple like sparc64: the X86 LLVM backend (which the
    // modelled win64/SysV cases already exercise) covers i686 on EVERY platform's
    // LLVM build, so `Target::from_triple` succeeds and codegen reaches the
    // `classify_aggregate` fail-closed path deterministically — rather than
    // failing earlier with `TargetSetup` on a host whose LLVM lacks the exotic
    // backend. i686's `size_t` width IS modelled (`runtime_size_ty`'s 32-bit
    // allowlist), so `verify_runtime_size_width` passes and the aggregate
    // classifier is the sole backstop that trips.
    opts.target_triple = Some("i686-unknown-linux-gnu");
    match emit_module(&pipeline, &opts) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("not modelled") || msg.contains("i686"),
                "FailClosed must explain the unmodelled aggregate ABI; got: {msg}"
            );
        }
        Err(other) => {
            panic!("expected CodegenError::FailClosed for an unmodelled target, got {other:?}")
        }
        Ok(_) => panic!(
            "a `-> bytes` extern on an unmodelled target must fail closed; got Ok(_) — \
             boundary-fail-closed regression"
        ),
    }
}

// ---------------------------------------------------------------------------
// `bytes` param: one unconditional rule — declared `ptr`, never gated
// ---------------------------------------------------------------------------

/// An arbitrary `bytes`-param extern — one that was never on any allowlist —
/// declares its `bytes` parameter as a plain `ptr` and emits successfully. There
/// is no longer any "unclassified bytes param" fail-closed state: the by-pointer
/// rule is unconditional. This is the deletion proof for the two consumer
/// allowlists (`is_bytes_by_pointer_consumer` /
/// `is_bytes_struct_expansion_consumer`).
#[test]
fn arbitrary_bytes_param_extern_declares_pointer_not_gated() {
    let pipeline = pipeline_with_extern(
        "probe_unregistered_bytes_param",
        ResolvedTy::I32,
        vec![ResolvedTy::Bytes],
    );
    let ir = emit_ir(&pipeline, "bytes_param");
    // The single `bytes` parameter must be declared as `ptr` (the caller's
    // BytesTriple alloca address), NOT as an expanded `{ptr, i32, i32}` struct or
    // a field-expansion triple. Teeth: assert the exact declared signature.
    assert!(
        ir.contains("declare i32 @probe_unregistered_bytes_param(ptr)"),
        "a `bytes` param must be declared as a single `ptr`;\ngot:\n{ir}"
    );
}

// ---------------------------------------------------------------------------
// Regression: known bytes producers/consumers still compile
// ---------------------------------------------------------------------------

/// `hew_sha256_hew(data: bytes) -> bytes` must still emit: its `bytes` param
/// declares `ptr` (by-pointer rule) and its `-> bytes` return is classified by
/// the R5 aggregate ABI classifier. Neither side is gated by an allowlist.
fn assert_known_bytes_extern_does_not_fail_closed(
    symbol: &str,
    return_ty: ResolvedTy,
    param_tys: Vec<ResolvedTy>,
) {
    let pipeline = pipeline_with_extern(symbol, return_ty, param_tys);
    emit_module(&pipeline, &emit_options(symbol))
        .unwrap_or_else(|error| panic!("{symbol} must emit successfully: {error:?}"));
}

#[test]
fn known_bytes_extern_does_not_fail_closed() {
    // hew_sha256_hew takes bytes (declared `ptr`) and returns bytes (classified).
    assert_known_bytes_extern_does_not_fail_closed(
        "hew_sha256_hew",
        ResolvedTy::Bytes,
        vec![ResolvedTy::Bytes],
    );
}

#[test]
fn quic_bytes_externs_do_not_fail_closed() {
    // QUIC receive wrappers produce a classified BytesTriple, and send wrappers
    // consume a `bytes` param declared as `ptr`. Every direction emits cleanly.
    assert_known_bytes_extern_does_not_fail_closed(
        "hew_quic_stream_recv",
        ResolvedTy::Bytes,
        vec![ResolvedTy::I64],
    );
    assert_known_bytes_extern_does_not_fail_closed(
        "hew_quic_stream_recv_timeout_hew",
        ResolvedTy::Bytes,
        vec![ResolvedTy::I64, ResolvedTy::I32],
    );
    assert_known_bytes_extern_does_not_fail_closed(
        "hew_quic_stream_send",
        ResolvedTy::I32,
        vec![ResolvedTy::I64, ResolvedTy::Bytes],
    );
    assert_known_bytes_extern_does_not_fail_closed(
        "hew_quic_stream_send_timeout_hew",
        ResolvedTy::I32,
        vec![ResolvedTy::I64, ResolvedTy::Bytes, ResolvedTy::I32],
    );
}
