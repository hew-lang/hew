//! E4: codegen happy-path coverage for `Instr::CallRuntimeAbi` lowering
//! of `hew_duplex_pair` + `hew_duplex_send`, plus the `Instr::Drop`
//! ritual for `hew_duplex_close`.
//!
//! Every assertion targets the textual LLVM IR (`.ll`), so these tests pass
//! `native: false, wasm: false` and do not emit object files.
//!
//! LESSONS: boundary-fail-closed, exhaustive-traversal-and-lowering,
//! cleanup-all-exits, raii-null-after-move, dedup-semantic-boundary.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, Instr, IrPipeline, Place, RawMirFunction, RuntimeCall, Terminator,
};
use hew_types::ResolvedTy;

/// Build a minimal pipeline that exercises the three E4 codegen seams:
/// the `hew_duplex_pair` out-param call, the `hew_duplex_send` message
/// call, and the `hew_duplex_close` drop ritual. The locals layout
/// mirrors what `lower_duplex_pair` + `lower_duplex_send` produce:
///
/// - locals[0] = cap (i64)
/// - locals[1] = duplex A handle (`ResolvedTy::Named{name:"Duplex",..}` → `ptr`)
/// - locals[2] = duplex B handle (same)
/// - locals[3] = message payload (i64, holds the value to send)
/// - locals[4] = length (i64, holds the byte length 8)
fn duplex_exemplar_pipeline() -> IrPipeline {
    let duplex_ty = ResolvedTy::Named {
        name: "Duplex".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            // cap = 16
            Instr::ConstI64 {
                dest: Place::Local(0),
                value: 16,
            },
            // hew_duplex_pair(cap, cap, &A, &B)
            Instr::CallRuntimeAbi(
                RuntimeCall::new(
                    "hew_duplex_pair",
                    vec![
                        Place::Local(0),
                        Place::Local(0),
                        Place::DuplexHandle(1),
                        Place::DuplexHandle(2),
                    ],
                    None,
                )
                .expect("hew_duplex_pair is on the M2 allowlist"),
            ),
            // msg = 42
            Instr::ConstI64 {
                dest: Place::Local(3),
                value: 42,
            },
            // len = 8
            Instr::ConstI64 {
                dest: Place::Local(4),
                value: 8,
            },
            // hew_duplex_send(A, &msg, len)
            Instr::CallRuntimeAbi(
                RuntimeCall::new(
                    "hew_duplex_send",
                    vec![Place::DuplexHandle(1), Place::Local(3), Place::Local(4)],
                    None,
                )
                .expect("hew_duplex_send is on the M2 allowlist"),
            ),
            // Drop A then B (LIFO close-on-exit ritual).
            Instr::Drop {
                place: Place::DuplexHandle(2),
                ty: duplex_ty.clone(),
                drop_fn: Some(hew_mir::DropFnSpec::Runtime(
                    hew_types::runtime_call::RuntimeDropDescriptor::DuplexClose,
                )),
            },
            Instr::Drop {
                place: Place::DuplexHandle(1),
                ty: duplex_ty.clone(),
                drop_fn: Some(hew_mir::DropFnSpec::Runtime(
                    hew_types::runtime_call::RuntimeDropDescriptor::DuplexClose,
                )),
            },
            // Populate the return slot so the integer-only return
            // contract holds (the spine subset declares main->i64).
            Instr::ConstI64 {
                dest: Place::ReturnSlot,
                value: 0,
            },
        ],
        terminator: Terminator::Return,
    }];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![
                ResolvedTy::I64,   // 0 cap
                duplex_ty.clone(), // 1 Duplex A
                duplex_ty.clone(), // 2 Duplex B
                ResolvedTy::I64,   // 3 msg
                ResolvedTy::I64,   // 4 len
            ],
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: raw_blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            blocks: raw_blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(ExitPath::Return { block: 0 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
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
        lint_warnings: vec![],
        resource_record_close: vec![],
    }
}

fn emit_textual_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-e4-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts =
        emit_module(pipeline, &options).expect("E4 exemplar pipeline must emit successfully");
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(&ll_path).expect("read emitted .ll")
}

/// Verify the three runtime-ABI extern declarations are interned with
/// the C-ABI shapes pinned by `hew-runtime/src/duplex.rs` (and the E4
/// plan §D1-D3): `(i64, i64, ptr, ptr) -> i32` for pair,
/// `(ptr, ptr, i64) -> i32` for send, `(ptr) -> i32` for close.
/// Function-decl interning means each symbol appears exactly once even
/// though pair and send both run, and close runs twice (one per drop).
#[test]
fn duplex_runtime_decls_match_c_abi_shapes() {
    let pipeline = duplex_exemplar_pipeline();
    let ll = emit_textual_ll(&pipeline, "duplex_decls");
    assert!(
        ll.contains("declare i32 @hew_duplex_pair(i64, i64, ptr, ptr)"),
        "hew_duplex_pair declaration must match runtime C-ABI shape; got:\n{ll}"
    );
    assert!(
        ll.contains("declare i32 @hew_duplex_send(ptr, ptr, i64)"),
        "hew_duplex_send declaration must match runtime C-ABI shape; got:\n{ll}"
    );
    assert!(
        ll.contains("declare i32 @hew_duplex_close(ptr)"),
        "hew_duplex_close declaration must match runtime C-ABI shape; got:\n{ll}"
    );
    // Each declare appears exactly once — interning evidence.
    assert_eq!(
        ll.matches("declare i32 @hew_duplex_pair").count(),
        1,
        "hew_duplex_pair must be interned exactly once per module"
    );
    assert_eq!(
        ll.matches("declare i32 @hew_duplex_close").count(),
        1,
        "hew_duplex_close must be interned once even when called from \
         multiple Drop sites"
    );
}

/// Verify each runtime symbol is called the expected number of times.
/// One pair, one send, two close (one per drop). The exemplar discards
/// every i32 return (no `store i32` immediately after the calls).
#[test]
fn duplex_exemplar_emits_one_call_per_site() {
    let pipeline = duplex_exemplar_pipeline();
    let ll = emit_textual_ll(&pipeline, "duplex_call_sites");
    let pair_calls = ll.matches("call i32 @hew_duplex_pair").count();
    let send_calls = ll.matches("call i32 @hew_duplex_send").count();
    let close_calls = ll.matches("call i32 @hew_duplex_close").count();
    assert_eq!(
        pair_calls, 1,
        "exactly one hew_duplex_pair call expected; got {pair_calls}:\n{ll}"
    );
    assert_eq!(
        send_calls, 1,
        "exactly one hew_duplex_send call expected; got {send_calls}:\n{ll}"
    );
    assert_eq!(
        close_calls, 2,
        "exactly two hew_duplex_close calls expected (LIFO drop ritual \
         for handles A and B); got {close_calls}:\n{ll}"
    );
}

/// Verify the post-close alloca null-store fires (LESSONS
/// `raii-null-after-move`: a structurally-reachable double drop must
/// hit a null pointer at the codegen layer, not a stale handle). The
/// `ptr null` constant store appears alongside each close call.
#[test]
fn duplex_close_drop_zeroes_alloca() {
    let pipeline = duplex_exemplar_pipeline();
    let ll = emit_textual_ll(&pipeline, "duplex_null_after_close");
    let null_stores = ll.matches("store ptr null").count();
    assert!(
        null_stores >= 2,
        "expected at least two `store ptr null` instructions (one per \
         hew_duplex_close drop) to neutralise the alloca for defence-in-depth \
         per LESSONS raii-null-after-move; got {null_stores}:\n{ll}"
    );
}

/// Verify the module passes LLVM verification end-to-end. The
/// `emit_module` call already runs `verify` internally and propagates
/// `CodegenError::LlvmVerify` on failure, so reaching `Ok(_)` is the
/// gate. Wrap the assertion with an explicit check so a regression is
/// pinned to this test by name.
#[test]
fn duplex_exemplar_module_verifies() {
    let pipeline = duplex_exemplar_pipeline();
    let tmp = std::env::temp_dir().join("hew-e4-verify");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "duplex_verify",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    emit_module(&pipeline, &options).expect("the duplex exemplar must pass `Module::verify()`");
}
