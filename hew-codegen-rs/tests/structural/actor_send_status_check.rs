//! Regression test: `Terminator::Send` codegen must check the i32 return
//! value of `hew_actor_send_by_id` and trap on nonzero status.
//!
//! Before this fix, the call result was discarded and execution fell through
//! to `next_bb` unconditionally — a fail-closed violation. Any failure mode
//! (recipient gone, mailbox full, remote partition rejection) was silently
//! ignored. Codegen now emits:
//!
//!   1. A call to `hew_actor_send_by_id` with the SSA result bound.
//!   2. An `icmp ne i32 <status>, 0` comparison.
//!   3. A conditional branch: zero → next_bb, nonzero → send_fail_bb.
//!   4. In `send_fail_bb`: `hew_trap_with_code(206)` + `llvm.trap` +
//!      `unreachable`, so the supervisor sees `ExitReason::ActorSendFailed`.
//!
//! LESSONS: boundary-fail-closed (P0), fail-closed-not-pretend.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::{BuiltinType, ResolvedTy};
use std::path::Path;

/// Build a minimal `IrPipeline` containing one function whose only block
/// terminates with `Terminator::Send`. The function has:
///
/// - `local 0` — `LocalPid<Unit>` (opaque `ptr`):
///   the canonical actor-local handle used as the send target.
/// - `local 1` — `ResolvedTy::Unit`: the payload value. `actor_payload_ptr_size`
///   short-circuits to `(null, 0)` for Unit, so no payload alloca is needed.
/// - Block 0: `Terminator::Send { actor: ActorHandle(0), msg_type: 1, value: Local(1), next: 1 }`
/// - Block 1: `Terminator::Return` (the success continuation; reachable only
///   when send status == 0).
fn send_status_pipeline() -> IrPipeline {
    // `LocalPid<Unit>` is the canonical actor-dispatch-local handle.
    let actor_ty =
        ResolvedTy::named_builtin("LocalPid", BuiltinType::LocalPid, vec![ResolvedTy::Unit]);
    let raw_blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Send {
                actor: Place::ActorHandle(0),
                msg_type: 1,
                value: Place::Local(1),
                next: 1,
                alias_mode: hew_mir::SendAliasMode::Copy,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];

    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            source_origin: hew_mir::SourceOrigin::Unknown,
            name: "send_probe".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![actor_ty, ResolvedTy::Unit],
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
            name: "send_probe".to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: raw_blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "send_probe".to_string(),
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
        resource_opaque_close: vec![],
    }
}

/// Codegen must emit an `icmp ne i32` check on the `hew_actor_send_by_id`
/// return value and branch to a fail-closed trap block on nonzero status.
/// The IR must contain all five structural markers.
#[test]
fn send_terminator_checks_return_status_and_traps_on_failure() {
    let pipeline = send_status_pipeline();
    let tmp = std::env::temp_dir().join("hew-actor-send-status-check");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "send_status_probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("send_status pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    let ll = std::fs::read_to_string(ll_path).expect("read emitted .ll");

    // 1. The send call must be present.
    assert!(
        ll.contains("@hew_actor_send_by_id"),
        "emitted IR must call hew_actor_send_by_id; got:\n{ll}"
    );
    // 2. The return status must be compared against zero.
    assert!(
        ll.contains("icmp ne i32"),
        "emitted IR must compare send status (icmp ne i32); got:\n{ll}"
    );
    // 3. A conditional branch on the comparison result must be present.
    assert!(
        ll.contains("br i1"),
        "emitted IR must have a conditional branch on the send status (br i1); got:\n{ll}"
    );
    // 4. The fail-closed path must route through hew_trap_with_code(206).
    let needle = "@hew_trap_with_code(i32 206)";
    assert!(
        ll.contains(needle),
        "emitted IR must call hew_trap_with_code with code 206 on send failure; \
         got:\n{ll}"
    );
    // 5. The fallback non-actor terminator must be present.
    assert!(
        ll.contains("@llvm.trap"),
        "send fail block must still emit llvm.trap as the fallback terminator; got:\n{ll}"
    );
    assert!(
        ll.contains("unreachable"),
        "send fail block must end with unreachable; got:\n{ll}"
    );
}
